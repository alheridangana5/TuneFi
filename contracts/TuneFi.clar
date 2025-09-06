(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SONG_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_SONG_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NO_SHARES_OWNED (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_INVALID_PERCENTAGE (err u107))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u108))
(define-constant ERR_PROPOSAL_EXPIRED (err u109))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u110))
(define-constant ERR_ALREADY_VOTED (err u111))
(define-constant ERR_VOTING_PERIOD_NOT_ENDED (err u112))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u113))
(define-constant ERR_INSUFFICIENT_VOTING_POWER (err u114))
(define-constant ERR_LISTING_NOT_FOUND (err u115))
(define-constant ERR_LISTING_EXPIRED (err u116))
(define-constant ERR_LISTING_ALREADY_EXISTS (err u117))
(define-constant ERR_CANNOT_BUY_OWN_LISTING (err u118))
(define-constant ERR_LISTING_INACTIVE (err u119))
(define-constant ERR_INSUFFICIENT_SHARES_FOR_LISTING (err u120))
(define-constant ERR_AUCTION_NOT_FOUND (err u121))
(define-constant ERR_AUCTION_ENDED (err u122))
(define-constant ERR_AUCTION_NOT_ENDED (err u123))
(define-constant ERR_BID_TOO_LOW (err u124))
(define-constant ERR_AUCTION_ALREADY_EXISTS (err u125))
(define-constant ERR_AUCTION_INACTIVE (err u126))
(define-constant ERR_CANNOT_BID_ON_OWN_AUCTION (err u127))
(define-constant ERR_AUCTION_ALREADY_FINALIZED (err u128))

(define-data-var next-song-id uint u1)
(define-data-var platform-fee-percentage uint u5)
(define-data-var next-proposal-id uint u1)
(define-data-var minimum-proposal-threshold uint u100)
(define-data-var voting-period-blocks uint u1440)
(define-data-var next-listing-id uint u1)
(define-data-var marketplace-fee-percentage uint u2)
(define-data-var next-auction-id uint u1)
(define-data-var auction-fee-percentage uint u2)

(define-map songs
  { song-id: uint }
  {
    title: (string-ascii 100),
    artist: (string-ascii 50),
    total-shares: uint,
    price-per-share: uint,
    shares-sold: uint,
    total-royalties-collected: uint,
    creator: principal,
    active: bool
  }
)

(define-map user-shares
  { song-id: uint, investor: principal }
  { shares-owned: uint }
)

(define-map royalty-distributions
  { song-id: uint, distribution-id: uint }
  {
    total-amount: uint,
    per-share-amount: uint,
    timestamp: uint
  }
)

(define-map user-royalty-claims
  { song-id: uint, investor: principal, distribution-id: uint }
  { claimed: bool }
)

(define-map song-distribution-counter
  { song-id: uint }
  { counter: uint }
)

(define-map proposals
  { proposal-id: uint }
  {
    song-id: uint,
    title: (string-ascii 200),
    description: (string-ascii 500),
    proposal-type: (string-ascii 50),
    proposer: principal,
    voting-start-block: uint,
    voting-end-block: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-power: uint,
    executed: bool,
    active: bool,
    budget-amount: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    voting-power: uint,
    vote-direction: bool,
    timestamp: uint
  }
)

(define-map proposal-executions
  { proposal-id: uint }
  {
    executed-by: principal,
    execution-timestamp: uint,
    execution-successful: bool
  }
)

(define-map marketplace-listings
  { listing-id: uint }
  {
    song-id: uint,
    seller: principal,
    shares-amount: uint,
    price-per-share: uint,
    total-price: uint,
    created-at: uint,
    expires-at: uint,
    active: bool
  }
)

(define-map song-price-history
  { song-id: uint, timestamp: uint }
  {
    average-price: uint,
    volume: uint,
    high-price: uint,
    low-price: uint
  }
)

(define-map listing-offers
  { listing-id: uint, buyer: principal }
  {
    offer-amount: uint,
    expires-at: uint,
    active: bool
  }
)

;; Auction data maps
(define-map royalty-auctions
  { auction-id: uint }
  {
    song-id: uint,
    artist: principal,
    shares-amount: uint,
    minimum-bid: uint,
    current-highest-bid: uint,
    highest-bidder: (optional principal),
    start-block: uint,
    end-block: uint,
    active: bool,
    finalized: bool
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  {
    bid-amount: uint,
    timestamp: uint,
    active: bool
  }
)

(define-public (create-song (title (string-ascii 100)) (artist (string-ascii 50)) (total-shares uint) (price-per-share uint))
  (let
    (
      (song-id (var-get next-song-id))
    )
    (asserts! (> total-shares u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-share u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? songs { song-id: song-id })) ERR_SONG_ALREADY_EXISTS)
    
    (map-set songs
      { song-id: song-id }
      {
        title: title,
        artist: artist,
        total-shares: total-shares,
        price-per-share: price-per-share,
        shares-sold: u0,
        total-royalties-collected: u0,
        creator: tx-sender,
        active: true
      }
    )
    
    (map-set song-distribution-counter
      { song-id: song-id }
      { counter: u0 }
    )
    
    (var-set next-song-id (+ song-id u1))
    (ok song-id)
  )
)

(define-public (buy-shares (song-id uint) (shares-to-buy uint))
  (let
    (
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (current-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: song-id, investor: tx-sender }))))
      (total-cost (* shares-to-buy (get price-per-share song-data)))
      (new-shares-sold (+ (get shares-sold song-data) shares-to-buy))
    )
    (asserts! (get active song-data) ERR_UNAUTHORIZED)
    (asserts! (> shares-to-buy u0) ERR_INVALID_AMOUNT)
    (asserts! (<= new-shares-sold (get total-shares song-data)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? total-cost tx-sender (get creator song-data)))
    
    (map-set user-shares
      { song-id: song-id, investor: tx-sender }
      { shares-owned: (+ current-shares shares-to-buy) }
    )
    
    (map-set songs
      { song-id: song-id }
      (merge song-data { shares-sold: new-shares-sold })
    )
    
    (ok true)
  )
)

(define-public (distribute-royalties (song-id uint) (total-amount uint))
  (let
    (
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (distribution-counter-data (unwrap! (map-get? song-distribution-counter { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (distribution-id (+ (get counter distribution-counter-data) u1))
      (platform-fee (/ (* total-amount (var-get platform-fee-percentage)) u100))
      (distributable-amount (- total-amount platform-fee))
      (per-share-amount (if (> (get shares-sold song-data) u0)
                          (/ distributable-amount (get shares-sold song-data))
                          u0))
    )
    (asserts! (is-eq tx-sender (get creator song-data)) ERR_UNAUTHORIZED)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> (get shares-sold song-data) u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set royalty-distributions
      { song-id: song-id, distribution-id: distribution-id }
      {
        total-amount: distributable-amount,
        per-share-amount: per-share-amount,
        timestamp: stacks-block-height
      }
    )
    
    (map-set song-distribution-counter
      { song-id: song-id }
      { counter: distribution-id }
    )
    
    (map-set songs
      { song-id: song-id }
      (merge song-data { total-royalties-collected: (+ (get total-royalties-collected song-data) distributable-amount) })
    )
    
    (ok distribution-id)
  )
)

(define-public (claim-royalties (song-id uint) (distribution-id uint))
  (let
    (
      (user-shares-data (unwrap! (map-get? user-shares { song-id: song-id, investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (distribution-data (unwrap! (map-get? royalty-distributions { song-id: song-id, distribution-id: distribution-id }) ERR_SONG_NOT_FOUND))
      (already-claimed (default-to false (get claimed (map-get? user-royalty-claims { song-id: song-id, investor: tx-sender, distribution-id: distribution-id }))))
      (user-royalty-amount (* (get shares-owned user-shares-data) (get per-share-amount distribution-data)))
    )
    (asserts! (> (get shares-owned user-shares-data) u0) ERR_NO_SHARES_OWNED)
    (asserts! (not already-claimed) ERR_UNAUTHORIZED)
    (asserts! (> user-royalty-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (as-contract (stx-transfer? user-royalty-amount tx-sender tx-sender)))
    
    (map-set user-royalty-claims
      { song-id: song-id, investor: tx-sender, distribution-id: distribution-id }
      { claimed: true }
    )
    
    (ok user-royalty-amount)
  )
)

(define-public (transfer-shares (song-id uint) (recipient principal) (shares-to-transfer uint))
  (let
    (
      (sender-shares-data (unwrap! (map-get? user-shares { song-id: song-id, investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (recipient-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: song-id, investor: recipient }))))
      (sender-new-shares (- (get shares-owned sender-shares-data) shares-to-transfer))
    )
    (asserts! (>= (get shares-owned sender-shares-data) shares-to-transfer) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> shares-to-transfer u0) ERR_INVALID_AMOUNT)
    
    (map-set user-shares
      { song-id: song-id, investor: tx-sender }
      { shares-owned: sender-new-shares }
    )
    
    (map-set user-shares
      { song-id: song-id, investor: recipient }
      { shares-owned: (+ recipient-shares shares-to-transfer) }
    )
    
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u20) ERR_INVALID_PERCENTAGE)
    (var-set platform-fee-percentage new-fee-percentage)
    (ok true)
  )
)

(define-public (deactivate-song (song-id uint))
  (let
    (
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator song-data)) ERR_UNAUTHORIZED)
    
    (map-set songs
      { song-id: song-id }
      (merge song-data { active: false })
    )
    
    (ok true)
  )
)

(define-public (create-listing (song-id uint) (shares-amount uint) (price-per-share uint) (duration-blocks uint))
  (let
    (
      (listing-id (var-get next-listing-id))
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (seller-shares-data (unwrap! (map-get? user-shares { song-id: song-id, investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (total-price (* shares-amount price-per-share))
      (expires-at (+ stacks-block-height duration-blocks))
    )
    (asserts! (get active song-data) ERR_SONG_NOT_FOUND)
    (asserts! (> shares-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-share u0) ERR_INVALID_AMOUNT)
    (asserts! (<= shares-amount (get shares-owned seller-shares-data)) ERR_INSUFFICIENT_SHARES_FOR_LISTING)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (<= duration-blocks u10080) ERR_INVALID_AMOUNT)
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      {
        song-id: song-id,
        seller: tx-sender,
        shares-amount: shares-amount,
        price-per-share: price-per-share,
        total-price: total-price,
        created-at: stacks-block-height,
        expires-at: expires-at,
        active: true
      }
    )
    
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (buy-from-listing (listing-id uint))
  (let
    (
      (listing-data (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (seller-shares-data (unwrap! (map-get? user-shares { song-id: (get song-id listing-data), investor: (get seller listing-data) }) ERR_NO_SHARES_OWNED))
      (buyer-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: (get song-id listing-data), investor: tx-sender }))))
      (marketplace-fee (/ (* (get total-price listing-data) (var-get marketplace-fee-percentage)) u100))
      (seller-amount (- (get total-price listing-data) marketplace-fee))
      (new-seller-shares (- (get shares-owned seller-shares-data) (get shares-amount listing-data)))
    )
    (asserts! (get active listing-data) ERR_LISTING_INACTIVE)
    (asserts! (<= stacks-block-height (get expires-at listing-data)) ERR_LISTING_EXPIRED)
    (asserts! (not (is-eq tx-sender (get seller listing-data))) ERR_CANNOT_BUY_OWN_LISTING)
    (asserts! (>= (get shares-owned seller-shares-data) (get shares-amount listing-data)) ERR_INSUFFICIENT_SHARES_FOR_LISTING)
    
    (try! (stx-transfer? (get total-price listing-data) tx-sender (get seller listing-data)))
    
    (map-set user-shares
      { song-id: (get song-id listing-data), investor: (get seller listing-data) }
      { shares-owned: new-seller-shares }
    )
    
    (map-set user-shares
      { song-id: (get song-id listing-data), investor: tx-sender }
      { shares-owned: (+ buyer-shares (get shares-amount listing-data)) }
    )
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      (merge listing-data { active: false })
    )
    
    (let ((price-update-result (update-price-history (get song-id listing-data) (get price-per-share listing-data) (get shares-amount listing-data))))
      (ok true)
    )
  )
)

(define-public (cancel-listing (listing-id uint))
  (let
    (
      (listing-data (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller listing-data)) ERR_UNAUTHORIZED)
    (asserts! (get active listing-data) ERR_LISTING_INACTIVE)
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      (merge listing-data { active: false })
    )
    
    (ok true)
  )
)

(define-public (create-offer (listing-id uint) (offer-amount uint) (duration-blocks uint))
  (let
    (
      (listing-data (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (expires-at (+ stacks-block-height duration-blocks))
    )
    (asserts! (get active listing-data) ERR_LISTING_INACTIVE)
    (asserts! (<= stacks-block-height (get expires-at listing-data)) ERR_LISTING_EXPIRED)
    (asserts! (not (is-eq tx-sender (get seller listing-data))) ERR_CANNOT_BUY_OWN_LISTING)
    (asserts! (> offer-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (<= duration-blocks u1440) ERR_INVALID_AMOUNT)
    
    (map-set listing-offers
      { listing-id: listing-id, buyer: tx-sender }
      {
        offer-amount: offer-amount,
        expires-at: expires-at,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (accept-offer (listing-id uint) (buyer principal))
  (let
    (
      (listing-data (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (offer-data (unwrap! (map-get? listing-offers { listing-id: listing-id, buyer: buyer }) ERR_LISTING_NOT_FOUND))
      (seller-shares-data (unwrap! (map-get? user-shares { song-id: (get song-id listing-data), investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (buyer-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: (get song-id listing-data), investor: buyer }))))
      (marketplace-fee (/ (* (get offer-amount offer-data) (var-get marketplace-fee-percentage)) u100))
      (seller-amount (- (get offer-amount offer-data) marketplace-fee))
      (new-seller-shares (- (get shares-owned seller-shares-data) (get shares-amount listing-data)))
    )
    (asserts! (is-eq tx-sender (get seller listing-data)) ERR_UNAUTHORIZED)
    (asserts! (get active listing-data) ERR_LISTING_INACTIVE)
    (asserts! (get active offer-data) ERR_LISTING_INACTIVE)
    (asserts! (<= stacks-block-height (get expires-at offer-data)) ERR_LISTING_EXPIRED)
    (asserts! (>= (get shares-owned seller-shares-data) (get shares-amount listing-data)) ERR_INSUFFICIENT_SHARES_FOR_LISTING)
    
    (try! (stx-transfer? (get offer-amount offer-data) buyer tx-sender))
    
    (map-set user-shares
      { song-id: (get song-id listing-data), investor: tx-sender }
      { shares-owned: new-seller-shares }
    )
    
    (map-set user-shares
      { song-id: (get song-id listing-data), investor: buyer }
      { shares-owned: (+ buyer-shares (get shares-amount listing-data)) }
    )
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      (merge listing-data { active: false })
    )
    
    (map-set listing-offers
      { listing-id: listing-id, buyer: buyer }
      (merge offer-data { active: false })
    )
    
    (let ((price-update-result (update-price-history (get song-id listing-data) (/ (get offer-amount offer-data) (get shares-amount listing-data)) (get shares-amount listing-data))))
      (ok true)
    )
  )
)

(define-public (set-marketplace-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u10) ERR_INVALID_PERCENTAGE)
    (var-set marketplace-fee-percentage new-fee-percentage)
    (ok true)
  )
)

;; Royalty Auction Functions
(define-public (start-auction (song-id uint) (shares-amount uint) (minimum-bid uint) (duration-blocks uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (artist-shares-data (unwrap! (map-get? user-shares { song-id: song-id, investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (end-block (+ stacks-block-height duration-blocks))
    )
    ;; Validate auction parameters
    (asserts! (is-eq tx-sender (get creator song-data)) ERR_UNAUTHORIZED)
    (asserts! (get active song-data) ERR_SONG_NOT_FOUND)
    (asserts! (> shares-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= shares-amount (get shares-owned artist-shares-data)) ERR_INSUFFICIENT_SHARES_FOR_LISTING)
    (asserts! (> minimum-bid u0) ERR_INVALID_AMOUNT)
    (asserts! (>= duration-blocks u144) ERR_INVALID_AMOUNT)  ;; Minimum 1 day (144 blocks)
    (asserts! (<= duration-blocks u2016) ERR_INVALID_AMOUNT) ;; Maximum 2 weeks (2016 blocks)
    
    ;; Create the auction
    (map-set royalty-auctions
      { auction-id: auction-id }
      {
        song-id: song-id,
        artist: tx-sender,
        shares-amount: shares-amount,
        minimum-bid: minimum-bid,
        current-highest-bid: u0,
        highest-bidder: none,
        start-block: stacks-block-height,
        end-block: end-block,
        active: true,
        finalized: false
      }
    )
    
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction-data (unwrap! (map-get? royalty-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (previous-highest-bid (get current-highest-bid auction-data))
      (previous-highest-bidder (get highest-bidder auction-data))
      (required-bid (if (> previous-highest-bid u0) (+ previous-highest-bid u1) (get minimum-bid auction-data)))
    )
    ;; Validate bid parameters
    (asserts! (get active auction-data) ERR_AUCTION_INACTIVE)
    (asserts! (<= stacks-block-height (get end-block auction-data)) ERR_AUCTION_ENDED)
    (asserts! (>= stacks-block-height (get start-block auction-data)) ERR_AUCTION_INACTIVE)
    (asserts! (not (is-eq tx-sender (get artist auction-data))) ERR_CANNOT_BID_ON_OWN_AUCTION)
    (asserts! (>= bid-amount required-bid) ERR_BID_TOO_LOW)
    
    ;; Transfer bid amount to contract
    (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
    
    ;; Refund previous highest bidder if any
    (match previous-highest-bidder
      previous-bidder
        (try! (as-contract (stx-transfer? previous-highest-bid tx-sender previous-bidder)))
      true
    )
    
    ;; Record the new bid
    (map-set auction-bids
      { auction-id: auction-id, bidder: tx-sender }
      {
        bid-amount: bid-amount,
        timestamp: stacks-block-height,
        active: true
      }
    )
    
    ;; Update auction with new highest bid
    (map-set royalty-auctions
      { auction-id: auction-id }
      (merge auction-data {
        current-highest-bid: bid-amount,
        highest-bidder: (some tx-sender)
      })
    )
    
    (ok true)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let
    (
      (auction-data (unwrap! (map-get? royalty-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (song-data (unwrap! (map-get? songs { song-id: (get song-id auction-data) }) ERR_SONG_NOT_FOUND))
      (artist-shares-data (unwrap! (map-get? user-shares { song-id: (get song-id auction-data), investor: (get artist auction-data) }) ERR_NO_SHARES_OWNED))
      (highest-bidder (get highest-bidder auction-data))
      (winning-bid (get current-highest-bid auction-data))
      (auction-fee (/ (* winning-bid (var-get auction-fee-percentage)) u100))
      (artist-amount (- winning-bid auction-fee))
      (shares-amount (get shares-amount auction-data))
      (new-artist-shares (- (get shares-owned artist-shares-data) shares-amount))
    )
    ;; Validate auction can be finalized
    (asserts! (get active auction-data) ERR_AUCTION_INACTIVE)
    (asserts! (> stacks-block-height (get end-block auction-data)) ERR_AUCTION_NOT_ENDED)
    (asserts! (not (get finalized auction-data)) ERR_AUCTION_ALREADY_FINALIZED)
    
    ;; Check if there's a winning bidder and process accordingly
    (if (is-some highest-bidder)
      (let ((winner (unwrap-panic highest-bidder)))
        (begin
          ;; Transfer payment to artist (minus fee)
          (try! (as-contract (stx-transfer? artist-amount tx-sender (get artist auction-data))))
          
          ;; Transfer shares to winner
          (let ((winner-current-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: (get song-id auction-data), investor: winner })))))
            (map-set user-shares
              { song-id: (get song-id auction-data), investor: (get artist auction-data) }
              { shares-owned: new-artist-shares }
            )
            
            (map-set user-shares
              { song-id: (get song-id auction-data), investor: winner }
              { shares-owned: (+ winner-current-shares shares-amount) }
            )
          )
          
          ;; Update price history with the winning bid price
          (let ((final-price (/ winning-bid shares-amount)))
            (unwrap! (update-price-history (get song-id auction-data) final-price shares-amount) ERR_TRANSFER_FAILED)
          )
        )
      )
      ;; No bidders - auction ends with no sale
      true
    )
    
    ;; Mark auction as finalized
    (map-set royalty-auctions
      { auction-id: auction-id }
      (merge auction-data { finalized: true, active: false })
    )
    
    (ok true)
  )
)

(define-private (update-price-history (song-id uint) (price uint) (volume uint))
  (let
    (
      (current-block stacks-block-height)
      (rounded-block (- current-block (mod current-block u144)))
      (existing-data (map-get? song-price-history { song-id: song-id, timestamp: rounded-block }))
    )
    (match existing-data
      existing-info
        (map-set song-price-history
          { song-id: song-id, timestamp: rounded-block }
          {
            average-price: (/ (+ (* (get average-price existing-info) (get volume existing-info)) (* price volume)) (+ (get volume existing-info) volume)),
            volume: (+ (get volume existing-info) volume),
            high-price: (if (> price (get high-price existing-info)) price (get high-price existing-info)),
            low-price: (if (< price (get low-price existing-info)) price (get low-price existing-info))
          }
        )
      (map-set song-price-history
        { song-id: song-id, timestamp: rounded-block }
        {
          average-price: price,
          volume: volume,
          high-price: price,
          low-price: price
        }
      )
    )
    (ok true)
  )
)

(define-read-only (get-song-info (song-id uint))
  (map-get? songs { song-id: song-id })
)

(define-read-only (get-user-shares (song-id uint) (investor principal))
  (map-get? user-shares { song-id: song-id, investor: investor })
)

(define-read-only (get-distribution-info (song-id uint) (distribution-id uint))
  (map-get? royalty-distributions { song-id: song-id, distribution-id: distribution-id })
)

(define-read-only (get-claim-status (song-id uint) (investor principal) (distribution-id uint))
  (map-get? user-royalty-claims { song-id: song-id, investor: investor, distribution-id: distribution-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (get-next-song-id)
  (var-get next-song-id)
)

(define-read-only (calculate-user-royalties (song-id uint) (distribution-id uint) (investor principal))
  (let
    (
      (user-shares-data (map-get? user-shares { song-id: song-id, investor: investor }))
      (distribution-data (map-get? royalty-distributions { song-id: song-id, distribution-id: distribution-id }))
    )
    (match user-shares-data
      shares-info
        (match distribution-data
          dist-info
            (some (* (get shares-owned shares-info) (get per-share-amount dist-info)))
          none)
      none)
  )
)

(define-read-only (get-song-distribution-count (song-id uint))
  (map-get? song-distribution-counter { song-id: song-id })
)

(define-public (create-proposal (song-id uint) (title (string-ascii 200)) (description (string-ascii 500)) (proposal-type (string-ascii 50)) (budget-amount uint))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (song-data (unwrap! (map-get? songs { song-id: song-id }) ERR_SONG_NOT_FOUND))
      (proposer-shares (default-to u0 (get shares-owned (map-get? user-shares { song-id: song-id, investor: tx-sender }))))
      (voting-end-block (+ stacks-block-height (var-get voting-period-blocks)))
    )
    (asserts! (>= proposer-shares (var-get minimum-proposal-threshold)) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (get active song-data) ERR_SONG_NOT_FOUND)
    (asserts! (> (len title) u0) ERR_INVALID_AMOUNT)
    (asserts! (> (len description) u0) ERR_INVALID_AMOUNT)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        song-id: song-id,
        title: title,
        description: description,
        proposal-type: proposal-type,
        proposer: tx-sender,
        voting-start-block: stacks-block-height,
        voting-end-block: voting-end-block,
        votes-for: u0,
        votes-against: u0,
        total-voting-power: (get shares-sold song-data),
        executed: false,
        active: true,
        budget-amount: budget-amount
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (voter-shares-data (unwrap! (map-get? user-shares { song-id: (get song-id proposal-data), investor: tx-sender }) ERR_NO_SHARES_OWNED))
      (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
      (voting-power (get shares-owned voter-shares-data))
      (current-votes-for (get votes-for proposal-data))
      (current-votes-against (get votes-against proposal-data))
    )
    (asserts! (get active proposal-data) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get voting-end-block proposal-data)) ERR_PROPOSAL_EXPIRED)
    (asserts! (>= stacks-block-height (get voting-start-block proposal-data)) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (> voting-power u0) ERR_NO_SHARES_OWNED)
    
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        voting-power: voting-power,
        vote-direction: vote-for,
        timestamp: stacks-block-height
      }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data {
        votes-for: (if vote-for (+ current-votes-for voting-power) current-votes-for),
        votes-against: (if vote-for current-votes-against (+ current-votes-against voting-power))
      })
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (song-data (unwrap! (map-get? songs { song-id: (get song-id proposal-data) }) ERR_SONG_NOT_FOUND))
      (votes-for (get votes-for proposal-data))
      (votes-against (get votes-against proposal-data))
      (total-votes (+ votes-for votes-against))
      (majority-threshold (/ (get total-voting-power proposal-data) u2))
    )
    (asserts! (get active proposal-data) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (> stacks-block-height (get voting-end-block proposal-data)) ERR_VOTING_PERIOD_NOT_ENDED)
    (asserts! (not (get executed proposal-data)) ERR_PROPOSAL_ALREADY_EXECUTED)
    (asserts! (> votes-for majority-threshold) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (> votes-for votes-against) ERR_INSUFFICIENT_VOTING_POWER)
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data { executed: true, active: false })
    )
    
    (map-set proposal-executions
      { proposal-id: proposal-id }
      {
        executed-by: tx-sender,
        execution-timestamp: stacks-block-height,
        execution-successful: true
      }
    )
    
    (ok true)
  )
)

(define-public (cancel-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer proposal-data)) ERR_UNAUTHORIZED)
    (asserts! (get active proposal-data) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get voting-end-block proposal-data)) ERR_PROPOSAL_EXPIRED)
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data { active: false })
    )
    
    (ok true)
  )
)

(define-public (set-minimum-proposal-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-threshold u0) ERR_INVALID_AMOUNT)
    (var-set minimum-proposal-threshold new-threshold)
    (ok true)
  )
)

(define-public (set-voting-period (new-period-blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (>= new-period-blocks u144) ERR_INVALID_AMOUNT)
    (asserts! (<= new-period-blocks u10080) ERR_INVALID_AMOUNT)
    (var-set voting-period-blocks new-period-blocks)
    (ok true)
  )
)

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote-info (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-execution-info (proposal-id uint))
  (map-get? proposal-executions { proposal-id: proposal-id })
)

(define-read-only (get-voting-power (song-id uint) (investor principal))
  (default-to u0 (get shares-owned (map-get? user-shares { song-id: song-id, investor: investor })))
)

(define-read-only (get-proposal-results (proposal-id uint))
  (let
    (
      (proposal-data (map-get? proposals { proposal-id: proposal-id }))
    )
    (match proposal-data
      proposal-info
        (some {
          votes-for: (get votes-for proposal-info),
          votes-against: (get votes-against proposal-info),
          total-voting-power: (get total-voting-power proposal-info),
          participation-rate: (/ (* (+ (get votes-for proposal-info) (get votes-against proposal-info)) u100) (get total-voting-power proposal-info)),
          status: (if (get executed proposal-info) "executed" (if (get active proposal-info) "active" "cancelled"))
        })
      none)
  )
)

(define-read-only (get-governance-settings)
  {
    minimum-proposal-threshold: (var-get minimum-proposal-threshold),
    voting-period-blocks: (var-get voting-period-blocks),
    next-proposal-id: (var-get next-proposal-id)
  }
)

(define-read-only (calculate-proposal-outcome (proposal-id uint))
  (let
    (
      (proposal-data (map-get? proposals { proposal-id: proposal-id }))
    )
    (match proposal-data
      proposal-info
        (let
          (
            (votes-for (get votes-for proposal-info))
            (votes-against (get votes-against proposal-info))
            (total-voting-power (get total-voting-power proposal-info))
            (majority-threshold (/ total-voting-power u2))
          )
          (some {
            will-pass: (and (> votes-for majority-threshold) (> votes-for votes-against)),
            votes-needed: (if (> votes-for votes-against) u0 (+ (- votes-against votes-for) u1)),
            majority-reached: (> votes-for majority-threshold)
          })
        )
      none)
  )
)

(define-read-only (get-listing-info (listing-id uint))
  (map-get? marketplace-listings { listing-id: listing-id })
)

(define-read-only (get-song-price-history (song-id uint) (timestamp uint))
  (map-get? song-price-history { song-id: song-id, timestamp: timestamp })
)

(define-read-only (get-offer-info (listing-id uint) (buyer principal))
  (map-get? listing-offers { listing-id: listing-id, buyer: buyer })
)

(define-read-only (get-marketplace-settings)
  {
    marketplace-fee-percentage: (var-get marketplace-fee-percentage),
    next-listing-id: (var-get next-listing-id)
  }
)

(define-read-only (calculate-listing-value (listing-id uint))
  (let
    (
      (listing-data (map-get? marketplace-listings { listing-id: listing-id }))
    )
    (match listing-data
      listing-info
        (some {
          total-value: (get total-price listing-info),
          price-per-share: (get price-per-share listing-info),
          shares-amount: (get shares-amount listing-info),
          active: (get active listing-info),
          expires-at: (get expires-at listing-info)
        })
      none)
  )
)

(define-read-only (get-song-market-data (song-id uint))
  (let
    (
      (current-block stacks-block-height)
      (rounded-block (- current-block (mod current-block u144)))
      (recent-data (map-get? song-price-history { song-id: song-id, timestamp: rounded-block }))
    )
    (match recent-data
      price-info
        (some {
          current-average-price: (get average-price price-info),
          daily-volume: (get volume price-info),
          daily-high: (get high-price price-info),
          daily-low: (get low-price price-info),
          timestamp: rounded-block
        })
      none)
  )
)

;; Auction read-only functions
(define-read-only (get-auction-info (auction-id uint))
  (map-get? royalty-auctions { auction-id: auction-id })
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-auction-status (auction-id uint))
  (let
    (
      (auction-data (map-get? royalty-auctions { auction-id: auction-id }))
    )
    (match auction-data
      auction-info
        (some {
          active: (get active auction-info),
          finalized: (get finalized auction-info),
          ended: (> stacks-block-height (get end-block auction-info)),
          time-remaining: (if (> (get end-block auction-info) stacks-block-height) 
                           (- (get end-block auction-info) stacks-block-height) 
                           u0),
          has-bids: (> (get current-highest-bid auction-info) u0)
        })
      none)
  )
)

(define-read-only (get-auction-settings)
  {
    auction-fee-percentage: (var-get auction-fee-percentage),
    next-auction-id: (var-get next-auction-id)
  }
)

(define-read-only (calculate-auction-value (auction-id uint))
  (let
    (
      (auction-data (map-get? royalty-auctions { auction-id: auction-id }))
    )
    (match auction-data
      auction-info
        (let
          (
            (current-bid (get current-highest-bid auction-info))
            (shares (get shares-amount auction-info))
          )
          (some {
            current-highest-bid: current-bid,
            current-price-per-share: (if (> current-bid u0) (/ current-bid shares) u0),
            minimum-bid: (get minimum-bid auction-info),
            shares-amount: shares,
            highest-bidder: (get highest-bidder auction-info)
          })
        )
      none)
  )
)

(define-public (set-auction-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u10) ERR_INVALID_PERCENTAGE)
    (var-set auction-fee-percentage new-fee-percentage)
    (ok true)
  )
)



