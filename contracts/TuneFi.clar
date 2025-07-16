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

(define-data-var next-song-id uint u1)
(define-data-var platform-fee-percentage uint u5)
(define-data-var next-proposal-id uint u1)
(define-data-var minimum-proposal-threshold uint u100)
(define-data-var voting-period-blocks uint u1440)

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
