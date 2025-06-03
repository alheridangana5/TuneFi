(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SONG_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_SONG_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NO_SHARES_OWNED (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_INVALID_PERCENTAGE (err u107))

(define-data-var next-song-id uint u1)
(define-data-var platform-fee-percentage uint u5)

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