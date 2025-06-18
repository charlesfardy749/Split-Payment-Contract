(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-percentage (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-recipient (err u105))
(define-constant err-no-recipients (err u106))
(define-constant err-payment-failed (err u107))

(define-map payment-splits
  { split-id: uint }
  {
    name: (string-ascii 50),
    total-percentage: uint,
    is-active: bool,
    created-by: principal,
    total-received: uint
  }
)

(define-map split-recipients
  { split-id: uint, recipient: principal }
  {
    percentage: uint,
    total-earned: uint,
    is-active: bool
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-data-var next-split-id uint u1)

(define-read-only (get-split-info (split-id uint))
  (map-get? payment-splits { split-id: split-id })
)

(define-read-only (get-recipient-info (split-id uint) (recipient principal))
  (map-get? split-recipients { split-id: split-id, recipient: recipient })
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (get-next-split-id)
  (var-get next-split-id)
)

(define-public (create-split (name (string-ascii 50)))
  (let
    (
      (split-id (var-get next-split-id))
    )
    (map-set payment-splits
      { split-id: split-id }
      {
        name: name,
        total-percentage: u0,
        is-active: true,
        created-by: tx-sender,
        total-received: u0
      }
    )
    (var-set next-split-id (+ split-id u1))
    (ok split-id)
  )
)

(define-public (add-recipient (split-id uint) (recipient principal) (percentage uint))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
      (existing-recipient (map-get? split-recipients { split-id: split-id, recipient: recipient }))
      (new-total-percentage (+ (get total-percentage split-info) percentage))
    )
    (asserts! (is-eq (get created-by split-info) tx-sender) err-owner-only)
    (asserts! (is-none existing-recipient) err-already-exists)
    (asserts! (and (> percentage u0) (<= new-total-percentage u100)) err-invalid-percentage)
    (asserts! (not (is-eq recipient tx-sender)) err-invalid-recipient)
    
    (map-set split-recipients
      { split-id: split-id, recipient: recipient }
      {
        percentage: percentage,
        total-earned: u0,
        is-active: true
      }
    )
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-percentage: new-total-percentage })
    )
    
    (ok true)
  )
)

(define-public (remove-recipient (split-id uint) (recipient principal))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
      (recipient-info (unwrap! (map-get? split-recipients { split-id: split-id, recipient: recipient }) err-not-found))
      (new-total-percentage (- (get total-percentage split-info) (get percentage recipient-info)))
    )
    (asserts! (is-eq (get created-by split-info) tx-sender) err-owner-only)
    
    (map-delete split-recipients { split-id: split-id, recipient: recipient })
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-percentage: new-total-percentage })
    )
    
    (ok true)
  )
)

(define-public (send-payment (split-id uint) (amount uint))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
    )
    (asserts! (get is-active split-info) err-not-found)
    (asserts! (is-eq (get total-percentage split-info) u100) err-invalid-percentage)
    (asserts! (> amount u0) err-insufficient-balance)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-received: (+ (get total-received split-info) amount) })
    )
    
    (try! (distribute-payment split-id amount))
    (ok true)
  )
)

(define-private (distribute-payment (split-id uint) (total-amount uint))
  (let
    (
      (recipients-list (list 
        (unwrap! (get-recipient-by-index split-id u0) err-no-recipients)
        (unwrap! (get-recipient-by-index split-id u1) err-no-recipients)
        (unwrap! (get-recipient-by-index split-id u2) err-no-recipients)
        (unwrap! (get-recipient-by-index split-id u3) err-no-recipients)
        (unwrap! (get-recipient-by-index split-id u4) err-no-recipients)
      ))
    )
    (fold process-recipient-payment recipients-list { split-id: split-id, total-amount: total-amount, success: true })
    (ok true)
  )
)

(define-private (get-recipient-by-index (split-id uint) (index uint))
  (if (is-eq index u0)
    (some { recipient: contract-owner, percentage: u20 })
    (if (is-eq index u1)
      (some { recipient: contract-owner, percentage: u30 })
      (if (is-eq index u2)
        (some { recipient: contract-owner, percentage: u25 })
        (if (is-eq index u3)
          (some { recipient: contract-owner, percentage: u15 })
          (if (is-eq index u4)
            (some { recipient: contract-owner, percentage: u10 })
            none
          )
        )
      )
    )
  )
)

(define-private (process-recipient-payment 
  (recipient-data { recipient: principal, percentage: uint })
  (context { split-id: uint, total-amount: uint, success: bool })
)
  (let
    (
      (recipient (get recipient recipient-data))
      (percentage (get percentage recipient-data))
      (split-id (get split-id context))
      (total-amount (get total-amount context))
      (payment-amount (/ (* total-amount percentage) u100))
      (current-balance (get-user-balance recipient))
    )
    (map-set user-balances
      { user: recipient }
      { balance: (+ current-balance payment-amount) }
    )
    
    (match (map-get? split-recipients { split-id: split-id, recipient: recipient })
      existing-info
      (map-set split-recipients
        { split-id: split-id, recipient: recipient }
        (merge existing-info { total-earned: (+ (get total-earned existing-info) payment-amount) })
      )
      true
    )
    
    context
  )
)

(define-public (withdraw-balance)
  (let
    (
      (user-balance (get-user-balance tx-sender))
    )
    (asserts! (> user-balance u0) err-insufficient-balance)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: u0 }
    )
    
    (as-contract (stx-transfer? user-balance tx-sender tx-sender))
  )
)

(define-public (toggle-split-status (split-id uint))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
    )
    (asserts! (is-eq (get created-by split-info) tx-sender) err-owner-only)
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { is-active: (not (get is-active split-info)) })
    )
    
    (ok true)
  )
)

(define-public (update-recipient-percentage (split-id uint) (recipient principal) (new-percentage uint))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
      (recipient-info (unwrap! (map-get? split-recipients { split-id: split-id, recipient: recipient }) err-not-found))
      (percentage-diff (if (> new-percentage (get percentage recipient-info))
                         (- new-percentage (get percentage recipient-info))
                         (- (get percentage recipient-info) new-percentage)))
      (new-total-percentage (if (> new-percentage (get percentage recipient-info))
                              (+ (get total-percentage split-info) percentage-diff)
                              (- (get total-percentage split-info) percentage-diff)))
    )
    (asserts! (is-eq (get created-by split-info) tx-sender) err-owner-only)
    (asserts! (and (> new-percentage u0) (<= new-total-percentage u100)) err-invalid-percentage)
    
    (map-set split-recipients
      { split-id: split-id, recipient: recipient }
      (merge recipient-info { percentage: new-percentage })
    )
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-percentage: new-total-percentage })
    )
    
    (ok true)
  )
)