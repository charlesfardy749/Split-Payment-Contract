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

(define-map split-recipient-list
  { split-id: uint }
  { recipients: (list 20 principal) }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-map payment-history
  { split-id: uint, payment-id: uint }
  {
    amount: uint,
    timestamp: uint,
    sender: principal,
    recipient-count: uint
  }
)

(define-map split-analytics
  { split-id: uint }
  {
    total-payments: uint,
    payment-count: uint,
    largest-payment: uint,
    last-payment-time: uint
  }
)

(define-data-var next-split-id uint u1)
(define-data-var temp-recipient principal tx-sender)
(define-data-var next-payment-id uint u1)

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

(define-read-only (get-split-recipients (split-id uint))
  (default-to (list) (get recipients (map-get? split-recipient-list { split-id: split-id })))
)

(define-read-only (get-payment-history (split-id uint) (payment-id uint))
  (map-get? payment-history { split-id: split-id, payment-id: payment-id })
)

(define-read-only (get-split-analytics (split-id uint))
  (map-get? split-analytics { split-id: split-id })
)

(define-read-only (get-average-payment (split-id uint))
  (match (map-get? split-analytics { split-id: split-id })
    analytics
    (if (> (get payment-count analytics) u0)
      (some (/ (get total-payments analytics) (get payment-count analytics)))
      (some u0)
    )
    none
  )
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
    (map-set split-recipient-list
      { split-id: split-id }
      { recipients: (list) }
    )
    (map-set split-analytics
      { split-id: split-id }
      {
        total-payments: u0,
        payment-count: u0,
        largest-payment: u0,
        last-payment-time: u0
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
    
    (let
      (
        (current-recipients (default-to (list) (get recipients (map-get? split-recipient-list { split-id: split-id }))))
      )
      (map-set split-recipient-list
        { split-id: split-id }
        { recipients: (unwrap! (as-max-len? (append current-recipients recipient) u20) err-invalid-recipient) }
      )
    )
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-percentage: new-total-percentage })
    )
    
    (ok true)
  )
)

(define-private (is-not-target-recipient (current-recipient principal))
  (not (is-eq current-recipient (var-get temp-recipient)))
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
    
    (var-set temp-recipient recipient)
    (let
      (
        (current-recipients (default-to (list) (get recipients (map-get? split-recipient-list { split-id: split-id }))))
        (updated-recipients (filter is-not-target-recipient current-recipients))
      )
      (map-set split-recipient-list
        { split-id: split-id }
        { recipients: updated-recipients }
      )
    )
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-percentage: new-total-percentage })
    )
    
    (ok true)
  )
)

(define-private (record-payment-history (split-id uint) (amount uint) (recipient-count uint))
  (let
    (
      (payment-id (var-get next-payment-id))
      (current-analytics (default-to 
        { total-payments: u0, payment-count: u0, largest-payment: u0, last-payment-time: u0 }
        (map-get? split-analytics { split-id: split-id })
      ))
      (new-total (+ (get total-payments current-analytics) amount))
      (new-count (+ (get payment-count current-analytics) u1))
      (new-largest (if (> amount (get largest-payment current-analytics)) amount (get largest-payment current-analytics)))
    )
    (map-set payment-history
      { split-id: split-id, payment-id: payment-id }
      {
        amount: amount,
        timestamp: stacks-block-height,
        sender: tx-sender,
        recipient-count: recipient-count
      }
    )
    (map-set split-analytics
      { split-id: split-id }
      {
        total-payments: new-total,
        payment-count: new-count,
        largest-payment: new-largest,
        last-payment-time: stacks-block-height
      }
    )
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

(define-public (send-payment (split-id uint) (amount uint))
  (let
    (
      (split-info (unwrap! (map-get? payment-splits { split-id: split-id }) err-not-found))
      (recipients-list (default-to (list) (get recipients (map-get? split-recipient-list { split-id: split-id }))))
      (recipient-count (len recipients-list))
    )
    (asserts! (get is-active split-info) err-not-found)
    (asserts! (is-eq (get total-percentage split-info) u100) err-invalid-percentage)
    (asserts! (> amount u0) err-insufficient-balance)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set payment-splits
      { split-id: split-id }
      (merge split-info { total-received: (+ (get total-received split-info) amount) })
    )
    
    (unwrap-panic (record-payment-history split-id amount recipient-count))
    (unwrap-panic (distribute-payment split-id amount))
    (ok true)
  )
)

(define-private (distribute-payment (split-id uint) (total-amount uint))
  (let
    (
      (recipients-list (default-to (list) (get recipients (map-get? split-recipient-list { split-id: split-id }))))
    )
    (fold process-recipient-distribution recipients-list { split-id: split-id, total-amount: total-amount, success: true })
    (ok true)
  )
)

(define-private (process-recipient-distribution 
  (recipient principal)
  (context { split-id: uint, total-amount: uint, success: bool })
)
  (let
    (
      (split-id (get split-id context))
      (total-amount (get total-amount context))
      (recipient-info (map-get? split-recipients { split-id: split-id, recipient: recipient }))
    )
    (match recipient-info
      info
      (let
        (
          (percentage (get percentage info))
          (payment-amount (/ (* total-amount percentage) u100))
          (current-balance (get-user-balance recipient))
        )
        (map-set user-balances
          { user: recipient }
          { balance: (+ current-balance payment-amount) }
        )
        (map-set split-recipients
          { split-id: split-id, recipient: recipient }
          (merge info { total-earned: (+ (get total-earned info) payment-amount) })
        )
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
