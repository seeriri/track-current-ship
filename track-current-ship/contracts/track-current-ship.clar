;; Supply Chain Batch Management Smart Contract
;; Tracks batches with quality metrics, compliance, and carbon footprint

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-exists (err u104))

;; Batch status enumeration
(define-constant status-created u0)
(define-constant status-in-transit u1)
(define-constant status-delivered u2)
(define-constant status-quality-issue u3)
(define-constant status-rejected u4)

;; Data structures
(define-map batches
    { batch-id: (string-ascii 64) }
    {
        creator: principal,
        current-handler: principal,
        product-type: (string-ascii 100),
        quantity: uint,
        status: uint,
        temperature: int,
        humidity: uint,
        carbon-footprint: uint,
        quality-score: uint,
        compliance-verified: bool,
        created-at: uint,
        updated-at: uint
    }
)

(define-map batch-history
    { batch-id: (string-ascii 64), event-id: uint }
    {
        handler: principal,
        location: (string-ascii 100),
        temperature: int,
        humidity: uint,
        timestamp: uint,
        notes: (string-ascii 256)
    }
)

(define-map stakeholder-reputation
    { stakeholder: principal }
    {
        total-batches: uint,
        successful-deliveries: uint,
        quality-issues: uint,
        reputation-score: uint
    }
)

(define-data-var event-counter uint u0)

;; Read-only functions
(define-read-only (get-batch (batch-id (string-ascii 64)))
    (map-get? batches { batch-id: batch-id })
)

(define-read-only (get-batch-event (batch-id (string-ascii 64)) (event-id uint))
    (map-get? batch-history { batch-id: batch-id, event-id: event-id })
)

(define-read-only (get-reputation (stakeholder principal))
    (default-to 
        { total-batches: u0, successful-deliveries: u0, quality-issues: u0, reputation-score: u100 }
        (map-get? stakeholder-reputation { stakeholder: stakeholder })
    )
)

;; Private functions
(define-private (update-reputation (handler principal) (is-success bool) (has-quality-issue bool))
    (let (
        (current-rep (get-reputation handler))
        (new-total (+ (get total-batches current-rep) u1))
        (new-successful (if is-success (+ (get successful-deliveries current-rep) u1) (get successful-deliveries current-rep)))
        (new-issues (if has-quality-issue (+ (get quality-issues current-rep) u1) (get quality-issues current-rep)))
    )
    (map-set stakeholder-reputation
        { stakeholder: handler }
        {
            total-batches: new-total,
            successful-deliveries: new-successful,
            quality-issues: new-issues,
            reputation-score: (if (> new-total u0)
                (/ (* new-successful u100) new-total)
                u100
            )
        }
    ))
)

;; Public functions
(define-public (create-batch 
    (batch-id (string-ascii 64))
    (product-type (string-ascii 100))
    (quantity uint)
    (initial-temperature int)
    (initial-humidity uint))
    (let (
        (existing-batch (map-get? batches { batch-id: batch-id }))
    )
    (asserts! (is-none existing-batch) err-already-exists)
    (map-set batches
        { batch-id: batch-id }
        {
            creator: tx-sender,
            current-handler: tx-sender,
            product-type: product-type,
            quantity: quantity,
            status: status-created,
            temperature: initial-temperature,
            humidity: initial-humidity,
            carbon-footprint: u0,
            quality-score: u100,
            compliance-verified: true,
            created-at: block-height,
            updated-at: block-height
        }
    )
    (update-reputation tx-sender false false)
    (ok true))
)

(define-public (update-batch-conditions
    (batch-id (string-ascii 64))
    (temperature int)
    (humidity uint)
    (location (string-ascii 100))
    (carbon-addition uint)
    (notes (string-ascii 256)))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
        (current-event-id (var-get event-counter))
    )
    (asserts! (is-eq (get current-handler batch) tx-sender) err-unauthorized)
    
    ;; Add event to history
    (map-set batch-history
        { batch-id: batch-id, event-id: current-event-id }
        {
            handler: tx-sender,
            location: location,
            temperature: temperature,
            humidity: humidity,
            timestamp: block-height,
            notes: notes
        }
    )
    (var-set event-counter (+ current-event-id u1))
    
    ;; Update batch
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            temperature: temperature,
            humidity: humidity,
            carbon-footprint: (+ (get carbon-footprint batch) carbon-addition),
            updated-at: block-height
        })
    )
    (ok true))
)

(define-public (transfer-batch
    (batch-id (string-ascii 64))
    (new-handler principal))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
    )
    (asserts! (is-eq (get current-handler batch) tx-sender) err-unauthorized)
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            current-handler: new-handler,
            status: status-in-transit,
            updated-at: block-height
        })
    )
    (ok true))
)

(define-public (update-quality-score
    (batch-id (string-ascii 64))
    (new-score uint))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
    )
    (asserts! (is-eq (get current-handler batch) tx-sender) err-unauthorized)
    (asserts! (<= new-score u100) err-invalid-status)
    
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            quality-score: new-score,
            status: (if (< new-score u70) status-quality-issue (get status batch)),
            updated-at: block-height
        })
    )
    
    (update-reputation tx-sender false (< new-score u70))
    (ok true))
)

(define-public (complete-delivery
    (batch-id (string-ascii 64)))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
    )
    (asserts! (is-eq (get current-handler batch) tx-sender) err-unauthorized)
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            status: status-delivered,
            updated-at: block-height
        })
    )
    (update-reputation tx-sender true false)
    (ok true))
)

(define-public (reject-batch
    (batch-id (string-ascii 64))
    (reason (string-ascii 256)))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
        (current-event-id (var-get event-counter))
    )
    (asserts! (is-eq (get current-handler batch) tx-sender) err-unauthorized)
    
    (map-set batch-history
        { batch-id: batch-id, event-id: current-event-id }
        {
            handler: tx-sender,
            location: "REJECTED",
            temperature: (get temperature batch),
            humidity: (get humidity batch),
            timestamp: block-height,
            notes: reason
        }
    )
    (var-set event-counter (+ current-event-id u1))
    
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            status: status-rejected,
            updated-at: block-height
        })
    )
    (update-reputation (get creator batch) false true)
    (ok true))
)

(define-public (verify-compliance
    (batch-id (string-ascii 64))
    (is-compliant bool))
    (let (
        (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set batches
        { batch-id: batch-id }
        (merge batch {
            compliance-verified: is-compliant,
            updated-at: block-height
        })
    )
    (ok true))
)