;; title: healthcare_patient_records_system
;; version: 1.0.0
;; summary: Smart contract for Healthcare Patient Records System

(define-constant contract-owner tx-sender)
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u403))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INSUFFICIENT-PERMISSIONS (err u401))

(define-data-var total-patients uint u0)
(define-data-var total-records uint u0)
(define-data-var system-active bool true)

(define-map patients
  { patient-id: uint }
  {
    patient-address: principal,
    full-name: (string-ascii 100),
    date-of-birth: uint,
    blood-type: (string-ascii 5),
    medical-conditions: (string-ascii 500),
    created-at: uint,
    updated-at: uint,
    is-active: bool
  }
)

(define-map patient-records
  { record-id: uint }
  {
    patient-id: uint,
    healthcare-provider: principal,
    record-type: (string-ascii 50),
    diagnosis: (string-ascii 500),
    treatment: (string-ascii 500),
    medications: (string-ascii 500),
    notes: (string-ascii 1000),
    recorded-at: uint,
    is-private: bool
  }
)

(define-map patient-access-permissions
  { patient-id: uint, provider-address: principal }
  {
    access-level: uint,
    granted-at: uint,
    expires-at: uint,
    is-active: bool
  }
)

(define-map provider-registry
  { provider-address: principal }
  {
    provider-name: (string-ascii 100),
    license-number: (string-ascii 50),
    specialization: (string-ascii 100),
    registered-at: uint,
    is-verified: bool
  }
)

(define-read-only (get-patient (patient-id uint))
  (ok (map-get? patients { patient-id: patient-id }))
)

(define-read-only (get-patient-record (record-id uint))
  (ok (map-get? patient-records { record-id: record-id }))
)

(define-read-only (check-access-permission (patient-id uint) (provider principal))
  (match (map-get? patient-access-permissions { patient-id: patient-id, provider-address: provider })
    permission (ok permission)
    ERR-NOT-FOUND
  )
)

(define-read-only (get-provider-info (provider principal))
  (ok (map-get? provider-registry { provider-address: provider }))
)

(define-read-only (get-total-patients)
  (ok (var-get total-patients))
)

(define-read-only (get-total-records)
  (ok (var-get total-records))
)

(define-public (register-provider (provider-name (string-ascii 100)) (license-number (string-ascii 50)) (specialization (string-ascii 100)))
  (begin
    (asserts! (var-get system-active) ERR-INVALID-INPUT)
    (map-set provider-registry
      { provider-address: tx-sender }
      {
        provider-name: provider-name,
        license-number: license-number,
        specialization: specialization,
        registered-at: burn-block-height,
        is-verified: false
      }
    )
    (ok true)
  )
)

(define-public (create-patient (full-name (string-ascii 100)) (date-of-birth uint) (blood-type (string-ascii 5)) (medical-conditions (string-ascii 500)))
  (let ((new-patient-id (+ (var-get total-patients) u1)))
    (begin
      (asserts! (var-get system-active) ERR-INVALID-INPUT)
      (asserts! (> (len full-name) u0) ERR-INVALID-INPUT)
      (map-set patients
        { patient-id: new-patient-id }
        {
          patient-address: tx-sender,
          full-name: full-name,
          date-of-birth: date-of-birth,
          blood-type: blood-type,
          medical-conditions: medical-conditions,
          created-at: burn-block-height,
          updated-at: burn-block-height,
          is-active: true
        }
      )
      (var-set total-patients new-patient-id)
      (ok new-patient-id)
    )
  )
)

(define-public (add-patient-record (patient-id uint) (record-type (string-ascii 50)) (diagnosis (string-ascii 500)) (treatment (string-ascii 500)) (medications (string-ascii 500)) (notes (string-ascii 1000)))
  (let ((new-record-id (+ (var-get total-records) u1)))
    (begin
      (asserts! (var-get system-active) ERR-INVALID-INPUT)
      (asserts! (is-some (map-get? patients { patient-id: patient-id })) ERR-NOT-FOUND)
      (asserts! (is-some (map-get? provider-registry { provider-address: tx-sender })) ERR-INSUFFICIENT-PERMISSIONS)
      (map-set patient-records
        { record-id: new-record-id }
        {
          patient-id: patient-id,
          healthcare-provider: tx-sender,
          record-type: record-type,
          diagnosis: diagnosis,
          treatment: treatment,
          medications: medications,
          notes: notes,
          recorded-at: burn-block-height,
          is-private: false
        }
      )
      (var-set total-records new-record-id)
      (ok new-record-id)
    )
  )
)

(define-public (grant-access (patient-id uint) (provider principal) (access-level uint) (duration uint))
  (let ((patient-data (map-get? patients { patient-id: patient-id })))
    (begin
      (asserts! (is-some patient-data) ERR-NOT-FOUND)
      (asserts! (is-eq tx-sender (get patient-address (unwrap! patient-data ERR-NOT-FOUND))) ERR-UNAUTHORIZED)
      (asserts! (> access-level u0) ERR-INVALID-INPUT)
      (map-set patient-access-permissions
        { patient-id: patient-id, provider-address: provider }
        {
          access-level: access-level,
          granted-at: burn-block-height,
          expires-at: (+ burn-block-height duration),
          is-active: true
        }
      )
      (ok true)
    )
  )
)

(define-public (revoke-access (patient-id uint) (provider principal))
  (let ((patient-data (map-get? patients { patient-id: patient-id })))
    (begin
      (asserts! (is-some patient-data) ERR-NOT-FOUND)
      (asserts! (is-eq tx-sender (get patient-address (unwrap! patient-data ERR-NOT-FOUND))) ERR-UNAUTHORIZED)
      (map-set patient-access-permissions
        { patient-id: patient-id, provider-address: provider }
        (merge (unwrap! (map-get? patient-access-permissions { patient-id: patient-id, provider-address: provider }) ERR-NOT-FOUND)
          { is-active: false }
        )
      )
      (ok true)
    )
  )
)

(define-public (update-patient-info (patient-id uint) (medical-conditions (string-ascii 500)))
  (let ((patient-data (map-get? patients { patient-id: patient-id })))
    (begin
      (asserts! (is-some patient-data) ERR-NOT-FOUND)
      (asserts! (is-eq tx-sender (get patient-address (unwrap! patient-data ERR-NOT-FOUND))) ERR-UNAUTHORIZED)
      (map-set patients
        { patient-id: patient-id }
        (merge (unwrap! (map-get? patients { patient-id: patient-id }) ERR-NOT-FOUND)
          {
            medical-conditions: medical-conditions,
            updated-at: burn-block-height
          }
        )
      )
      (ok true)
    )
  )
)
