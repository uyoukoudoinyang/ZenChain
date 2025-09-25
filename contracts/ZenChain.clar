;; ZenChain: Wellness and Mindfulness Tracking System
;; Version: 1.0.0

;; Constants
(define-constant WELLNESS_CENTER_CAPACITY u3800000)
(define-constant BASE_WELLNESS_REWARD u42)
(define-constant MINDFULNESS_BONUS u26)
(define-constant MAX_WELLNESS_LEVEL u28)
(define-constant ERR_INVALID_WELLNESS_SESSION u1)
(define-constant ERR_NO_WELLNESS_TOKENS u2)
(define-constant ERR_CENTER_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_WELLNESS_CYCLE u2880)
(define-constant CHALLENGE_MULTIPLIER u16)
(define-constant MIN_CHALLENGE_PERIOD u1440)
(define-constant WELLNESS_LAPSE_PENALTY u38)

;; Data Variables
(define-data-var total-wellness-tokens-distributed uint u0)
(define-data-var total-wellness-sessions uint u0)
(define-data-var wellness-coordinator principal tx-sender)

;; Data Maps
(define-map practitioner-sessions principal uint)
(define-map practitioner-wellness-tokens principal uint)
(define-map wellness-session-start-time principal uint)
(define-map practitioner-mindfulness-level principal uint)
(define-map practitioner-last-session principal uint)
(define-map practitioner-wellness-challenge principal uint)
(define-map practitioner-challenge-start-block principal uint)
(define-map practice-intensity principal uint)
(define-map practitioner-achievement-count principal uint)
(define-map wellness-specialization principal uint)

;; Public Functions
(define-public (start-wellness-session (practice-type uint) (intensity-level uint))
  (let
    (
      (practitioner tx-sender)
    )
    (asserts! (and (> practice-type u0) (> intensity-level u0) (<= intensity-level u10)) (err ERR_INVALID_WELLNESS_SESSION))
    (map-set wellness-session-start-time practitioner burn-block-height)
    (map-set practice-intensity practitioner intensity-level)
    (ok true)
  ))

(define-public (complete-wellness-session (practice-type uint) (mindfulness-score uint))
  (let
    (
      (practitioner tx-sender)
      (start-block (default-to u0 (map-get? wellness-session-start-time practitioner)))
      (blocks-practicing (- burn-block-height start-block))
      (last-session-block (default-to u0 (map-get? practitioner-last-session practitioner)))
      (mindfulness-level (default-to u0 (map-get? practitioner-mindfulness-level practitioner)))
      (capped-mindfulness (if (<= mindfulness-level MAX_WELLNESS_LEVEL) mindfulness-level MAX_WELLNESS_LEVEL))
      (mindfulness-bonus-calc (/ (* mindfulness-score u16) u100))
      (specialization-bonus (default-to u0 (map-get? wellness-specialization practitioner)))
      (wellness-reward (+ BASE_WELLNESS_REWARD (* capped-mindfulness MINDFULNESS_BONUS) mindfulness-bonus-calc specialization-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-practicing practice-type) (<= mindfulness-score u100)) (err ERR_INVALID_WELLNESS_SESSION))
    
    (map-set practitioner-sessions practitioner (+ (default-to u0 (map-get? practitioner-sessions practitioner)) u1))
    (map-set practitioner-wellness-tokens practitioner (+ (default-to u0 (map-get? practitioner-wellness-tokens practitioner)) wellness-reward))
    
    (if (< (- burn-block-height last-session-block) BLOCKS_PER_WELLNESS_CYCLE)
      (map-set practitioner-mindfulness-level practitioner (+ mindfulness-level u1))
      (map-set practitioner-mindfulness-level practitioner u1)
    )
    
    (if (>= mindfulness-score u88)
      (map-set wellness-specialization practitioner (+ specialization-bonus u8))
      true
    )
    
    (map-set practitioner-last-session practitioner burn-block-height)
    (var-set total-wellness-sessions (+ (var-get total-wellness-sessions) u1))
    (var-set total-wellness-tokens-distributed (+ (var-get total-wellness-tokens-distributed) wellness-reward))
    
    (asserts! (<= (var-get total-wellness-tokens-distributed) WELLNESS_CENTER_CAPACITY) (err ERR_CENTER_CAPACITY_EXCEEDED))
    (ok wellness-reward)
  ))

(define-public (claim-wellness-rewards)
  (let
    (
      (practitioner tx-sender)
      (token-balance (default-to u0 (map-get? practitioner-wellness-tokens practitioner)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_WELLNESS_TOKENS))
    (map-set practitioner-wellness-tokens practitioner u0)
    (ok token-balance)
  ))

;; Wellness Challenge Features
(define-public (start-wellness-challenge (challenge-scope uint))
  (let
    (
      (practitioner tx-sender)
    )
    (asserts! (> challenge-scope u0) (err ERR_INVALID_WELLNESS_SESSION))
    (asserts! (>= (var-get total-wellness-tokens-distributed) challenge-scope) (err ERR_CENTER_CAPACITY_EXCEEDED))
    
    (map-set practitioner-wellness-challenge practitioner challenge-scope)
    (map-set practitioner-challenge-start-block practitioner burn-block-height)
    (var-set total-wellness-tokens-distributed (- (var-get total-wellness-tokens-distributed) challenge-scope))
    (ok challenge-scope)
  ))

(define-public (complete-wellness-challenge)
  (let
    (
      (practitioner tx-sender)
      (challenge-amount (default-to u0 (map-get? practitioner-wellness-challenge practitioner)))
      (challenge-start-block (default-to u0 (map-get? practitioner-challenge-start-block practitioner)))
      (blocks-challenging (- burn-block-height challenge-start-block))
      (penalty (if (< blocks-challenging MIN_CHALLENGE_PERIOD) (/ (* challenge-amount WELLNESS_LAPSE_PENALTY) u100) u0))
      (challenge-bonus (if (>= blocks-challenging MIN_CHALLENGE_PERIOD) (/ (* challenge-amount CHALLENGE_MULTIPLIER) u100) u0))
      (final-amount (+ (- challenge-amount penalty) challenge-bonus))
    )
    (asserts! (> challenge-amount u0) (err ERR_NO_WELLNESS_TOKENS))
    
    (map-set practitioner-wellness-challenge practitioner u0)
    (map-set practitioner-challenge-start-block practitioner u0)
    (map-set practitioner-achievement-count practitioner (+ (default-to u0 (map-get? practitioner-achievement-count practitioner)) u1))
    (var-set total-wellness-tokens-distributed (+ (var-get total-wellness-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (publish-wellness-achievement (achievement-quality uint) (community-validation uint))
  (let
    (
      (practitioner tx-sender)
      (mindfulness-level (default-to u0 (map-get? practitioner-mindfulness-level practitioner)))
      (achievement-count (default-to u0 (map-get? practitioner-achievement-count practitioner)))
      (achievement-bonus (+ (* achievement-quality u22) (* community-validation u20) (* achievement-count u14)))
    )
    (asserts! (and (> achievement-quality u0) (> community-validation u0) (>= mindfulness-level u12)) (err ERR_INVALID_WELLNESS_SESSION))
    
    (map-set practitioner-wellness-tokens practitioner (+ (default-to u0 (map-get? practitioner-wellness-tokens practitioner)) achievement-bonus))
    (var-set total-wellness-tokens-distributed (+ (var-get total-wellness-tokens-distributed) achievement-bonus))
    
    (ok achievement-bonus)
  ))

(define-public (coach-wellness-practitioners (client-count uint) (coaching-hours uint))
  (let
    (
      (practitioner tx-sender)
      (mindfulness-level (default-to u0 (map-get? practitioner-mindfulness-level practitioner)))
      (specialization-level (default-to u0 (map-get? wellness-specialization practitioner)))
      (coaching-bonus (+ (* client-count u32) (* coaching-hours u9) (* specialization-level u5)))
    )
    (asserts! (and (> client-count u0) (> coaching-hours u0) (>= mindfulness-level u16)) (err ERR_INVALID_WELLNESS_SESSION))
    
    (map-set practitioner-wellness-tokens practitioner (+ (default-to u0 (map-get? practitioner-wellness-tokens practitioner)) coaching-bonus))
    (var-set total-wellness-tokens-distributed (+ (var-get total-wellness-tokens-distributed) coaching-bonus))
    
    (ok coaching-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-wellness-session-count (user principal))
  (default-to u0 (map-get? practitioner-sessions user)))

(define-read-only (get-wellness-token-balance (user principal))
  (default-to u0 (map-get? practitioner-wellness-tokens user)))

(define-read-only (get-mindfulness-level (user principal))
  (default-to u0 (map-get? practitioner-mindfulness-level user)))

(define-read-only (get-achievement-count (user principal))
  (default-to u0 (map-get? practitioner-achievement-count user)))

(define-read-only (get-wellness-challenge (user principal))
  (default-to u0 (map-get? practitioner-wellness-challenge user)))

(define-read-only (get-wellness-specialization (user principal))
  (default-to u0 (map-get? wellness-specialization user)))

(define-read-only (get-wellness-center-stats)
  {
    total-wellness-sessions: (var-get total-wellness-sessions),
    total-wellness-tokens-distributed: (var-get total-wellness-tokens-distributed),
    wellness-center-capacity: WELLNESS_CENTER_CAPACITY
  })

(define-read-only (calculate-wellness-reward (mindfulness-level uint) (mindfulness-score uint) (specialization-bonus uint))
  (let
    (
      (capped-mindfulness (if (<= mindfulness-level MAX_WELLNESS_LEVEL) mindfulness-level MAX_WELLNESS_LEVEL))
      (mindfulness-bonus-calc (/ (* mindfulness-score u16) u100))
    )
    (+ BASE_WELLNESS_REWARD (* capped-mindfulness MINDFULNESS_BONUS) mindfulness-bonus-calc specialization-bonus)
  ))

;; Private Functions
(define-private (is-wellness-coordinator)
  (is-eq tx-sender (var-get wellness-coordinator)))

(define-private (validate-wellness-parameters (practice-type uint) (mindfulness-score uint))
  (and (> practice-type u0) (<= mindfulness-score u100)))