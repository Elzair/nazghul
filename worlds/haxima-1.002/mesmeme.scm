;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define mesmeme-lvl 2)
(define mesmeme-species sp_gazer)
(define mesmeme-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_mesmeme
               (list 0  0 campfire-1 "sleeping")
               (list 9  0 cantina "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (mesmeme-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (mesmeme-hail knpc kpc)
  (kern-log-msg "You meet a battered gazer.")
  (say knpc "I see you.")
  )

(define (mesmeme-default knpc kpc)
  (say knpc "No knowing. No asking. Alone.")
  )

(define (mesmeme-name knpc kpc)
  (say knpc "I Mesmeme.")
  )

(define (mesmeme-join knpc kpc)
  (say knpc "Yes! I alone.")
  (join-player knpc)
  (kern-conv-end)
  )

(define (mesmeme-job knpc kpc)
  (say knpc "No job. No kindred. Alone.")
  )

(define (mesmeme-bye knpc kpc)
  (say knpc "Until void.")
  )

(define (mesmeme-alon knpc kpc)
  (say knpc "I crippled. No... think voice? Mind voice? No talk kindred. Alone.")
  )

(define (mesmeme-kind knpc kpc)
  (say knpc "Kindred. Gazers. Swarm.")
  )

(define (mesmeme-crip knpc kpc)
  (say knpc "Gint slave, mine. Too strong. Broke free, hurt!")
  )

(define (mesmeme-slav knpc kpc)
  (say knpc "Tools. Hands. One body too few!")
  )

(define mesmeme-conv
  (ifc nil

       ;; basics
       (method 'default mesmeme-default)
       (method 'hail mesmeme-hail)
       (method 'bye mesmeme-bye)
       (method 'job mesmeme-job)
       (method 'name mesmeme-name)
       (method 'join mesmeme-join)
       
       (method 'alon mesmeme-alon)
       (method 'kind mesmeme-kind)
       (method 'crip mesmeme-crip)
       (method 'slav mesmeme-slav)
       ))

(define (mk-mesmeme)
  (bind 
   (set-level
   (kern-mk-char 
    'ch_mesmeme           ; tag
    "Mesmeme"             ; name
    mesmeme-species         ; species
    mesmeme-occ              ; occ
    s_gazer     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    (/ pc-hp-off 2)  ; hp bonus
    (/ pc-hp-gain 2) ; hp per-level bonus
    0 ; mp off
    1 ; mp gain
    (max-hp mesmeme-species mesmeme-occ mesmeme-lvl 0 0) ; hp
    0                   ; xp
    (max-mp mesmeme-species mesmeme-occ mesmeme-lvl 0 0) ; mp
    mesmeme-lvl
    #f               ; dead
    'mesmeme-conv         ; conv
    sch_mesmeme           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   mesmeme-lvl)
   (mesmeme-mk)))
