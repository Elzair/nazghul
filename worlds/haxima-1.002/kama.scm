;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define kama-lvl 4)
(define kama-species sp_forest_goblin)
(define kama-occ oc_warrior)
(define kama-exit-x 34)
(define kama-exit-y 4)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define kama-cell gtl-cell1)
(kern-mk-sched 'sch_kama
               (list 0  0 kama-cell        "sleeping")
               (list 7  0 kama-cell        "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (kama-mk) (list #f))
(define (kama-gave-food? gob) (car gob))
(define (kama-gave-food! gob) (set-car! gob #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (kama-hail knpc kpc)
  (meet "You meet a calm goblin who regards you with a fearless, calculating gaze.")
  (if (kama-gave-food? (gob knpc))
      (say knpc "Bonaha.")
      (say knpc "Nuki?")
      ))

(define (kama-no knpc kpc)
  (if (kama-gave-food? (gob knpc))
      (say knpc "[He points to himself] Kama.")
      (kama-default knpc kpc)))

(define (kama-me knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)
      (begin
        (say knpc "Kahato. [He points to you] Zuto?")
        (if (yes? kpc)
            (say knpc "[He nods]")
            (say knpc "[He chuckles as if he disbelieves you]")))))

(define (kama-jo knpc kpc)
  (define (exit-point)
    (mk-loc (kobj-place knpc)
            kama-exit-x
            kama-exit-y))
  (define (door-still-locked?)
    (println "door-still-locked?")
    (not (can-pathfind? knpc (exit-point))))
  (define (join)    
    (println "join")
    (say knpc "Hajo! Bona ka ruka! Iki [" (loc-x angriss-lair-loc)
         " " (loc-y angriss-lair-loc) "]")
    (join-player knpc))
  (if (is-player-party-member? knpc)
      (say knpc "[He looks confused] Ha...")
      (if (not (kama-gave-food? (gob knpc)))
          (kama-default knpc kpc)
          (if (door-still-locked?)
              (say knpc "[He points to the locked cell door and shrugs]")
              (join)
              ))))

(define (kama-food knpc kpc)
  (kern-log-msg "[Do you give him some food?]")
  (define (no-food)
    (say knpc "[He grunts and turns away]")
    (kern-conv-end))
  (define (yes-food)
    (kama-gave-food! (gob knpc))
    (say knpc "[He gobbles it down hungrily and smacks his lips] "
         "Ha nuki! [He points to you] Bonaha."))
  (if (yes? kpc)
      (if (> (get-food-donation knpc kpc) 0)
          (yes-food)
          (no-food))
      (no-food)))
          
(define (kama-rune knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)
      (if (any-in-inventory? kpc rune-types)
          (say knpc "[You show him a Rune. He nods uneasily] Ruka.")
          (say knpc "[He looks confused as you try to describe a Rune]"))))

(define (kama-ruka knpc kpc)
  (say knpc "[In the dust on the cell floor he draws a circle with jointed legs. A spider. He then points to you, himself, and then he scuffs out the spider.]")
  (prompt-for-key)
  (say knpc "[You get the impression he is proposing an alliance with you against the spider, or whatever it is.]"))

(define (kama-default knpc kpc)
  (say knpc "[no response]"))

(define (kama-bye knpc kpc)
  (say knpc "[his expression never changes]"))

(define (kama-clov knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)  
      (say knpc "[He looks puzzled at first, but then nods] Ruka ka choto.")))

(define kama-conv
  (ifc nil
       (method 'hail kama-hail)
       (method 'default kama-default)
       (method 'bye kama-bye)
       (method 'no kama-no)
       (method 'me kama-me)
       (method 'jo kama-jo)
       (method 'food kama-food)
       (method 'rune kama-rune)
       (method 'ruka kama-ruka)
       (method 'clov kama-clov)
    ))

(define (mk-kama)
  (bind 
   (set-level
    (kern-mk-char 
     'ch_kama           ; tag
     "Kama"             ; name
     kama-species         ; species
     kama-occ              ; occ
     s_orc     ; sprite
     faction-men      ; starting alignment
     10 0 10            ; str/int/dex
     3 2              ; hp mod/mult
     0 0              ; mp mod/mult
     (max-hp kama-species kama-occ kama-lvl 3 2) ; hp
     0                   ; xp
     (max-mp kama-species kama-occ kama-lvl 0 0) ; mp
     kama-lvl
     #f               ; dead
     'kama-conv         ; conv
     sch_kama           ; sched
     nil              ; special ai
     nil              ; container
     nil              ; readied
     )
    kama-lvl)
   (kama-mk)))
