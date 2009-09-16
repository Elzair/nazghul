;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define slywan-lvl 2)
(define slywan-species sp_human)
(define slywan-occ oc_wrogue)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the monster town of Kun.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_slywan
               (list 0 0 kun-road "working")
               (list 2 0 campfire-2 "sleeping")
               (list 9 0 cantina-9 "idle")
               (list 14 0 black-market "idle")
               (list 17 0 cantina-9 "idle")
               (list 23 0 kun-road "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (slywan-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Slywan is a thief (an accomplished pickpocket) 
;; living in the monster village of Kun.
;;----------------------------------------------------------------------------

;; Basics...
(define (slywan-hail knpc kpc)
  (kern-log-msg "You meet a lithe man.")
  (say knpc "G'day")
  )

(define (slywan-default knpc kpc)
  (say knpc "[He shrugs and smiles wanly]")
  )

(define (slywan-name knpc kpc)
  (say knpc "Slywan, at your service")
  )

(define (slywan-join knpc kpc)
  (say knpc "Sorry, mate")
  )

(define (slywan-job knpc kpc)
  (say knpc "This and that. You know.")
  )

(define (slywan-bye knpc kpc)
  (let ((q (min (kern-player-get-gold)
                (kern-dice-roll "1d20"))))
    (take-player-gold q)
    (kern-obj-add-to-inventory knpc t_gold_coins q)
    (say knpc "Seeya")
    ))

(define (slywan-this knpc kpc)
  (say knpc "And that"))

(define (slywan-that knpc kpc)
  (say knpc "And this"))

(define (slywan-bust knpc kpc)
  (say knpc "Oops! Gotta go!")
  (kern-obj-add-effect knpc ef_invisibility nil)
  (kern-char-set-fleeing knpc #t)
  (kern-being-set-current-faction knpc faction-outlaw)
  (kern-conv-end)
  )

(define (slywan-thie knpc kpc)
  (say knpc "What? Did somebody steal from gold from you?")
  (if (yes? kpc)
      (begin
        (say knpc "Surely you're not accusing me?")
        (if (yes? kpc)
            (slywan-bust knpc kpc)
            (say knpc "Oh... heh. Of course not. Sorry, I didn't see anything.")))
      (say knpc "You're lucky, this is a rough town. Don't trust anybody!")))

(define slywan-conv
  (ifc nil

       ;; basics
       (method 'default slywan-default)
       (method 'hail slywan-hail)
       (method 'bye  slywan-bye)
       (method 'job  slywan-job)
       (method 'name slywan-name)
       (method 'join slywan-join)

       (method 'this slywan-this)
       (method 'that slywan-that)
       (method 'bust slywan-bust)
       (method 'thie slywan-thie)
       (method 'gold slywan-thie)
       (method 'stol slywan-thie)
       ))

(define (mk-slywan)
  (bind 
   (kern-char-force-drop
   (kern-mk-char 
    'ch_slywan           ; tag
    "Slywan"             ; name
    slywan-species         ; species
    slywan-occ              ; occ
    s_brigand     ; sprite
    faction-men      ; starting alignment
    1 0 4            ; str/int/dex
    0  ; hp bonus
    0 ; hp per-level bonus
    0 ; mp off
    1 ; mp gain
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    slywan-lvl
    #f               ; dead
    'slywan-conv         ; conv
    sch_slywan           ; sched
    'townsman-ai              ; special ai
    (mk-inventory nil) ;; container
    (list t_sword
					         t_armor_leather
					         )               ; readied
    )
   #t)
   (slywan-mk)))
