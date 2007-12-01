;;----------------------------------------------------------------------------
;; Janice
;;
;; Initially Commander Jeffries's assistant, she will be elected to replace
;; him after the trial.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_jan
               (list 0  0  gjan-bed    "sleeping")
               (list 6  0  gc-hall     "working")
               (list 7  0  ghg-s6      "eating")
               (list 8  0  gc-hall     "working")
               (list 11 0  ghg-s3      "eating")
               (list 12 0  gc-hall     "working")
               (list 17 0  ghg-s3      "eating")
               (list 18 0  g-fountain  "idle")
               (list 20 0  gjan-bed    "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jan-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (jan-hail knpc kpc)
  (say knpc "Hail, traveler.")
  )

(define (jan-name knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "I am Commander Janice.")
      (say knpc "I am Janice.")
      ))

(define (jan-job knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "I command Glasdrin's militia.")
      (say knpc "I am a strategic adviser to Commander Jeffries.")
      ))

;; Special
(define (jan-comm knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "I was elected to replace Jeffries, the former Commander.")
      (say knpc "Commander Jeffries is a capable leader.")
      ))

(define (jan-repl knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "It's a shame that his long history of good service to Glasdrin should end in such disgrace.")
      (say knpc "Why, whatever do you mean?")
      ))

(define (jan-mili knpc kpc)
  (say knpc "Glasdrin's militia is currently engaged in Kurpolis and border patrols. "
       "We are not actively fighting any wars."))

(define (jan-bord knpc kpc)
  (say knpc "Although we are currently at peace with Trigrave and Green Tower, we must never relax our vigilance.")
  )

;; Townspeople...

(define jan-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'hail jan-hail)
       (method 'job  jan-job)
       (method 'name jan-name)

       (method 'comm jan-comm)
       (method 'jeff jan-comm)
       (method 'mili jan-mili)
       (method 'repl jan-repl)
       (method 'bord jan-bord)
       ))

(define (mk-janice)
  (bind 
   (kern-mk-char 'ch_janice       ; tag
                 "Janice"          ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_cloaked_female ; sprite
                 faction-glasdrin         ; starting alignment
                 2 1 1               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 5  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'jan-conv         ; conv
                 sch_jan           ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_sword
                       ))         ; readied
   (jan-mk)))
