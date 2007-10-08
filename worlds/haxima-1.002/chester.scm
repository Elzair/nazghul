;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ches
               (list 0  0  gc-bed       "sleeping")
               (list 7  0  ghg-s3       "eating")
               (list 8  0  gas-counter "working")
               (list 11 0  ghg-s3       "eating")
               (list 12 0  gas-counter "working")
               (list 18 0  ghg-s3       "eating")
               (list 19 0  ghg-hall     "idle")
               (list 21 0  gc-bed       "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ches-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (ches-hail knpc kpc)
  (say knpc "[You meet an enormous man with arms like tree trunks] "
       "Hail, warrior!"))

(define (ches-default knpc kpc)
  (say knpc "'Tis beyond my ken."))

(define (ches-name knpc kpc)
  (say knpc "I'm Chester, of the Axe and Shield. "
       "You look like a serious character, "
       "in need of some serious weaponry. "
       "Perhaps I can interest you in something?")
  (if (kern-conv-get-yes-no? kpc)
      (ches-trade knpc kpc)))

(define (ches-join knpc kpc)
  (say knpc "Nay, friend, somebody has to keep you adventurers provisioned!"))

(define (ches-job knpc kpc)
  (say knpc "I sell the finest weapons and armor in the land. "
       "Care to have a look?")
       (if (kern-conv-get-yes-no? kpc)
           (ches-trade knpc kpc)
           (say knpc "As you like it. You'll find none better, "
                "I assure you.")))

(define (ches-bye knpc kpc)
  (say knpc "Farewell, and tell your friends about my shop!"))

;; Trade...
(define (ches-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by the Axe and Shield when I'm working, "
           "and I'll ply you with the best weapons and armor money can buy "
           "between 9:00AM and 6:00PM.")
      (begin
        (kern-conv-trade knpc kpc
                         (list t_staff            20)
                         (list t_dagger           65)
                         (list t_mace             80)
                         (list t_axe              85)
                         (list t_sword            85)
                         (list t_2H_axe           90)
                         (list t_2H_sword        100)
                         (list t_morning_star    105)
                         (list t_halberd         150)

                         (list t_sling            50)
                         (list t_spear            15)

			 (list t_self_bow        120)
                         (list t_bow             200)
                         (list t_arrow             1)

                         (list t_crossbow        380)
			 (list t_hvy_crossbow    600)
                         (list t_bolt              1)

                         (list t_leather_helm     50)
                         (list t_chain_coif      100)
                         (list t_iron_helm       150)
                         (list t_armor_leather   150)
                         (list t_armor_chain     330)
                         (list t_armor_plate    1000)

			 (list t_shield           30)

			 (list t_spiked_helm     250)
			 (list t_spiked_shield   250)
                         )
        (say knpc "Now go bash some skulls! And come back when you need "
             "an upgrade!"))))

;; Paladins...
(define (ches-pala knpc kpc)
  (say knpc "I served several tours with the paladins, but that's no way "
       "to make a fortune, so I retired and opened my shop."))

;; Townspeople...
(define (ches-glas knpc kpc)
  (say knpc "Not a bad town. Business is good here, "
       "with the paladins and all."))

(define (ches-ange knpc kpc)
  (say knpc "Nice-looking woman. But I prefer them a bit more wild.")) 

(define (ches-patc knpc kpc)
  (say knpc "I've never been sick, but word is he's a good healer."))

(define (ches-jess knpc kpc)
  (say knpc "Now there's a fine piece of woman flesh! "
       "Too bad about that scar, "
       "but in the dark it's all the same, eh?"))

(define ches-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default ches-default)
       (method 'hail ches-hail)
       (method 'bye ches-bye)
       (method 'job ches-job)
       (method 'name ches-name)
       (method 'join ches-join)
       
       ;; trade
       (method 'trad ches-trade)
       (method 'buy ches-trade)
       (method 'sell ches-trade)

       ;; paladin
       (method 'pala ches-pala)

       ;; town & people
       (method 'glas ches-glas)
       (method 'ange ches-ange)
       (method 'patc ches-patc)
       (method 'jess ches-jess)

       ))

(define (mk-chester)
  (bind 
   (kern-mk-char 'ch_chester         ; tag
                 "Chester"           ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_townsman          ; sprite
                 faction-men         ; starting alignment
                 5 0 2               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'ches-conv          ; conv
                 sch_ches            ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_mace   ; we need a 'hammer'
                 			t_armor_chain)
                                 ; readied
                 )
   (ches-mk)))
