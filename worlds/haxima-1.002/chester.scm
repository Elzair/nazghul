;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Glasdrin.
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
(define (ches-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Chester is a burly weaponsmith who lives in Glasdrin.
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

(define ches-catalog
  (list
   (list t_staff            20 "Equip your mages with staffs so they can feel useful.")
   (list t_dagger           65 "Always keep an extra dagger or two in your boots.")
   (list t_mace             80 "The mace is a fine weapon for crushing skulls and breaking bones.")
   (list t_axe              85 "That axe is just the thing for splitting shields.")
   (list t_sword            85 "The sword is the workhorse of the fighting class.")
   (list t_2H_axe           90 "You'll mow down your enemies like grass with my two-handed axes.")
   (list t_2H_sword        100 "Ah yes, my personal favorite is the mighty the two-handed sword.")
   (list t_morning_star    105 "Your foes will give you plenty of room when you whip out a morning star.")
   (list t_halberd         150 "With the halberd your back ranks can reinforce your front.")
   
   (list t_sling            50 "If you have any children a sling will make a fine starter weapon.")
   (list t_spear            15 "The spear is quite popular with goblins and other savages.")

   (list t_self_bow        120 "These little bows are perfect for hunting small animals.")
   (list t_bow             200 "The rangers like to skulk about in the woods with bows like this.")
   (list t_arrow             1 "If you don't wish to face your enemies in manly combat, be sure to carry plenty of arrows.")
   
   (list t_crossbow        380 "When your enemies flee from your blade, drop them with a crossbow.")
   (list t_hvy_crossbow    600 "When besieged, the manly warrior turns to one of these beauties.")
   (list t_bolt              1 "The Glasdrin militia demands the best arms, and I am their sole supplier for bolts.")
   
   (list t_leather_helm     50 "The light helm is perfect for any pencil-necked wrogues in your party.")
   (list t_chain_coif      100 "The chain coif will protect your neck without obstructing your vision.")
   (list t_iron_helm       150 "With an iron helm you can charge head first into the thickest melee.")
   (list t_armor_leather   150 "Leather armour won't slow you down, but it only gives minimal protection.")
   (list t_armor_chain     330 "Chain is the preferred armour of paladins, and they know their stuff.")
   (list t_armor_plate    1000 "With plate armour you will be invincible in battle.")
   
   (list t_shield           30 "A sturdy shield will save you from craven foes who shoot from afar.")
   
   (list t_spiked_helm     250 "The best defense is a good offense! This spiked helm will prove my point. No pun intended.")
   (list t_spiked_shield   250 "With the spiked shield you practically don't need a weapon.")
   ))

(define ches-merch-msgs
  (list "Come by the Axe and Shield when I'm working, and I'll ply you with the best weapons and armor money can buy between 9:00AM and 6:00PM."
        "Come have a look at my armory!" ;; buy
        "I'll offer you some trade-in value on your used arms." ;; sell
        "I equip the finest warriors in the land. What can I do for you?" ;; trade
        "Now go bash some skulls! And come back when you need an upgrade." ;; sold-something
        "Your equipment is looking a bit shabby, you should reconsider." ;; sold-nothing
        "Now you should buy something to replace that old junk." ;; bought-something
        "I was only trying to help. I don't think anyone else will want that rubbish." ;; bought-nothing
        "Now you're ready to storm Gintspar!" ;; traded-something
        "Browse all you like." ;; traded-nothing
        ))

;; Trade...
(define (ches-trade knpc kpc) (conv-trade knpc kpc "trade" ches-merch-msgs ches-catalog))
(define (ches-buy knpc kpc) (conv-trade knpc kpc "buy" ches-merch-msgs ches-catalog))
(define (ches-sell knpc kpc) (conv-trade knpc kpc "sell" ches-merch-msgs ches-catalog))

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
       (method 'buy ches-buy)
       (method 'sell ches-sell)

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
                 faction-glasdrin         ; starting alignment
                 5 0 2               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'ches-conv          ; conv
                 sch_ches            ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_mace)
                                     (list 1 t_armor_chain))) ; container
                 nil ;;  readied
                 )
   (ches-mk)))
