;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Glasdrin.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_jess
               (list 0  0  gj-bed      "sleeping")
               (list 7  0  ghg-counter "working")
               (list 9  0  g-fountain  "idle")
               (list 10 0  ghg-counter "working")
               (list 13 0  gc-hall     "idle")
               (list 14 0  ghg-counter "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jess-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Jess is the female proprietress of the Holy Grail tavern in Glasdrin.
;; Her face is scarred, the result of an encounter with goblins 
;; when serving as a crossbowman in the Glasdrin militia.
;;----------------------------------------------------------------------------

;; Basics...
(define (jess-hail knpc kpc)
  (if (string=? "working" (kern-obj-get-activity knpc))
      (say knpc "[You meet an otherwise attractive young woman with a hideous "
           "scar on one side of her face]. Relax, gentlemen, you've finally "
           "found the Holy Grail!")
      (say knpc "[You meet an otherwise attractive young woman with a hideous "
           "scar on one side of her face]. Nice day, isn't it?")))

(define (jess-default knpc kpc)
  (say knpc "Hm... I don't know."))

(define (jess-name knpc kpc)
  (if (working? knpc)
      (say knpc "I'm Jess, proprieter of this here oasis.")
      (say knpc "I'm Jess, proprieter of the Holy Grail. "
           "Come by some time when you're thirsty.")))

(define (jess-join knpc kpc)
  (say knpc "No thanks, I get enough action throwing out drunks and "
       "breaking up fights."))

(define (jess-job knpc kpc)
  (say knpc "I tempt the weak-minded with my wares. Care to see them?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (say knpc "[She winks] I thought you would!")
        (jess-trade knpc kpc))
      (say knpc "Too bad.")))

(define (jess-bye knpc kpc)
  (say knpc "So long, come back again!"))

(define jess-catalog
  (list
   (list t_food 7  "The paladins love my roast beef.")
   (list t_beer 12 "Have a grail-ful!")
   ))

(define jess-merch-msgs
  (list "Come by the Holy Grail when I'm working. Breakfast is between 7:00AM and 9:00AM, lunch is 10:00AM to 1:00PM and I reopen at 2:00PM until midnight."
        "Here's a menu!"
        nil ;; sell
        nil ;; trade
        "Enjoy!" ;; sold-something
        "Well, it beats hard-tack!" ;; sold-nothing
        nil ;; the rest are nil
        ))

;; Trade...
(define (jess-buy knpc kpc) (conv-trade knpc kpc "buy" jess-merch-msgs jess-catalog))

;; Holy Grail
(define (jess-grai knpc kpc)
  (say knpc "According to legend, a Wanderer suggested the title. "
       "It was based on a popular story in his home-world."))

;; Scar
(define (jess-scar knpc kpc)
  (say knpc "I got that courtesy of some cave goblins and a guard who "
       "couldn't stay awake. They ambushed us while we were sleeping, "
       "I caught the edge of an axe with my cheek, but my helmet took "
       "the worst of it. Do you like stories of battles?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Everyone in town has at least one. We've all served.")
      (say knpc "Too bad, because this is the place to hear them.")))

(define (jess-serv knpc kpc)
  (say knpc "All citizens of Glasdrin must serve a tour of duty. "
       "I was a crossbowman, my job was to support the heavy armor from "
       "behind. My outfit served on the eastern marches of the Great Wood."))

(define (jess-wood knpc kpc)
  (say knpc "The rangers keep things pretty well in hand, but that year "
       "the cave goblins and trolls were coming out in force."))

;; Townspeople...
(define (jess-glas knpc kpc)
  (say knpc "It's a nice enough place, considering its an armed camp."))

(define (jess-ange knpc kpc)
  (say knpc "A nice lady. Not exactly a fighter, but she does her part."))

(define (jess-patc knpc kpc)
  (say knpc "Dirty old man! No, I'm just teasing him."))

(define jess-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default jess-default)
       (method 'hail jess-hail)
       (method 'bye  jess-bye)
       (method 'job  jess-job)
       (method 'name jess-name)
       (method 'join jess-join)
       
       ;; trade
       (method 'grai jess-grai)
       (method 'holy jess-grai)
       (method 'trad jess-buy)
       (method 'room jess-buy)
       (method 'buy  jess-buy)
       (method 'drin jess-buy)
       (method 'ware jess-buy)
       (method 'food jess-buy)

       ;; scar
       (method 'trade jess-buy)
       (method 'scar  jess-scar)
       (method 'serv  jess-serv)
       (method 'wood  jess-wood)

       ;; town & people
       (method 'glas jess-glas)
       (method 'ange jess-ange)
       (method 'patc jess-patc)

       ))

(define (mk-jess)
  (bind 
   (kern-mk-char 'ch_jess           ; tag
                 "Jess"             ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townswoman        ; sprite
                 faction-glasdrin         ; starting alignment
                 0 0 1               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'jess-conv         ; conv
                 sch_jess           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_dagger)))                 ; container
                 nil                 ; readied
                 )
   (jess-mk)))
