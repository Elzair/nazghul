;; shroom.scm - an old hag with an interesting history who lives in the
;; northeast corner of green tower.

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_green_tower x y w h))
(kern-mk-sched 'sch_shroom
               (list 0  0  (mk-zone 51 9  1  1)  "sleeping")
               (list 8  0  (mk-zone 40 11 3  3)  "idle")
               (list 9  0  (mk-zone 49 6  7  1)  "working")
               (list 12 0  (mk-zone 50 9  1  1)  "eating")
               (list 13 0  (mk-zone 49 6  7  1)  "working")
               (list 18 0  (mk-zone 56 54 1  1)  "eating")
               (list 19 0  (mk-zone 53 50 4  7)  "idle")
               (list 21 0  (mk-zone 51 9  1  1)  "sleeping"))

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (shroom-mk gave-quest? finished-quest?) (list gave-quest? 
                                                      finished-quest?))
(define (shroom-gave-quest? shroom) (car shroom))
(define (shroom-quest-done? shroom) (cadr shroom))
(define (shroom-give-quest shroom) (set-car! shroom #t))
(define (shroom-set-quest-done! shroom) (set-car! (cdr shroom) #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Shroom's merchant procedure
(define (shroom-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Yes, I trade in mushrooms and the like. "
           "Come by my shop in the northeast corner when I'm open.")
      (begin
        (say knpc "Are ye interested in mushrooms or other reagents?")
        (if (not (kern-conv-get-yes-no? kpc))
            (say knpc "Don't try to pick your own. "
                 "Kill you, the bad ones will!")
            (begin
              ;; Trading!
              (kern-conv-trade knpc kpc
                               (list sulphorous_ash (* 2 reagent-price-mult))
                               (list garlic         (* 3 reagent-price-mult))
                               (list ginseng        (* 3 reagent-price-mult))
                               (list blood_moss     (* 4 reagent-price-mult))
                               (list spider_silk    (* 5 reagent-price-mult))
                               (list nightshade     (* 10 reagent-price-mult))
                               (list mandrake       (* 8 reagent-price-mult))
                               (list t_heal_potion  55)
                               (list t_mana_potion  48)
                               (list t_cure_potion  50)
                               (list t_poison_immunity_potion 75)
                               )
              (say knpc "Be careful with those."))))))

;; Shroom's mushroom quest
(define (shroom-wards knpc kpc)
  (let ((shroom (kobj-gob-data knpc)))
    (if (shroom-gave-quest? shroom)
        ;; gave quest
        (if (shroom-quest-done? shroom)
            ;; quest already done
            (say knpc "I've forgotten all the others. But long ago the "
                 "librarian from Glasdrin had me teach him and he wrote them "
                 "all down. Check with him.")
            ;; quest NOT yet done
            (begin
              (say knpc "Bring me the mushrooms and I will teach ye the ward "
                   "of panic. "
                   "Do ye remember where they are?")
              (if (kern-conv-get-yes-no? kpc)
                  (say knpc "Well...")
                  (say knpc "[sigh] Perhaps ye should write this down. "
                       "Leave town and go south to the mountains by the sea. "
                       "There you will find the cave entrance."))))
        (begin
          (say knpc "In my time I knew many battle wards. "
               "Be wanting me to teach ye, now, won't ye?")
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (say knpc "I know a battle ward that will throw your enemies "
                     "into a panic. But perhaps ye can do me a favor first, "
                     "yes?")
                (if (kern-conv-get-yes-no? kpc)
                    (begin
                      (say knpc "In a cave to the south grows "
                           "a purple mushroom. Bring me one. Agreed?")
                      (if (kern-conv-get-yes-no? kpc)
                          (begin
                            (say knpc "Good. A colony of slimes infests that "
                                 "cave, so take plenty of flaming oil!")
                            (shroom-give-quest shroom))
                          (say knpc "Yes, perhaps ye are afraid.")))
                    (say knpc "Naught for naught, youngling!")))
              (say knpc "Of course, a skillfull warrior such as you has "
                   "nothing to learn from an old witch like me."))))))
                               
(define (shroom-hail knpc kpc)
  (let ((shroom (kobj-gob-data knpc)))
    (display "shroom: ")
    (display shroom)(newline)
    (if (shroom-gave-quest? shroom)
        ;; gave quest
        (if (shroom-quest-done? shroom)
            ;; quest done
            (say knpc "Hello again, young wanderer. Come visit a bit with old "
               "Shroom.")
            ;; quest not done yet
            (if (in-inventory? kpc t_royal_cape)
                (begin
                  ;; player has shrooms
                  (say knpc "Ah, ye have the mushroom, as I requested!")
                  (kern-obj-remove-from-inventory kpc t_royal_cape 1)
                  (shroom-set-quest-done! shroom)
                  (say knpc "Now for your reward. The ward is called "
                       "In Flam Sanct, of the first circle. Mix royal cape, "
                       "sulphorous ash and garlic. Cast it on yourself or a companion "
                       "and fire will not harm!"))
                ;; player does NOT have shrooms yet
                (say knpc "No purple mushroom yet, I see. No rush, dear. "
                      "But I would like it before I die.")))
        ;; has NOT given quest yet
        (say knpc "Hello and well met."))))

(define (shroom-thie knpc kpc)
  (say knpc "I've seen no one strange about here."))

(define (shroom-roya knpc kpc)
  (say knpc "Do you know what the royal cape mushroom is for?")
  (if (yes? kpc)
      (say knpc "Very rare it is, but often found with yellow slimes.")
      (say knpc "It is used in spells which absorb effects!")))

(define shroom-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Long ago I might have "
                                                "known about that.")))
       (method 'hail shroom-hail)
       (method 'bye (lambda (knpc kpc) (say knpc "Toodaloo!")))
       (method 'job (lambda (knpc kpc) (say knpc "I sell potions, reagents "
                                            "and the like.")))
       (method 'name (lambda (knpc kpc) (say knpc "I'm known as Shroom. At "
                                                "your service.")))
       (method 'shro (lambda (knpc kpc) (say knpc "Mushrooms are my "
                                                "specialty. That's why they "
                                                "call me Shroom.")))
       (method 'maid (lambda (knpc) (say knpc "[she grins with crooked "
                                           "teeth] Is it so hard to believe I "
                                           "was once a fair war-maiden? [she "
                                           "cackles obscenely]")))
       (method 'mush shroom-trade)
       (method 'buy shroom-trade)
       (method 'trad shroom-trade)
       (method 'sell shroom-trade)
       (method 'reag shroom-trade)
       (method 'poti shroom-trade)
       (method 'join (lambda (knpc) (say knpc "You're too young for me, "
                                         "sweetie!")))
       (method 'gen (lambda (knpc) (say knpc "Aye, a handsome young man he "
                                        "was, once. He could stay up all night"
                                        " in bed! But gone a bit strange, he "
                                        "has, befriending the goblins and all "
                                        "that.")))
       (method 'stra (lambda (knpc) (say knpc "He meets them in the forest "
                                            "and runs with their hunts. "
                                            "Half-goblin he nearly is; "
                                            "learned their ways he has. But "
                                            "ye could never tell him what to "
                                            "do, the silly man.")))
       (method 'gobl (lambda (knpc) (say knpc "I trade with them now and "
                                            "then. Their shamans know well "
                                            "the plants in these woods. I "
                                            "even speak a little. Know some "
                                            "of their magic. But I never "
                                            "trust them.")))
       (method 'thie shroom-thie)
       (method 'trus (lambda (knpc) (say knpc "The goblins will turn on us "
                                          "when their opportunity comes. I "
                                          "would do the same in their "
                                          "place!")))
       (method 'wars (lambda (knpc) (say knpc "Ha! Yes, I fought the goblins. "
                                         "Long ago that was. People forget.")))
       (method 'ward shroom-wards)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-shroom tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Shroom"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_companion_druid   ; sprite
                 faction-men         ; starting alignment
                 8 14 8              ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 30 0 9 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'shroom-conv        ; conv
                 sch_shroom          ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (shroom-mk #f #f)))
