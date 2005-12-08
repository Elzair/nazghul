;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_bole 48 39 pal_expanded
 (list
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~~ ^^ ^^ ^^ ~~ ^^ ^^ ^^ ~~ || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~~ || || || ~~ || || ^^ ~~ || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ ^^ ^^ tt tt tt .. ^^ ^^ ^^ ^^ ^^ ^^ ~~ ~~ || || ~~ || || ~~ ~~ || || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ ^^ tt tt tt .. .. .. .. ^^ ^^ ^^ ^^ || ~~ ~~ ~~ ~~ ~~ ~~ ~~ || || || || || || || || || || || || ^^ ^^ ^^ || || || || ^^ tt tt tt tt tt ^^ ^^ "
            "^^ ^^ tt .. .. .. .. .. .. .. ^^ tt tt || || ~~ ~~ bb ~~ ~~ || || || || tt tt tt tt tt tt tt tt || ^^ ^^ || || || || || tt tt bb .. bb tt ^^ ^^ "
            "^^ tt tt .. .. .. .. .. .. .. bb .. .. tt || || ~~ ~~ ~~ || || || || tt .. .. .. .. .. tt tt tt || || || || || || || ^^ tt bb .. .. .. bb tt ^^ "
            "^^ tt .. .. .. .. .. .. .. bb .. .. .. tt tt tt tt ~~ tt tt || || tt .. .. tt tt tt .. .. .. tt tt tt tt || || || ^^ ^^ tt .. .. .. .. tt tt ^^ "
            "^^ tt .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. == .. .. tt tt tt .. tt || || || || tt .. .. .. .. tt || || || ^^ ^^ tt bb .. .. .. bb tt ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. .. tt tt tt ~~ tt .. .. .. .. .. tt || || || || tt .. .. .. .. tt || || ^^ ^^ ^^ tt tt bb .. bb tt tt ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || || ~~ ~~ || tt tt tt tt tt || || || || || tt tt .. .. tt tt || ^^ ^^ ^^ ^^ ^^ tt tt tt tt tt ^^ ^^ "
            "^^ tt .. .. rr .. .. && .. .. .. .. .. tt || || ~~ || || || || || || || || || || || || || tt .. .. tt || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || || ~~ || || || || || || || || || || tt tt tt tt .. tt || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || ~~ ~~ tt tt tt tt tt tt tt tt || || tt .. .. .. .. tt || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr rr rr .. rr rr rr .. .. || || ~~ tt tt tt tt tt tt tt tt tt tt || tt .. tt tt tt || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. .. .. .. .. .. bb .. .. .. || ~~ ~~ tt xx xx xx xx xx xx xx xx tt tt tt .. tt tt tt tt tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt tt .. .. .. .. .. bb .. .. tt tt || ~~ tt tt xx cc cc cc cc cc cc xx tt bb .. .. tt tt tt tt tt tt tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt tt tt .. .. .. .. ^^ ^^ tt tt || || ~~ tt tt xx cc xx cc cc cc cc xx .. .. .. tt tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
            "^^ ^^ tt tt .. .. .. ^^ ^^ tt tt || tt tt ~~ tt tt xx xx xx cc cc cc cc cc .. .. bb tt tt xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ tt tt tt tt ^^ ^^ ^^ tt tt || tt ~~ ~~ tt tt tt tt xx cc cc cc cc xx .. .. tt tt tt xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ tt tt ^^ ^^ ^^ tt tt tt || tt ~~ tt tt tt tt tt xx cc cc cc cc xx .. .. bb tt tt xx xx xx xx cc cc xx xx xx xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt || || tt tt ~~ tt || || tt tt xx xx xx && xx xx .. .. .. tt tt xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt || tt tt ~~ ~~ tt || || tt tt tt tt xx xx xx tt bb .. .. bb tt xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt tt tt tt tt tt ~~ ~~ tt tt || || || tt tt tt tt tt tt tt tt .. .. tt tt xx xx xx xx cc cc xx xx xx xx xx xx xx cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt || || || tt ~~ ~~ tt tt || || || || || || tt tt tt tt tt bb .. .. bb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt || || tt tt ~~ tt tt || || || || || || || || || tt tt tt tt .. .. tt tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "^^ || ^^ ^^ ^^ tt tt tt tt ~~ ~~ tt || || || || || || || || || || || tt bb .. .. .. bb tt xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
            "^^ || || ^^ ^^ ^^ tt tt ~~ ~~ tt tt || || || || || || || || || || || tt .. .. .. .. .. .. xx cc cc 00 cc cc && xx && cc cc cc xx cc cc xx ^^ ^^ "
            "^^ || tt tt ^^ ^^ ~~ ~~ ~~ ~~ tt || || || || || || || || || || || || bb .. .. .. .. .. .. cc cc cc 00 cc cc && xx && cc cc cc xx xx xx xx ^^ ^^ "
            "^^ tt tt ~~ ~~ ~~ ~~ ~~ ~~ ~~ tt || || || || || || tt tt tt || || || tt .. .. .. .. .. .. xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
            "^^ tt ~~ ~~ ~~ bb ~~ ~~ ~~ ~~ tt || || || || || tt tt ^^ tt tt || || tt bb .. .. .. bb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ tt tt || || || || tt tt ^^ ^^ ^^ tt tt || || tt tt .. .. tt tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "bb ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ tt || || || || || tt ^^ ^^ ^^ ^^ ^^ tt || || || bb .. .. bb tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
            "~~ ~~ ~~ bb ~~ ~~ ~~ ~~ tt tt || || || || || tt tt ^^ ^^ ^^ tt tt || || || tt .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt ^^ ^^ ^^ "
            "~~ ~~ ~~ ~~ ~~ tt tt tt tt || || || || || || || tt tt tt tt tt || || || tt bb .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt ^^ ^^ "
            "~~ bb ~~ tt tt tt tt tt || || || || || || || || || || || || || || || || tt .. .. .. tt tt tt || || || || || || || || || || || tt tt tt tt tt ^^ "
            "~~ ~~ || tt tt tt tt tt tt || || || || || || || || || || || || || || || bb .. .. .. bb || || || || || || || || || || || || || || || tt tt ^^ ^^ "
            "~~ ~~ || || tt tt tt tt tt || || || || || || || || || || || || || || || tt .. .. tt tt || || || || || || || || || || || || || || || tt ^^ ^^ ^^ "
            "~~ || || || tt tt || || || || || || || || || || || || || || || || || || bb .. .. bb || || || || || || || || || || || || || || || || tt tt ^^ ^^ "
            "|| || || || || || || || || || || || || || || || || || || || || || || || tt .. .. tt || || || || || || || || || || || || || || || || || tt tt ^^ "
   )
 )

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "may.scm")
(mk-may)

(kern-load "kathryn.scm")
(mk-kathryn)

(kern-load "thud.scm")
(mk-thud)

(kern-load "bill.scm")
(mk-bill)

(kern-load "melvin.scm")
(mk-melvin)

(kern-load "hackle.scm")
(mk-hackle)

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_bole     ; tag
 "Bole"      ; name
 s_hamlet           ; sprite
 m_bole             ; map
 #f                 ; wraps
 #f                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list ;; objects

   ;; Tag the special door used as the player's guest room:
   (list (kern-tag 'bole-inn-room-door (mk-locked-door)) 33 17)

   (list (mk-locked-door) 36 17)
   (list (mk-locked-door) 33 20)
   (list (mk-locked-door) 36 20)
   (list (mk-locked-door) 42 25)
   (list (mk-door) 42 28)
   (list (mk-windowed-door) 30 27)
   (list (mk-bed) 31 18)
   (list (mk-bed) 38 18)
   (list (mk-bed) 31 21)
   (list (mk-bed) 38 21)
   (list (mk-bed) 40 17)
   (list (mk-bed) 44 17)

   ;; Bill's hut
   (list (mk-locked-door) 24 17)
   (list (mk-locked-door) 19 15)
   (list (mk-bed) 23 19)

   ;; Hackle's hut
   (list (mk-bed) 5 8)
   (list (mk-windowed-door) 7 13)
   (list (mk-windowed-door) 10 10)

   ;; Thief's door
   (put (mk-thief-door 'p_traps_1 4 16) 43 6)

   ;; npc's
   (list ch_may   44 17)
   (list ch_kathryn 31 18)
   (list ch_thud 32 18)
   (list ch_bill  22 8)
   (put ch_melvin 44 17)
   (put ch_hackle 0 0)
   (put (mk-npc 'bull 1) 6 4)

   (put (mk-npc 'dryad 8) 36 38)
   
   )

 nil ; hooks

 (list ;; edge entrances
  (list north 26 38)
  )

 )

;; (list (mk-cave-entrance 'p_cave_level_1 1 30) 44 1) 
;;   (list (mk-wolf) 11 33)
