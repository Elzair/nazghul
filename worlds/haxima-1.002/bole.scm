;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_bole 48 48 pal_expanded
 (list
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~~ ^^ ^^ ^^ ~~ ^^ ^^ ^^ ~~ || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~~ || || || ~~ || || ^^ ~~ || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. ^^ ^^ ^^ "
            "^^ ^^ ^^ tt tt tt .. ^^ ^^ ^^ ^^ ^^ ^^ ~~ ~~ || || ~~ || || ~~ ~~ || || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ tt ^^ ^^ ^^ ^^ ^^ .. ^^ ^^ ^^ "
            "^^ ^^ tt tt tt .. .. .. .. ^^ ^^ ^^ ^^ || ~~ ~~ ~~ ~~ ~~ ~~ ~~ || || || || || || || || || || || || ^^ ^^ ^^ tt tt .. tt ^^ ^^ ^^ .. .. .. ^^ ^^ "
            "^^ ^^ tt .. .. .. .. .. .. .. ^^ || || || || ~~ ~~ bb ~~ ~~ || || || || || || || .. .. .. .. .. tt ^^ ^^ tt tt .. .. .. ^^ .. .. .. .. .. ^^ ^^ "
            "^^ tt tt .. .. .. .. .. .. .. bb .. .. || || || ~~ ~~ ~~ || || || || || .. .. .. .. || || || .. tt tt tt tt .. .. tt .. .. .. .. .. .. .. ^^ ^^ "
            "^^ tt .. .. .. .. .. .. .. bb .. .. .. || || || tt ~~ tt || || || || .. .. || || || || || || .. .. .. .. .. .. || || tt ^^ .. .. .. tt tt ^^ ^^ "
            "^^ tt .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. == .. .. || || || .. || || || || || || || || .. || || || || || || || ^^ tt tt tt || || ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. .. || || tt ~~ tt .. .. .. .. .. || || || || || || || .. .. || || || || || ^^ ^^ ^^ ^^ || || || ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. || || || ~~ ~~ || || || || || || || || || || || .. .. .. || || || || ^^ ^^ ^^ ^^ ^^ ^^ || || ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. && .. .. .. .. .. || || || ~~ || || || || || || || || || || || .. .. || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. || || || ~~ || || || || || || || || || || || .. || || || || || || ^^ ^^ || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr .. .. .. .. .. rr .. .. || || ~~ ~~ .. .. .. .. .. .. .. .. tt tt || .. || || || || || || ^^ ^^ || || || || || || tt ^^ ^^ ^^ ^^ "
            "^^ tt .. .. rr rr rr .. rr rr rr .. .. || || ~~ .. .. .. .. .. .. .. .. .. .. tt || .. || || || || || || ^^ ^^ || || || || || tt tt tt ^^ ^^ ^^ "
            "^^ tt .. .. .. .. .. .. .. bb .. .. .. || ~~ ~~ .. xx xx xx xx xx xx xx xx .. tt tt .. tt tt || || || || || || || || || || || tt tt tt ^^ ^^ ^^ "
            "^^ tt tt .. .. .. .. .. bb .. .. tt tt || ~~ .. .. xx cc cc cc cc cc cc xx .. .. .. .. tt tt tt || || || || || || || || || || || tt ^^ ^^ ^^ ^^ "
            "^^ tt tt tt .. .. .. .. ^^ ^^ tt tt || || ~~ .. .. xx cc xx cc cc cc cc xx .. .. .. .. tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
            "^^ ^^ tt tt .. .. .. ^^ ^^ tt tt || tt tt ~~ .. .. xx xx xx cc cc cc cc cc /d /d /2 .. .. xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ tt tt tt tt ^^ ^^ ^^ tt tt || tt ~~ ~~ .. .. .. .. xx cc cc cc cc xx .. .. /7 .. .. xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ tt tt ^^ ^^ ^^ tt tt tt || tt ~~ tt .. .. .. .. xx cc cc cc cc xx .. .. /7 .. .. xx xx xx xx cc cc xx xx xx xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt || || tt tt ~~ tt tt .. .. .. xx xx xx && xx xx .. .. /7 .. .. xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt || tt tt ~~ ~~ tt tt .. .. .. .. tt xx xx xx tt .. .. /7 .. .. xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt tt tt tt tt tt ~~ ~~ tt tt tt .. .. .. .. .. .. .. .. .. .. /0 /a .. tt xx xx xx xx cc cc xx xx xx xx xx xx xx cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt || || || tt ~~ ~~ tt tt || tt tt tt tt .. .. .. .. .. .. .. /7 .. .. tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt || || tt tt ~~ tt tt || || || || || tt tt tt tt tt tt .. .. /7 .. tt tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "|| || ^^ ^^ ^^ tt tt tt tt ~~ ~~ tt || || || || || || || || || || || tt .. .. /7 .. .. tt xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
            "|| || || ^^ ^^ ^^ tt tt ~~ ~~ || || || /0 /d /d /d /d /d /d /2 || || .. .. /0 /9 /2 .. .. xx cc cc 00 cc cc && xx && cc cc cc xx cc cc xx ^^ ^^ "
            "|| || || || ^^ ^^ ~~ ~~ ~~ ~~ || /0 /d /a || || || || || || /8 /d /d /d /d /6 ~~ /4 /d /d cc cc cc 00 cc cc && xx && cc cc cc xx xx xx xx ^^ ^^ "
            "|| || || ~~ ~~ ~~ ~~ ~~ ~~ ~~ || /7 || || || || || tt tt || || || || .. .. /8 /d /a .. .. xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
            "|| || ~~ ~~ ~~ bb ~~ ~~ ~~ ~~ .. /7 .. .. || || tt tt ^^ || tt || || .. .. .. .. .. .. tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ .. .. /7 .. .. .. || tt ^^ ^^ ^^ tt tt || || .. .. .. .. tt tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
            "bb ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ .. .. /7 .. .. .. || ^^ ^^ ^^ ^^ ^^ tt || || || || || || || || xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
            "~~ ~~ ~~ bb ~~ ~~ ~~ ~~ .. .. .. /7 .. .. .. || || ^^ ^^ ^^ tt tt tt tt tt || || || || || || || || || || || || || || || || tt ^^ ^^ ^^ ^^ ^^ ^^ "
            "~~ ~~ ~~ ~~ ~~ tt tt tt .. .. .. /7 .. .. .. .. || tt tt tt tt tt ^^ ^^ tt tt || || || || || || || || tt || || || || || || tt tt tt ^^ ^^ ^^ ^^ "
            "~~ bb ~~ tt tt tt tt tt .. .. .. /7 .. .. .. || || || tt || tt ^^ ^^ ^^ ^^ tt tt tt tt tt || || || tt tt tt || || || || || || || tt tt tt ^^ ^^ "
            "~~ ~~ || tt tt tt tt tt tt .. .. /7 .. .. || || || || || || tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt || || || || tt || || || || || || || || || || tt ^^ ^^ "
            "~~ ~~ || || tt tt tt tt tt .. .. /7 .. .. || || || || || || || tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt tt tt tt tt || || || || || || || tt || || || ^^ "
            "~~ || || || tt tt tt tt || || .. /7 .. || || /0 /d /d /d /2 || tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt tt || || || || || || tt tt tt || || ^^ "
            "|| || || || || tt tt || || || || /7 || || || /7 tt tt tt /7 || || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt ^^ ^^ ^^ ^^ || || || tt || || || ^^ "
            "|| || || || || || tt tt tt || || /8 /d /d /d /a || || || /8 /d /2 || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt ^^ ^^ ^^ ^^ ^^ || || || || || || || ^^ "
            "|| || || || || tt tt ^^ tt || || || || || || || || || || || || /7 || || || tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ || || || || || || ^^ ^^ "
            "|| || tt tt tt tt ^^ ^^ tt || || || || || || || || || || || || /7 || || || tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ || || || || || || ^^ ^^ "
            "tt || tt ^^ ^^ ^^ ^^ ^^ tt || || || || || || || || || || || .. /7 .. || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. tt tt tt tt tt ^^ ^^ ^^ "
            "tt tt tt ^^ ^^ ^^ ^^ ^^ tt || || tt tt || || || || || || tt .. /7 .. tt || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. .. .. tt tt tt ^^ ^^ ^^ "
            "tt ^^ ^^ ^^ ^^ ^^ tt tt tt || || tt tt tt || || || || tt tt .. /7 .. tt tt || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ ^^ tt || || || || || tt tt || || || || tt .. .. /7 .. .. tt || || || tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. .. .. tt tt ^^ ^^ ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt tt || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. tt tt ^^ ^^ ^^ ^^ ^^ "
            "^^ ^^ ^^ ^^ ^^ tt || || || || || || || || || || || tt .. .. .. /7 .. .. .. tt || || || || || tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
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

   (list (mk-door) 24 17)
   (list (mk-locked-door) 19 15)
   (list (mk-bed) 23 19)

   ;; Hackle's hut
   (list (mk-bed) 5 8)
   (list (mk-windowed-door) 7 13)
   (list (mk-windowed-door) 10 10)

   ;; Thief's door
   (put (mk-thief-door 'p_bole 15 15) 45 5)

   ;; npc's
   (list ch_may   44 17)
   (list ch_kathryn 31 18)
   (list ch_thud 32 18)
   (list ch_bill  22 8)
   (put ch_melvin 44 17)
   (put ch_hackle 0 0)
   
   )

 nil ; hooks

 (list ;; edge entrances
  (list north 21 47)
  )

 )

;; (list (mk-cave-entrance 'p_cave_level_1 1 30) 44 1) 
;;   (list (mk-wolf) 11 33)
