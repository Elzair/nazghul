;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_bole 48 39 pal_expanded
	(list
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~6 ^a ^^ ^c ~6 ^a ^^ ^^ ~6 || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^c t7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~6 |B || |% ~6 |# |% ^e ~6 || || || || || || || || || ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^c t3 tt tc {5 ^^ ^^ ^^ ^^ ^^ ^^ ~a ~5 |A |C ~6 |A |C ~3 ~c || || || || || || || || || |% ^a ^^ ^^ ^^ ^^ ^c |& ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ t3 tt tc t# .. {1 {5 ^^ ^^ ^^ ^c |& ~a ~1 ~~ ~~ ~~ ~1 ~c |# || || || || || || || || || || |% ^^ ^^ ^c |# || || |% ^e t3 tt tt tt t5 ^^ ^^ "
		"^^ ^c tt t# .. .. .. .. .. {5 ^c tb tt || |% ~a ~~ bb ~~ ~c |# || || || tt tt tt tt tt tt tt tt || ^a ^c |# || || || || tt tc bb .. bb te ^a ^^ "
		"^^ t3 tc .. .. .. .. .. .. .. bb .. t% tt || tH ~a ~~ ~c tG || || || tc t# .. .. .. t% ta tt tt || || || || || || |C ^7 tt bb .. .. .. bb t7 ^^ "
		"^^ tt t# .. .. .. .. .. .. bb .. .. .. ta tt tt td ~6 tb tt || || tt t# tC t3 tt t5 tA .. t% ta tt tt tt || || || ^3 ^^ tt .. .. .. .. tb tt ^^ "
		"^^ tt .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. == .. t% ta tt tc .. t3 || || || || t5 .. .. .. t% tt || || |C ^^ ^^ tt bb .. .. .. bb tt ^^ "
		"^^ tt .. .. rr .. .. .. .. .. rr .. .. tC t3 tt td ~6 t7 tA .. .. .. tC tt || || || || tt tA .. .. tC tt || |C ^3 ^^ ^^ ta t5 bb .. bb t3 tc ^^ "
		"^^ tt .. .. rr .. .. .. .. .. rr .. .. t3 || || ~3 ~c || tt tt tt tt tt || || || || || tt t5 .. .. t3 tt || ^3 ^^ ^^ ^^ ^5 ta tt tt tt tc ^3 ^^ "
		"^^ tt .. .. rr .. .. && .. .. .. .. .. tt || || ~6 |# || || || || || || || || || || || || tt .. tC tt || |C ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || |C ~6 || || || || || || || || || || tt tt tt tc .. t3 || |C ^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || ~3 ~c tt tt tt tt tt tt tt tt || || tt t# .. .. tC tt || ^b ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ tt .. .. rr rr rr .. rr rr rr .. .. || |C ~6 t3 tt tt tt tt tt tt tt tt tt || tt .. t3 tt tt || || |% ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ tt tA .. .. .. .. .. .. bb .. .. tC || ~3 ~c tt xx xx xx xx xx xx xx xx tt tt tc .. tt tt tt tt tt tt t5 ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ tt t5 tA .. .. .. .. bb {8 tC t3 tt || ~6 t3 tt xx cc cc cc cc cc cc xx te bb .. tC tt tt tt tt tt tt tt tt td ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ta tt t5 .. .. .. {c ^^ ^c t3 tt || || ~6 tt tt xx cc xx cc cc cc cc xx .. .. .. t3 tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
		"^^ ^5 tt tt tA .. {c ^^ ^^ t3 tt || tt tc ~6 tt tt xx xx xx cc cc cc cc cc .. .. bb tt tt xx cc cc cc cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
		"^^ ^^ ta tt tt td ^^ ^^ ^c tt tt || tt ~3 ~c tt tt tt t5 xx cc cc cc cc xx .. .. tb tt tt xx cc cc x! cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
		"^^ ^^ ^5 ta tc ^3 ^^ ^^ t3 tt tt || tt ~6 t3 tt tt tt tt xx cc cc cc cc xx .. .. bb tt tt xx xx xx xx cc cc xx xx xx xx cc cc cc cc cc xx ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^c tt || || tt tc ~6 tt || || tt tt xx xx xx && xx xx .. .. .. ta tt xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^c t3 tt || tt tc ~3 ~c tt || || tt tt tt t5 xx xx xx t7 bb .. .. bb tt xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ t3 tt tt tt tt tc ~3 ~c t3 tt || || || tt tt tt tt tt tt tt td .. .. tb tt xx xx xx xx cc cc xx xx xx xx xx xx xx cc cc xx ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ tt || || || tt ~3 ~c t3 tt || || || || || || tt tt tt tt tt bb .. .. bb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ tt || || tt tc ~6 t3 tt || || || || || || || || || tt tt tt td .. .. tb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
		"^^ |& ^a ^^ ^c ta tt tt tc ~3 ~c tt || || || || || || || || || || || tt bb .. .. .. bb te xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
		"|| || |% ^a td td ta tL ~3 ~4 t3 tt || || || || || || || || || || || tc .. .. .. .. .. .. sI cc cc 00 cc cc && xx && cc cc cc xx cc cc xx ^^ ^^ "
		"|| || tt td td tL ~3 ~~ ~~ ~4 tt || || || || || || tt tt tt || || || bb .. .. .. .. .. .. cc cc cc 00 cc cc && xx && cc cc cc xx xx xx xx ^^ ^^ "
		"|| tt tt ~3 ~1 ~~ ~~ ~~ ~~ ~4 tt || || || || || tt tt tt tt tt || || t5 .. .. .. .. .. .. xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
		"tt tt tt ~2 ~~ b~ ~~ ~~ ~~ ~c tt || || || || tt tt tc ^7 ta tt tt || tt bb .. .. .. bb t7 xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
		"tt tt tt ~a ~~ ~~ ~~ b~ ~~ tG tt || || || tt tt tc ^3 ^^ ^5 ta tt || || tt td .. .. tb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
		"tt tt tt tH ~a ~~ ~~ ~~ ~c tt || || || || tt tt ^b ^^ ^^ ^^ ^d tt tt || || bb .. .. bb tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
		"|| tt tt tt tH ~a ~8 ~c tG tt || || || || tt tt t5 ^a ^^ ^c t3 tt tt || || td .. .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 ^^ ^^ ^^ "
		"|| || || tt tt tt tt tt tt || || || tt tt tt tt tt tt tt tt tt tt || || tt bb .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 ^a ^^ "
		"|| || || || || tt tt tt || || || tt tt tt tt tt tt tt tt tt tt || || || tc .. .. .. tb tt tt || || || || || || || || || || || tt tt tt tt td ^^ "
		"|| || || || || tt bb tt || || tt tt bb tt || || || || || || || || || || bb .. .. .. bb || || || || || || || || || || || || || || || tt tc ^3 ^^ "
		"|| || || || || tt tt tt tt tt tt tt tt tt || || || || || || || || || || td .. .. tb tt || || || || || || || || || || || || || || || tt ^b ^^ ^^ "
		"|| || || || || || tt tt tt tt tt tt || || || || || || || || || || || || bb .. .. bb || || || || || || || || || || || || || || || || tt t5 ^a ^^ "
		"|| || || || || || || || || || || || || || || || || || || || || || || || t5 .. .. t3 || || || || || || || || || || || || || || || || || tt t5 ^^ "
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

   (list (mk-locked-door) 36 18)
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
   (list (mk-clock) 35 17)

   ;; Bill's hut
   (list (mk-locked-door) 24 17)
   (list (mk-locked-door) 19 15)
   (list (mk-bed) 23 19)

   ;; Hackle's hut
   (list (mk-bed) 5 8)
   (list (mk-windowed-door-in-rock) 7 13)
   (list (mk-windowed-door-in-rock) 10 10)

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
   )

 ;; on-entry-hook
 (list 'lock-inn-room-doors)

 (list ;; edge entrances
  (list north 26 38)
  (list east  0  30)
  (list northeast 7 38)
  (list northwest 45 38)
  )

 )

(mk-place-music p_bole 'ml-small-town)

;;-----------------------------------------------------------------------------
;; Make a special cave for the dryad so it doen't kill the town with its wolves
;;-----------------------------------------------------------------------------
(kern-mk-map
 'm_dryad_grove 19 19 pal_expanded
	(list
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^c |# || || || || || || || || || || |% ^a ^^ "
		"^^ ^^ ^^ {3 tb tt tt || || tt tt tt || || || || || |% ^^ "
		"^^ ^^ {3 .. tD tt tt tt tt tc t& ta tt tt || || || || || "
		"^^ ^c t7 tE t3 tt tt tt tt tB .. tD tt tt tt tt || || || "
		"^^ |# tt tt tt || || tt tt t5 tE t3 tt tt tt tt tt tt tt "
		"^^ || || || || || || || || tt tt tt || || tt tt tt tt tt "
		"^^ || || || || || || || || || || || || || tt tt tc t# .. "
		"^^ || || || || || || || || || || || || || tt tt t# .. .. "
		"^^ || || || || || || || || || || || || || tt tt .. .. .. "
		"^^ || || tt tt tt || || || || || || || || tt tt tA .. .. "
		"^^ || tt tt tt tt tt || || || || || || || tt tt t5 tA .. "
		"^^ || tt tt tt tt tt tt || || || || || tt tt tt tt tt tt "
		"^^ || tt tt tt tt tt tt tt || || || || tt tt tt tt tt tt "
		"^^ || tt tt tt tt tt tt tt || || || tt tt tt || || || || "
		"^^ || || tt tt tt tt tt tt || || || tt tt tt || || || || "
		"^^ || || || tt tt tt tt || || || tt tc t& ta tt || |C ^^ "
		"^^ |A || || || || || || || || || tc t# {8 t% ta |C ^3 ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
	)
 )
(kern-mk-place 
 'p_dryad_grove ; tag
 "Dryad Grove"   ; name
 nil     ; sprite
 m_dryad_grove  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-npc 'dryad 8) 4 14)
  (put (kern-mk-obj t_2H_axe 1) 5 15)
  )

 nil ; hooks
 nil ; edge entrances
 )

(mk-dungeon-level 
 (list p_dryad_grove p_bole)
 )

(mk-place-music p_dryad_grove 'ml-small-town)
