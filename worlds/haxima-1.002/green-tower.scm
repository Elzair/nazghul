;;
;; Copyright-Only Dedication (based on United States law)
;;
;; The person or persons who have associated their work with this document (the
;; "Dedicator") hereby dedicate the entire copyright in the work of authorship
;; identified below (the "Work") to the public domain.
;;
;; Dedicator makes this dedication for the benefit of the public at large and
;; to the detriment of Dedicator's heirs and successors. Dedicator intends this
;; dedication to be an overt act of relinquishment in perpetuity of all present
;; and future rights under copyright law, whether vested or contingent, in the
;; Work. Dedicator understands that such relinquishment of all rights includes
;; the relinquishment of all rights to enforce (by lawsuit or otherwise) those
;; copyrights in the Work.
;;
;; Dedicator recognizes that, once placed in the public domain, the Work may be
;; freely reproduced, distributed, transmitted, used, modified, built upon, or
;; otherwise exploited by anyone for any purpose, commercial or non-commercial,
;; and in any way, including by methods that have not yet been invented or
;; conceived.
;;

(kern-mk-map 
 'm_green_tower 64 64 pal_expanded
	(list
		"|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
		"|| || || || || || || || || || || |C rr rr rr rr rr rr rr || || || || || || || || || || tt td .. /7 .. tb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
		"|| || rr rr rr rr rr || || || rr rr rr && cc cc cc cc rr || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || xx xx xx xx xx xx xx xx xx tt tt || || || || || "
		"|| || rr cc cc cc rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || || tt tt td .. /7 .. tb tt || || || || || || || || || || || |C xx .. .. .. .. .. .. .. xx tt tt tt || || || || "
		"|| || rr cc cc cc rr || || || rr cc cc cc cc cc cc cc rr || || || || || || || || || tt tt bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
		"|| || rr cc cc cc rr || || || rr cc rr cc cc cc rr rr rr || || || || || || || || tt tt tt td .. /7 .. bb t3 || || || || || || || || || |% /7 tf sR @@ @@ @@ @@ @@ @@ @@ xx tt tt tt || || || || "
		"|| || rr rr cc rr rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || tt tt tt bb .. /7 .. t3 tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. .. xx tt tt || || || || || "
		"|| || || |% /7 |B || || || || |D /7 rr rr rr rr rr rr rr || || || || || || || tt tt tt tt td .. /7 .. ta tt || || || || || || || || || || /7 t7 xx .S .H .R .O .O .M .S xx || || || || || || || "
		"|| || || || /8 /d /d /d /1 /d /d /a |# || || || || || || || || || || || || || tt tt tt tt bb .. /7 .. bb tt || || || || || || || || || tt /7 tt xx xx xx xx xx xx xx xx xx || || || || || || || "
		"|| || || || || || || |% /7 |# || || || || || || || || || || || || || || || tt tt tt tt tt td .. /7 .. tb tt || || || || || || || || tt tt /7 te xx .. .. .. xx .. .. .. xx || || || || || || || "
		"|| || |C rr rr rr |A || /7 |A || || || || || || || || || || || || || || || tt tt tt tt tt bb .. /7 .. bb tt || || || || || || || || tt tt /8 /d .. .. .. .. ?? .. .. .. ?? tt tt tt tt tt tt tt "
		"|| rr rr rr cc rr rr |E /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt td .. /7 .. tb tt || || || || tt tt tt tt tt tt tt t5 xx .. .. tf xx .. .. .. xx || || || || || || || "
		"|| rr cc cc cc cc rr rr /7 |# || || || || || || || || |% /7 || || || || tt tt tt tt tt tt bb .. /7 .. bb tt || || || || tt t& ta tt tt || || || xx xx ws xx xx xx .. xx xx || || || || || || || "
		"|| rr cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt td .. /7 .. tb tt || tt tt tt tt tA tD tt || || || || || || || || || || || || || || || || || || || || "
		"|| rr rr cc cc cc rr rr |# || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt bb .. /7 .. bb tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
		"|| |% rr rr && rr rr |# tt || || || tt tc bb || || tt || /7 || tt tt tt tt tt tt tt tt tt td .. /7 .. tb tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
		"|| || |% rr rr rr |# tt tt tt || tt tc t& tb || || tt bb /7 bb .. bb te bb te bb te bb te t# .. /7 .. t% te bb te bb ta tc bb te bb te bb ta tt || || || || || || || || || || || || || || || || "
		"|| || || tt || || || || tt || tt tt tB .. tD tt || tc .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb tt || || || || || || || || || || || || || || || "
		"|| || tt tt tt || || || || || || tt t5 tE t3 || || bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
		"|| || || tt || || || || || tt tt tt tt tt tt || || td .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. ta tt || || || || || || || || || || || || || || || "
		"|| || || || || || || tt tt tt tt tt tt tt || || || bb .. /7 .. .. .. bb t7 bb t7 bb t7 .. .. .. /7 .. .. .. bb t3 t5 bb t7 bb tf tA .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || || tt tt tc t& ta tt tt || || || || td .. /7 .. .. t3 tt tt tt tt tt tt bb .. .. /7 .. .. bb t3 tt tt tt tt t5 bb t7 .. /7 .. tb tt || || || || || || || || || || || || || || || "
		"|| || || || || bb tt tc t# .. t% ta tt tt || || || bb .. /7 .. bb tt || || || || || tc xx w+ xx cc xx w+ xx ta || || || || || tt tc .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || tt tt tB .. .. .. tD tt tt || || || || .. /7 .. tb tt || || || |C xx w+ xx cc cc cc cc cc xx xx xx |A || || || tt bb .. /7 .. tb tt || || || || || || || || || || || || || || || "
		"|| || || || || tt tt t5 tA .. tC t3 tt tt || || || bb .. /7 .. bb tt || |C xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx |A || tt td .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || tt tt tt t5 tE t3 tt tt || || || || || .. /7 .. tb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. t3 tt || || || || || || || || || || || || || || || "
		"|| || tt tt tt tt || tt tt tt tt bb || || tt tt || bb .. /7 .. bb tt |C xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |A tt td .. /7 .. ta tt || || || || || || || || || || || || || || || "
		"|| || tt || || || || || tt tt tt || || || || || || td .. /7 .. tb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || tt "
		"tt tt tt || || || || || || || || || || || || || tt bb .. /7 .. bb te xx cc cc cc xx xx xx |v .. cc .. tb |v xx xx cc cc cc xx ta td .. /7 .. tb tt || || || || || || || || || || || || || || tt "
		"bb ta tt tt tt tt tt tt tt tt tt tt tt tt tt tt tc .. .. /7 .. tf bb xx cc cc cc x! |v |v |v .. cc .. t% ta |v x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || || || || tt "
		".. .. bb te bb te bb te bb te bb te bb te bb bb .. .. .. /7 .. .. .. w+ cc cc cc xx te t# bb .. cc .. bb t% te xx cc cc cc w+ .. .. .. /7 .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. bb t7 bb t7 bb t7 bb t7 bb t7 bb t3 tt t5 bb .. .. /7 .. .. .. w+ cc cc cc xx t7 tA bb .. cc .. bb tC t7 xx cc cc cc w+ .. .. .. /7 .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"bb t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 .. /7 .. .. bb xx cc cc cc x! |v t5 tA .. cc .. tC t3 |v x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || tt tt tt tt "
		"tt tt || || || || tt tt tt tt tt tt tt tt || || tt tc .. /7 .. .. t7 xx cc cc cc xx xx |v td .. cc .. tb |v xx xx cc cc cc xx t7 .. .. /7 .. tb tt || || || || || || || || || || || || || || tt "
		"tt tt || || || || tt tt tt tt tt tt tt tt || || tt bb .. /7 .. bb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || tt tt tt tt tt tt tt || || tt td .. /7 .. tb tt |% xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |# tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || || tt tt tt tt tt tt || || tt bb .. /7 .. bb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || || || || || || || || || || tt td .. /7 .. tb tt || |% xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx |# || tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || |% xx w+ xx cc cc cc cc cc xx xx xx |# || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || || || || || || || || rr rr tt td .. /7 .. tb tt || || || || || t5 xx w+ xx cc xx w+ xx t3 || || || || tt tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
		"|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt tt tt tt tt tt tc bb .. .. /7 .. .. bb ta tt tt tt tt tt tt bb .. /7 .. bb tt tt tt tt tt tt tt || || || || || || || || || "
		"|| || || || || || || tt tt tt tt || || || || || tt td .. /7 .. .. te bb te bb te bb .. .. .. .. /7 .. .. .. .. bb te bb te bb te .. .. /7 .. .. te bb te bb te bb tt || || || || || || || || || "
		"|| || || || || tt rr tt rr rr tt rr rr |A || || tt bb .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. t% tb tt || || || || || || || || || "
		"|| || || rr rr tt tt tt || || tt t5 rr rr rr || tt td .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 .. bb tt || || || || || || || || || "
		"|| || || rr |# || || tt tt tt || tt tt |% rr || || bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tb tt || || || || || || || || || "
		"|| || || || tt tt tt tc t# t% ta || tt tt || rr rr || || t5 bb t7 bb t7 bb t7 bb t7 bb t7 bb .. /7 .. t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb tf bb tb td bb /4 /2 bb ta || || || || || || || || || "
		"|| rr rr || tt tt |C t# .. bb t% tt tt tt tt |% rr rr || tt tt tt tt tt tt tt tt tt tt tt t5 .. /7 .. tt tt tt tt tt tt tt tt tt tt tt xx xx xx xx ws sT cc cc sI ws xx xx xx xx || || || || || "
		"|| || || || tt tc t# .. .. .. .. te t& ta tt || |% rr || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
		"|| tt tt tt tc t# .. .. .. .. .. .. .. t% ta tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
		"|| tt tt tt t# bb .. .. .. .. .. .. .. bb t% tt tF ta tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx || || || || || "
		"|| || || tt tA .. .. .. .. aa .. .. .. .. tC tt || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || |C xx cc cc 00 cc cc && && cc cc 00 cc cc xx |A || || || || "
		"|| rr || tt tt tt t5 .. .. .. .. .. .. t3 tt || || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || xx xx xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx xx xx || || || "
		"|| rr |A || || tF tt .. .. .. .. .. .. tt tt || rr || || |C rr rr tt rr rr |A || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
		"|| rr rr || tt tt tt bb .. .. .. bb .. tt tt |C rr || || rr rr tb tt td rr rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
		"|| |% rr || tt tt tt tt t5 tA .. .. tC tt || rr rr || || rr tf .! .! .! tf rr || || || tt tt .. /7 .. tt || || || || || || || |C xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |A || || "
		"|| || || || || || || || tt tt tt tt tt || tt rr |# || || rr .! .! && .! .! rr || || || tt tt .. /7 .. || || || || || || || || xx xx cc xx xx @@ @@ @@ @@ @@ @@ @@ @@ @@ @@ xx xx cc xx xx || || "
		"|| || || rr rr |A || || tt tt tt || tt tt || rr || || || rr tf .! .! .! .! rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || "
		"|| || || |% rr rr || rr rr rr || rr rr rr rr || || || || rr rr .! .! tb t5 rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx .W .H .I .T .E @@ .S .T .A .G xx cc cc cc xx || || "
		"|| || || || || || || || || || || || || || || || || || || |% rr rr rr rr tt || || || tt tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx || || "
		"|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tc .. /7 .. ta tt || || || || || || xx xx xx xx xx |# || || || || || || || || |% xx xx xx xx xx || || "
		"|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt t# .. /7 .. t% tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "
	)
)

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "shroom.scm")
(mk-shroom 'ch_shroom)

(kern-load "gen.scm")
(mk-gen 'ch_gen)

(kern-load "doris.scm")
(mk-doris 'ch_doris)

(kern-load "deric.scm")
(mk-deric 'ch_deric)

(kern-load "jorn.scm")
(kern-load "abe.scm")
(kern-load "kama.scm")
(kern-load "abigail.scm")
(kern-load "edward.scm") ;; for GTL

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_green_tower "Green Tower" s_towertown_ruin m_green_tower #f #f #f #f 
 nil ;; subplaces
 nil ;; neighbors

 ;; objects:
 (list

  ;; characters:
  (put ch_shroom  14 1)
  (put ch_gen     0 0)
  (put ch_doris   0 0)
  (put (mk-abigail) 0 0)
  (put ch_deric   0 0)
  (put (mk-jorn) 0 0)
  (put (mk-abe) 0 0)
  
  ;; Shroom's Shop
  (put (mk-locked-door) 48 10)
  (put (mk-door) 48 6)
  (put (mk-door) 48 4)
  (put (mk-magic-locked-door) 54 12)
  (put (mk-bed) 51 9)
  (put (mk-chest 'burn-trap
                 '((50 sulphorous_ash)
                              (50 garlic)
                              (50 ginseng)
                              (50 blood_moss)
                              (50 black_pearl)
                              (50 spider_silk)
                              (50 mandrake)
                              (50 nightshade)))
       53 9)
  (put (mk-chest 'poison-trap
                 '((10 t_cure_potion)
                              (10 t_heal_potion)
                              (10 t_mana_potion)
                              (10 t_poison_immunity_potion)
                              (1 t_invisibility_potion)))
       54 9)
  (put (mk-chest 'sleep-trap
                 '((1 t_armor_leather)
                              (1 t_sword)
                              (1 t_shield)
                              (1 t_leather_helm)))
       55 9)


  ;; Gen's Hut
  (put (mk-door-in-rock) 7 13)
  (put (mk-bed) 2 13)
  (put (mk-chest nil 
                 '((100 t_arrow)
                              (1 t_bow)))
       4 11)
                
  ;; Deric's Hut
  (put (mk-door-in-rock) 11 6)
  (put (mk-door-in-rock) 16 6)
  (put (mk-bed) 17 4)
  (put (mk-chest 'burn-trap
                 '((100 t_bolt)
                              (1 t_crossbow)
                              (1 t_shield)
                              (2 t_cure_potion)
                              (5 t_heal_potion)))
       17 6)
  
  ;; White Stag Lodge
  (put (mk-door) 51 49)
  (put (mk-door) 52 49)
  (put (mk-door) 57 59)
  (put (mk-clock) 46 50)
  (put (kern-tag 'white-stag-door (mk-locked-door)) 44 58)
  (put (mk-bed) 43 61)
  (put (mk-bed) 58 61)
  (put (mk-locked-door) 59 58)
  
  ;; Tower 
  (put (mk-lever 'gtl-portcullis-1) 29 28)
  (put (mk-ladder-down 'p_green_tower_lower 10 26) 32 32)
  (put (mk-door) 32 22)
  (put (mk-door) 41 32)
  (put (mk-door) 32 42)
  (put (mk-door) 23 32)

  ;; Guards
  (put (mk-monman) 0 0)
  (put (spawn-pt 'ranger) 31 5)
  (put (spawn-pt 'ranger) 5  31)
  (put (spawn-pt 'ranger) 56 31)
  (put (spawn-pt 'ranger) 31 56)
  
  )
 ;; On-entry hook
 (list 'on-entry-to-dungeon-room 
       'lock-inn-room-doors
       )
 nil   ; edge entrances
)

(mk-place-music p_green_tower 'ml-large-town)
