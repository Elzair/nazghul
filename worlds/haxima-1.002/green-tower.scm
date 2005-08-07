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
  ;;                                1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  
  ;; 00  1  2  3  4  5  6  7  8  9 00  1  2  3  4  5  6  7  8  9 00  1  2  3  4  5  6  7  8  9 00  1  2  3  4  5  6  7  8  9  
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || ";  ;;  0
  "|| || || || || || || || || || || || rr rr rr rr rr rr rr || || || || || || || || || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || ";  ;;  1
  "|| || rr rr rr rr rr || || || rr rr rr && cc cc cc cc rr || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || xx xx xx xx xx xx xx xx xx tt tt || || || || || ";  ;;  2
  "|| || rr cc cc cc rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || xx .. .. .. .. .. .. .. xx tt tt tt || || || || ";  ;;  3
  "|| || rr cc cc cc rr || || || rr cc cc cc cc cc cc cc rr || || || || || || || || || tt tt bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || ";  ;;  4
  "|| || rr cc cc cc rr || || || rr cc rr cc cc cc rr rr rr || || || || || || || || tt tt tt tt .. /7 .. bb tt || || || || || || || || || || /7 tt xx @@ @@ @@ @@ @@ @@ @@ xx tt tt tt || || || || ";  ;;  5
  "|| || rr rr cc rr rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || tt tt tt bb .. /7 .. tt tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. .. xx tt tt || || || || || ";  ;;  6
  "|| || || || /7 || || || || || || /7 rr rr rr rr rr rr rr || || || || || || || tt tt tt tt tt .. /7 .. tt tt || || || || || || || || || || /7 tt xx .S .H .R .O .O .M .S xx || || || || || || || ";  ;;  7
  "|| || || || /8 /d /d /d /1 /d /d /a || || || || || || || || || || || || || || tt tt tt tt bb .. /7 .. bb tt || || || || || || || || || tt /7 tt xx xx xx xx xx xx xx xx xx || || || || || || || ";  ;;  8
  "|| || || || || || || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt tt .. /7 .. tt tt || || || || || || || || tt tt /7 tt xx .. .. .. xx .. .. .. xx || || || || || || || ";  ;;  9
  "|| || || rr rr rr || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt bb .. /7 .. bb tt || || || || || || || || tt tt /8 /d .. .. .. .. ?? .. .. .. ?? tt tt tt tt tt tt tt ";  ;; 10
  "|| rr rr rr cc rr rr || /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt tt .. /7 .. tt tt || || || || tt tt tt tt tt tt tt tt xx .. .. tt xx .. .. .. xx || || || || || || || ";  ;; 11
  "|| rr cc cc cc cc rr rr /7 || || || || || || || || || || /7 || || || || tt tt tt tt tt tt bb .. /7 .. bb tt || || || || tt .. tt tt tt || || || xx xx ws xx xx xx .. xx xx || || || || || || || ";  ;; 12
  "|| rr cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt tt tt tt .. .. tt || || || || || || || || || || || || || || || || || || || || ";  ;; 13
  "|| rr rr cc cc cc rr rr || || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt bb .. /7 .. bb tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || ";  ;; 14
  "|| || rr rr && rr rr || tt || || || tt tt bb || || tt || /7 || tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || ";  ;; 15
  "|| || || rr rr rr || tt tt tt || tt tt .. tt || || tt bb /7 bb .. bb tt bb tt bb tt bb tt .. .. /7 .. .. tt bb tt bb tt tt bb tt bb tt bb tt tt || || || || || || || || || || || || || || || || ";  ;; 17
  "|| || || tt || || || || tt || tt tt .. .. .. tt || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb tt || || || || || || || || || || || || || || || ";  ;; 18
  "|| || tt tt tt || || || || || || tt tt .. tt || || bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || ";  ;; 19
  "|| || || tt || || || || || tt tt tt tt tt tt || || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 20
  "|| || || || || || || tt tt tt tt tt tt tt || || || bb .. /7 .. .. .. bb tt bb tt bb tt .. .. .. /7 .. .. .. bb tt tt bb tt bb tt .. .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 21
  "|| || || || || || tt tt tt .. tt tt tt || || || || tt .. /7 .. .. tt tt tt tt tt tt tt bb .. .. /7 .. .. bb tt tt tt tt tt tt bb tt .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 22
  "|| || || || || bb tt tt .. .. .. tt tt tt || || || bb .. /7 .. bb tt || || || || || tt xx w+ xx cc xx w+ xx tt || || || || || tt tt .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 23
  "|| || || || || tt tt .. .. .. .. .. tt tt || || || || .. /7 .. tt tt || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt bb .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 24
  "|| || || || || tt tt tt .. .. .. tt tt tt || || || bb .. /7 .. bb tt || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 25
  "|| || || || || tt tt tt tt .. tt tt tt || || || || || .. /7 .. tt tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 26
  "|| || tt tt tt tt || tt tt tt tt bb || || tt tt || bb .. /7 .. bb tt || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 27
  "|| || tt || || || || || tt tt tt || || || || || || tt .. /7 .. tt tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || tt ";  ;; 28
  "tt tt tt || || || || || || || || || || || || || tt bb .. /7 .. bb tt xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt tt .. /7 .. tt tt || || || || || || || || || || || || || || tt ";  ;; 29
  "bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. /7 .. tt bb xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || || || || tt ";  ;; 30
  ".. .. bb tt bb tt bb tt bb tt bb tt bb tt bb bb .. .. .. /7 .. .. .. w+ cc cc cc xx tt .. bb .. cc .. bb .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt ";  ;; 31
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  ;; 33
  "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d ";  ;; 32
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ";  ;; 33
  ".. .. bb tt bb tt bb tt bb tt bb tt bb tt tt tt bb .. .. /7 .. .. .. w+ cc cc cc xx tt .. bb .. cc .. bb .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt ";  ;; 34
  "bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. .. bb xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || tt tt tt tt ";  ;; 35
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tt .. /7 .. .. tt xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt .. .. /7 .. tt tt || || || || || || || || || || || || || || tt ";  ;; 36
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt bb .. /7 .. bb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 37
  "|| || || || || || || tt tt tt tt tt tt tt || || tt tt .. /7 .. tt tt || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 38
  "|| || || || || || || || tt tt tt tt tt tt || || tt bb .. /7 .. bb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 39
  "|| || || || || || || || || || || || || || || || tt tt .. /7 .. tt tt || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 40
  "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || ";  ;; 41
  "|| || || || || || || || || || || || || || rr rr tt tt .. /7 .. tt tt || || || || || tt xx w+ xx cc xx w+ xx tt || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || ";  ;; 42
  "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt tt tt tt tt tt tt bb .. .. /7 .. .. bb tt tt tt tt tt tt tt bb .. /7 .. bb tt tt tt tt tt tt tt || || || || || || || || || ";  ;; 43
  "|| || || || || || || tt tt tt tt || || || || || tt tt .. /7 .. .. tt bb tt bb tt bb .. .. .. .. /7 .. .. .. .. bb tt bb tt bb tt .. .. /7 .. .. tt bb tt bb tt bb tt || || || || || || || || || ";  ;; 44
  "|| || || || || tt rr tt rr rr tt rr rr || || || tt bb .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || || || || || || || ";  ;; 45
  "|| || || rr rr tt tt tt || || tt tt rr rr rr || tt tt .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 .. bb tt || || || || || || || || || ";  ;; 46
  "|| || || rr || || || tt tt tt || tt tt || rr || || bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || ";  ;; 47
  "|| || || || tt tt tt tt .. .. tt || tt tt || rr rr || || tt bb tt bb tt bb tt bb tt bb tt bb  .. /7 .. tt bb tt bb tt bb tt bb tt bb tt bb tt bb tt tt bb /4 /2 bb tt || || || || || || || || || ";  ;; 48
  "|| rr rr || tt tt || .. .. bb .. tt tt tt tt || rr rr || tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt tt tt tt tt tt tt tt tt tt xx xx xx xx ws xx cc cc xx ws xx xx xx xx || || || || || ";  ;; 49
  "|| || || || tt tt .. .. .. .. .. tt .. tt tt || || rr || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || ";  ;; 50
  "|| tt tt tt tt .. .. .. .. .. .. .. .. .. tt tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || ";  ;; 51
  "|| tt tt tt .. bb .. .. .. .. .. .. .. bb .. tt .. tt tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx || || || || || ";  ;; 52
  "|| || || tt .. .. .. .. .. aa .. .. .. .. .. tt || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc && && cc cc 00 cc cc xx || || || || || ";  ;; 53
  "|| rr || tt tt tt tt .. .. .. .. .. .. tt tt || || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || xx xx xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx xx xx || || || ";  ;; 54
  "|| rr || || || .. tt .. .. .. .. .. .. tt tt || rr || || || rr rr tt rr rr || || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || ";  ;; 55
  "|| rr rr || tt tt tt bb .. .. .. bb .. tt tt || rr || || rr rr tt tt tt rr rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || ";  ;; 56
  "|| || rr || tt tt tt tt tt .. .. .. .. tt || rr rr || || rr tt tt || tt || rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || ";  ;; 57
  "|| || || || || || || || tt tt tt tt tt || tt rr || || || rr tt || || || || rr || || || tt tt .. /7 .. || || || || || || || || xx xx cc xx xx @@ @@ @@ @@ @@ @@ @@ @@ @@ @@ xx xx cc xx xx || || ";  ;; 58
  "|| || || rr rr || || || tt tt tt || tt tt || rr || || || rr tt tt || || || rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || ";  ;; 59
  "|| || || || rr rr || rr rr rr || rr rr rr rr || || || || rr rr || || || tt rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx .W .H .I .T .E @@ .S .T .A .G xx cc cc cc xx || || ";  ;; 60
  "|| || || || || || || || || || || || || || || || || || || || rr rr rr rr tt || || || tt tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx || || ";  ;; 61
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || xx xx xx xx xx || || || || || || || || || || xx xx xx xx xx || || ";  ;; 62
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || ";  ;; 63
    ))

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "shroom.scm")
(mk-shroom 'ch_shroom)

(kern-load "gen.scm")
(mk-gen 'ch_gen)

(kern-load "doris.scm")
(mk-doris 'ch_doris)

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_green_tower "Green Tower" s_town m_green_tower #f #f #f #f 
 nil ;; subplaces
 nil ;; neighbors

 ;; objects:
 (list

  ;; characters:
  (put ch_shroom  14 1)
  (put ch_gen     0 0)
  (put ch_doris   0 0)

  ;; Shroom's Shop
  (put (mk-locked-door) 48 10)
  (put (mk-door) 48 6)
  (put (mk-door) 48 4)
  (put (mk-magic-locked-door) 54 12)
  (put (mk-bed) 51 9)
  (put (mk-chest 'burn-trap
                 (mk-contents (add-content 50 sulphorous_ash)
                              (add-content 50 garlic)
                              (add-content 50 ginseng)
                              (add-content 50 blood_moss)
                              (add-content 50 black_pearl)
                              (add-content 50 spider_silk)
                              (add-content 50 mandrake)
                              (add-content 50 nightshade)))
       53 9)
  (put (mk-chest 'poison-trap
                 (mk-contents (add-content 10 t_cure_potion)
                              (add-content 10 t_heal_potion)
                              (add-content 10 t_mana_potion)
                              (add-content 10 t_poison_immunity_potion)
                              (add-content 1 t_inv_potion)))
       54 9)
  (put (mk-chest 'sleep-trap
                 (mk-contents (add-content 1 t_armor_leather)
                              (add-content 1 t_sword)
                              (add-content 1 t_shield)
                              (add-content 1 t_leather_helm)))
       55 9)


  ;; Gen's Hut
  (put (mk-door) 7 13)
  (put (mk-bed) 2 13)
  (put (mk-chest nil 
                 (mk-contents (add-content 100 t_arrow)
                              (add-content 1 t_bow)))
       4 11)
                
  ;; Deric's Hut
  (put (mk-door) 11 6)
  (put (mk-door) 16 6)
  (put (mk-bed) 17 4)
  (put (mk-chest 'burn-trap
                 (mk-contents (add-content 100 t_bolt)
                              (add-content 1 t_crossbow)
                              (add-content 1 t_shield)
                              (add-content 2 t_cure_potion)
                              (add-content 5 t_heal_potion)))
       17 6)
  
  ;; White Stag Lodge
  (put (mk-door) 51 49)
  (put (mk-door) 52 49)
  (put (mk-door) 57 59)
  (put (kern-tag 'white-stag-door (mk-locked-door)) 44 58)
  (put (mk-bed) 43 61)
  (put (mk-bed) 58 61)
  (put (mk-locked-door) 59 58)
  
  ;; Tower 
  (put (mk-lever 'gtl-drawbridge-1) 35 29)
  (put (mk-lever 'gtl-portcullis-1) 29 29)
  (put (mk-ladder-down 'p_green_tower_lower 32 32) 32 32)
  (put (mk-door) 32 22)
  (put (mk-door) 41 32)
  (put (mk-door) 32 42)
  (put (mk-door) 23 32)
  
  )
 nil   ; hooks
 nil   ; edge entrances
)
