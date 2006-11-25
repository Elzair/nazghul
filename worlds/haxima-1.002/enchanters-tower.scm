(kern-load "enchanters-tower-mech.scm")

;;----------------------------------------------------------------------------
;; Map
;;
;; Declare the map for the place here.
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_enchanters_tower 31 31 pal_expanded
	(list
	  "tt tt bb te bb || || tt %a %% %% %% %% %c .. .. .. %a %% %% %% %% %% %% %% %% %% %% %% %% %% "
	  "bb te .. t% tb tt || || tt t5 %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. %a %% "
	  "td t# .. .. .. .. bb || || tt %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
	  "bb .. .. .. .. .. .. ta || || %a %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. .. .. bb %% "
	  "|| .. .. .. .. .. .. .. bb || t5 %% %% %d .. .. .. %b %% %% %% %% %% %% %% %% %5 .. .. .. %% "
	  "|| bb .. .. .. .. .. .. .. || || %a %c xx w+ cc w+ xx %e tb tt tt tt tt t5 %a %% bb .. bb %% "
	  "|| || .. .. .. .. && .. .. ta || xx xx xx ,, cc ,, xx xx xx || || || tt tt t5 %% .. .. .. %% "
	  "tc bb .. .. bb .. .. .. .. xx w+ xx && xx ,, cc ,, xx ,, xx xx xx || || || tt %% bb .. bb %% "
	  "%5 .. .. || || tA .. .. xx xx ,, ,, ,, xx x! cc x! xx ,, 00 x! xx ?? xx || tt %% .. .. .. %% "
	  "%% bb .. bb || t5 tA xx xx ,, ,, ,, ,, xx ,, cc ,, xx ,, ,, xx ,, ,, xx || tt %% bb .. bb %% "
	  "%% .. .. || || tt td xx ,, ,, 00 ,, ,, xx ,, cc ,, xx ,, ,, xx xx xx xx || tc %% .. .. .. %% "
	  "%% bb .. bb || || xx xx ,, ,, 00 ,, ,, xx ,, cc ,, xx ,, ,, ,, ,, ,, xx xx %3 %% bb .. bb %% "
	  "%% .. .. %f ta || xx ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, ,, && xx %a %% .. .. .. %% "
	  "%c bb .. bb %f xx xx xx xx xx xx xx xx xx ,, cc ,, xx xx xx xx xx xx xx xx xx %e bb .. bb %a "
	  ".. .. .. .. .. w+ ,, ,, ,, ,, xx ,, ,, x! ,, cc ,, ,, xx ,, xx ,, ,, ,, ,, w+ .. .. .. .. .. "
	  ".. .. .. .. .. cc cc cc cc ,, xx ,, ,, ,, ,, cc cc ,, xx w+ xx ,, cc cc cc cc .. .. .. .. .. "
	  ".. .. .. .. .. w+ ,, ,, cc ,, xx ,, ,, xx ,, cc ,, ,, xx ,, xx ,, cc ,, ,, w+ .. .. .. .. .. "
	  "%5 bb .. bb %7 xx xx x! cc x! xx xx xx xx ,, cc ,, xx xx xx xx x! cc x! xx xx %7 bb .. bb %3 "
	  "%% %% %% %% %% %5 w+ ,, cc ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, cc ,, w+ t7 %a %% %% %% %% "
	  "%% %c .. %a %% %% xx xx cc ,, pp ,, ,, pp ,, cc ,, pp ,, ,, pp ,, cc xx xx tt tt tt tt tt tt "
	  "%% .. .. .. %% %% %5 w+ cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx t3 || || || || || tt "
	  "%% %5 .. %3 %% %% %% xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || || bb te bb || || "
	  "%% %% %% %% %% %% %% %5 xx xx x! ,, ,, pp ,, cc ,, pp ,, ,, x! xx xx || || tt td t& tb tt || "
	  "%% %c t3 tt tt t5 tA %a %5 xx xx xx ,, ,, ,, cc ,, ,, ,, xx xx xx tb || || bb .. .. .. bb || "
	  "%c t3 tL ~3 ~5 tJ t5 tA %a %% %5 xx w+ xx xx cc xx xx w+ xx %3 %% ~5 ta || td .. ++ .. tb || "
	  "tt tL ~3 -- -- ~5 tJ t5 .. %% %% %% %5 xx ,, cc ,, xx %3 %% %% ~b ~~ -d || bb .. .. .. bb || "
	  "tt ~3 -- __ __ -- ~5 tt .. %% %% %% %% xx ,, cc ,, xx %a %% %% %% ~c %% || || td .. tb || || "
	  "tt ~a -- __ __ -- ~c tt .. %% %% %% %% xx w+ cc w+ xx .. bb .. bb %% %c || || bb .. bb || || "
	  "tt tH ~a -- -- ~c tG tc .. %e .. %a %% .. .. .. .. .. .. .. .. .. == .. tt tc tA .. t3 || || "
	  "%5 ta tH ~a ~c tG tc t# %7 .. .. .. %% bb .. .. .. bb .. bb .. bb %% %5 || bb t7 bb || || || "
	  "%% %5 tt tt tt tt t# %3 %% %5 .. %3 %% %5 .. .. .. %3 %% %% %% %% %% %% || || || || || || || "
	)
)

(kern-mk-map 
 'm_enchanters_tower_l2 31 31 pal_expanded
	(list
	  "tt tt bb te bb |. |. tt %a %% %% %% %% %c .. .. .. %a %% %% %% %% %% %% %% %% %% %% %% %% %% "
	  "bb te .. t% tb tt |. |. tt t5 %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. %a %% "
	  "td t# .. .. .. .. bb |. |. tt %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
	  "bb .. .. .. .. .. .. ta |. |. %a %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. .. .. bb %% "
	  "|. .. .. .. .. .. .. .. bb |. t5 %% %% %d .. .. .. %b %% %% %% %% %% %% %% %% %5 .. .. .. %% "
	  "|. bb .. .. .. .. .. .. .. |. |. %a %c ee ee ee ee ee %e tb tt tt tt tt t5 %a %% bb .. bb %% "
	  "|. |. .. .. .. .. && .. .. ta |. ee ee ee ee ee ee ee ee ee |. |. |. tt tt t5 %% .. .. .. %% "
	  "tc bb .. .. bb .. .. .. .. ee ee ee ee ee ee ee ee ee ee ee ee ee |. |. |. tt %% bb .. bb %% "
	  "%5 .. .. |. |. tA .. .. ee ee ee ee xx w+ xx xx xx w+ xx ee ee ee ee ee |. tt %% .. .. .. %% "
	  "%% bb .. bb |. t5 tA ee ee ee xx xx xx ,, ,, xx ,, ,, xx xx xx ee ee ee |. tt %% bb .. bb %% "
	  "%% .. .. |. |. tt td ee ee xx xx ,, ,, ,, ,, xx ,, ,, ,, 00 xx xx ee ee |. tc %% .. .. .. %% "
	  "%% bb .. bb |. |. ee ee ee w+ ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, w+ ee ee ee %3 %% bb .. bb %% "
	  "%% .. .. %f ta |. ee ee xx xx ,, 00 ,, ,, ,, xx ,, ,, ,, xx xx xx xx ee ee %a %% .. .. .. %% "
	  "%c bb .. bb %f ee ee ee xx ,, ,, ,, xx xx ,, xx ,, xx xx xx [[ ]] xx ee ee ee %e bb .. bb %a "
	  ".. .. .. .. .. ee ee ee w+ ,, ,, ,, xx ,, ,, ,, ,, ,, xx 00 ,, 00 xx ee ee ee .. .. .. .. .. "
	  ".. .. .. .. .. ee ee ee xx ,, ,, ,, x! ,, cc cc cc ,, x! ,, ,, 00 xx ee ee ee .. .. .. .. .. "
	  ".. .. .. .. .. ee ee ee xx xx xx xx xx ,, ,, ,, ,, ,, xx 00 ,, 00 xx ee ee ee .. .. .. .. .. "
	  "%5 bb .. bb %7 ee ee ee xx ,, ,, xx xx xx x! ,, x! xx xx xx [[ ]] xx ee ee ee %7 bb .. bb %3 "
	  "%% %% %% %% %% %5 ee ee xx xx xx x! ,, ,, ,, ,, ,, ,, ,, x! xx xx xx ee ee t7 %a %% %% %% %% "
	  "%% %c .. %a %% %% ee ee ee w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ee ee ee tt tt tt tt tt tt "
	  "%% .. .. .. %% %% %5 ee ee xx xx [[ ]] ,, ,, ,, ,, ,, [[ ]] xx xx ee ee t3 |. |. |. |. |. tt "
	  "%% %5 .. %3 %% %% %% ee ee ee xx w+ xx [[ @@ @@ @@ ]] xx w+ xx ee ee ee |. |. bb te bb |. |. "
	  "%% %% %% %% %% %% %% %5 ee ee ee ee xx w+ xx xx xx w+ xx ee ee ee ee |. |. tt td t& tb tt |. "
	  "%% %c t3 tt tt t5 tA %a %5 ee ee ee ee ee ee ee ee ee ee ee ee ee tb |. |. bb .. .. .. bb |. "
	  "%c t3 tL ~3 ~5 tJ t5 tA %a %% %5 ee ee ee ee ee ee ee ee ee %3 %% ~5 ta |. td .. ++ .. tb |. "
	  "tt tL ~3 -- -- ~5 tJ t5 .. %% %% %% %5 ee ee ee ee ee %3 %% %% ~b ~~ -d |. bb .. .. .. bb |. "
	  "tt ~3 -- __ __ -- ~5 tt .. %% %% %% %% ee ee ee ee ee %a %% %% %% ~c %% |. |. td .. tb |. |. "
	  "tt ~a -- __ __ -- ~c tt .. %% %% %% %% ee ee ee ee ee .. bb .. bb %% %c |. |. bb .. bb |. |. "
	  "tt tH ~a -- -- ~c tG tc .. %e .. %a %% .. .. .. .. .. .. .. .. .. == .. tt tc tA .. t3 |. |. "
	  "%5 ta tH ~a ~c tG tc t# %7 .. .. .. %% bb .. .. .. bb .. bb .. bb %% %5 |. bb t7 bb |. |. |. "
	  "%% %5 tt tt tt tt t# %3 %% %5 .. %3 %% %5 .. .. .. %3 %% %% %% %% %% %% |. |. |. |. |. |. |. "
	)
)

(kern-mk-map 
 'm_enchanters_tower_l3 31 31 pal_expanded
	(list
	  "tt tt bb te bb |. |. tt %a %% %% %% %% %c .. .. .. %a %% %% %% %% %% %% %% %% %% %% %% %% %% "
	  "bb te .. t% tb tt |. |. tt t5 %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. %a %% "
	  "td t# .. .. .. .. bb |. |. tt %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
	  "bb .. .. .. .. .. .. ta |. |. %a %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. .. .. bb %% "
	  "|. .. .. .. .. .. .. .. bb |. t5 %% %% %d .. .. .. %b %% %% %% %% %% %% %% %% %5 .. .. .. %% "
	  "|. bb .. .. .. .. .. .. .. |. |. %a %c ee ee ee ee ee %e tb tt tt tt tt t5 %a %% bb .. bb %% "
	  "|. |. .. .. .. .. && .. .. ta |. ee ee ee ee ee ee ee ee ee |. |. |. tt tt t5 %% .. .. .. %% "
	  "tc bb .. .. bb .. .. .. .. ee ee ee ee ee ee ee ee ee ee ee ee ee |. |. |. tt %% bb .. bb %% "
	  "%5 .. .. |. |. tA .. .. ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee |. tt %% .. .. .. %% "
	  "%% bb .. bb |. t5 tA ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee |. tt %% bb .. bb %% "
	  "%% .. .. |. |. tt td ee ee ee ee ee ee 00 00 00 00 00 ee ee ee ee ee ee |. tc %% .. .. .. %% "
	  "%% bb .. bb |. |. ee ee ee ee ee 00 00 00 ,, ,, ,, 00 00 00 ee ee ee ee ee %3 %% bb .. bb %% "
	  "%% .. .. %f ta |. ee ee ee ee ee 00 ,, ,, ,, xx ,, ,, ,, 00 ee ee ee ee ee %a %% .. .. .. %% "
	  "%c bb .. bb %f ee ee ee ee ee 00 00 ,, x! ,, ,, ,, x! ,, 00 00 ee ee ee ee ee %e bb .. bb %a "
	  ".. .. .. .. .. ee ee ee ee ee 00 ,, ,, ,, ,, cc ,, ,, ,, ,, 00 ee ee ee ee ee .. .. .. .. .. "
	  ".. .. .. .. .. ee ee ee ee ee 00 ,, xx ,, cc cc cc ,, xx ,, 00 ee ee ee ee ee .. .. .. .. .. "
	  ".. .. .. .. .. ee ee ee ee ee 00 ,, ,, ,, ,, cc ,, ,, ,, ,, 00 ee ee ee ee ee .. .. .. .. .. "
	  "%5 bb .. bb %7 ee ee ee ee ee 00 00 ,, x! ,, ,, ,, x! ,, 00 00 ee ee ee ee ee %7 bb .. bb %3 "
	  "%% %% %% %% %% %5 ee ee ee ee ee 00 ,, ,, ,, xx ,, ,, ,, 00 ee ee ee ee ee t7 %a %% %% %% %% "
	  "%% %c .. %a %% %% ee ee ee ee ee 00 00 00 ,, ,, ,, 00 00 00 ee ee ee ee ee tt tt tt tt tt tt "
	  "%% .. .. .. %% %% %5 ee ee ee ee ee ee 00 00 00 00 00 ee ee ee ee ee ee t3 |. |. |. |. |. tt "
	  "%% %5 .. %3 %% %% %% ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee |. |. bb te bb |. |. "
	  "%% %% %% %% %% %% %% %5 ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee |. |. tt td t& tb tt |. "
	  "%% %c t3 tt tt t5 tA %a %5 ee ee ee ee ee ee ee ee ee ee ee ee ee tb |. |. bb .. .. .. bb |. "
	  "%c t3 tL ~3 ~5 tJ t5 tA %a %% %5 ee ee ee ee ee ee ee ee ee %3 %% ~5 ta |. td .. ++ .. tb |. "
	  "tt tL ~3 -- -- ~5 tJ t5 .. %% %% %% %5 ee ee ee ee ee %3 %% %% ~b ~~ -d |. bb .. .. .. bb |. "
	  "tt ~3 -- __ __ -- ~5 tt .. %% %% %% %% ee ee ee ee ee %a %% %% %% ~c %% |. |. td .. tb |. |. "
	  "tt ~a -- __ __ -- ~c tt .. %% %% %% %% ee ee ee ee ee .. bb .. bb %% %c |. |. bb .. bb |. |. "
	  "tt tH ~a -- -- ~c tG tc .. %e .. %a %% .. .. .. .. .. .. .. .. .. == .. tt tc tA .. t3 |. |. "
	  "%5 ta tH ~a ~c tG tc t# %7 .. .. .. %% bb .. .. .. bb .. bb .. bb %% %5 |. bb t7 bb |. |. |. "
	  "%% %5 tt tt tt tt t# %3 %% %5 .. %3 %% %5 .. .. .. %3 %% %% %% %% %% %% |. |. |. |. |. |. |. "
	)
)

;;----------------------------------------------------------------------------
;; Characters
;;
;; kern-load the character files here and call their first-time constructors.
;;----------------------------------------------------------------------------
(kern-load "zane.scm")
(mk-zane-first-time 'ch_zane)

;;(kern-load "gate-guard.scm")

(kern-load "enchanter.scm")
(mk-enchanter-first-time 'ch_enchanter)

;;----------------------------------------------------------------------------
;; Place
;;
;; Call the place constructor here.
;;----------------------------------------------------------------------------
(kern-mk-place '
 p_enchanters_tower ; tag
 "Enchanters Tower" ; name
 s_keep             ; sprite
 m_enchanters_tower ; map
 #f                 ; wraps
 #f                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;;objects
 (list  

  ;; characters
  (list ch_zane 0 0) 

  ;; magically locked doors at all entrances
  (put (mk-magic-locked-door) 5 15)
  (put (mk-magic-locked-door) 25 15)
  (put (mk-magic-locked-door) 15 5)
  (put (mk-magic-locked-door) 15 27)
  
  ;; dbg hack
  ;; (put (kern-mk-obj t_poison_immunity_potion 1) 14 4)
  ;; (put (kern-mk-obj t_in_ex_por_scroll 1) 16 4)

  ;; portcullis's & levers for west store room
  (put (kern-tag 'enchtwr-port-4 (mk-portcullis)) 13 15)
  (put (mk-lever 'enchtwr-port-4) 21 9)

  ;; treasure in west store room
  (put (mk-chest 
        'burn-trap
        (mk-contents (add-content 10 sulphorous_ash)
                     (add-content 10 ginseng)
                     (add-content 10 garlic)
                     (add-content 10 spider_silk)
                     (add-content 10 blood_moss)
                     (add-content 10 black_pearl)
                     (add-content 10 nightshade)
                     (add-content 10 mandrake)))
       11 14)
  (put (mk-chest
        'spike-trap
        (mk-contents (add-content 10 t_heal_potion)
                     (add-content 10 t_mana_potion)
                     (add-content 5 t_cure_potion)
                     (add-content 5 t_poison_immunity_potion)
                     (add-content 1 t_inv_potion)))
       11 15)
  (put (mk-chest
        'lightning-trap
        (mk-contents (add-content 100 t_gold_coins)))
       11 16)

  (put (mk-bed) 21 11)
  
  (put (mk-magic-locked-door) 19 14)

  (put (mk-door) 15  8)
  (put (mk-door) 15 24)
  (put (mk-door) 22 17)
  (put (mk-door)  8 17)

  (put (mk-ladder-up 'p_enchanters_tower_l2 16 15) 
		16 15)

  (put (mk-door) 13 12)
  (put (mk-door) 17 12)
  (put (mk-clock) 7 12)
  
  (put (mk-shelf) 18 7)
  (put (mk-shelf) 19 10)
  )

 nil ; hooks
 nil ; edge entrances
 )
 
(kern-mk-place '
 p_enchanters_tower_l2 ; tag
 "Enchanters Tower" ; name
 s_keep             ; sprite
 m_enchanters_tower_l2 ; map
 #f                 ; wraps
 #f                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 ;;objects
 (list
 
 	(put (mk-ladder-down 'p_enchanters_tower 16 15) 
		16 15)
 	(put (mk-ladder-up 'p_enchanters_tower_l3 14 15) 
		14 15)
 
    (put (mk-door) 16 13)
	(put (mk-door) 14 13)
	(put (mk-door) 15 17)
	
	(put (mk-bed) 18 12)
	
	(put (mk-shelf) 14 9)
	(put (mk-shelf) 14 10)
	(put (mk-shelf) 14 11)
	(put (mk-shelf) 11 10)
	(put (mk-shelf) 12 10)
	(put (mk-shelf) 12 12)
	(put (mk-shelf) 9 13)
	(put (mk-shelf) 11 13)
	
	(put (mk-shelf) 10 15)
	(put (mk-shelf) 11 15)

	(put (mk-shelf) 16 18)
	(put (mk-shelf) 17 18)

	(put (mk-hidden 't_ylem_an_ex_book 1) 14 9)
   
    ;; treasure in east store room
	(put (kern-mk-obj t_doom_staff 1) 20 13)
  (put (kern-mk-obj t_xen_corp_scroll 1) 21 13)
  (put (kern-mk-obj t_an_tym_scroll 1) 21 14)
  (put (kern-mk-obj t_vas_rel_por_scroll 1) 20 17)
  (put (kern-mk-obj t_in_vas_por_ylem_scroll 1) 21 17)
  (put (kern-mk-obj t_gold_coins 500) 21 16)
  (put (kern-mk-obj t_mans_note 1) 19 16)
  
    (put (kern-tag 'openlibwall 
                 (mk-tblitter 'p_
                              8
                              0
                              3
                              1
                              'm_hall_section))
       0
       0)

    (put (kern-tag 'closelibwall 
                 (mk-tblitter 'p_
                              8
                              0
                              3
                              1
                              'm_hall_section))
       0
       0)
  
 )
 nil ; hooks
 nil ; edge entrances
 )
 
;;plain old mirror mirror
(let* ((kmir (kern-tag 'ench-guest-mirror (mk-mirror 's_mirror_bg_flagstones)))
         (ksen (mk-char-sensor 'ench-guest-mirror)))
	(kern-obj-put-at kmir (list p_enchanters_tower_l2 16 9))
	(kern-obj-put-at ksen (list p_enchanters_tower_l2 16 10)))

;;decorative test mirror
(let* ((testmirror1 (kern-tag 'ench-test-mirror1 
			(mk-mag-mirror 's_mirror_bg_flagstones
				(list 'p_enchanters_tower_l2 14 19) 
				(list 'p_enchanters_tower_l2 12 19)
				#t 'testmirhandler
				)))
         (testmirror2 (kern-tag 'ench-test-mirror2
			(mk-mag-mirror 's_mirror_bg_flagstones
				(list 'p_enchanters_tower_l2 12 19)
				(list 'p_enchanters_tower_l2 14 19)
				#t 'testmirhandler
				)))
		(sigsplit (kern-tag 'ench-test-mirrors
			(mk-sig-splitter (list 'ench-test-mirror1 'ench-test-mirror2 nil))
			)))
	(kern-obj-put-at testmirror1 (list p_enchanters_tower_l2 12 18))
	(kern-obj-put-at testmirror2 (list p_enchanters_tower_l2 14 18))
	(kern-obj-put-at sigsplit (list p_enchanters_tower_l2  0 0))
	(kern-obj-put-at (mk-char-sensor 'ench-test-mirrors) (list p_enchanters_tower_l2 12 19))
	(kern-obj-put-at (mk-char-sensor 'ench-test-mirrors) (list p_enchanters_tower_l2 14 19))
	(kern-obj-put-at (mk-lever-on 'ench-test-mirrors)  (list p_enchanters_tower_l2 13 18))
	)
	
;;working portal mirror
(let* ((doormirror1 (kern-tag 'ench-door-mirror1 
			(mk-mag-mirror 's_mirror_bg_flagstones
				(list 'p_enchanters_tower_l2 20 15)
				(list 'p_enchanters_tower_l2 9 17) 
				#f 'doormirhandler
				)))
         (doormirror2 (kern-tag 'ench-door-mirror2
			(mk-mag-mirror 's_mirror_bg_flagstones
				(list 'p_enchanters_tower_l2 9 17)
				(list 'p_enchanters_tower_l2 20 15)
				#f 'doormirhandler
				)))
		(sigsplit (kern-tag 'ench-door-mirrors
			(mk-sig-splitter (list 'ench-door-mirror1 'ench-door-mirror2 nil))
			)))
	(kern-obj-put-at doormirror1 (list p_enchanters_tower_l2 10 17))
	(kern-obj-put-at doormirror2 (list p_enchanters_tower_l2 19 15))
	(kern-obj-put-at sigsplit (list p_enchanters_tower_l2  0 0))
	(kern-obj-put-at (mk-char-sensor 'ench-door-mirrors) (list p_enchanters_tower_l2 9 17))
	(kern-obj-put-at (mk-char-sensor 'ench-door-mirrors) (list p_enchanters_tower_l2 20 15))
	(kern-obj-put-at (mk-lever 'ench-door-mirrors)  (list p_enchanters_tower 19 16))
	)
 
;;ench bedroom entrance
(let* ((broomwall (kern-tag 'ench-broom-wall
			(mk-bim-secret 't_secret_door 't_wall (list 'p_enchanters_tower 19 13)))))
	(kern-obj-put-at ench-broom-wall (list p_enchanters_tower 19 13))
	(kern-obj-put-at (mk-hidden-mech) (list p_enchanters_tower 20 8))
	(kern-obj-put-at (mk-disg-lvr 'ench-broom-wall 's_wall_torch) (list p_enchanters_tower 20 8))
	) 
 
;;library wall entrance
(let* ((libwall (kern-tag 'ench-lib-wall
			(mk-bim-secret 't_secret_door 't_wall (list 'p_enchanters_tower_l2 9 16)))))
	(kern-obj-put-at ench-lib-wall (list p_enchanters_tower_l2 9 16))
	(kern-obj-put-at (mk-moving-shelf 
		(list 'p_enchanters_tower_l2 10 15)
		(list 'p_enchanters_tower_l2 9 15)
		'ench-lib-wall)
			(list p_enchanters_tower_l2 9 15))
	)	
	
;;magic clock
(kern-obj-put-at (mk-mag-clock) (list p_enchanters_tower_l2 18 18))
 
 
(kern-mk-place '
 p_enchanters_tower_l3 ; tag
 "Enchanters Tower Roof" ; name
 s_keep             ; sprite
 m_enchanters_tower_l3 ; map
 #f                 ; wraps
 #f                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 ;;objects
 (list
 	(put (mk-ladder-down 'p_enchanters_tower_l2 14 15) 
		14 15)
	;; this needs a telescope 
 )
 
 nil ; hooks
 nil ; edge entrances
 )

(define (obj-line objfactory place yloc xloc xmax)
	(kern-obj-put-at (objfactory xloc yloc) (list place xloc yloc))
	(if (< xloc xmax)
		(obj-line objfactory place yloc (+ xloc 1) xmax)
	))  
	
(define (obj-rect objfactory place xmin xmax ymin ymax)
	(obj-line objfactory place ymin xmin xmax)
	(if (< ymin ymax)
		(obj-rect objfactory place xmin xmax (+ ymin 1) ymax)
	))

(define (obj-list objfactory place loclist)
	(kern-obj-put-at (objfactory (caar loclist) (cadar loclist)) (list place (caar loclist) (cadar loclist)))	
	(if (not (equal? (cadr loclist) '()))
		(obj-list objfactory place (cdr loclist))
	))
	
	
;;no teleporting out of the tower!

(let ((blockers
		(lambda (xloc yloc) (mk-blocker))))
	(obj-rect blockers p_enchanters_tower_l2 0 30 0 7)
	(obj-rect blockers p_enchanters_tower_l2 0 7 8 22)
	(obj-rect blockers p_enchanters_tower_l2 23 30 8 22)
	(obj-rect blockers p_enchanters_tower_l2 0 30 23 30)
	(obj-rect blockers p_enchanters_tower_l2 8 11 8 8)
	(obj-rect blockers p_enchanters_tower_l2 19 22 8 8)
	(obj-rect blockers p_enchanters_tower_l2 8 11 22 22)
	(obj-rect blockers p_enchanters_tower_l2 19 22 22 22)
	(obj-list blockers p_enchanters_tower_l2 (list 
		(list 8 9) (list 8 10) (list 8 11) (list 9 9) 
		(list 22 9) (list 22 10) (list 22 11) (list 21 9) 
		(list 8 19) (list 8 20) (list 8 21) (list 9 21) 
		(list 22 19) (list 22 20) (list 22 21) (list 21 21) 
		nil))
	(obj-rect blockers p_enchanters_tower_l3 0 30 0 9)
	(obj-rect blockers p_enchanters_tower_l3 0 9 10 20)
	(obj-rect blockers p_enchanters_tower_l3 21 30 10 20)
	(obj-rect blockers p_enchanters_tower_l3 0 30 21 30)
	(obj-rect blockers p_enchanters_tower_l3 10 12 10 10)
	(obj-rect blockers p_enchanters_tower_l3 18 20 10 10)
	(obj-rect blockers p_enchanters_tower_l3 10 12 20 20)
	(obj-rect blockers p_enchanters_tower_l3 18 20 20 20)
	(obj-list blockers p_enchanters_tower_l3 (list 
		(list 10 11) (list 10 12) (list 20 11) (list 20 12) 
		(list 10 18) (list 10 19) (list 20 18) (list 20 29) 
		nil))
	)
	
	
	