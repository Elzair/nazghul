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
		"tc bb .. .. bb .. .. .. .. xx w+ xx && xx ,, cc ,, xx && xx xx xx || || || tt %% bb .. bb %% "
		"%5 .. .. || || tA .. .. xx xx ,, ,, ,, xx x! cc x! xx ,, ,, xx xx ?? xx || tt %% .. .. .. %% "
		"%% bb .. bb || t5 tA xx xx ,, ,, ,, ,, xx ,, cc ,, ,, ,, ,, xx ,, ,, xx || tt %% bb .. bb %% "
		"%% .. .. || || tt td xx ,, ,, 00 ,, ,, xx ,, cc ,, xx xx xx xx xx xx xx || tc %% .. .. .. %% "
		"%% bb .. bb || || xx xx ,, ,, 00 ,, ,, xx ,, cc ,, xx ,, ,, ,, ,, ,, xx xx %3 %% bb .. bb %% "
		"%% .. .. %f ta || xx ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, ,, && xx %a %% .. .. .. %% "
		"%c bb .. bb %f xx xx xx xx xx xx xx xx xx ,, cc ,, xx xx xx xx xx xx xx xx xx %e bb .. bb %a "
		".. .. .. .. .. w+ ,, ,, ,, ,, xx ,, ,, x! ,, cc ,, x! ,, ,, xx ,, ,, ,, ,, w+ .. .. .. .. .. "
		".. .. .. .. .. cc cc cc cc ,, xx ,, ,, ,, ,, cc ,, xx ,, ,, xx ,, cc cc cc cc .. .. .. .. .. "
		".. .. .. .. .. w+ ,, ,, cc ,, xx ,, ,, xx ,, cc ,, xx ,, ,, xx ,, cc ,, ,, w+ .. .. .. .. .. "
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

  ;; treasure in east store room
  (put (kern-mk-obj t_doom_staff 1) 19 14)
  (put (kern-mk-obj t_xen_corp_scroll 1) 19 15)
  (put (kern-mk-obj t_an_tym_scroll 1) 19 16)
  (put (kern-mk-obj t_vas_rel_por_scroll 1) 18 14)
  (put (kern-mk-obj t_in_vas_por_ylem_scroll 1) 18 15)
  (put (kern-mk-obj t_gold_coins 500) 18 16)

  (put (mk-bed) 21 11)
  (put (mk-bed) 19 8)

  (put (mk-door) 15  8)
  (put (mk-door) 15 24)
  (put (mk-door) 22 17)
  (put (mk-door)  8 17)

  (put (mk-door) 13 12)
  (put (mk-door) 17  9)
  (put (mk-door) 17 12)
  (put (mk-clock) 7 12)
  
  (put (mk-shelf) 8 10)
  (put (mk-shelf) 18 11)
  (put (mk-hidden t_ylem_an_exe_book 1) 18 11)
  )

 nil ; hooks
 nil ; edge entrances
 )
