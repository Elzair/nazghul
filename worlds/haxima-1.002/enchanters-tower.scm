;;----------------------------------------------------------------------------
;; Map
;;
;; Declare the map for the place here.
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_enchanters_tower 31 31 pal_expanded
 (list
            "tt tt bb tt bb || || tt %% %% %% %% %% %% .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "bb tt .. .. tt tt || || tt tt %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. %% %% "
            "tt .. .. .. .. .. bb || || tt %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
            "bb .. .. .. .. .. .. tt || || %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. .. .. bb %% "
            "|| .. .. .. .. .. .. .. bb || tt %% %% %% .. .. .. %% %% %% %% %% %% %% %% %% %% .. .. .. %% "
            "|| bb .. .. .. .. .. .. .. || || %% %% xx w+ cc w+ xx %% tt tt tt tt tt tt %% %% bb .. bb %% "
            "|| || .. .. .. .. && .. .. tt || xx xx xx ,, cc ,, xx xx xx || || || tt tt tt %% .. .. .. %% "
            "tt bb .. .. bb .. .. .. .. xx w+ xx && xx ,, cc ,, xx && xx xx xx || || || tt %% bb .. bb %% "
            "%% .. .. || || .. .. .. xx xx ,, ,, ,, xx x! cc x! xx ,, ,, xx xx ?? xx || tt %% .. .. .. %% "
            "%% bb .. bb || tt .. xx xx ,, ,, ,, ,, xx ,, cc ,, ,, ,, ,, xx ,, ,, xx || tt %% bb .. bb %% "
            "%% .. .. || || tt tt xx ,, ,, 00 ,, ,, xx ,, cc ,, xx xx xx xx xx xx xx || tt %% .. .. .. %% "
            "%% bb .. bb || || xx xx ,, ,, 00 ,, ,, xx ,, cc ,, xx ,, ,, ,, ,, ,, xx xx %% %% bb .. bb %% "
            "%% .. .. %% tt || xx ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, ,, && xx %% %% .. .. .. %% "
            "%% bb .. bb %% xx xx xx xx xx xx xx xx xx ,, cc ,, xx xx xx xx xx xx xx xx xx %% bb .. bb %% "
            ".. .. .. .. .. w+ ,, ,, ,, ,, xx ,, ,, x! ,, cc ,, x! ,, ,, xx ,, ,, ,, ,, w+ .. .. .. .. .. "
            ".. .. .. .. .. cc cc cc cc ,, xx ,, ,, ,, ,, cc ,, xx ,, ,, xx ,, cc cc cc cc .. .. .. .. .. "
            ".. .. .. .. .. w+ ,, ,, cc ,, xx ,, ,, xx ,, cc ,, xx ,, ,, xx ,, cc ,, ,, w+ .. .. .. .. .. "
            "%% bb .. bb %% xx xx x! cc x! xx xx xx xx ,, cc ,, xx xx xx xx x! cc x! xx xx %% bb .. bb %% "
            "%% %% %% %% %% %% w+ ,, cc ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, cc ,, w+ tt %% %% %% %% %% "
            "%% %% .. %% %% %% xx xx cc ,, pp ,, ,, pp ,, cc ,, pp ,, ,, pp ,, cc xx xx tt tt tt tt tt tt "
            "%% .. .. .. %% %% %% w+ cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx tt || || || || || tt "
            "%% %% .. %% %% %% %% xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || || bb tt bb || || "
            "%% %% %% %% %% %% %% %% xx xx x! ,, ,, pp ,, cc ,, pp ,, ,, x! xx xx || || tt tt .. tt tt || "
            "%% %% tt tt tt tt .. %% %% xx xx xx ,, ,, ,, cc ,, ,, ,, xx xx xx tt || || bb .. .. .. bb || "
            "%% tt tt ~~ ~~ tt tt .. %% %% %% xx w+ xx xx cc xx xx w+ xx %% %% ~~ tt || tt .. ++ .. tt || "
            "tt tt ~~ -- -- ~~ tt tt .. %% %% %% %% xx ,, cc ,, xx %% %% %% ~~ ~~ -- || bb .. .. .. bb || "
            "tt ~~ -- __ __ -- ~~ tt .. %% %% %% %% xx ,, cc ,, xx %% %% %% %% ~~ %% || || tt .. tt || || "
            "tt ~~ -- __ __ -- ~~ tt .. %% %% %% %% xx w+ cc w+ xx .. bb .. bb %% %% || || bb .. bb || || "
            "tt tt ~~ -- -- ~~ tt tt .. %% .. %% %% .. .. .. .. .. .. .. .. .. == .. tt tt .. .. tt || || "
            "%% tt tt ~~ ~~ tt tt .. %% .. .. .. %% bb .. .. .. bb .. bb .. bb %% %% || bb tt bb || || || "
            "%% %% tt tt tt tt .. %% %% %% .. %% %% %% .. .. .. %% %% %% %% %% %% %% || || || || || || || "
  ))

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
  )

 nil ; hooks
 nil ; edge entrances
 )
