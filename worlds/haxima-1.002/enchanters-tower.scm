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
            ".. .. .. .. .. cc cc cc cc ,, xx ,, ,, ,, ,, cc ,, ,, ,, ,, xx ,, cc cc cc cc .. .. .. .. .. "
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

(kern-load "gate-guard.scm")

;;(kern-load "enchanter.scm")
;;(mk-enchanter-first-time 'ch_enchanter)

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

  ;;(put (mk-mongen t_mage_guard_gen 0 1) 8 15)

  ;; west guardhouse
  (put (mk-gate-guard 'enchtwr-lever-west) 9 16)
  (put (mk-gate-guard-gen 'char-is-gate-guard? 
                          'mk-gate-guard 
                          'enchtwr-lever-west) 6 14)
  (put (kern-tag 'enchtwr-port-west (mk-portcullis)) 5 15)
  (put (kern-tag 'enchtwr-lever-west (mk-lever 'enchtwr-port-west)) 7 14)

  ;; east guardhouse
  (put (mk-gate-guard 'enchtwr-lever-east) 24 14)
  (put (mk-gate-guard-gen 'char-is-gate-guard? 
                          'mk-gate-guard 
                          'enchtwr-lever-east) 24 14)
  (put (kern-tag 'enchtwr-port-east (mk-portcullis)) 25 15)
  (put (kern-tag 'enchtwr-lever-east (mk-lever 'enchtwr-port-east)) 23 14)

  ;; south guardhouse
  (put (mk-gate-guard 'enchtwr-lever-south) 16 26)
  (put (mk-gate-guard-gen 'char-is-gate-guard? 
                          'mk-gate-guard 
                          'enchtwr-lever-south) 16 26)
  (put (kern-tag 'enchtwr-port-south (mk-portcullis)) 15 27)
  (put (kern-tag 'enchtwr-lever-south (mk-lever 'enchtwr-port-south)) 16 25)

  ;; north guardhouse
  (put (mk-gate-guard 'enchtwr-lever-north) 16 6)
  (put (mk-gate-guard-gen 'char-is-gate-guard? 
                          'mk-gate-guard 
                          'enchtwr-lever-north) 16 6)
  (put (kern-tag 'enchtwr-port-north (mk-portcullis)) 15 5)
  (put (kern-tag 'enchtwr-lever-north (mk-lever 'enchtwr-port-north)) 16 7)

  ;; portcullis's & levers for off-limit supply rooms
  (put (kern-tag 'enchtwr-port-4 (mk-portcullis)) 13 15)
  (put (kern-tag 'enchtwr-port-5 (mk-portcullis)) 17 15)
  (put (mk-lever 'enchtwr-port-4) 21 9)
  (put (mk-lever 'enchtwr-port-5) 22 9)

  ;; slimes invade the south
  (put (mk-mongen2 900 3 'is-green-slime? 'mk-green-slime nil) 4 22)

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
