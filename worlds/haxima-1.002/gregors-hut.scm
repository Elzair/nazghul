(kern-mk-map 
 'm_gregors_hut 32 32 pal_expanded
 (list
  "|| || || || tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt "
  "|| || || || tt .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt "
  "|| || tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. tt || || || tt tt tt "
  "|| tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || tt tt tt "
  "tt tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt || || tt || || tt .. "
  "tt .. .. .. tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. tt tt tt || tt tt tt || tt .. "
  ".. .. .. .. tt tt tt tt .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt || || tt || || tt .. "
  ".. .. .. .. /0 /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /d /2 tt tt || || || tt tt .. "
  ".. .. .. /0 /a tt tt tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /8 /d /d /2 tt tt tt .. .. "
  ".. .. .. /7 tt tt tt tt tt rr rr ws rr rr rr ws rr rr rr rr rr bb bb bb bb bb /8 /d /2 tt .. .. "
  ".. .. .. /7 tt tt tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr .. .. .. .. .. bb .. /7 .. .. .. "
  ".. .. .. /7 .. tt tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr tt .. .. .. .. bb .. /7 .. .. .. "
  ".. .. .. /7 .. .. tt || || rr cc cc [[ @@ ]] cc cc rr cc cc rr tt tt .. .. .. bb .. /7 .. .. .. "
  ".. .. .. /7 .. .. tt || || rr cc cc cc cc cc cc cc rr cc cc rr tt tt tt .. .. bb .. /7 .. .. .. "
  ".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc cc cc cc rr tt tt || tt .. bb .. /7 .. .. .. "
  ".. .. .. /7 .. .. ws cc cc rr rr cc rr && rr rr rr rr rr rr rr tt tt tt .. .. bb .. /7 .. .. .. "
  "/d /d /d /6 .. .. rr cc cc cc cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /4 /d /d /d "
  ".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /7 tt tt tt "
  ".. .. .. /7 .. .. rr cc cc rr cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb tt /7 tt tt tt "
  ".. .. .. /7 .. .. ws cc cc cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb tt /7 tt tt %% "
  ".. .. .. /7 .. .. rr rr rr rr rr ws rr cc rr ws rr rr .. rr rr bb bb bb bb bb /0 /d /a tt %% %% "
  ".. .. .. /7 .. .. .. .. .. .. .. tt tt /7 .. tt tt tt /4 /d /d /d /d /d /d /d /a tt tt %% %% %% "
  ".. .. .. /7 .. .. .. .. .. .. .. .. .. /7 .. .. .. .. /7 .. .. .. .. .. .. tt tt tt %% %% ~~ ~~ "
  ".. .. .. /8 /d /d /d /d /d /d /d /d /d /9 /d /1 /d /d /a .. .. .. .. .. tt tt tt %% ~~ ~~ ~~ %% "
  ".. .. .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt .. .. .. tt tt tt tt tt %% ~~ %% %% %% "
  "tt tt .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt tt tt .. .. .. tt ~~ ~~ ~~ ~~ %% %% %% "
  "|| tt tt .. .. .. .. tt tt tt tt .. .. .. .. /7 .. .. tt tt tt tt tt .. .. ~~ .. tt %% %% tt .. "
  "|| || tt tt .. .. .. .. tt tt tt .. .. .. .. /7 .. .. .. tt tt tt tt ~~ ~~ -- .. .. .. tt tt .. "
  "|| || tt tt tt .. .. .. .. .. .. .. .. .. .. /8 /2 .. .. tt tt tt ~~ ~~ ~~ ~~ tt tt .. tt || .. "
  "|| || || tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. tt tt tt ~~ tt tt tt tt tt .. .. .. .. "
  "|| || || || || tt tt .. .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ ~~ tt .. .. tt tt || .. .. .. "
  "|| || || || || || tt tt .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ tt tt .. tt tt || || .. .. .. "
  )
 )

(kern-load "ilya.scm")

(bind 
 (kern-mk-char 'ch_ilya ; tag
               "Ilya"              ; name
               sp_human            ; species
               nil                 ; occ
               s_child             ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               #f                  ; dead
               'ilya-conv          ; conv
               sch_ilya           ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (ilya-mk #f #f))

(kern-mk-place 'p_gregors_hut     ; tag
               "Gregor's Hut"     ; name
               s_hamlet           ; sprite
               m_gregors_hut      ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               ;;objects
               (list
                
                ;; characters
                (list ch_ilya   15 15)

                ;; mechanisms
                (list (mk-door) 13 20)
                (list (mk-door)  9 19)
                (list (mk-door)  9 16)
                (list (mk-door) 11 15)
                (list (mk-door) 17 14)
                (list (mk-door) 18 20)
                (list (mk-bridge 'east) 25 26)
                (put (mk-bed) 7 18)
                (put (mk-bed) 7 16)
                
                )
               nil ; hooks
               nil ; edge entrances
               )
