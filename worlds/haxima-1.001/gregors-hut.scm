(kern-mk-map 
 'm_gregors_hut 32 32 pal_expanded
 (list
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. tt  r  r  r  r  r  r  r  r  r  r  r  r  b  b  b  b  b .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. tt  r cc cc cc cc cc cc cc  r cc cc  r .. .. .. .. ..  b .. .. .. .. .. "
  ".. .. .. .. .. .. .. tt tt  r cc cc cc cc cc cc cc  r cc cc  r tt .. .. .. ..  b .. .. .. .. .. "
  ".. .. .. .. .. .. tt || ||  r cc cc  0  0  0 cc cc  r cc cc  r tt tt .. .. ..  b .. .. .. .. .. "
  ".. .. .. .. .. .. tt || ||  r cc cc cc cc cc cc cc  r cc cc  r tt tt tt .. ..  b .. .. .. .. .. "
  ".. .. .. .. .. ..  r  r  r  r cc cc cc cc cc cc cc cc cc cc  r tt tt || tt ..  b .. .. .. .. .. "
  ".. .. .. .. .. ..  r cc cc  r  r cc  r  &  r  r  r  r  r  r  r tt tt tt .. ..  b .. .. .. .. .. "
  ".. .. .. .. .. ..  r cc cc cc cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. .. .. .. tt "
  ".. .. .. .. .. ..  r  r  r  r cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. .. tt tt tt "
  ".. .. .. .. .. ..  r cc cc  r cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt tt tt tt tt "
  ".. .. .. .. .. ..  r cc cc cc cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt tt tt tt %% "
  ".. .. .. .. .. ..  r  r  r  r  r  r  r cc  r  r  r  r ..  r  r  b  b  b  b  b tt tt tt tt %% %% "
  ".. .. .. .. .. .. .. .. .. .. .. tt tt /7 .. tt tt tt .. .. .. .. .. .. .. tt tt tt tt %% %% %% "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. tt tt tt %% %% ~~ ~~ "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. /8 /d /2 .. .. .. .. .. .. .. .. tt tt tt %% ~~ ~~ ~~ %% "
  ".. .. .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt .. .. .. tt tt tt tt tt %% ~~ %% %% %% "
  "tt tt .. .. .. .. .. tt tt tt .. .. .. .. .. /7 .. .. tt tt tt .. .. .. tt ~~ ~~ ~~ ~~ %% %% %% "
  "|| tt tt .. .. .. .. tt tt tt tt .. .. .. .. /7 .. .. tt tt tt tt tt .. .. == .. tt %% %% tt .. "
  "|| || tt tt .. .. .. .. tt tt tt .. .. .. .. /7 .. .. .. tt tt tt tt ~~ ~~ -- .. .. .. tt tt .. "
  "|| || tt tt tt .. .. .. .. .. .. .. .. .. .. /8 /2 .. .. tt tt tt ~~ ~~ ~~ ~~ tt tt .. tt || .. "
  "|| || || tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. tt tt tt ~~ tt tt tt tt tt .. .. .. .. "
  "|| || || || || tt tt .. .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ ~~ tt .. .. tt tt || .. .. .. "
  "|| || || || || || tt tt .. .. .. .. .. .. .. .. /7 .. .. tt tt ~~ tt tt .. tt tt || || .. .. .. "
  )
 )

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
                (list (mk-door) 13 20)
                (list (mk-door)  9 19)
                (list (mk-door)  9 16)
                (list (mk-door) 11 15)
                (list (mk-door) 17 14)
                (list (mk-door) 18 20)
                )
               nil ; hooks
               )
