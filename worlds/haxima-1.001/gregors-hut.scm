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
  ".. .. .. /7 tt tt tt tt tt  r  r ws  r  r  r ws  r  r  r  r  r  b  b  b  b  b /8 /d /2 tt .. .. "
  ".. .. .. /7 tt tt tt tt tt  r cc cc cc cc cc cc cc  r cc cc  r .. .. .. .. ..  b .. /7 .. .. .. "
  ".. .. .. /7 .. tt tt tt tt  r cc cc cc cc cc cc cc  r cc cc  r tt .. .. .. ..  b .. /7 .. .. .. "
  ".. .. .. /7 .. .. tt || ||  r cc cc  [  @  ] cc cc  r cc cc  r tt tt .. .. ..  b .. /7 .. .. .. "
  ".. .. .. /7 .. .. tt || ||  r cc cc cc cc cc cc cc  r cc cc  r tt tt tt .. ..  b .. /7 .. .. .. "
  ".. .. .. /7 .. ..  r  r  r  r cc cc cc cc cc cc cc cc cc cc  r tt tt || tt ..  b .. /7 .. .. .. "
  ".. .. .. /7 .. .. ws cc cc  r  r cc  r  &  r  r  r  r  r  r  r tt tt tt .. ..  b .. /7 .. .. .. "
  "/d /d /d /6 .. ..  r cc cc cc cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. /4 /d /d /d "
  ".. .. .. /7 .. ..  r  r  r  r cc cc cc cc cc cc cc  r .. ..  r .. .. .. .. ..  b .. /7 tt tt tt "
  ".. .. .. /7 .. ..  r cc cc  r cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt /7 tt tt tt "
  ".. .. .. /7 .. .. ws cc cc cc cc cc cc cc cc cc cc  r .. .. .. .. .. .. .. ..  b tt /7 tt tt %% "
  ".. .. .. /7 .. ..  r  r  r  r  r ws  r cc  r ws  r  r ..  r  r  b  b  b  b  b /0 /d /a tt %% %% "
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
                (list (mk-bridge east) 25 26)
                )
               nil ; hooks
               )


;; A carpet (3x2)
;; A proper fireplace
;; A wood pile
;; beds
;; chest & dresser
;; hay bales
;; sheep
;; a goat
;; a cat
;; a dog
;; crops
;; tombstones
;; cookpot
;; barrels
;; place settings
;; chairs
;; flowers in vase
;; water trough
;; bucket

;                 (list (kern-mk-obj tf_stone_lantern 1) 14 24)
;                 (list (kern-mk-obj tf_stone_lantern 1) 16 24)
;                 (list (kern-mk-obj tf_stone_lantern 1) 15 29)
;                 (list (kern-mk-obj tf_stone_lantern 1) 17 29)
