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

;;----------------------------------------------------------------------------
;; Zones
;;
;; Zones are rectangular areas defined as scheme variables. They can be
;; referred to by NPC schedules and other parts of the script. They cannot be
;; defined in this file, however, and must be kern-loaded, for a couple of
;; reasons:
;;
;; 1. When the kernel saves and reloads a game, the reloaded game will not load
;; this file (this one, right here). The reason is that everything in this file
;; defines an initial game state, and the game will change as it plays. When
;; the kernel saves it will save all of this state as part of a single file.
;;
;; 2. When the kernel saves a game it will not save the zones because it
;; doesn't know about them.
;;
;; 3. The kern-load procedure tells the kernel that when it reloads a game it
;; needs to reload the given file. Think of the zone file as read-only data,
;; whereas this file contains read/write data.
;;----------------------------------------------------------------------------
(kern-load "gregors-hut-zones.scm")

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
               'ilya-conv          ; conv
               sch_ilya           ; sched
               nil                 ; special ai
               nil                 ; readied
               )
 (ilya-mk #f #f))

(kern-mk-place 'p_gregors_hut     ; tag
               "Gregor's Hut"     ; name
               s_hut              ; sprite
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
                (list (mk-bridge east) 25 26)

                )
               nil ; hooks
               nil ; edge entrances
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
