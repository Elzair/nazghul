;;----------------------------------------------------------------------------
;; Map
;;
;; Declare the map for the place here.
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_enchanters_tower 65 65 pal_expanded
 (list
            "__ __ -- ~~ %% %% %% %% %% %% %% %% ~~ ~~ ~~ %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% b! .. .. .. b! %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "__ __ -- ~~ %% %% %% %% %% %% %% ~~ ~~ -- ~~ ~~ %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "-- -- xx xx ws xx xx %% %% %% %% ~~ -- __ -- ~~ %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% xx xx ws xx xx %% %% "
            "~~ ~~ xx cc cc cc x! %% %% %% %% ~~ ~~ -- ~~ ~~ %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% xx cc cc cc xx %% %% "
            "%% %% ws cc b! cc cc ee ee ee .. %% ~~ ~~ ~~ %% .. %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% ws cc b! cc ws %% %% "
            "%% %% xx cc cc cc x! %% %% .. .. .. ee ee ee .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% xx cc cc cc xx %% %% "
            "%% %% xx xx ws xx xx %% %% %% .. %% %% %% %% %% .. %% %% bb .. bb .. bb .. bb .. bb .. bb .. .. .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb %% %% xx x! cc x! xx %% %% "
            "%% ~~ ~~ -- __ -- ~~ ~~ %% %% %% ~~ ~~ ~~ %% %% ee %% bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% %% %% %% ee %% %% %% %% "
            "%% ~~ -- __ __ __ -- ~~ %% %% ~~ ~~ -- ~~ ~~ %% ee .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb %% %% %% %% ee %% %% %% %% "
            "%% ~~ ~~ -- -- -- ~~ ~~ %% ~~ ~~ -- __ -- ~~ ~~ %% %% bb .. .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. .. .. .. .. bb .. bb .. bb .. %% %% "
            "%% %% ~~ ~~ ~~ ~~ ~~ %% %% ~~ -- __ __ __ -- ~~ %% %% .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. .. .. .. .. .. bb %% %% "
            "%% %% %% %% %% %% %% %% %% ~~ ~~ -- __ -- ~~ ~~ %% %% bb .. .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. .. .. .. .. .. .. .. .. %% %% "
            "%% %% %% %% %% %% %% %% %% %% ~~ ~~ -- ~~ ~~ %% %% %% .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. bb .. bb .. bb .. .. .. bb %% %% "
            "%% %% %% %% %% %% %% %% %% %% %% ~~ ~~ ~~ %% %% %% %% bb .. .. bb %% %% %% %% %% %% %% %% xx xx xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% %% "
            "%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% %% %% tt tt %% %% %% xx cc cc cc xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% %% "
            "%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% %% tt tt tt tt xx xx xx cc cc cc xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% %% "
            "%% bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. .. .. .. %% tt tt || || xx xx xx xx x! cc x! xx xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% %% "
            "%% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb %% tt tt || ?? ?? x! 00 00 cc cc cc 00 xx xx xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% %% tt xx xx ?? xx 00 cc cc cc cc cc xx xx xx xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% %% "
            "%% .. .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb %% bb %% %% tt xx cc cc x! 00 cc cc 00 cc cc && ?? cc cc xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. %% %% %% %% %% %% %% %% %% %% %% .. %% %% %% %% %% %% %% xx xx cc cc xx 00 cc cc 00 cc cc && xx cc cc xx xx || || || || tt ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx %% %% bb .. .. bb %% %% "
            "%% .. .. .. bb %% %% %% %% %% %% %% %% %% %% bb .. bb %% %% %% %% %% %% xx cc cc cc xx 00 cc cc cc cc cc xx xx cc cc cc xx || tt tt tt tt ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. %% %% %% %% %% %% %% %% %% %% .. .. %% tt tt tt %% %% %% xx cc cc x! xx xx x! cc cc cc x! xx xx x! cc cc xx || tt .. .. .. ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% bb .. .. bb %% %% "
            "%% .. .. .. bb %% %% %% %% %% %% %% %% %% bb .. bb tt tt || tt tt %% %% xx cc cc xx cc cc cc cc cc cc cc cc cc xx cc cc xx || tt .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. %% %% %% %% %% %% %% %% %% %% .. .. tt || || || tt %% %% xx xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx xx || tt .. .. .. ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% bb .. .. bb %% %% "
            "%% .. .. .. bb %% %% %% %% %% %% %% %% tt bb .. bb tt tt || tt tt tt tt tt xx xx xx cc cc 00 cc cc cc 00 cc cc xx xx xx || || tt .. .. .. ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. %% %% %% %% %% tt tt tt tt tt .. .. tt tt tt tt tt tt bb tt xx xx && cc cc 00 cc cc cc 00 cc cc && xx xx || ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx %% %% bb .. .. bb %% %% "
            "%% .. .. .. bb %% %% %% %% tt tt || tt tt tt bb .. bb tt tt tt bb tt .. .. .. xx xx cc cc 00 cc cc cc 00 cc cc xx xx || || ,, || || || || ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% .. .. .. .. %% %% "
            "%% bb .. .. .. %% %% %% %% tt || || || tt tt tt .. .. .. bb .. .. .. .. .. .. .. xx cc cc cc cc cc cc cc cc cc xx || || || ,, || || || || ,, tt tt ,, tt tt ,, tt tt ,, xx %% %% bb .. .. bb %% %% "
            "%% .. .. .. bb %% %% %% %% tt tt || tt tt tt bb .. .. .. .. .. .. .. .. .. .. .. xx x! cc cc cc cc cc cc cc x! xx b! ,, ,, ,, ,, ,, b! || ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx %% %% .. .. .. .. bb %% "
            "b! .. .. .. .. %% %% %% %% %% tt tt tt || tt tt tt bb .. .. .. .. .. .. .. .. .. xx xx xx xx cc x! cc xx xx xx xx ,, ,, ,, ,, ,, ,, ,, || ,, .. .. .. .. .. .. .. .. .. xx %% %% bb .. .. .. .. b! "
            ".. .. .. .. .. bb %% %% %% %% %% tt || || || tt tt tt .. .. .. .. .. .. .. .. .. .. xx .X xx cc cc cc ws .. .. .. ,, ,, ~~ -- ~~ ,, ,, || ,, tt tt tt tt tt tt tt tt .. xx %% %% .. .. .. .. .. .. "
            ".. .. .. .. .. .. %% %% %% %% %% tt tt || tt tt tt .. .. .. .. .. .. && .. .. .. .. xx xx xx cc cc cc cc ,, ,, ,, ,, ,, -- __ -- ,, ,, ,, ,, || || || || || || || tt tt xx %% %% bb .. .. .. .. .. "
            ".. .. .. .. .. bb %% %% %% %% %% %% xx xx xx xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. xx cc cc cc ws .. .. .. ,, ,, ~~ -- ~~ ,, ,, || || || xx xx xx xx xx || || || xx %% %% .. .. .. .. .. .. "
            "b! .. .. .. .. %% %% %% %% %% xx xx xx cc cc cc xx xx xx .. .. .. .. .. .. .. .. .. xx xx xx cc cc cc xx xx xx tt ,, ,, ,, ,, ,, ,, ,, || xx xx xx cc cc cc xx xx xx || xx %% %% bb .. .. .. .. b! "
            "%% .. .. .. bb %% %% %% %% xx xx cc cc cc cc cc cc cc xx xx .. .. .. .. .. .. .. xx xx cc cc cc cc cc cc cc xx xx b! ,, ,, ,, ,, ,, b! xx xx xx xx xx ?? xx xx xx xx xx xx %% %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% %% xx xx cc cc xx xx x! xx xx x! xx xx .. .. .. .. .. .. xx x! cc cc cc cc cc cc cc cc cc x! xx tt .. ,, .. tt xx xx xx cc cc xx && cc cc xx cc xx xx %% %% %% bb .. .. bb %% "
            "%% .. .. .. bb %% %% xx xx cc cc xx x! 00 00 00 00 00 00 x! xx xx .. .. .. xx xx cc cc cc cc cc cc cc cc cc cc cc xx xx .. ,, .. xx xx .X xx cc cc xx xx cc cc xx cc .X xx xx %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% xx cc cc cc xx 00 cc cc cc cc cc cc 00 xx xx .. .. .. ws cc cc cc cc cc cc cc cc cc cc cc cc cc xx .. ,, .. xx cc cc xx cc cc && xx cc cc xx cc cc cc xx xx %% bb .. .. bb %% "
            "%% .. .. .. bb %% xx xx xx x! cc x! 00 cc cc cc cc cc cc 00 xx xx xx ws xx x! cc cc cc cc b! cc cc cc b! cc cc cc cc x! ws cc ws x! xx xx x! cc x! xx x! cc x! xx xx xx xx xx xx %% .. .. .. .. %% "
            "%% bb .. .. .. %% xx cc cc cc cc xx cc cc cc 00 00 cc cc cc cc x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx %% bb .. .. bb %% "
            "%% .. .. .. bb %% xx cc cc cc cc cc cc cc cc 00 00 cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .X cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc b! cc xx %% .. .. .. .. %% "
            "%% bb .. .. .. %% xx cc cc cc cc xx cc cc cc 00 00 cc cc cc cc x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx %% bb .. .. bb %% "
            "%% .. .. .. bb %% xx xx xx x! cc x! 00 cc cc cc cc cc cc 00 xx xx xx ws xx x! cc cc cc cc b! cc cc cc b! cc cc cc cc x! xx ws xx x! xx xx xx x! cc x! xx x! cc x! xx xx xx xx xx %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% xx cc cc cc xx 00 cc cc cc cc cc cc 00 xx xx %% %% %% ws cc cc cc cc cc cc cc cc cc cc cc cc cc ws %% %% %% xx cc cc xx cc cc xx && cc cc xx cc cc cc xx %% %% bb .. .. bb %% "
            "%% .. .. .. bb %% %% xx xx cc cc xx x! 00 00 00 00 00 00 x! xx xx %% %% %% xx xx cc cc cc cc cc cc cc cc cc cc cc xx xx %% %% %% xx xx .X xx cc cc xx xx cc cc xx cc .X xx xx %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% %% xx xx cc cc xx xx x! xx x! xx xx xx %% %% %% %% %% %% xx x! cc cc cc cc cc cc cc cc cc x! xx %% %% %% %% %% xx xx xx cc cc && xx cc cc xx cc xx xx %% %% %% bb .. .. bb %% "
            "%% .. .. .. bb %% %% %% %% xx xx cc cc cc cc cc cc cc xx xx %% %% %% %% %% %% %% xx xx cc cc cc cc cc cc cc xx xx %% %% %% %% %% %% %% xx xx xx xx ?? xx xx xx xx xx xx %% %% %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% %% %% %% xx xx xx cc cc cc xx xx xx %% %% %% %% %% %% %% %% %% xx ws xx cc cc cc xx ws xx %% %% %% tt tt tt %% %% %% xx xx xx cc cc cc xx xx xx %% %% %% %% %% bb .. .. bb %% "
            "%% .. .. .. bb %% %% %% %% %% %% %% xx xx xx xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% x! cc xx cc x! .. %% %% %% tt tt ~~ tt tt %% %% %% %% xx xx xx xx xx %% %% %% %% %% %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% tt ~~ -- ~~ tt %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% "
            "%% .. .. .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. cc cc cc .. bb %% %% %% tt tt ~~ tt tt %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% "
            "%% bb .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% %% tt tt tt %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% "
            "%% .. .. .. bb .. bb .. bb .. bb .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. cc cc cc .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. %% "
            "%% bb .. .. .. .. .. .. .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. .. bb %% "
            "%% .. .. .. .. .. .. .. .. .. .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. b! .. cc cc cc .. b! .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. .. .. .. %% "
            "%% bb .. .. .. .. .. bb .. bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb %% "
            "%% %% %% bb .. bb %% %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
            "%% %% xx x! .. x! xx %% %% %% bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb %% "
            "%% %% xx cc cc cc xx %% %% %% %% bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. b! .. cc cc cc .. b! .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. bb .. %% %% "
            "%% %% ws cc b! cc ws %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "%% %% xx cc cc cc xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. cc cc cc .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "%% %% xx xx ws xx xx %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% bb .. cc cc cc .. bb %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
            "%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. cc cc cc .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% "
  ))

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
(kern-load "enchanters-tower-zones.scm")

;;----------------------------------------------------------------------------
;; Characters
;;
;; kern-load the character files here and call their first-time constructors.
;;----------------------------------------------------------------------------
(kern-load "zane.scm")
(mk-zane-first-time 'ch_zane)

;;(kern-load "enchanter.scm")
;;(mk-enchanter-first-time 'ch_enchanter)

;;----------------------------------------------------------------------------
;; Place
;;
;; Call the place constructor here.
;;----------------------------------------------------------------------------
(kern-mk-place 'p_enchanters_tower ; tag
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
                (list ch_zane  32 32)

                (list (mk-door) 31 49)
                (list (mk-door) 33 49)
                (list (mk-door) 34 32)
                (list (mk-door) 41 39)
                (list (mk-door) 10 39)
                (list (mk-door) 10 43)
                (list (mk-door) 32 16)

                (list (mk-door) 47 39)
                (list (mk-door) 51 39) 
                (list (mk-door) 48 43)
                (list (mk-door) 52 43)

                ;; workshop
                (list (mk-door-full solid-wood-door-in-stone #f #t nil) 21 41)
                (list (mk-windowed-door) 11 41)

                ;; bedrooms
                (list (mk-bed) 52 36)
                (list (mk-bed) 47 36)
                (list (mk-bed) 47 46)
                (list (mk-bed) 52 46)

                )
               nil ; hooks
               nil ; edge entrances
               )
