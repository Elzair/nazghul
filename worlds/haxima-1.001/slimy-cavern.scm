;;----------------------------------------------------------------------------
;; Slimy Cavern
;;
;;----------------------------------------------------------------------------

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
(kern-load "slimy-cavern-zones.scm")

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "roland.scm")
(mk-roland-first-time 'ch_roland)


;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map 
 'm_slimy_cavern 16 32 pal_expanded
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. rr "
  "rr rr rr rr rr .. .. && .. .. rr rr .. .. .. rr "
  "rr rr rr rr bb .. .. .. .. .. bb rr rr .. rr rr "
  "rr bb .. .. bb .. .. .. .. .. bb .. .. .. rr rr "
  "rr bb .. .. .. bb bb .. bb bb .. .. .. .. .. rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
  "rr bb .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
  "rr rr bb .. .. .. .. .. .. .. .. .. rr rr rr rr "
  "rr rr rr .. .. .. .. .. .. .. .. .. rr .. .. rr "
  "rr .. .. .. .. .. .. rr rr .. .. rr rr .. .. rr "
  "rr .. rr .. rr bb rr rr rr rr .. rr rr .. rr rr "
  "rr %% rr .. .. .. rr .. .. rr .. rr rr .. rr rr "
  "~~ bb bb rr .. .. rr .. .. .. .. .. .. .. rr rr "
  "rr ~~ bb ~~ .. .. rr rr .. rr rr .. rr rr rr rr "
  "rr rr %% ~~ ~~ ~~ bb rr rr rr rr .. rr rr rr rr "
  "rr .. .. .. .. ~~ bb bb rr rr .. .. .. rr rr rr "
  "rr .. rr .. %% rr bb ~~ bb %% .. .. .. rr rr rr "
  "rr .. rr rr rr rr rr bb ~~ ~~ %% .. .. %% rr rr "
  "rr .. rr rr rr .. .. rr %% ~~ bb ~~ bb ~~ ~~ rr "
  "rr .. .. .. rr .. %% %% %% %% %% .. %% bb ~~ ~~ "
  "rr rr rr .. rr .. .. rr rr .. .. .. %% rr rr rr "
  "rr rr rr .. rr rr rr rr rr .. .. .. .. .. bb rr "
  "rr rr .. .. .. rr rr .. .. .. rr .. .. .. .. rr "
  "rr .. .. .. .. .. .. .. rr rr rr .. .. .. .. rr "
  "rr .. .. .. .. rr rr rr rr rr rr rr .. .. rr rr "
  "rr rr .. .. rr rr rr .. .. .. rr rr rr .. rr rr "
  "rr rr rr .. rr rr .. .. .. .. .. rr rr .. rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
  "rr .. rr rr rr rr .. .. .. .. .. rr .. rr .. rr "
  "rr .. rr rr rr rr rr .. .. .. rr rr .. .. .. rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
(define rolands-chest
  (kern-mk-container
   t_small_iron_chest ;; type
   nil ;; trap
   (list
    ;; Food
    (list 2 t_mushroom)

    ;; Arms
    (list 1 t_2H_sword)
    (list 1 t_armor_chain)
    (list 1 t_iron_helm)

    ;; Hints/instructions
    )))


(kern-mk-place 'p_slimy_cavern    ; tag
               "Slimy Cavern"     ; name
               nil                ; sprite
               m_slimy_cavern     ; map
               #f                 ; wraps
               #t                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil                ; subplaces
               nil                ; neighbors

               ;; objects
               (list

                ;; loot and loose-lying objects
                (list rolands-chest 9 2)

                ;; terrain features
                (list (mk-ladder-up 'p_moongate_clearing 20 1) 8 30)
                (list (mk-bridge 'north) 4 15)
                (list (mk-bridge 'north) 11 19)                

                ;; doors/mechanisms
                (list (mk-door-full windowed-wood-door-in-rock #t #f nil) 13 3)

                ;; monster generators
                (list (mk-slime-generator 9 17) 11 17)

                ;; existing npc's
                (list (mk-bandit) 5 4)
                (list (mk-bandit) 9 4)
                (list (mk-bandit) 9 2)                
                (list ch_roland  14 1)

                )
               (list 'slimy-cavern-entry) ; hooks
               nil ; edge entrances
               )
