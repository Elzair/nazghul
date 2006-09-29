;;----------------------------------------------------------------------------
;; Slimy Cavern
;;
;;  Wherein the player fights some slimes, some bandits, and optionally rescues
;;  Roland.
;;----------------------------------------------------------------------------

(kern-load "slimy-cavern-zones.scm")

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "roland.scm")

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
		"rr %7 rr .. .. .. rr .. .. rr .. rr rr .. rr rr "
		"~~ b~ b~ rr .. .. rr .. .. .. .. .. .. .. rr rr "
		"rr ~a b~ ~4 .. .. rr rr .. rr rr .. rr rr rr rr "
		"rr rr %a ~a ~9 ~1 b~ rr rr rr rr .. rr rr rr rr "
		"rr .. .. .. .. ~a b~ b~ rr rr .. .. .. rr rr rr "
		"rr .. rr .. %f rr b~ ~~ bb %7 .. .. .. rr rr rr "
		"rr .. rr rr rr rr rr b~ ~~ ~5 %5 .. .. %7 rr rr "
		"rr .. rr rr rr .. .. rr %% ~a b~ ~9 b~ ~1 ~5 rr "
		"rr .. .. .. rr .. %b %% %% %% %c .. %% b~ ~8 ~~ "
		"rr rr rr .. rr .. .. rr rr .. .. .. %e rr rr rr "
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
   t_chest ;; type
   nil ;; trap
   (list
    ;; Food
    (list 2 t_food)

    ;; Arms
    (list 1 t_2H_sword)
    (list 1 t_armor_chain)
    (list 1 t_iron_helm)

    ;; Hints/instructions
    )))

(define (mk-green-slime-verbose msg)
  (kern-log-msg msg)
  (mk-npc 'green-slime 3))
  

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
                (put (mk-monman) 0 0)

                ;; loot and loose-lying objects
                (list rolands-chest 9 2)
                (put (mk-corpse-with-loot) 12 21)

                ;; Trapped chest with some reasonably nice thiefly items
                (put (mk-chest 'poison-trap
                               (list (list 1 t_in_ex_por_scroll)
                                     (list 1 t_wis_quas_scroll)
                                     (list 3 t_gem)
                                     (list 5 t_picklock)
                                     (list 50 t_gold_coins)
                                     ))
                     5 2)

                ;; terrain features
                (list (mk-ladder-up 'p_shard 13 8) 8 30)
                (list (mk-bridge 'north) 4 15)
                (list (mk-bridge 'north) 11 19)                

                ;; doors/mechanisms
                (list (mk-door-full windowed-wood-door-in-rock #t #f nil) 13 3)

                ;; existing npc's
                (put (mk-npc 'blackguard 2) 5 4)
                (put (mk-npc 'blackguard 2) 9 4)
                (put (mk-npc 'blackguard 2) 9 2)
                (put (spawn-pt 'green-slime) 9 17)
                (put (spawn-pt 'green-slime) 10 18)
                (put (spawn-pt 'green-slime) 13 18)
                (put (mk-roland) 14 1)

                )
               (list 'on-entry-to-dungeon-room) ; hooks
               nil ; edge entrances
               )
