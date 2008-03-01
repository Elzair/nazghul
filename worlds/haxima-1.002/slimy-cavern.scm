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
		"rn rn rn rn rn rn rn rn rn rn rn rn r8 r8 r8 rn "
		"rn rn rn rn rn r8 r8 r8 r8 r8 rn r4 .. .. .. r2 "
		"rn rn rn rn rc .. .. && .. .. ra r4 .. .. .. r2 "
		"rn r8 r8 rc bb .. .. .. .. .. bb rr rr .. r3 rn "
		"r4 bb .. .. bb .. .. .. .. .. bb .. .. .. ra rn "
		"r4 bb .. .. .. bb bb .. bb bb .. .. .. .. .. r2 "
		"r4 .. .. .. .. .. .. .. .. .. .. .. .. .. .. r2 "
		"r4 bb .. .. .. .. .. .. .. .. .. .. .. .. r3 rn "
		"rn r5 bb .. .. .. .. .. .. .. .. .. r3 r9 r8 rn "
		"rn r8 rd .. .. .. .. .. .. .. .. .. r6 .. .. r2 "
		"r4 .. .. .. .. .. .. r3 r5 .. .. r3 r4 .. .. r2 "
		"r4 .. r7 .. rf bb r3 r8 r8 r5 .. r2 r4 .. r3 rn "
		"rc %7 re .. .. .. r6 .. .. re .. ra rc .. r2 rn "
		"~~ b~ b~ rf .. .. r6 .. .. .. .. .. .. .. r2 rn "
		"r5 ~a b~ ~4 .. .. ra r5 .. r3 r5 .. r3 r1 rn rn "
		"rn rd %a ~a ~9 ~1 b~ ra r1 rn rc .. ra rn rn rn "
		"r4 .. .. .. .. ~a b~ b~ ra rc .. .. .. r2 rn rn "
		"r4 .. r7 .. %f r7 b~ ~~ bb %7 .. .. .. ra rn rn "
		"r4 .. r2 r1 r1 r8 rd b~ ~~ ~5 %5 .. .. %7 ra rn "
		"r4 .. ra r8 r4 .. .. rf %% ~a b~ ~9 b~ ~1 ~5 ra "
		"r4 .. .. .. r6 .. %b %% %% %% %c .. %% b~ ~8 ~~ "
		"rn r1 r5 .. r6 .. .. r3 r5 .. .. .. %e rb r9 r1 "
		"rn rn rc .. ra r1 r1 r8 rc .. .. .. .. .. bb r2 "
		"rn rc .. .. .. ra rc .. .. .. r7 .. .. .. .. r2 "
		"r4 .. .. .. .. .. .. .. r3 r1 r4 .. .. .. .. r2 "
		"r4 .. .. .. .. r3 r1 r9 r8 r8 rn r5 .. .. r3 rn "
		"rn r5 .. .. r3 rn rc .. .. .. ra rn r5 .. r2 rn "
		"rn r8 rd .. ra rc .. .. .. .. .. ra rc .. ra rn "
		"r4 .. .. .. .. .. .. .. .. .. .. .. .. .. .. r2 "
		"r4 .. r3 r1 r1 r5 .. .. .. .. .. r7 .. rf .. r2 "
		"r4 .. r2 rn rn rn r5 .. .. .. r3 r4 .. .. .. r2 "
		"rn r1 rn rn rn rn rn r1 r1 r1 rn rn r1 r1 r1 rn "
	)
 )

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
(define rolands-chest
  (mk-chest
   nil ;; trap
   '(
    ;; Food
    (2 t_food)

    ;; Arms
    (1 t_2H_sword)
    (1 t_armor_chain)
    (1 t_iron_helm)

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
                               '((1 t_in_ex_por_scroll)
                                 (1 t_wis_quas_scroll)
                                 (3 t_gem)
                                 (5 t_picklock)
                                 (50 t_gold_coins)
                                 ))
                     5 2)

                ;; terrain features
                (list (mk-ladder-up 'p_shard 13 8) 8 30)
                (list (mk-bridge 'north) 4 15)
                (list (mk-bridge 'north) 11 19)                

                ;; doors/mechanisms
                (list (mk-door-full 'windowed-wood-door-in-rock #t #f nil) 13 3)

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

(mk-place-music p_slimy_cavern 'ml-dungeon-adventure)
