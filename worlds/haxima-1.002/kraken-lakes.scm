
(mk-dungeon-room
 'p_hydra_fen "Hydra Fen"
	(list
		"rn rn rn rn rn rn r8 r8 r8 r8 r8 r8 r8 rn rn rn rn rn rn "
		"rn rn rn rn rn rc {{ {{ {{ {{ {{ {{ {{ ra rn rn rn rn rn "
		"rn rn rn rn rc {{ {{ {{ {{ {{ {{ {{ {{ {{ ra rn rn rn rn "
		"rn rn rn rc {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ra rn rn rn "
		"rn rn rc {{ {{ {{ {{ {C %3 %% %5 {A {{ {{ {{ {{ ra rn rn "
		"rn rc {{ {{ {{ {C %3 %% %% %% %% %% %5 {A {{ {{ {{ ra rn "
		"r4 {{ {{ {{ {C %3 %% %% %% %% %% %% %% %5 {A {{ {{ {{ r2 "
		"r4 {{ {{ {{ %3 %% %% %% %% %% %% %% %% %% %5 {{ {{ {{ r2 "
		"r4 {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ r2 "
		"r4 {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ r2 "
		"r4 {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ r2 "
		"r4 {{ {{ {{ %% %% %% %% ee ee ee %% %% %% %c {{ {{ {{ r2 "
		"r4 {{ {{ {{ %a %% %% oo ee ee ee oo %% %% {# {{ {{ {{ r2 "
		"rn r5 {{ {{ {% %a %% %% ee ee ee %% %% %c {{ {{ {{ r3 rn "
		"rn rn r5 {{ {{ {% %e oo ee ee ee oo %e {# {{ {{ r3 rn rn "
		"rn rn rn r5 {{ {{ {{ {{ ee ee ee {{ {{ {{ {{ r3 rn rn rn "
		"rn rn rn rn r5 {{ {{ {{ {2 .. {4 {{ {{ {{ r3 rn rn rn rn "
		"rn rn rn rn rn r5 {{ {{ {2 .. {4 {{ {{ r3 rn rn rn rn rn "
		"rn rn rn rn rn rn r1 r5 .. .. .. r3 r1 rn rn rn rn rn rn "
	)
 (put (spawn-pt 'hydra) 9 9)
 )

(mk-place-music p_hydra_fen 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_pools "Pools"
 (list
		"rn rn r4 .. .. .. .. bb dd dd dd .. .. .. .. .. ra rn rn "
		"rn r8 r8 r5 .. .. .. .. dd dd dd bb .. .. .. .. .. ra rn "
		"r4 -- -- r2 r5 .. .. bb dd dd dd .. %3 %% %% %5 .. .. r2 "
		"r4 -- -- ra rc .. .. .. dd dd dd bb %% ~3 ~5 %% %5 .. r2 "
		"r4 -- -- ~~ %5 .. .. bb dd dd dd ~C ~3 -- -- ~5 %% %5 r2 "
		"rn r5 ~~ ~c %% .. .. .. dd dd dd bb -2 -- -- -- ~5 %% r2 "
		"rn r4 %a %% %c .. .. bb dd dd dd %% -- -- -- -- ~4 %% r2 "
		"rn rc .. .. .. .. .. xx w+ d, bb xx ~~ -- -- -- ~c %% r2 "
		"r4 .. .. .. .. .. .. rr ,, ,, ,, xx ~~ -- -- ~c %% %c r2 "
		"r4 .. %3 %% %% %5 .. w+ gg ,, ,, rr ~% ~a ~c %% %c rb rn "
		"r4 %3 %% ~3 ~5 %% .. gg ,, ,, ,, gg .. %% %% %% .. .. r2 "
		"r4 %% ~3 -- ~c %% .. rr xx bb gg rr .. %% ~7 %% %5 .. r2 "
		"r4 %% ~a ~c %% %c .. .. .. .. .. .. %3 ~3 -- ~5 %% .. r2 "
		"r4 %a %% %% %c .. .. .. .. .. %3 %% ~3 -- -- ~4 %% .. r2 "
		"rn r5 .. r3 r5 .. .. .. .. .. %% ~3 -- -- -- ~c %% .. r2 "
		"rn rn r1 rn rc .. .. .. .. .. %% ~a -- -- ~c %% %c .. r2 "
		"rn rn rn r4 .. .. .. .. .. .. %a %% ~a ~c %% %c .. r3 rn "
		"rn rn rn rn r5 .. .. r3 r5 .. .. %a %% %% %c .. .. r2 rn "
		"rn rn rn rn rn r1 r1 rn rn r1 r1 r1 r1 r1 r1 r1 r1 rn rn "
      )
 (put (mk-door) 9 7)
 (put (mk-ladder-up 'p_great_hall 9 6) 9 9)
 (put (spawn-pt 'yellow-slime) 11  3)
 (put (spawn-pt 'yellow-slime)  4  6)
 (put (spawn-pt 'yellow-slime) 10 13)
 
 )

(mk-place-music p_pools 'ml-dungeon-adventure)
 
(kern-mk-place 
 'p_deepness
 "The Deepness"
 nil     ; sprite
 (kern-mk-map 
  nil 38 38 pal_expanded 
	(list
		"rn rn r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 rn r8 rn rn rn r8 r8 r8 r8 r8 r8 r8 rc bb .. .. .. bb ra r8 r8 r8 r8 rn "
		"rn rc _3 _1 _1 _5 {{ {{ {{ {{ {{ {{ {{ {{ re {{ ra r8 rc bb %3 %% %% ~6 %% %% %5 .. .. .. .. .. %3 %% %% %% %5 r2 "
		"r4 _3 __ __ __ __ =| _1 _1 _5 rb r9 r5 {{ {{ {{ {{ {{ {{ {% %e bb %a ~e %% %% %% bb .. .. .. bb %% %% %% %% %% r2 "
		"r4 _a _8 _c r7 _e {{ __ __ __ __ _5 ra r9 rd {{ rb r1 r5 {{ {2 == .. bb %e bb %% %5 .. .. .. %3 %% bb %% bb %% r2 "
		"r4 {{ {{ rb rn rd {{ _2 __ __ __ __ ~~ ~1 ~1 ~1 ~5 ra rc ~7 .. ~6 .. .. .. .. ~7 %c .. .. .. %a %% %% %% %% %% r2 "
		"r4 {{ {{ {{ re {{ {L __ __ __ __ ~8 ~8 ~8 ~~ ~~ ~~ ~~ ~~ b~ ~9 ~~ ~~ ~d .. .. == .. .. .. .. .. .. .. %e bb %% r2 "
		"r4 {{ rf {{ {{ {L _3 __ __ _c {G {{ {{ {{ {H -a ~~ ~~ ~~ ~~ %% b~ ~~ bb %7 bb ~6 %5 .. .. .. %3 %5 .. .. %b %% r2 "
		"r4 {{ {L _3 _1 __ __ __ __ {G {{ r3 r9 r5 {{ {H ~~ ~~ ~~ ~~ ~~ ~~ ~~ %% %% %% ~6 %% %5 .. ~3 ~9 ~9 ~d .. bb %% r2 "
		"r4 {{ _3 __ __ __ __ __ _4 {{ r3 rc {{ ra r5 {{ ~2 ~~ ~~ ~~ ~~ ~~ ~~ ~9 ~1 ~9 ~c %% %% %% ~6 %% %% .. .. %b %% r2 "
		"r4 {{ _a __ __ __ __ __ _4 {{ r6 {{ {{ {{ r6 {{ ~2 ~~ -- ~~ ~~ b~ %% %% ~6 %% %% %% %% bb ~~ bb %% .. %7 bb %% ra "
		"r4 {{ {H _a __ __ __ __ _c {{ r6 {{ {{ {{ r6 {{ ~2 -- __ -- ~~ ~~ ~5 %% ~6 %c bb ~3 ~1 ~9 ~8 ~9 ~9 =| ~9 ~9 ~9 ~~ "
		"r4 {{ {{ {H _a _8 _8 _c {G {{ ra r5 {{ rb rc {{ ~2 __ __ __ ~~ ~~ ~~ b~ ~~ bb %3 ~~ bb bb %% %% %% .. .. bb %e r3 "
		"rn r5 {{ {{ {{ {{ {{ {{ bb {{ {{ r6 {{ {{ {{ {L ~~ -- __ -- ~~ ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~5 %% %% %% %5 .. .. .. r2 "
		"rn rn r1 r1 r5 {1 r7 {{ {{ {{ r3 rc b~ ~1 ~1 ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~~ __ __ ~~ b~ %% ~~ ~9 b~ ~9 ~d .. bb .. r2 "
		"rn rn rn rn rc .. ra r1 r1 r1 rc bb ~~ ~~ b~ ~~ ~~ ~8 ~8 ~8 ~8 ~8 ~~ ~~ __ __ ~~ ~~ ~~ %% %% %% %c .. .. .. .. r2 "
		"rn rn rn r4 {{ {2 .. r2 rn r4 bb ~3 ~~ b~ ~~ ~c {G {{ {{ {{ {{ {{ {H ~a ~~ ~~ ~~ ~~ ~~ ~d bb .. .. .. bb %7 .. r2 "
		"rn rn r8 rc {{ {2 .. ra rn rc b~ ~~ ~~ ~~ ~c {G {{ rr rr rr rr rr bb bb {H ~a ~~ -- ~~ %c .. .. .. .. %3 %% %5 r2 "
		"rn rc {{ bb {1 .. .. bb re bb ~~ ~~ ~~ ~~ {G {{ {{ rr .. .. .. rr .. .. bb {H ~~ -- ~4 bb .. .. .. bb %% ~7 %% r2 "
		"r4 {{ {{ {2 .. .. .. .. ~C ~3 ~~ -- -- ~4 {A {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %d .. .. .. %b ~b ~~ ~9 r2 "
		"r4 {{ bb .. .. .. bb ~C ~3 ~~ -- -- -- ~4 bb {{ rr && .. .. .. .. .. .. .. bb ~2 -- ~4 bb .. .. .. bb %% ~e %% r2 "
		"r4 {{ {2 .. .. .. .. ~b ~~ -- -- __ -- ~4 {# {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %5 .. .. .. .. %a %% %c r2 "
		"r4 {{ bb .. .. .. bb ~% ~~ -- __ __ __ ~~ {J {{ {{ rr .. .. .. rr .. .. bb {L ~~ -- ~~ %% bb .. .. .. bb %e .. r2 "
		"rn r5 {{ {2 .. .. .. .. ~a -- -- __ -- -- ~5 {J {{ rr rr .. rr rr bb bb {L ~3 ~~ -- ~~ ~d .. .. .. .. .. .. .. r2 "
		"rn rn r5 bb .. .. .. bb {H ~~ -- __ __ -- ~~ b~ {J {{ {{ {6 {{ {{ {{ {L ~3 ~~ -- -- ~~ bb .. bb .. .. .. bb .. r2 "
		"rn rn r4 {{ {2 .. {4 {{ r7 ~a -- -- __ -- -- ~~ ~~ ~1 ~1 ~1 ~5 {N ~3 ~~ ~~ -- -- -- ~~ b~ rb r5 .. .. .. .. r3 rn "
		"rn rn r4 bb .. .. .. bb r2 rd ~~ -- __ __ -- -- ~~ -- -- -- ~~ ~~ ~~ -- -- -- ~~ ~~ ~~ ~~ b~ ra r9 r5 .. .. r2 rn "
		"rn rn rc {{ {2 .. {4 {{ re ~b ~~ -- __ __ __ -- -- __ __ __ -- -- -- -- -- ~~ ~~ b~ ~~ ~~ ~~ ~~ ~5 r6 .. .. ra rn "
		"rn r4 {{ bb .. .. .. bb {{ {H ~~ -- -- -- __ __ __ __ __ __ __ -- -- ~~ ~~ ~~ b~ bb b~ ~~ ~~ b~ ~~ r6 .. .. .. r2 "
		"rn rc {{ {2 .. .. {4 {{ {{ {{ ~a ~8 ~8 ~~ -- -- -- __ __ __ -- -- ~~ ~~ b~ r3 r1 r1 r1 xx xx ~~ ~~ xx xx .. .. r2 "
		"r4 {{ bb .. .. .. bb {A {{ {{ {{ {{ {{ {H ~a ~8 ~~ -- -- -- ~~ ~8 ~c rb r9 r8 xx xx xx xx __ __ __ __ xx ,, xx xx "
		"r4 {{ {2 .. .. .. .. bb {1 bb {{ {{ {{ {{ {{ {{ {H ~a ~8 ~c {G {{ {{ bb .. bb xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
		"r4 {{ bb .. .. .. .. .. .. .. {1 bb {1 bb {1 bb {1 bb .. bb {1 bb {1 .. .. .. ,, ,, ,, ,, ee ee ee ee ,, ,, ,, xx "
		"rn r5 {{ {2 {8 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
		"rn r4 {{ bb {{ bb {8 bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. {8 bb xx xx xx xx __ __ __ __ xx xx xx xx "
		"rn rn r1 r1 r5 {{ {{ {2 .. .. .. .. .. .. .. .. {8 .. {8 .. .. .. {8 bb {{ {{ xx xx xx __ __ __ __ __ __ xx xx xx "
		"rn rn rn rn rc {{ {{ bb .. .. .. bb {8 bb {8 bb {{ bb {{ bb {8 bb {{ {{ {{ r3 xx xx xx __ __ __ __ __ __ xx xx xx "
		"rn rn r8 rc {{ {{ {{ {2 .. .. .. {4 {{ {{ {{ rb r1 r1 r5 {{ {{ {{ {{ r3 r1 rn xx xx xx xx __ __ __ __ xx xx xx xx "
		"rn r4 {{ {{ {{ {{ {{ bb .. .. .. bb {{ {{ {{ {{ r2 rn rn r1 r1 r1 r1 rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx "
	)
)
 #f      ; wraps
 #t      ; underground
 #f      ; large-scale (wilderness)
 #f      ; tmp combat place
 nil     ; subplaces
 nil     ; neighbors
 (list ;; objects

  (put (mk-portcullis) 31 28)
  (put (mk-portcullis) 32 28)
  (put (mk-door) 26 31)
  (put (mk-door) 35 29)
  (put (mk-windowed-door) 34 31)
  (put (mk-windowed-door) 29 31)
  (put (mk-ladder-down 'p_lost_garrison 11 11) 12 9)

  ;; monster generators
  (put (mk-monman) 0 0)
  (put (guard-pt 'headless) 3 22)
  (put (guard-pt 'headless) 7 22)
  (put (guard-pt 'headless) 5 21)
  (put (guard-pt 'headless) 3 20)
  (put (spawn-pt 'kraken) 5 9)
  (put (spawn-pt 'sea-serpent) 18 11)
  (put (step-pt "A yellow slime oozes up!" (list 'yellow-slime 32 11)) 33 10)
  (put (step-pt "A yellow slime oozes up!" (list 'yellow-slime 26 3)) 26 5)

  ;; put a special step trigger on the footbridge across the cistern
  (put (mk-step-trig 'spawn-kraken-lakes-sea-serpent nil) 31 31)

  )
 (list 'on-entry-to-dungeon-room) ; hooks
 (list ;; edge entrances
  (list north 9 37)
  (list south 29 0)
  )
 )

(mk-place-music p_deepness 'ml-dungeon-adventure)

;; tie rooms together
(kern-place-set-neighbor south p_deepness p_pools)
(kern-place-set-neighbor north p_deepness p_hydra_fen)
