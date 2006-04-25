
(mk-dungeon-room
 'p_hydra_fen "Hydra Fen"
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr "
		"rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr "
		"rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr "
		"rr rr rr {{ {{ {{ {{ {C %3 %% %5 {A {{ {{ {{ {{ rr rr rr "
		"rr rr {{ {{ {{ {C %3 %% %% %% %% %% %5 {A {{ {{ {{ rr rr "
		"rr {{ {{ {{ {C %3 %% %% %% %% %% %% %% %5 {A {{ {{ {{ rr "
		"rr {{ {{ {{ %3 %% %% %% %% %% %% %% %% %% %5 {{ {{ {{ rr "
		"rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
		"rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
		"rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
		"rr {{ {{ {{ %% %% %% %% ee ee ee %% %% %% %c {{ {{ {{ rr "
		"rr {{ {{ {{ %a %% %% oo ee ee ee oo %% %% {# {{ {{ {{ rr "
		"rr rr {{ {{ {% %a %% %% ee ee ee %% %% %c {{ {{ {{ rr rr "
		"rr rr rr {{ {{ {% %e oo ee ee ee oo %e {# {{ {{ rr rr rr "
		"rr rr rr rr {{ {{ {{ {{ ee ee ee {{ {{ {{ {{ rr rr rr rr "
		"rr rr rr rr rr {{ {{ {{ {2 .. {4 {{ {{ {{ rr rr rr rr rr "
		"rr rr rr rr rr rr {{ {{ {2 .. {4 {{ {{ rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
	)
 (put (spawn-pt 'hydra) 9 9)
 )

(mk-dungeon-room
 'p_pools "Pools"
	(list
		"rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
		"rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
		"rr -- -- rr rr .. .. .. .. .. .. .. %3 %% %% %5 .. .. rr "
		"rr -- -- rr rr .. .. .. .. .. .. %b %% ~3 ~5 %% %5 .. rr "
		"rr -- -- ~~ %5 .. .. .. .. .. .. ~C ~3 -- -- ~5 %% %5 rr "
		"rr rr ~~ ~c %% .. .. .. .. .. .. ~3 -- -- -- -- ~5 %% rr "
		"rr rr %a %% %c .. .. .. .. .. .. ~~ -- -- -- -- ~4 %% rr "
		"rr rr .. .. .. .. .. xx w+ d, ,, xx ~~ -- -- -- ~c %% rr "
		"rr .. .. .. .. .. .. rr ,, ,, ,, xx ~~ -- -- ~c %% %c rr "
		"rr .. %3 %% %% %5 .. w+ .. ,, ,, rr ~% ~a ~c %% %c rr rr "
		"rr %3 %% ~3 ~5 %% .. ,, ,, ,, ,, ,, .. %% %% %% .. .. rr "
		"rr %% ~3 -- ~c %% .. rr xx .. ,, rr .. %% ~7 %% %5 .. rr "
		"rr %% ~a ~c %% %c .. .. .. .. .. .. %3 ~3 -- ~5 %% .. rr "
		"rr %a %% %% %c .. .. .. .. .. %3 %% ~3 -- -- ~4 %% .. rr "
		"rr rr .. rr rr .. .. .. .. .. %% ~3 -- -- -- ~c %% .. rr "
		"rr rr rr rr rr .. .. .. .. .. %% ~a -- -- ~c %% %c .. rr "
		"rr rr rr rr .. .. .. .. .. .. %a %% ~a ~c %% %c .. rr rr "
		"rr rr rr rr rr .. .. rr rr .. .. %a %% %% %c .. .. rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
 (put (mk-door) 9 7)
 (put (mk-ladder-up 'p_great_hall 9 6) 9 9)
 (put (spawn-pt 'yellow-slime) 11  3)
 (put (spawn-pt 'yellow-slime)  4  6)
 (put (spawn-pt 'yellow-slime) 10 13)
 
 )

(kern-mk-place 
 'p_deepness
 "The Deepness"
 nil     ; sprite
 (kern-mk-map 
  nil 38 38 pal_expanded 
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr bb .. .. .. bb rr rr rr rr rr rr "
		"rr rr _3 _1 _1 _5 {{ {{ {{ {{ {{ {{ {{ {{ rr {{ rr rr rr bb %3 %% %% ~7 %% %% %5 .. .. .. .. .. %3 %% %% %% %5 rr "
		"rr _3 __ __ __ __ =| _1 _1 _5 rr rr rr {{ {{ {{ {{ {{ {{ {% %e bb %a ~e %% %% %% bb .. .. .. bb %% %% %% %% %% rr "
		"rr _a _8 _c rr _e {{ __ __ __ __ _5 rr rr rr {{ rr rr rr {{ {2 == .. bb %e bb %% %5 .. .. .. %3 %% bb %% bb %% rr "
		"rr {{ {{ rr rr rr {{ _2 __ __ __ __ ~~ ~1 ~1 ~1 ~5 rr rr ~7 .. ~6 .. .. .. .. ~7 %c .. .. .. %a %% %% %% %% %% rr "
		"rr {{ {{ {{ rr {{ {L __ __ __ __ ~8 ~8 ~8 ~~ ~~ ~~ ~~ ~~ b~ ~9 ~~ ~~ ~d .. .. == .. .. .. .. .. .. .. %e bb %% rr "
		"rr {{ rr {{ {{ {L _3 __ __ _c {G {{ {{ {{ {H -a ~~ ~~ ~~ ~~ %% b~ ~~ bb %7 bb ~6 %5 .. .. .. %3 %5 .. .. %b %% rr "
		"rr {{ {L _3 _1 __ __ __ __ {G {{ rr rr rr {{ {H ~~ ~~ ~~ ~~ ~~ ~~ ~~ %% %% %% ~6 %% %5 .. ~3 ~9 ~9 ~d .. bb %% rr "
		"rr {{ _3 __ __ __ __ __ _4 {{ rr rr {{ rr rr {{ ~2 ~~ ~~ ~~ ~~ ~~ ~~ ~9 ~1 ~9 ~c %% %% %% ~6 %% %% .. .. %b %% rr "
		"rr {{ _a __ __ __ __ __ _4 {{ rr {{ {{ {{ rr {{ ~2 ~~ -- ~~ ~~ b~ %% %% ~6 %% %% %% %% bb ~~ bb %% .. %7 bb %% rr "
		"rr {{ {H _a __ __ __ __ _c {{ rr {{ {{ {{ rr {{ ~2 -- __ -- ~~ ~~ ~5 %% ~6 %c bb ~3 ~1 ~9 ~8 ~9 ~9 =| ~9 ~9 ~9 ~~ "
		"rr {{ {{ {H _a _8 _8 _c {G {{ rr rr {{ rr rr {{ ~2 __ __ __ ~~ ~~ ~~ b~ ~~ bb %3 ~~ bb bb %% %% %% .. .. bb %e rr "
		"rr rr {{ {{ {{ {{ {{ {{ bb {{ {{ rr {{ {{ {{ {L ~~ -- __ -- ~~ ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~5 %% %% %% %5 .. .. .. rr "
		"rr rr rr rr rr {1 rr {{ {{ {{ rr rr b~ ~1 ~1 ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~~ __ __ ~~ b~ %% ~~ ~9 b~ ~9 ~d .. bb .. rr "
		"rr rr rr rr rr .. rr rr rr rr rr bb ~~ ~~ b~ ~~ ~~ ~8 ~8 ~8 ~8 ~8 ~~ ~~ __ __ ~~ ~~ ~~ %% %% %% %c .. .. .. .. rr "
		"rr rr rr rr {{ {2 .. rr rr rr bb ~3 ~~ b~ ~~ ~c {G {{ {{ {{ {{ {{ {H ~a ~~ ~~ ~~ ~~ ~~ ~d bb .. .. .. bb %7 .. rr "
		"rr rr rr rr {{ {2 .. rr rr rr b~ ~~ ~~ ~~ ~c {G {{ rr rr rr rr rr bb bb {H ~a ~~ -- ~~ %c .. .. .. .. %3 %% %5 rr "
		"rr rr {{ bb {1 .. .. bb rr bb ~~ ~~ ~~ ~~ {G {{ {{ rr .. .. .. rr .. .. bb {H ~~ -- ~4 bb .. .. .. bb %% ~7 %% rr "
		"rr {{ {{ {2 .. .. .. .. ~C ~3 ~~ -- -- ~4 {A {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %d .. .. .. %b ~b ~~ ~d rr "
		"rr {{ bb .. .. .. bb ~C ~3 ~~ -- -- -- ~4 bb {{ rr && .. .. .. .. .. .. .. bb ~2 -- ~4 bb .. .. .. bb %% ~e %% rr "
		"rr {{ {2 .. .. .. .. ~b ~~ -- -- __ -- ~4 {# {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %5 .. .. .. .. %a %% %c rr "
		"rr {{ bb .. .. .. bb ~% ~~ -- __ __ __ ~~ {J {{ {{ rr .. .. .. rr .. .. bb {L ~~ -- ~~ %% bb .. .. .. bb %e .. rr "
		"rr rr {{ {2 .. .. .. .. ~a -- -- __ -- -- ~5 {J {{ rr rr .. rr rr bb bb {L ~3 ~~ -- ~~ ~d .. .. .. .. .. .. .. rr "
		"rr rr rr bb .. .. .. bb {H ~~ -- __ __ -- ~~ b~ {J {{ {{ {6 {{ {{ {{ {L ~3 ~~ -- -- ~~ bb .. bb .. .. .. bb .. rr "
		"rr rr rr {{ {2 .. {4 {{ rr ~a -- -- __ -- -- ~~ ~~ ~1 ~1 ~1 ~5 {N ~3 ~~ ~~ -- -- -- ~~ b~ rr rr .. .. .. .. rr rr "
		"rr rr rr bb .. .. .. bb rr rr ~~ -- __ __ -- -- ~~ -- -- -- ~~ ~~ ~~ -- -- -- ~~ ~~ ~~ ~~ b~ rr rr rr .. .. rr rr "
		"rr rr rr {{ {2 .. {4 {{ rr ~b ~~ -- __ __ __ -- -- __ __ __ -- -- -- -- -- ~~ ~~ b~ ~~ ~~ ~~ ~~ ~5 rr .. .. rr rr "
		"rr rr {{ bb .. .. .. bb {{ {H ~~ -- -- -- __ __ __ __ __ __ __ -- -- ~~ ~~ ~~ b~ bb b~ ~~ ~~ b~ ~~ rr .. .. .. rr "
		"rr rr {{ {2 .. .. {4 {{ {{ {{ ~a ~8 ~8 ~~ -- -- -- __ __ __ -- -- ~~ ~~ b~ rr rr rr rr xx xx ~~ ~~ xx xx .. .. rr "
		"rr {{ bb .. .. .. bb {A {{ {{ {{ {{ {{ {H ~a ~8 ~~ -- -- -- ~~ ~8 ~c rr rr rr xx xx xx xx __ __ __ __ xx ,, xx xx "
		"rr {{ {2 .. .. .. .. bb {1 bb {{ {{ {{ {{ {{ {{ {H ~a ~8 ~c {G {{ {{ bb .. bb xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
		"rr {{ bb .. .. .. .. .. .. .. {1 bb {1 bb {1 bb {1 bb .. bb {1 bb {1 .. .. .. ,, ,, ,, ,, ee ee ee ee ,, ,, ,, xx "
		"rr rr {{ {2 {8 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
		"rr rr {{ bb {{ bb {8 bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. {8 bb xx xx xx xx __ __ __ __ xx xx xx xx "
		"rr rr rr rr rr {{ {{ {2 .. .. .. .. .. .. .. .. {8 .. {8 .. .. .. {8 bb {{ {{ xx xx xx __ __ __ __ __ __ xx xx xx "
		"rr rr rr rr rr {{ {{ bb .. .. .. bb {8 bb {8 bb {{ bb {{ bb {8 bb {{ {{ {{ rr xx xx xx __ __ __ __ __ __ xx xx xx "
		"rr rr rr rr {{ {{ {{ {2 .. .. .. {4 {{ {{ {{ rr rr rr rr {{ {{ {{ {{ rr rr rr xx xx xx xx __ __ __ __ xx xx xx xx "
		"rr rr {{ {{ {{ {{ {{ bb .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx "
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

;; tie rooms together
(kern-place-set-neighbor south p_deepness p_pools)
(kern-place-set-neighbor north p_deepness p_hydra_fen)
