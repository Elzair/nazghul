
(mk-dungeon-room
 'p_hydra_fen "Hydra Fen"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr "
  "rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr {{ {{ {{ {{ {{ %% %% %% {{ {{ {{ {{ {{ rr rr rr "
  "rr rr {{ {{ {{ {{ %% %% %% %% %% %% %% {{ {{ {{ {{ rr rr "
  "rr {{ {{ {{ {{ %% %% %% %% %% %% %% %% %% {{ {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% %% ee ee ee %% %% %% %% {{ {{ {{ rr "
  "rr {{ {{ {{ %% %% %% oo ee ee ee oo %% %% {{ {{ {{ {{ rr "
  "rr rr {{ {{ {{ %% %% %% ee ee ee %% %% %% {{ {{ {{ rr rr "
  "rr rr rr {{ {{ {{ %% oo ee ee ee oo %% {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ {{ ee ee ee {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ .. .. .. {{ {{ {{ rr rr rr rr rr "
  "rr rr rr rr rr rr {{ {{ .. .. .. {{ {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'hydra) 9 9)
 )

(mk-dungeon-room
 'p_pools "Pools"
 (list
  "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
  "rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
  "rr -- -- rr rr .. .. .. .. .. .. .. %% %% %% %% .. .. rr "
  "rr -- -- rr rr .. .. .. .. .. .. %% %% ~~ ~~ %% %% .. rr "
  "rr -- -- ~~ %% .. .. .. .. .. .. .. ~~ -- -- ~~ %% %% rr "
  "rr rr ~~ ~~ %% .. .. .. .. .. .. ~~ -- -- -- -- ~~ %% rr "
  "rr rr %% %% %% .. .. .. .. .. .. ~~ -- -- -- -- ~~ %% rr "
  "rr rr .. .. .. .. .. xx w+ d, ,, xx ~~ -- -- -- ~~ %% rr "
  "rr .. .. .. .. .. .. rr ,, ,, ,, xx ~~ -- -- ~~ %% %% rr "
  "rr .. %% %% %% %% .. w+ .. ,, ,, rr .. ~~ ~~ %% %% rr rr "
  "rr %% %% ~~ ~~ %% .. ,, ,, ,, ,, ,, .. %% %% %% .. .. rr "
  "rr %% ~~ -- ~~ %% .. rr xx .. ,, rr .. %% ~~ %% %% .. rr "
  "rr %% ~~ ~~ %% %% .. .. .. .. .. .. %% ~~ -- ~~ %% .. rr "
  "rr %% %% %% %% .. .. .. .. .. %% %% ~~ -- -- ~~ %% .. rr "
  "rr rr .. rr rr .. .. .. .. .. %% ~~ -- -- -- ~~ %% .. rr "
  "rr rr rr rr rr .. .. .. .. .. %% ~~ -- -- ~~ %% %% .. rr "
  "rr rr rr rr .. .. .. .. .. .. %% %% ~~ ~~ %% %% .. rr rr "
  "rr rr rr rr rr .. .. rr rr .. .. %% %% %% %% .. .. rr rr "
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
   "rr rr __ __ __ __ {{ {{ {{ {{ {{ {{ {{ {{ rr {{ rr rr rr bb %% %% %% ~~ %% %% %% .. .. .. .. .. %% %% %% %% %% rr "
   "rr __ __ __ __ __ =| __ __ __ rr rr rr {{ {{ {{ {{ {{ {{ {{ %% bb %% ~~ %% %% %% bb .. .. .. bb %% %% %% %% %% rr "
   "rr __ __ __ rr __ {{ __ __ __ __ __ rr rr rr {{ rr rr rr {{ .. == .. bb %% bb %% %% .. .. .. %% %% bb %% bb %% rr "
   "rr {{ {{ rr rr rr {{ __ __ __ __ __ ~~ ~~ ~~ ~~ ~~ rr rr ~~ .. ~~ .. .. .. .. ~~ %% .. .. .. %% %% %% %% %% %% rr "
   "rr {{ {{ {{ rr {{ {{ __ __ __ __ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ ~~ ~~ ~~ .. .. == .. .. .. .. .. .. .. %% bb %% rr "
   "rr {{ rr {{ {{ {{ __ __ __ __ {{ {{ {{ {{ {{ -- ~~ ~~ ~~ ~~ %% bb ~~ bb %% bb ~~ %% .. .. .. %% %% .. .. %% %% rr "
   "rr {{ {{ __ __ __ __ __ __ {{ {{ rr rr rr {{ {{ ~~ ~~ ~~ ~~ ~~ ~~ ~~ %% %% %% ~~ %% %% .. ~~ ~~ ~~ ~~ .. bb %% rr "
   "rr {{ __ __ __ __ __ __ __ {{ rr rr {{ rr rr {{ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ %% %% %% ~~ %% %% .. .. %% %% rr "
   "rr {{ __ __ __ __ __ __ __ {{ rr {{ {{ {{ rr {{ ~~ ~~ -- ~~ ~~ bb %% %% ~~ %% %% %% %% bb ~~ bb %% .. %% bb %% rr "
   "rr {{ {{ __ __ __ __ __ __ {{ rr {{ {{ {{ rr {{ ~~ -- __ -- ~~ ~~ ~~ %% ~~ %% bb ~~ ~~ ~~ ~~ ~~ ~~ =| ~~ ~~ ~~ ~~ "
   "rr {{ {{ {{ __ __ __ __ {{ {{ rr rr {{ rr rr {{ ~~ __ __ __ ~~ ~~ ~~ bb ~~ bb %% ~~ bb bb %% %% %% .. .. bb %% rr "
   "rr rr {{ {{ {{ {{ {{ {{ bb {{ {{ rr {{ {{ {{ {{ ~~ -- __ -- ~~ ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~~ %% %% %% %% .. .. .. rr "
   "rr rr rr rr rr .. rr {{ {{ {{ rr rr bb ~~ ~~ ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~~ __ __ ~~ bb %% ~~ ~~ bb ~~ ~~ .. bb .. rr "
   "rr rr rr rr rr .. rr rr rr rr rr bb ~~ ~~ bb ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ __ __ ~~ ~~ ~~ %% %% %% %% .. .. .. .. rr "
   "rr rr rr rr {{ .. .. rr rr rr bb ~~ ~~ bb ~~ ~~ {{ {{ {{ {{ {{ {{ {{ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb .. .. .. bb %% .. rr "
   "rr rr rr rr {{ .. .. rr rr rr bb ~~ ~~ ~~ ~~ {{ {{ rr rr rr rr rr bb bb {{ ~~ ~~ -- ~~ %% .. .. .. .. %% %% %% rr "
   "rr rr {{ bb .. .. .. bb rr bb ~~ ~~ ~~ ~~ {{ {{ {{ rr .. .. .. rr .. .. bb {{ ~~ -- ~~ bb .. .. .. bb %% ~~ %% rr "
   "rr {{ {{ .. .. .. .. .. .. ~~ ~~ -- -- ~~ {{ {{ rr rr .. .. .. rr .. .. .. bb ~~ -- ~~ %% .. .. .. %% ~~ ~~ ~~ rr "
   "rr {{ bb .. .. .. bb .. ~~ ~~ -- -- -- ~~ bb {{ rr && .. .. .. .. .. .. .. bb ~~ -- ~~ bb .. .. .. bb %% ~~ %% rr "
   "rr {{ .. .. .. .. .. ~~ ~~ -- -- __ -- ~~ {{ {{ rr rr .. .. .. rr .. .. .. bb ~~ -- ~~ %% .. .. .. .. %% %% %% rr "
   "rr {{ bb .. .. .. bb .. ~~ -- __ __ __ ~~ {{ {{ {{ rr .. .. .. rr .. .. bb {{ ~~ -- ~~ %% bb .. .. .. bb %% .. rr "
   "rr rr {{ .. .. .. .. .. ~~ -- -- __ -- -- ~~ {{ {{ rr rr .. rr rr bb bb {{ ~~ ~~ -- ~~ ~~ .. .. .. .. .. .. .. rr "
   "rr rr rr bb .. .. .. bb {{ ~~ -- __ __ -- ~~ bb {{ {{ {{ .. {{ {{ {{ {{ ~~ ~~ -- -- ~~ bb .. bb .. .. .. bb .. rr "
   "rr rr rr {{ .. .. .. {{ rr ~~ -- -- __ -- -- ~~ ~~ ~~ ~~ ~~ ~~ {{ ~~ ~~ ~~ -- -- -- ~~ bb rr rr .. .. .. .. rr rr "
   "rr rr rr bb .. .. .. bb rr rr ~~ -- __ __ -- -- ~~ -- -- -- ~~ ~~ ~~ -- -- -- ~~ ~~ ~~ ~~ bb rr rr rr .. .. rr rr "
   "rr rr rr {{ .. .. .. {{ rr ~~ ~~ -- __ __ __ -- -- __ __ __ -- -- -- -- -- ~~ ~~ bb ~~ ~~ ~~ ~~ ~~ rr .. .. rr rr "
   "rr rr {{ bb .. .. .. bb {{ {{ ~~ -- -- -- __ __ __ __ __ __ __ -- -- ~~ ~~ ~~ bb bb bb ~~ ~~ bb ~~ rr .. .. .. rr "
   "rr rr {{ .. .. .. .. {{ {{ {{ ~~ ~~ ~~ ~~ -- -- -- __ __ __ -- -- ~~ ~~ bb rr rr rr rr xx xx ~~ ~~ xx xx .. .. rr "
   "rr {{ bb .. .. .. bb {{ {{ {{ {{ {{ {{ {{ ~~ ~~ ~~ -- -- -- ~~ ~~ ~~ rr rr rr xx xx xx xx __ __ __ __ xx ,, xx xx "
   "rr {{ .. .. .. .. .. bb .. bb {{ {{ {{ {{ {{ {{ {{ ~~ ~~ ~~ {{ {{ {{ bb .. bb xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
   "rr {{ bb .. .. .. .. .. .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. .. .. .. ,, ,, ,, ,, ee ee ee ee ,, ,, ,, xx "
   "rr rr {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
   "rr rr {{ bb {{ bb .. bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb xx xx xx xx __ __ __ __ xx xx xx xx "
   "rr rr rr rr rr {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb {{ {{ xx xx xx __ __ __ __ __ __ xx xx xx "
   "rr rr rr rr rr {{ {{ bb .. .. .. bb .. bb .. bb {{ bb {{ bb .. bb {{ {{ {{ rr xx xx xx __ __ __ __ __ __ xx xx xx "
   "rr rr rr rr {{ {{ {{ .. .. .. .. .. {{ {{ {{ rr rr rr rr {{ {{ {{ {{ rr rr rr xx xx xx xx __ __ __ __ xx xx xx xx "
   "rr rr {{ {{ {{ {{ {{ bb .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx "
   ))
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
