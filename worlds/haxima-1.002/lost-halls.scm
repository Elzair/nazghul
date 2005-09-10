;;----------------------------------------------------------------------------
;; Troll Cave
;;
;; Big underground complex; created by some civilized race, now a ruin
;; inhabited by trolls and other monsters.
;;----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map
 'm_lost_halls 64 39 pal_expanded
 (list
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx rr ";
    "rr xx xx rr .. rr rr .. .. rr rr rr rr rr rr rr rr rr rr %% rr rr rr rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr .. .. rr rr rr xx ,L ,A ,R ,D ,E ,R xx rr ";
    "rr xx cc cc .. .. .. .. .. .. [[ @@ ]] rr rr rr rr rr rr .. rr rr rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr .. .. rr rr .. .. rr rr rr rr rr .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr cc cc .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr ";
    "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. .. .. rr rr rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. .. bb rr rr xx ,, ,, ,, ,, ,, ,, xx rr ";
    "rr .. .. .. .. .. .. .. .. .. .. && .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. rr .. .. .. .. .. .. .. rr rr rr rr rr .. rr rr rr rr xx xx xx ,, ,, xx xx xx rr ";
    "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr .. .. rr rr .. .. .. .. .. .. .. rr rr rr ,, ,, ,, ,, ,, rr xx xx xx xx ,, ,, xx xx xx rr ";
    "rr .. .. .. .. .. .. .. .. .. bb .. bb .. .. rr .. .. .. .. xx xx xx xx xx .. .. .. .. .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. .. rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr .. .. .. .. && .. .. .. .. .. .. .. .. rr rr xx xx xx .. .X .. .O .. xx xx xx .. .. rr rr rr rr rr rr rr .. .. .. .. rr rr .. .. rr rr ,, ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr rr .. .. .. .. .. .. .. .. %% %% rr rr xx xx rr rr xx ,, ,, ,, ,, ,, xx ,, ,, xx xx .. rr rr rr rr rr .. ,, ,, ,, ,, rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr rr .. .. .. .. .. .. %% %% %% rr rr xx rr rr ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, xx .. .. .. rr .. ,, ,, rr .. .. .. .. .. .. rr rr .. ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr rr .. .. .. .. .. rr rr ~~ ~~ rr xx rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, rr rr rr .. .. .. .. .. rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr rr rr .. .. rr rr rr rr -- -- xx rr ,, ,, ,, ,, rr rr rr ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx .. rr .. rr rr rr rr .. .. .. .. .. rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr ";
    "rr rr rr rr .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, rr rr rr rr rr ,, ,, ,, rr rr rr ,, ,, ,, rr xx rr .. .. .. .. rr rr .. .. .. .. .. rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx xx rr ";
    "rr rr rr .. .. .. rr rr rr rr rr xx rr ,, ,, ,, ,, rr rr rr rr rr rr ,, ,, rr rr rr rr ,, ,, rr rr xx .. .. .. .. rr rr rr .. rr .. .. rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx rr rr ";
    "rr rr rr .. .. .. .. rr rr rr rr xx rr ,, ,, ,, ,, rr rr rr rr tt tt ,, ,, rr rr rr rr ,, ,, rr rr xx rr .. .. .. rr rr rr rr rr .. .. rr rr cc cc cc cc rr rr xx ,, ,, ,, ,, ,, ,, xx xx rr rr ";
    "rr rr rr .. .. .. .. rr rr rr rr xx rr rr rr && rr rr rr rr tt .. .. .. ,, rr rr rr rr ,, ,, ,, rr xx rr rr .. .. .. rr rr rr rr .. .. ,, cc cc cc cc cc cc rr xx ,, ,, ,, ,, ,, ,, xx xx rr rr ";
    "rr rr rr .. .. .. rr rr rr rr xx rr rr rr rr rr rr rr rr tt xx xx xx xx xx .. rr rr rr rr ,, ,, ,, ,, xx rr .. .. .. .. rr rr rr .. .. ,, cc cc 00 00 cc cc rr xx ,, ,, ,, ,, ,, ,, xx xx rr rr ";
    "rr rr rr .. .. .. rr rr rr rr xx rr rr ,, ,, ,, rr rr rr rr xx ,, ,, ,, xx bb rr rr rr ,, ,, ,, ,, ,, xx .. .. .. .. .. rr rr rr .. .. pp cc cc 00 00 cc cc rr xx xx xx ,, ,, xx xx xx xx rr rr ";
    "rr rr rr rr .. .. rr rr rr rr xx rr ,, ,, rr ,, rr rr rr rr xx ,, .. ,, xx .. rr rr rr ,, ,, ,, ,, ,, xx .. .. .. .. .. bb rr && .. .. ,, cc cc 00 00 cc cc rr xx xx xx ,, ,, xx xx xx xx rr rr ";
    "rr rr rr rr .. .. rr rr rr rr xx ,, .. rr ,, .. ,, rr rr ,, xx rr ,, ,, rr rr rr ,, ,, ,, ,, ,, .. ,, xx .. .. .. .. .. .. rr && .. .. ,, cc cc 00 00 cc cc rr rr tt tt .. .. .. tt tt tt rr rr ";
    "rr rr rr rr .. .. .. rr rr .. .. ,, .. ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, ,, .. bb .. xx .. .. .. .. && .. rr && .. .. ,, cc cc 00 00 cc cc rr rr tt .. bb .. .. .. bb tt tt rr ";
    "rr rr .. .. .. .. .. .. .. .. xx ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr ,, ,, .. xx .. .. .. .. .. .. rr rr rr .. .. pp cc cc 00 00 cc cc rr tt tt .. .. tt bb .. .. tt tt rr ";
    "rr rr .. .. .. .. .. .. .. .. .. xx rr ,, ,, rr rr rr rr ,, rr rr rr ,, ,, ,, rr rr rr rr rr rr ,, xx .. .. .. bb .. .. rr rr rr .. .. ,, cc cc cc cc cc cc rr tt .. bb .. .. .. .. tt tt rr rr ";
    "rr .. .. .. bb .. .. .. .. .. .. xx rr rr rr rr rr rr ,, ,, ,, ,, rr rr rr rr rr rr rr rr rr rr rr xx .. .. .. .. rr rr rr rr rr .. .. ,, cc cc cc cc cc rr rr rr tt tt .. .. bb tt tt rr rr rr ";
    "rr .. .. .. .. .. .. .. bb .. .. rr xx rr rr rr bb ,, ,, ,, .. ,, rr rr rr rr rr ,, ,, ,, rr xx xx rr .. rr rr rr rr rr rr rr rr .. .. rr rr bb cc cc rr rr rr rr rr rr tt .. .. rr rr rr rr rr ";
    "rr rr .. .. .. .. .. .. .. .. .. rr xx rr rr rr ,, ,, ,, .. bb .. ,, rr rr rr ,, ,, ,, ,, ,, rr xx .. .. rr rr rr rr rr rr .. rr .. .. rr rr rr rr rr rr rr rr rr rr rr .. bb .. rr rr rr rr rr ";
    "rr rr .. .. .. .. .. .. .. .. rr rr rr xx rr rr ,, ,, ,, ,, .. ,, ,, rr rr ,, ,, ,, ,, ,, ,, xx .. .. .. .. rr rr .. .. .. .. .. .. .. .. .. .. .. bb rr rr rr rr rr rr .. .. .. rr rr rr rr rr ";
    "rr rr rr .. .. .. .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, rr rr rr ,, ,, && ,, ,, ,, .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. .. bb rr rr ,R rr .. .. .. .. rr ;S rr rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx rr ,, rr rr rr rr rr rr rr ,, ,, .. .. .. rr rr .. .. .. .. .. .. rr rr rr rr rr bb .. .. && .. .. .. rr rr bb tt .. bb .. .. %% bb rr rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx .. xx ,, ,, rr rr rr xx xx .. .. .. rr rr rr rr .. rr rr rr rr rr .. .. .. rr rr .. .. .. .. .. .. rr rr .. .. .. .. bb .. %% %% bb rr rr ";
    "rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr .. .. .. .. .. .. xx xx xx rr rr .. rr rr rr rr rr .. .. rr rr rr rr .. .. .. && .. rr .. .. .. .. .. rr rr bb .. .. bb .. .. %% %% tt %% ;T rr ";
    "rr rr rr .. .. .. .. .. .. .. rr rr .. rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr .. .. rr rr rr rr .. .. .. .. .. rr xx xx ,, xx xx rr rr bb .. bb tt tt .. __ __ __ __ rr rr ";
    "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. .. .. .. rr rr rr rr rr rr rr .. .. rr .. .. .. .. .. .. .. .. .. .. rr rr xx ,, ,, ,, xx rr ;A %% .. .. .. tt .. .. __ __ __ __ rr ";
    "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr .. .. .. rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr xx ,, ,, ,, xx rr rr rr bb .. bb .. .. .. __ .. .. __ rr ";
    "rr rr rr .. .. .. .. .. rr rr rr .. .. rr rr rr rr rr rr rr .. rr rr rr rr rr rr .. .. .. .. .. .. .. .. rr rr .. rr rr rr rr rr rr rr bb ,, ,, ,, xx rr rr rr rr tt %% __ __ bb __ __ __ __ rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr .. rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr .. .. .. .. rr rr rr rr xx bb xx xx xx rr rr rr ,Q %% __ __ __ __ __ __ bb rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr x! rr rr rr rr rr rr rr rr .. .. rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr bb __ __ __ __ bb ,P rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ";
    ))

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_lost_halls      ; tag
 "Lost Halls"       ; name
 nil                ; sprite
 m_lost_halls       ; map
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil                ; subplaces
 nil                ; neighbors
 
 ;; objects
 (list

  (list (mk-mongen2 990 5 'is-troll? 'mk-troll nil) 11 6)
  (list (mk-mongen2 990 5 'is-troll? 'mk-troll nil) 28 28)
  (list (mk-mongen2 990 5 'is-troll? 'mk-troll nil) 38 21)
  (list (mk-mongen2 990 5 'is-troll? 'mk-troll nil) 41 32)
  (list (mk-mongen2 990 5 'is-troll? 'mk-troll nil) 48 30)
  (list (mk-mongen2 990 3 'is-queen-spider? 'mk-queen-spider nil) 15 28)
  (list (mk-step-gen 0 3 'is-green-slime? 'mk-green-slime 
                     (list "A slime emerges from the ooze!") 
                     (list 'p_lost_halls 11 9)) 10 8)
  (list (mk-step-gen 0 3 'is-green-slime? 'mk-green-slime 
                     (list "A slime emerges from the ooze!") 
                     (list 'p_lost_halls 60 30)) 57 31)
  
  ;; terrain features
  (list (mk-ladder-up 'p_shard 46 12) 1 4)
  (list (mk-bridge east) 59 34)
  
  )
 nil ; hooks
 nil ; edge entrances
 )
