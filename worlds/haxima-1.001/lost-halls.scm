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
    "rr rr rr rr .. .. rr rr rr rr xx ,, .. rr ,, .. ,, rr rr ,, xx rr ,, ,, rr rr rr ,, ,, ,, ,, ,, .. ,, xx .. .. .. .. .. .. rr && .. .. ,, cc cc 00 00 cc cc rr rr dd tt .. .. .. tt tt tt rr rr ";
    "rr rr rr rr .. .. .. rr rr .. .. ,, .. ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, ,, .. bb .. xx .. .. .. .. && .. rr && .. .. ,, cc cc 00 00 cc cc rr rr tt .. bb .. .. .. bb tt dd rr ";
    "rr rr .. .. .. .. .. .. .. .. xx ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr ,, ,, .. xx .. .. .. .. .. .. rr rr rr .. .. pp cc cc 00 00 cc cc rr tt dd .. .. tt bb .. .. tt tt rr ";
    "rr rr .. .. .. .. .. .. .. .. .. xx rr ,, ,, rr rr rr rr ,, rr rr rr ,, ,, ,, rr rr rr rr rr rr ,, xx .. .. .. bb .. .. rr rr rr .. .. ,, cc cc cc cc cc cc rr dd .. bb .. .. .. .. dd tt rr rr ";
    "rr .. .. .. bb .. .. .. .. .. .. xx rr rr rr rr rr rr ,, ,, ,, ,, rr rr rr rr rr rr rr rr rr rr rr xx .. .. .. .. rr rr rr rr rr .. .. ,, cc cc cc cc cc rr rr rr tt tt .. .. bb tt tt rr rr rr ";
    "rr .. .. .. .. .. .. .. bb .. .. rr xx rr rr rr bb ,, ,, ,, .. ,, rr rr rr rr rr ,, ,, ,, rr xx xx rr .. rr rr rr rr rr rr rr rr .. .. rr rr bb cc cc rr rr rr rr rr rr tt .. .. rr rr rr rr rr ";
    "rr rr .. .. .. .. .. .. .. .. .. rr xx rr rr rr ,, ,, ,, .. bb .. ,, rr rr rr ,, ,, ,, ,, ,, rr xx .. .. rr rr rr rr rr rr .. rr .. .. rr rr rr rr rr rr rr rr rr rr rr .. bb .. rr rr rr rr rr ";
    "rr rr .. .. .. .. .. .. .. .. rr rr rr xx rr rr ,, ,, ,, ,, .. ,, ,, rr rr ,, ,, ,, ,, ,, ,, xx .. .. .. .. rr rr .. .. .. .. .. .. .. .. .. .. .. bb rr rr rr rr rr rr .. .. .. rr rr rr rr rr ";
    "rr rr rr .. .. .. .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, rr rr rr ,, ,, && ,, ,, ,, .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. .. bb rr rr ,R rr .. .. .. .. rr ;S rr rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx rr ,, rr rr rr rr rr rr rr ,, ,, .. .. .. rr rr .. .. .. .. .. .. rr rr rr rr rr bb .. .. && .. .. .. rr rr bb tt .. bb .. .. %% bb rr rr rr ";
    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx .. xx ,, ,, rr rr rr xx xx .. .. .. rr rr rr rr .. rr rr rr rr rr .. .. .. rr rr .. .. .. .. .. .. rr rr .. .. .. .. bb .. %% %% bb rr rr ";
    "rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr .. .. .. .. .. .. xx xx xx rr rr .. rr rr rr rr rr .. .. rr rr rr rr .. .. .. && .. rr .. .. .. .. .. rr rr bb .. .. bb .. .. %% %% dd %% ;T rr ";
    "rr rr rr .. .. .. .. .. .. .. rr rr .. rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr .. .. rr rr rr rr .. .. .. .. .. rr xx xx ,, xx xx rr rr bb .. bb tt tt .. __ __ __ __ rr rr ";
    "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr .. .. .. .. .. .. rr rr rr rr rr rr rr .. .. rr .. .. .. .. .. .. .. .. .. .. rr rr xx ,, ,, ,, xx rr ;A %% .. .. .. dd .. .. __ __ __ __ rr ";
    "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr .. .. .. rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr xx ,, ,, ,, xx rr rr rr bb .. bb .. .. .. __ .. .. __ rr ";
    "rr rr rr .. .. .. .. .. rr rr rr .. .. rr rr rr rr rr rr rr .. rr rr rr rr rr rr .. .. .. .. .. .. .. .. rr rr .. rr rr rr rr rr rr rr bb ,, ,, ,, xx rr rr rr rr dd %% __ __ bb __ __ __ __ rr ";
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
  
  ;; terrain features
  (list (mk-ladder-up 'p_shard 46 12) 1 4)
  (list (mk-bridge east) 59 34)
  
  )
 nil ; hooks
 nil ; edge entrances
 )
