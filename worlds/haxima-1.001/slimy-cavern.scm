;;----------------------------------------------------------------------------
;; Slimy Cavern
;;
;;----------------------------------------------------------------------------


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
  "rr .. rr rr .. rr rr rr rr rr .. rr rr .. rr rr "
  "rr %% rr rr .. .. rr .. .. rr .. rr rr .. rr rr "
  "~~ bb bb rr rr .. rr .. .. .. .. .. .. .. rr rr "
  "rr ~~ bb ~~ .. .. rr rr .. rr rr .. rr rr rr rr "
  "rr rr %% ~~ ~~ ~~ bb rr rr rr rr .. rr rr rr rr "
  "rr .. .. .. .. ~~ bb bb rr rr .. .. rr rr rr rr "
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
                (list (mk-ladder-up 'p_moongate_clearing 20 1) 8 30)
                (list (mk-bridge 'north) 4 15)
                (list (mk-bridge 'north) 11 19)                
                (list (mk-slime-generator) 4 14)
                (list (mk-slime-generator) 11 18)
                )
               (list 'slimy-cavern-entry) ; hooks
               nil ; edge entrances
               )
