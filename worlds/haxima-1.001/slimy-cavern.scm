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

                ;; terrain features
                (list (mk-ladder-up 'p_moongate_clearing 20 1) 8 30)
                (list (mk-bridge 'north) 4 15)
                (list (mk-bridge 'north) 11 19)                

                ;; doors/mechanisms
                (list (mk-locked-door) 13 3)

                ;; monster generators
                (list (mk-slime-generator 9 17) 11 17)

                ;; existing npc's
                (list (mk-bandit) 5 4)
                (list (mk-bandit) 9 4)
                (list (mk-bandit) 9 2)                

                )
               (list 'slimy-cavern-entry) ; hooks
               nil ; edge entrances
               )
