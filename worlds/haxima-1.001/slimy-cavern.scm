;;----------------------------------------------------------------------------
;; Cellar of Abandoned Farm
;;
;; This is the root cellar where Ilya his when the trolls attacked her farm.
;; She left Puska - a quest item - here in her hiding place. Initially it is
;; infested with spider eggs. A fair number of food items may be found here as
;; well, and some other basic materials. A paralyzed troll and a few troll
;; corpses should also decorate the place. The paralyzed troll might as well
;; have a conversation to make things interesting.
;;----------------------------------------------------------------------------

(kern-mk-map 
 'm_slimy_cavern 16 32 pal_expanded
 (list
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r .. .. ..  r "
  " r  r  r  r  r .. ..  & .. ..  r  r .. .. ..  r "
  " r  r  r  r  b .. .. .. .. ..  b  r  r ..  r  r "
  " r  b .. ..  b .. .. .. .. ..  b .. .. ..  r  r "
  " r  b .. .. ..  b  b ..  b  b .. .. .. .. ..  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r "
  " r  b .. .. .. .. .. .. .. .. .. .. .. ..  r  r "
  " r  r  b .. .. .. .. .. .. .. .. ..  r  r  r  r "
  " r  r  r .. .. .. .. .. .. .. .. ..  r .. ..  r "
  " r .. .. .. .. .. ..  r  r .. ..  r  r .. ..  r "
  " r ..  r  r ..  r  r  r  r  r ..  r  r ..  r  r "
  " r %%  r  r .. ..  r .. ..  r ..  r  r ..  r  r "
  "~~  b  b  r  r ..  r .. .. .. .. .. .. ..  r  r "
  " r ~~  b ~~ .. ..  r  r ..  r  r ..  r  r  r  r "
  " r  r %% ~~ ~~ ~~  b  r  r  r  r ..  r  r  r  r "
  " r .. .. .. .. ~~  b  b  r  r .. ..  r  r  r  r "
  " r ..  r .. %%  r  b ~~  b %% .. .. ..  r  r  r "
  " r ..  r  r  r  r  r  b ~~ ~~ %% .. .. %%  r  r "
  " r ..  r  r  r .. ..  r %% ~~  b ~~  b ~~ ~~  r "
  " r .. .. ..  r .. %% %% %% %% %% .. %%  b ~~ ~~ "
  " r  r  r ..  r .. ..  r  r .. .. .. %%  r  r  r "
  " r  r  r ..  r  r  r  r  r .. .. .. .. ..  b  r "
  " r  r .. .. ..  r  r .. .. ..  r .. .. .. ..  r "
  " r .. .. .. .. .. .. ..  r  r  r .. .. .. ..  r "
  " r .. .. .. ..  r  r  r  r  r  r  r .. ..  r  r "
  " r  r .. ..  r  r  r .. .. ..  r  r  r ..  r  r "
  " r  r  r ..  r  r .. .. .. .. ..  r  r ..  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r "
  " r ..  r  r  r  r .. .. .. .. ..  r ..  r ..  r "
  " r ..  r  r  r  r  r .. .. ..  r  r .. .. ..  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "
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
                )
               nil ; hooks
               nil ; edge entrances
               )

;; ----------------------------------------------------------------------------
;; The entry hooks must be kern-loaded from a separate file, since they are
;; read-only and not saved with the session.
;; ----------------------------------------------------------------------------
