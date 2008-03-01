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
 'm_abandoned_cellar 32 32 pal_expanded
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr {{ {{ {{ rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. {{ {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. .. .. .. .. .. .. .. {{ {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. .. .. .. .. .. .. .. .. .. {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. {{ {{ {{ .. .. .. .. {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr {{ {{ .. .. .. {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. {{ {{ rr rr rr "
  "rr rr rr rr rr x! .. x! rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ {{ rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ rr rr rr rr "
  "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr x! .. x! rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. .. .. .. {{ {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. .. .. .. .. {{ {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. {{ {{ {{ rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  ) ;; map m_abandoned_cellar
 )

(kern-mk-place 'p_abandoned_cellar ; tag
               "Musty Cellar"     ; name
               nil                ; sprite
               m_abandoned_cellar ; map
               #f                 ; wraps
               #t                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil                ; subplaces

               ;; neighbors
               (list
                (list p_abandoned_farm up)
                )

               ;; objects
               (list
                (list (mk-ladder-up 'p_abandoned_farm 6 25) 6 25)

                ;; existing webs
                (list (kern-mk-obj web-type 1) 5 12)
                (list (kern-mk-obj web-type 1) 5 13)
                (list (kern-mk-obj web-type 1) 5 14)
                (list (kern-mk-obj web-type 1) 6 11)
                (list (kern-mk-obj web-type 1) 6 12)
                (list (kern-mk-obj web-type 1) 6 13)
                (list (kern-mk-obj web-type 1) 6 14)
                (list (kern-mk-obj web-type 1) 6 15)
                (list (kern-mk-obj web-type 1) 7 12)
                (list (kern-mk-obj web-type 1) 7 13)
                (list (kern-mk-obj web-type 1) 7 14)
                (list (kern-mk-obj web-type 1) 7 15)
                (list (kern-mk-obj web-type 1) 7 16)
                (list (kern-mk-obj web-type 1) 8 13)
                (list (kern-mk-obj web-type 1) 9  12)
                (list (kern-mk-obj web-type 1) 9  13)
                (list (kern-mk-obj web-type 1) 9  14)
                (list (kern-mk-obj web-type 1) 10 12)
                (list (kern-mk-obj web-type 1) 10 13)
                (list (kern-mk-obj web-type 1) 10 14)
                (list (kern-mk-obj web-type 1) 11 12)
                (list (kern-mk-obj web-type 1) 11 13)
                (list (kern-mk-obj web-type 1) 11 14)
                (list (kern-mk-obj web-type 1) 9  16)
                (list (kern-mk-obj web-type 1) 10 16)
                (list (kern-mk-obj web-type 1) 10 17)
                (list (kern-mk-obj web-type 1) 10 16)
                (list (kern-mk-obj web-type 1) 11 17)
                (list (kern-mk-obj web-type 1) 11 18)

                ;; spider eggs
                (list (mk-spider-egg) 5 13)
                (list (mk-spider-egg) 6 15)
                (list (mk-spider-egg) 7 12)
                (list (mk-spider-egg) 9  14)
                (list (mk-spider-egg) 11 14)
                (list (mk-spider-egg) 10 16)
                (list (mk-spider-egg) 11 18)

                ;; troll victims
                (put (mk-npc 'troll 3) 11 12)

                ;; puska
                (list (kern-mk-obj t_puska 1) 10 18)

                )
               nil ; hooks
               nil ; edge entrances
               )

;; ----------------------------------------------------------------------------
;; The entry hooks must be kern-loaded from a separate file, since they are
;; read-only and not saved with the session.
;; ----------------------------------------------------------------------------

(mk-place-music p_abandoned_cellar 'ml-dungeon-adventure)
