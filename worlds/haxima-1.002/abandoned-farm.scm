;;----------------------------------------------------------------------------
;; Abandoned Farm
;;
;; The abandoned farm is a wrecked homestead. Not long ago (weeks or at most
;; months) renegade trolls attacked, killing the husband and wife who lived
;; here. On start-of-game the trolls are still here, but they are locked in a
;; life-and-death battle with a group of giant wood spiders.
;;
;; The farm has a cellar which contains a quest item. The farm itself should
;; contain the bones or corpses of the man and woman, who were eaten by the
;; trolls. Their remains should lie near the troll's campsite. I'd like to make
;; burying their remains a quest item if I can, since the woman is Gregor's
;; daughter and Ilya's mother.
;;
;; One or two very frightened sheep might also remain, and quite a few have
;; been eaten by the trolls.
;;----------------------------------------------------------------------------

(kern-mk-map 
 'm_abandoned_farm 32 32 pal_expanded
 (list
  "tt tt || || || || || || || || || || || tt tt .. .. .. tt || || || || || || || || tt tt tt tt tt "
  "tt tt || || || || || || || || || || || || tt .. .. .. tt || || || || tt || || || || tt tt bb tt "
  "|| || || || || || || || || || tt || || || tt .. .. .. .. tt || || tt tt tt || || || || tt tt tt "
  "|| || || || || || || || || tt tt tt || || tt tt .. .. .. tt || tt tt tt tt tt || || || tt tt tt "
  "|| || || || || || || || || || tt || || || tt tt .. .. .. tt || || tt tt tt tt || || || || || tt "
  "|| || || || || || || || || || || || || || tt .. .. .. .. tt || || || tt || tt tt tt || || || || "
  "|| || || || || || || || || || || || || tt tt .. .. .. tt tt tt || || || || tt || tt tt tt tt || "
  "|| || || tt tt || || || || || tt tt tt tt tt .. .. .. tt tt tt tt tt || tt tt || || || tt tt tt "
  "|| || || tt tt tt tt tt tt tt tt .. .. .. .. .. .. tt tt tt tt tt tt tt tt || || || tt tt bb tt "
  "|| || || tt tt tt tt tt tt tt .. .. bb bb bb bb bb bb bb bb .. tt tt tt tt tt tt || || tt tt tt "
  "|| || tt tt .. .. .. .. .. .. .. bb .. .. .. .. .. .. tt tt tt .. tt bb tt tt tt tt || || tt || "
  "|| || tt tt .. bb rr rr ws rr rr rr .. .. .. .. .. .. .. .. .. .. .. tt bb tt tt tt || || || || "
  "|| || tt tt .. rr .. cc cc cc cc rr .. .. .. .. .. .. .. .. .. && .. .. bb tt tt tt || || || || "
  "|| tt tt tt .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb tt bb tt || || || || "
  "tt tt tt tt .. ws cc cc cc cc cc ws .. .. .. .. .. .. .. .. .. bb .. tt bb tt tt .. .. .. || || "
  ".. tt tt .. .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb tt .. .. .. .. .. || "
  ".. .. .. .. .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. .. tt .. .. .. "
  ".. .. .. .. .. rr rr rr cc rr rr bb rr cc rr .. .. .. .. .. .. .. .. .. bb .. .. tt bb tt .. .. "
  "tt .. .. .. .. rr cc cc cc cc cc cc .. cc rr .. .. .. .. .. .. .. .. .. bb .. tt tt tt tt tt tt "
  "tt tt .. .. .. ws cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. bb .. tt tt tt tt tt bb "
  "tt tt tt .. .. rr cc cc cc cc cc .. cc cc rr rr rr rr ws rr rr bb rr bb .. .. tt tt tt tt tt tt "
  "tt tt tt .. .. rr cc cc cc [[ @@ ]] cc cc rr cc cc cc cc cc .. .. rr .. .. tt tt || || || tt tt "
  "|| || tt tt .. ws cc cc cc cc cc cc cc cc rr cc cc cc cc cc cc cc rr .. tt tt || || || || || tt "
  "|| || tt tt .. rr cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ws .. tt || || || || || || tt "
  "|| || || tt .. rr rr rr cc cc cc cc cc cc rr cc cc cc cc cc cc .. rr tt tt || || || tt || || || "
  "|| || || tt .. rr cc cc cc cc cc [[ @@ ]] rr cc cc cc cc cc .. .. bb tt tt || || || || || || || "
  "|| || || tt .. rr rr rr rr ws rr rr rr rr rr rr rr rr ws rr rr rr rr tt tt || || || || || || tt "
  "|| || || tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt || || || || || tt tt "
  "|| || tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. tt tt tt tt tt tt tt || || || || tt tt "
  "|| || tt tt || || || || || || || || tt tt tt .. .. .. tt tt tt || || || tt tt || || || || tt tt "
  "|| || || || || || || || || || || || || tt tt tt .. tt tt tt || || || || || || || || || tt tt tt "
  "|| || || || || || || || || || || || || || tt tt .. tt tt || || || tt || || || || || tt tt tt tt "
  )
 )

(kern-mk-place 'p_abandoned_farm  ; tag
               "Abandoned Farm"   ; name
               s_hamlet           ; sprite
               m_abandoned_farm   ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil                ; subplaces
               nil                ;; neighbors

               ;; objects
               (list
                (list (mk-wood-spider) 9 22)
                (list (mk-door) 13 17)
                (list (mk-door) 7 25)
                (list (mk-queen-spider faction-wood-spider) 9 23)
                (list (mk-ladder-down 'p_abandoned_cellar 6 25) 6 25)
                )
               (list 'af-entry) ; hooks
               nil ; edge entrances
               )

;; ----------------------------------------------------------------------------
;; The entry hooks must be kern-loaded from a separate file, since they are
;; read-only and not saved with the session.
;; ----------------------------------------------------------------------------
(kern-load "af-entry.scm")
