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

;; Make a monster generator specific to this place. It will generate trolls and
;; spiders periodically. It should not generate more than five trolls or
;; spiders at a time. It should not try to generate any trolls if two already
;; exist, and likewise for spiders.
;;
;; I'll have an invisible "abandoned farm" abstract object that runs every turn
;; the player is in the place. It will monitor the troll and spider counts to
;; maintain them.


(define (is-monster-type? kchar type)
  (eq? (kern-char-get-species kchar)
       (monster-type-get-species type)))

(define (count-monsters kgen type)
  (length (filter (lambda (kchar) (is-monster-type? kchar type)) 
                  (all-chars (loc-place (kern-obj-get-location kgen))))))

;; left off here


(define (abandoned-farm-exec kgen)
  (let ((trolls (count-monsters kgen trolls))
        (spiders (count-monsters kgen spiders)))
    (if (< trolls 2)
        (summon af-troll-spawn-point mk-troll (kern-dice-roll "1d3"))
    (if (< spiders 2)
        (summon af-spider-spawn-point mk-spider (kern-dice-roll "1d3"))))))

(kern-mk-map 
 'm_abandoned_farm 32 32 pal_expanded
 (list
  "tt tt || || || || || || || || || || || tt tt .. .. .. tt || || || || || || || || tt tt tt tt tt ";  //  0
  "tt tt || || || || || || || || || || || || tt .. .. .. tt || || || || tt || || || || tt tt tt tt ";  //  1
  "|| || || || || || || || || || tt || || || tt .. .. .. .. tt || || tt tt tt || || || || tt tt tt ";  //  2
  "|| || || || || || || || || tt tt tt || || tt tt .. .. .. tt || tt tt tt tt tt || || || tt tt tt ";  //  3
  "|| || || || || || || || || || tt || || || tt tt .. .. .. tt || || tt tt tt tt || || || || || tt ";  //  4
  "|| || || || || || || || || || || || || || tt .. .. .. .. tt || || || tt || tt tt tt || || || || ";  //  5
  "|| || || || || || || || || || || || || tt tt .. .. .. tt tt tt || || || || tt || tt tt tt tt || ";  //  6
  "|| || || tt tt || || || || || tt tt tt tt tt .. .. .. tt tt tt tt tt || tt tt || || || tt tt tt ";  //  7
  "|| || || tt tt tt tt tt tt tt tt .. .. .. .. .. .. tt tt tt tt tt tt tt tt || || || tt tt tt tt ";  //  8
  "|| || || tt tt tt tt tt tt tt .. .. b  b  b  b  b  b  b  b  b  tt tt tt tt tt tt || || tt tt tt ";  //  9
  "|| || tt tt .. .. .. .. .. .. .. b  .. .. .. .. .. .. tt tt tt .. tt b  tt tt tt tt || || tt || ";  // 10
  "|| || tt tt .. .. r  r  ws r  r  r  .. .. .. .. .. .. .. b  .. .. .. tt b  tt tt tt || || || || ";  // 11
  "|| || tt tt .. r  .. cc cc cc cc r  .. .. .. .. .. .. .. .. .. &  .. .. b  tt tt tt || || || || ";  // 12
  "|| tt tt tt .. r  cc cc cc cc cc r  .. .. .. .. .. .. .. .. .. .. .. .. b  tt tt tt || || || || ";  // 13
  "tt tt tt tt .. ws cc cc cc cc cc ws .. .. .. .. .. .. .. .. .. b  .. tt b  tt tt .. .. .. || || ";  // 14
  ".. tt tt .. .. r  cc cc cc cc cc r  .. .. .. .. .. .. .. .. .. .. .. .. b  tt .. .. .. .. .. || ";  // 15
  ".. .. .. .. .. r  cc cc cc cc cc r  .. .. .. .. .. .. .. .. .. .. .. .. b  .. .. .. tt .. .. .. ";  // 16
  ".. .. .. .. .. r  r  r  cc r  r  b  r  cc r  .. .. .. .. .. .. .. .. .. b  .. .. tt tt tt .. .. ";  // 17
  "tt .. .. .. .. r  cc cc cc cc cc cc .. cc r  .. .. .. .. .. .. .. .. .. b  .. tt tt tt tt tt tt ";  // 18
  "tt tt .. .. .. ws cc cc cc cc cc cc cc cc r  .. .. .. .. .. .. .. .. .. b  .. tt tt tt tt tt tt ";  // 19
  "tt tt tt .. .. r  cc cc cc cc cc .. cc cc r  r  r  r  ws r  r  .. r  b  .. .. tt tt tt tt tt tt ";  // 20
  "tt tt tt .. .. cc cc cc cc [  @  ]  cc cc r  cc cc cc cc cc .. .. r  .. .. tt tt || || || tt tt ";  // 21
  "|| || tt tt .. r  cc cc cc cc cc cc cc cc r  cc cc cc cc cc cc cc r  .. tt tt || || || || || tt ";  // 22
  "|| || tt tt .. ws cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ws .. tt || || || || || || tt ";  // 23
  "|| || || tt .. r  r  r  cc cc cc cc cc cc r  cc cc cc cc cc cc .. r  tt tt || || || tt || || || ";  // 24
  "|| || || tt .. r  cc cc cc cc cc [  @  ]  r  cc cc cc cc cc .. .. tt tt tt || || || || || || || ";  // 25
  "|| || || tt .. r  r  r  r  ws r  r  r  r  r  r  r  r  ws r  r  r  r  tt tt || || || || || || tt ";  // 26
  "|| || || tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt || || || || || tt tt ";  // 27
  "|| || tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. tt tt tt tt tt tt tt || || || || tt tt ";  // 28
  "|| || tt tt || || || || || || || || tt tt tt .. .. .. tt tt tt || || || tt tt || || || || tt tt ";  // 29
  "|| || || || || || || || || || || || || tt tt tt .. tt tt tt || || || || || || || || || tt tt tt ";  // 30
  "|| || || || || || || || || || || || || || tt tt .. tt tt || || || tt || || || || || tt tt tt tt ";  // 31
  )
 )

(kern-mk-place 'p_abandoned_farm     ; tag
               "Abandoned Farm"     ; name
               s_hut              ; sprite
               m_abandoned_farm      ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               ;;objects
               (list
                ;;(list (mk-troll faction-monster) 20 13)
                )
               (list 'af-entry) ; hooks
               nil ; edge entrances
               )

;; ----------------------------------------------------------------------------
;; The entry hooks must be kern-loaded from a separate file, since they are
;; read-only and not saved with the session.
;; ----------------------------------------------------------------------------
(kern-load "af-entry.scm")
