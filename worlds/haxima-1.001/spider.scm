;; Local variables
(define spider-melee-weapon t_hands)
(define web-spew-range      3)

;; Remapped display and newline to local procs so they can be disabled/enabled
;; for debug more conveniently
(define (spider-display . args) 
  (display (kern-get-ticks))
  (display ":")
  (apply display args))
(define (spider-newline) (newline))

;;----------------------------------------------------------------------------
;; Species declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-species 'sp_spider      ;; tag: script variable name
                 "spider"        ;; name: used to display name in the UI
                 12             ;; strength: limits armament weight
                 6              ;; intelligence: (just reported in stats)
                 14              ;; dexterity: used to avoid traps on chests
                 speed-insect   ;; speed: action points per turn
                 10              ;; vision radius: in tiles
                 mmode-walk     ;; movement mode
                 10             ;; base hp: hit points at level zero
                 4              ;; hp multiplier: extra hp per level
                 0              ;; base mp: mana points at level zero
                 0              ;; mp multiplier: extra mana points per level
                 s_corpse       ;; sleep sprite
                 t_hands        ;; natural weapon: used when unarmed
                 #t             ;; visible: can be seen
                 sound-damage   ;; damage sound
                 sound-walking  ;; walking sound
                 'spider-killed ;; on-death
                 humanoid-slots ;; slots: hands
                 nil            ;; native spells: currently unused
                 )

(kern-mk-species 'sp_queen_spider ;; tag: script variable name
                 "queen spider"   ;; name: used to display name in the UI
                 18             ;; strength: limits armament weight
                 6              ;; intelligence: (just reported in stats)
                 12             ;; dexterity: used to avoid traps on chests
                 speed-human    ;; speed: action points per turn
                 10             ;; vision radius: in tiles
                 mmode-walk     ;; movement mode
                 30             ;; base hp: hit points at level zero
                 4              ;; hp multiplier: extra hp per level
                 0              ;; base mp: mana points at level zero
                 0              ;; mp multiplier: extra mana points per level
                 s_corpse       ;; sleep sprite
                 t_hands        ;; natural weapon: used when unarmed
                 #t             ;; visible: can be seen
                 sound-damage   ;; damage sound
                 sound-walking  ;; walking sound
                 'queen-spider-killed ;; on-death closure
                 humanoid-slots ;; slots: hands
                 nil            ;; native spells: currently unused
                 )

;; ----------------------------------------------------------------------------
;; Spider Egg
;; 
;; ----------------------------------------------------------------------------
(define (is-spider? kchar)
  (spider-display "is-spider?")(spider-newline)
  (let ((species (kern-char-get-species kchar)))
    (or (eqv? species sp_spider)
        (eqv? species sp_queen_spider))))

(define (spider-egg-disturbed kegg)
  (spider-display "spider-egg-disturbed")(spider-newline)
  (define (check loc)
    (if (foldr (lambda (a b) (or a
                                 (and (kern-obj-is-char? b)
                                      (not (is-spider? b)))))
               #f
               (kern-place-get-objects-at loc))
        loc
        nil))
  (let ((loc (kern-obj-get-location kegg)))
    (notnull? (search-rect (loc-place loc)
                           (- (loc-x loc) 2)
                           (- (loc-y loc) 2)
                           5
                           5
                           check))))

(define (spider-egg-hatch kegg)
  (spider-display "spider-egg-hatch")(spider-newline)
  (kern-log-msg "A spider hatches!")
  (kern-obj-put-at (mk-wood-spider) (kern-obj-get-location kegg))
  (kern-obj-remove kegg)
  (kern-obj-destroy kegg))

(define (spider-egg-exec kegg)
  (spider-display "spider-egg-exec")(spider-newline)
  (if (spider-egg-disturbed kegg)
      (spider-egg-hatch kegg)))

(define spider-egg-ifc
  (ifc '()
       (method 'exec spider-egg-exec)))

(mk-obj-type 'spider-egg-type
             "spider egg"
             s_magic
             layer-item
             spider-egg-ifc)

(define (mk-spider-egg)
  (kern-mk-obj spider-egg-type 1))

;; ----------------------------------------------------------------------------
;; spider-killed -- called when a spider is killed, the character is passed as
;; an arg. Drops a spider corpse object with a random amount of spider silk
;; inside.
;; ----------------------------------------------------------------------------
(mk-obj-type 'spider-corpse-type "spider corpse" s_dead_spider
             layer-container nil)

(define (mk-spider-corpse)
  (kern-mk-container spider-corpse-type
                     nil
                     (list 
                      (list (kern-dice-roll "1d3") spider_silk)
                      )))

(define (spider-killed kspider)
  (kern-obj-put-at (mk-spider-corpse)
                   (kern-obj-get-location kspider)))

;; ----------------------------------------------------------------------------
;; queen-spider-killed -- virtually the same as spider-killed except for a few
;; details
;; ----------------------------------------------------------------------------
(mk-obj-type 'queen-spider-corpse-type "queen spider corpse" s_dead_queen_spider
             layer-container nil)

(define (mk-queen-spider-corpse)
  (kern-mk-container queen-spider-corpse-type
                     nil
                     (list 
                      (list (kern-dice-roll "1d10") spider_silk)
                      )))

(define (queen-spider-killed kspider)
  (kern-obj-put-at (mk-queen-spider-corpse)
                   (kern-obj-get-location kspider)))

;;----------------------------------------------------------------------------
;; Constructor
;;----------------------------------------------------------------------------
(define (mk-spider faction)
  (let ((spider (kern-mk-stock-char sp_spider 
                                    nil
                                    s_spider
                                    "a spider" 
                                    'spider-ai)))
    (kern-being-set-base-faction spider faction)
    spider ;; return the kernel object
    ))

(define (mk-queen-spider faction)
  (let ((spider (kern-mk-stock-char sp_queen_spider 
                                    nil
                                    s_queen_spider ;; no spider sprite yet
                                    "a queen spider" 
                                    'spider-ai)))
    (kern-being-set-base-faction spider faction)
    spider ;; return the kernel object
    ))

(define (mk-wood-spider)
  (mk-spider faction-wood-spider))

(define (char-is-spider? kchar)
  (eqv? (kern-char-get-species kchar) sp_spider))

;; ----------------------------------------------------------------------------
;; Spider "Skills"
;; ----------------------------------------------------------------------------

(define (suck-hp kspider ktarg amount)
  (kern-log-msg (kern-obj-get-name kspider) 
                " sucks the juices from " 
                (kern-obj-get-name ktarg))
  (let ((amount (min amount (kern-char-get-hp ktarg))))
    (kern-obj-apply-damage ktarg nil amount)
    (kern-obj-heal kspider amount)))

(define (spider-paralyze ktarg)
  (spider-display "spider-paralyze")(spider-newline)
  (paralyze ktarg))

(define (ensnare-loc loc)
  (spider-display "ensnare-loc")(spider-newline)
  (kern-obj-put-at (kern-mk-obj web-type 1) loc))

(define (spew-web kspider dir)
  (spider-display "spew-web:dir=")(spider-display dir)(spider-newline)
  (let ((loc (kern-obj-get-location kspider)))
    (spider-display "mark")(spider-newline)
    (cast-wind-spell2 loc
                      ensnare-loc
                      dir
                      web-spew-range)))


;; ----------------------------------------------------------------------------
;; Spider AI
;; ----------------------------------------------------------------------------

(define (is-queen-spider? kspider)
  (eqv? (kern-char-get-species kspider) sp_queen_spider))

(define (spider-try-to-lay-egg kspider)
  (spider-display "spider-try-to-lay-egg")(spider-newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (and (not (is-object-type-at? loc spider-egg-type))
             (> (kern-dice-roll "1d20") 18))
        (kern-obj-put-at (mk-spider-egg) loc))))

(define (spider-no-hostiles kspider)
  (spider-display "spider-no-hostiles")(spider-newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (not (is-object-type-at? loc web-type))
        (ensnare-loc loc))
    (if (is-queen-spider? kspider)
        (spider-try-to-lay-egg kspider)))
  (wander kspider))

(define (is-helpless? kchar)
  (or (kern-char-is-asleep? kchar)
      (is-ensnared? kchar)
      (is-paralyzed? kchar)))

(define (spider-attack-helpless-foe kspider kfoe)
  (define (attack kspider coords)
    (spider-display "spider-attack")(spider-newline)
    (if (is-paralyzed? kfoe)
        (suck-hp kspider kfoe (kern-dice-roll "1d6"))
        (spider-paralyze kfoe)))
  (spider-display "spider-attack-helpless-foe")(spider-newline)
  (do-or-goto kspider (kern-obj-get-location kfoe) attack))

(define (spider-can-spew-web? kspider)
  (eqv? (kern-char-get-species kspider) sp_queen_spider))

(define (spider-spew-web-at-foe kspider kfoe)
  (spider-display "spider-spew-web-at-foe")(spider-newline)
  (let* ((v (loc-diff (kern-obj-get-location kfoe)
                      (kern-obj-get-location kspider)))
         (dir (loc-to-cardinal-dir v)))
    (spider-display "v=")(spider-display v)(spider-newline)
    (spew-web kspider dir)))

(define (spider-foe-in-range-of-web-spew? kspider kfoe)
  (spider-display "spider-foe-in-range-of-web-spew?")(spider-newline)
  (let ((v (loc-diff (kern-obj-get-location kspider)
                      (kern-obj-get-location kfoe))))
    (and (< (abs (loc-x v)) web-spew-range)
         (< (abs (loc-y v)) web-spew-range))))

(define (spider-pathfind-to-foe kspider kfoe)
  (spider-display "spider-pathfind-to-foe")(spider-newline)
  (pathfind kspider (kern-obj-get-location kfoe)))

(define (spider-try-to-spew-web kspider foe)
  (spider-display "spider-try-to-spew-web")(spider-newline)
  (if (spider-foe-in-range-of-web-spew? kspider foe)
      (spider-spew-web-at-foe kspider foe)
      (spider-pathfind-to-foe kspider foe)))

(define (spider-no-helpless-foes kspider foes)
  (spider-display "spider-no-helpless-foes")(spider-newline)
  (if (is-queen-spider? kspider)
      (spider-try-to-spew-web kspider (closest-obj 
                                            (kern-obj-get-location kspider)
                                            foes))
      (evade kspider foes)))

(define (spider-hostiles kspider foes)
  (spider-display "spider-hostiles")(spider-newline)
  (let ((helpless-foes (filter is-helpless? foes)))
    (if (null? helpless-foes)
        (spider-no-helpless-foes kspider foes)
        (spider-attack-helpless-foe kspider 
                                         (closest-obj 
                                          (kern-obj-get-location kspider)
                                          helpless-foes)))))

(define (spider-ai kspider)
  (spider-newline)(spider-display "spider-ai")(spider-newline)
  (let ((foes (all-visible-hostiles kspider)))
    (if (null? foes)
        (spider-no-hostiles kspider)
        (spider-hostiles kspider foes))))
