;; Local variables
(define spider-melee-weapon t_hands)
(define web-spew-range      3)

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
                 humanoid-slots ;; slots: hands
                 nil            ;; native spells: currently unused
                 )

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
  (display "spider-paralyze")(newline)
  (paralyze ktarg))

(define (ensnare-loc loc)
  (display "ensnare-loc")(newline)
  (kern-obj-put-at (kern-mk-obj web-type 1) loc))

(define (spew-web kspider dir)
  (display "spew-web:dir=")(display dir)(newline)
  (let ((loc (kern-obj-get-location kspider)))
    (display "mark")(newline)
    (cast-wind-spell2 loc
                      ensnare-loc
                      dir
                      web-spew-range)))


;; ----------------------------------------------------------------------------
;; Spider AI
;; ----------------------------------------------------------------------------

(define (spider-no-hostiles kspider)
  (display "spider-no-hostiles")(newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (not (is-object-type-at? loc web-type))
        (ensnare-loc loc)))
  (wander kspider))

(define (is-helpless? kchar)
  (or (kern-char-is-asleep? kchar)
      (is-ensnared? kchar)
      (is-paralyzed? kchar)))

(define (spider-attack-helpless-foe kspider kfoe)
  (define (attack kspider coords)
    (display "spider-attack")(newline)
    (if (is-paralyzed? kfoe)
        (suck-hp kspider kfoe (kern-dice-roll "1d6"))
        (spider-paralyze kfoe)))
  (display "spider-attack-helpless-foe")(newline)
  (do-or-goto kspider (kern-obj-get-location kfoe) attack))

(define (spider-can-spew-web? kspider)
  (eqv? (kern-char-get-species kspider) sp_queen_spider))

(define (spider-spew-web-at-foe kspider kfoe)
  (display "spider-spew-web-at-foe")(newline)
  (let* ((v (loc-diff (kern-obj-get-location kfoe)
                      (kern-obj-get-location kspider)))
         (dir (loc-to-cardinal-dir v)))
    (display "v=")(display v)(newline)
    (spew-web kspider dir)))

(define (spider-foe-in-range-of-web-spew? kspider kfoe)
  (display "spider-foe-in-range-of-web-spew?")(newline)
  (let ((v (loc-diff (kern-obj-get-location kspider)
                      (kern-obj-get-location kfoe))))
    (and (< (abs (loc-x v)) web-spew-range)
         (< (abs (loc-y v)) web-spew-range))))

(define (spider-pathfind-to-foe kspider kfoe)
  (display "spider-pathfind-to-foe")(newline)
  (pathfind kspider (kern-obj-get-location kfoe)))

(define (spider-try-to-spew-web kspider foe)
  (display "spider-try-to-spew-web")(newline)
  (if (spider-foe-in-range-of-web-spew? kspider foe)
      (spider-spew-web-at-foe kspider foe)
      (spider-pathfind-to-foe kspider foe)))

(define (spider-no-helpless-foes kspider foes)
  (display "spider-no-helpless-foes")(newline)
  (if (spider-can-spew-web? kspider)
      (spider-try-to-spew-web kspider (closest-obj 
                                            (kern-obj-get-location kspider)
                                            foes))
      (evade kspider foes)))

(define (spider-hostiles kspider foes)
  (display "spider-hostiles")(newline)
  (let ((helpless-foes (filter is-helpless? foes)))
    (if (null? helpless-foes)
        (spider-no-helpless-foes kspider foes)
        (spider-attack-helpless-foe kspider 
                                         (closest-obj 
                                          (kern-obj-get-location kspider)
                                          helpless-foes)))))

(define (spider-ai kspider)
  (newline)(display "spider-ai")(newline)
  (let ((foes (all-visible-hostiles kspider)))
    (if (null? foes)
        (spider-no-hostiles kspider)
        (spider-hostiles kspider foes))))
