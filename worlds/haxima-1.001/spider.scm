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

;;----------------------------------------------------------------------------
;; Occupation declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-occ 'oc_spider           ;; tag
             "hunter"             ;; name 
             0.0                  ;; magic 
             +0                   ;; hp_mod 
             +0                   ;; hp_mult 
             0                    ;; mp_mod 
             0                    ;; mp_mult 
             0                    ;; hit_mod 
             0                    ;; def_mod 
             0                    ;; dam_mod 
             0                    ;; arm_mod
             nil                  ;; container (needed for items)
             nil                  ;; typical traps on the container
             nil                  ;; readied:
             nil                  ;; equipment
             )

;;----------------------------------------------------------------------------
;; Constructor
;;----------------------------------------------------------------------------
(define (mk-spider faction)
  (let ((spider (kern-mk-stock-char sp_spider 
                                   oc_spider
                                   s_spider ;; no spider sprite yet
                                   "a spider" 
                                   'wood-spider-ai)))
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

(define (wood-spider-no-hostiles kspider)
  (display "wood-spider-no-hostiles")(newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (not (is-object-type-at? loc web-type))
        (ensnare-loc loc)))
  (wander kspider))

(define (is-helpless? kchar)
  (or (kern-char-is-asleep? kchar)
      (is-ensnared? kchar)
      (is-paralyzed? kchar)))

(define (wood-spider-attack-helpless-foe kspider kfoe)
  (define (attack kspider coords)
    (display "wood-spider-attack")(newline)
    (if (is-paralyzed? kfoe)
        (suck-hp kspider kfoe (kern-dice-roll "1d6"))
        (spider-paralyze kfoe)))
  (display "wood-spider-attack-helpless-foe")(newline)
  (do-or-goto kspider (kern-obj-get-location kfoe) attack))

(define (wood-spider-can-spew-web? kspider)
  ;; fixme: I want this to depend on the spider's level
  #t)

(define (wood-spider-spew-web-at-foe kspider kfoe)
  (display "wood-spider-spew-web-at-foe")(newline)
  (let* ((v (loc-diff (kern-obj-get-location kfoe)
                      (kern-obj-get-location kspider)))
         (dir (loc-to-cardinal-dir v)))
    (display "v=")(display v)(newline)
    (spew-web kspider dir)))

(define (wood-spider-foe-in-range-of-web-spew? kspider kfoe)
  (display "wood-spider-foe-in-range-of-web-spew?")(newline)
  (let ((v (loc-diff (kern-obj-get-location kspider)
                      (kern-obj-get-location kfoe))))
    (and (< (abs (loc-x v)) web-spew-range)
         (< (abs (loc-y v)) web-spew-range))))

(define (wood-spider-pathfind-to-foe kspider kfoe)
  (display "wood-spider-pathfind-to-foe")(newline)
  (pathfind kspider (kern-obj-get-location kfoe)))

(define (wood-spider-try-to-spew-web kspider foe)
  (display "wood-spider-try-to-spew-web")(newline)
  (if (wood-spider-foe-in-range-of-web-spew? kspider foe)
      (wood-spider-spew-web-at-foe kspider foe)
      (wood-spider-pathfind-to-foe kspider foe)))

(define (wood-spider-no-helpless-foes kspider foes)
  (display "wood-spider-no-helpless-foes")(newline)
  (if (wood-spider-can-spew-web? kspider)
      (wood-spider-try-to-spew-web kspider (closest-obj 
                                            (kern-obj-get-location kspider)
                                            foes))
      (evade kspider foes)))

(define (wood-spider-hostiles kspider foes)
  (display "wood-spider-hostiles")(newline)
  (let ((helpless-foes (filter is-helpless? foes)))
    (if (null? helpless-foes)
        (wood-spider-no-helpless-foes kspider foes)
        (wood-spider-attack-helpless-foe kspider 
                                         (closest-obj 
                                          (kern-obj-get-location kspider)
                                          helpless-foes)))))

(define (wood-spider-ai kspider)
  (newline)(display "spider-ai")(newline)
  (let ((foes (all-visible-hostiles kspider)))
    (display "foes=")(display foes)(newline)
    (if (null? foes)
        (wood-spider-no-hostiles kspider)
        (wood-spider-hostiles kspider foes))))
