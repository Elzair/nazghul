;; Local variables
(define spider-melee-weapon t_hands)

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

;; ============================================================================
;; Wood Spider AI
;; ============================================================================

(define (ensnare-loc kspider loc)
  (display "ensnare-loc")(newline)
  (kern-obj-put-at (kern-mk-obj web-type 1) loc))

(define (wood-spider-no-hostiles kspider)
  (display "wood-spider-no-hostiles")(newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (not (is-object-type-at? loc web-type))
        (ensnare-loc kspider loc)))
  (wander kspider))

;; fixme: add is-paralyzed? once that's implemented
(define (is-helpless? kchar)
  (or (kern-char-is-asleep? kchar)
      (is-ensnared? kchar)))

(define (wood-spider-attack-helpless-foe kspider kfoe)
  (define (attack kspider coords)
    (display "wood-spider-attack")(newline)
    (kern-char-attack kspider spider-melee-weapon kfoe))
  (display "wood-spider-attack-helpless-foe")(newline)
  (do-or-goto kspider (kern-obj-get-location kfoe) attack))

(define (wood-spider-hostiles kspider foes)
  (display "wood-spider-hostiles")(newline)
  (let ((helpless-foes (filter is-helpless? foes)))
    (display "helpless-foes:")(display helpless-foes)(newline)
    (if (null? helpless-foes)
        (evade kspider foes)
        (wood-spider-attack-helpless-foe kspider 
                                         (closest-obj kspider 
                                                      helpless-foes)))))

(define (wood-spider-ai kspider)
  (newline)(display "spider-ai")(newline)
  (let ((foes (all-visible-hostiles kspider)))
    (display "foes=")(display foes)(newline)
    (if (null? foes)
        (wood-spider-no-hostiles kspider)
        (wood-spider-hostiles kspider foes))))
