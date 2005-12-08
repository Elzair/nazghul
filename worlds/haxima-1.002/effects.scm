;; ----------------------------------------------------------------------------
;; effects.scm - generic effects procedures used in multiple places
;; ----------------------------------------------------------------------------

(define (mk-effect tag exec apply rm restart hook sym ddc cum dur)
  (kern-mk-effect tag 
                  "undef" 
                  "undef" 
                  exec 
                  apply 
                  rm 
                  restart
                  hook 
                  sym 
                  ddc 
                  s_null 
                  cum 
                  dur))

(define (poison-exec fgob obj)
  (kern-obj-apply-damage obj "poisoned" 1))


;; ----------------------------------------------------------------------------
;; Poison Immunity
;;
;; Works by attaching an effect to the "add-hook-hook", which runs whenever
;; any new effect is applied. If anything tries to apply a poison effect, this
;; effect will catch it and block the application.
;;
;; The apply proc also attaches an expiration effect to the usual
;; start-of-turn-hook, to force the immunity effect to expire after a while.
;; ----------------------------------------------------------------------------
(define (poison-immunity-exec fgob effect)
  (if (eqv? effect ef_poison) #t #f))

;; ----------------------------------------------------------------------------
;; sleep
;;
;; The sleep effect is largely implemented in the kernel, and applies only to
;; character types. It expires naturally when the character makes a saving
;; throw. Note that this sleep effect is completely different than camping or
;; resting, which is managed entirely by the kernel.
;; ----------------------------------------------------------------------------
(define (sleep-exec fgob kchar)
  (if (> (kern-dice-roll "1d20")
          19)
      (begin
        (kern-log-msg (kern-obj-get-name kchar)
                      " wakes up!")
        (kern-obj-remove-effect kchar ef_sleep))
      (kern-char-set-sleep kchar #t)))

(define (sleep-rm fgob kchar)
  (kern-char-set-sleep kchar #f))

;; ----------------------------------------------------------------------------
;; paralyze
;;
;; The paralyze effect rolls to expire each turn. If the roll fails, the
;; character loses its turn. If it succeeds, the effect removes itself from the
;; character. Treats a natural roll of 20 as success.
;; ----------------------------------------------------------------------------
(define (can-paralyze? kobj)
  (and (obj-is-char? kobj)
       (not (species-is-immune-to-paralyze? (kern-char-get-species kobj)))))

(define (paralyze-apply fgob kobj)
  (kern-log-msg (kern-obj-get-name kobj) " paralyzed!"))

(define (paralyze-exec fgob kchar)
  (let ((droll (kern-dice-roll "1d20")))
    (if (or (= droll 20)
            (> droll
               dc-escape-paralyze))
        (begin
          (kern-log-msg "Paralysis wears off of " (kern-obj-get-name kchar))
          (kern-obj-remove-effect kchar ef_paralyze)
          #f)
        (begin
          (kern-obj-set-ap kchar 0)
          #f))))

(mk-effect 'ef_paralyze 'paralyze-exec 'paralyze-apply nil 'paralyze-apply 
           "keystroke-hook" "Z" 0 #f 15)

(define (paralyze kobj)
  (if (can-paralyze? kobj)
      (begin
        (kern-obj-add-effect kobj ef_paralyze nil))))

;;----------------------------------------------------------------------------
;; disease
;;
;; Drains life until victim is near death
;;----------------------------------------------------------------------------
(define (disease-exec fgob kchar)
  (if (> (kern-dice-roll "1d10")
         (kern-char-get-hp kchar))
      (begin
        (kern-log-msg (kern-obj-get-name kchar)
                      " fights off Disease")
        (kern-obj-remove-effect kchar ef_disease))
      (kern-obj-apply-damage kchar "disease" (kern-dice-roll "1d5"))))

;; ----------------------------------------------------------------------------
;; ensnare
;;
;; The ensnare effect rolls against a character's strength each turn. If the
;; roll fails, the character loses its turn. If it succeeds, the effect removes
;; itself from the character. Also treat a natural roll of 20 as success.
;; ----------------------------------------------------------------------------
(define (can-ensnare? kobj)
  (and (obj-is-char? kobj)
       (not (species-is-immune-to-ensnare? (kern-char-get-species kobj)))))

(define (ensnare-apply fgob kobj)
  (kern-log-msg (kern-obj-get-name kobj) " stuck in web!"))

(define (destroy-webs-at loc)
  (define (destroy-web web)
    (kern-obj-remove web))
  (map destroy-web (find-object-types-at loc web-type))
  (map destroy-web (find-object-types-at loc F_web_perm))
  )

(define (ensnare-exec fgob kchar)
  (display "ensnare-exec")(newline)
  (let ((droll (kern-dice-roll "1d20")))
    ;; special case -- paralysis prevents struggling against the ensnare
    (if (not (is-paralyzed? kchar))
        (if (or (= droll 20)
                (> (+ (kern-char-get-strength kchar) 
                      droll)
                   dc-escape-ensnare))
            (begin
              (kern-log-msg (kern-obj-get-name kchar) " breaks free of web!")
              (kern-obj-remove-effect kchar ef_ensnare)
              (destroy-webs-at (kern-obj-get-location kchar))
              #t)
            (begin
              (kern-log-msg (kern-obj-get-name kchar) " struggles in the web!")
              (kern-obj-set-ap kchar 0)
              #f)))))

(mk-effect 'ef_ensnare 'ensnare-exec 'ensnare-apply nil 'ensnare-apply
           "keystroke-hook" "E" 0 #f 15)

(define (is-ensnared? kobj)
  (in-list? ef_ensnare (kern-obj-get-effects kobj)))

(define (ensnare kobj)
  (if (can-ensnare? kobj)
      (begin
        (kern-obj-add-effect kobj ef_ensnare nil))))

;;----------------------------------------------------------------------------
;; fire immunity
(define (has-fire-immunity? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (or (in-list? ef_fire_immunity effects)
        (in-list? ef_temporary_fire_immunity effects))))

;; ----------------------------------------------------------------------------
;; light
;;
;; Light works by increasing the effected object's light value when the effect
;; is applied, and decreasing it when the effect is removed. It does this in a
;; two-step process. The first step is an effect which runs on the special
;; ----------------------------------------------------------------------------
(define minor-light-amount 512)
(define major-light-amount 8192)

(define (light-rm fgob kobj)
  (kern-obj-dec-light kobj minor-light-amount))

(define (light-apply fgob kobj)
  (kern-obj-inc-light kobj minor-light-amount))

(define (great-light-rm fgob kobj)
  (kern-obj-dec-light kobj major-light-amount))

(define (great-light-apply fgob kobj)
  (kern-obj-inc-light kobj major-light-amount))

;; ----------------------------------------------------------------------------
;; torchlight
;;
;; This is just like light but it's called out especially because it's
;; vulnerable to the douse effect, where as normal light is not.
;; ----------------------------------------------------------------------------
(define torchlight-amount 1024)

(define (torchlight-rm fgob kobj)
  (kern-log-msg "A torch flickers out!")
  (kern-obj-dec-light kobj torchlight-amount))

(define (torchlight-apply fgob kobj)
  (kern-obj-inc-light kobj torchlight-amount))

;; ----------------------------------------------------------------------------
;; Protection
;;
;; Used by the In Sanct spell.
;; ----------------------------------------------------------------------------
(define (protection-rm fgob kchar)
  ;;(kern-obj-remove-effect kchar ef_protection)
  (kern-char-add-defense kchar -10))

(define (protection-apply fgob kchar)
  (kern-char-add-defense kchar 10))

;; ----------------------------------------------------------------------------
;; Charm
;;
;; Used by the An Xen Exe spell. This effect was the first to use its own
;; non-nil gob.
;; ----------------------------------------------------------------------------
(define (charm-mk faction) (list faction))

(define (charm-faction charm) (car charm))

(define (charm-rm charm kchar)
  ;;(kern-obj-remove-effect kchar ef_charm)
  (kern-char-uncharm kchar))

(define (charm-apply charm kchar)
  (kern-char-charm kchar (charm-faction charm)))

;; ----------------------------------------------------------------------------
;; Invisibility
;;
;; Used by the Sanct Lor spell. Note: the kernel's kern-obj-set-visible proc
;; increments/decrements a visibility counter, naturally handling cumulative
;; invisibility effects.
;; ----------------------------------------------------------------------------
(define (invisibility-rm fgob kobj)
  ;;(kern-obj-remove-effect kobj ef_invisibility)
  (kern-obj-set-visible kobj #t))

(define (invisibility-apply fgob kobj)
  (kern-obj-set-visible kobj #f))

;; ----------------------------------------------------------------------------
;; Slime Split
;;
;; A special feature of the slime species. When a slime takes damage it rolls
;; to clone itself.
;; ----------------------------------------------------------------------------
(define (slime-split-exec fgob kobj)
  (let ((loc (kern-obj-get-location kobj)))
    (if (not (kern-place-is-wilderness? (loc-place loc)))
        (begin
          (if (> (kern-dice-roll "1d20") 15)
              (let ((clone (mk-npc 'green-slime
                                   (kern-char-get-level kobj))))
                (kern-being-set-base-faction clone
                                   (kern-being-get-base-faction kobj))
                (kern-log-msg "Slime divides!\n")
                (kern-obj-put-at clone (pick-loc loc clone)))
              )))))

;; ----------------------------------------------------------------------------
;; Grow Head
;;
;; A special feature of the hydra species. When a hydra takes damage it gains
;; experience, accelerating its advancement.
;; ----------------------------------------------------------------------------
(define (grow-head-exec fgob kobj)
  (println "grow-head-exec")
  (kern-char-add-experience kobj (kern-dice-roll "2d20")))

;; ----------------------------------------------------------------------------
;; Spider Calm
;;
;; Used by the An Xen Bet spell to prevent spiders from attacking.
;; ----------------------------------------------------------------------------
(define (spider-calm-rm fgob kchar)
  (kern-dtable-dec (kern-being-get-current-faction kchar)
                   faction-wood-spider)
  (kern-log-msg (kern-obj-get-name kchar) " seems less friendly to spiders"))

(define (spider-calm-apply fgob kchar)
  (kern-dtable-inc (kern-being-get-current-faction kchar)
                   faction-wood-spider)
  (kern-log-msg (kern-obj-get-name kchar) " makes spiders seem friendlier"))

;;----------------------------------------------------------------------------
;; Drunk
;;
;; Every keystroke at start-of-turn, roll to make the victim move in a random
;; direction as if staggering. If roll succeeds end the victim's turn. Ending
;; the turn prevents cumulative drinks from causing more than one "stagger" per
;; turn.
;;----------------------------------------------------------------------------
(define (drunk-exec fgob kchar)
  (if (> (kern-dice-roll "1d20") 16)
      (if (stagger kchar)
          (begin
            (kern-log-msg (kern-obj-get-name kchar) " staggers!")
            (end-turn kchar)))))

(define (drunk-apply fgob kchar)
  (kern-log-msg (kern-obj-get-name kchar) " feels tipsy!"))

(define (drunk-rm fgob kchar)
  (kern-log-msg (kern-obj-get-name kchar) " feels less inebriated"))

;; ----------------------------------------------------------------------------
;; Effects Table
;; ----------------------------------------------------------------------------

(define effects
  (list
   ;;    tag                           exec proc             apply proc          rm proc          restart proc        hook                 sym ddc cum dur
   ;;    ============================= ============          ==========          =========        ============        ==================== === === === ===
   (list 'ef_poison                    'poison-exec          nil                 nil              nil                 "start-of-turn-hook" "P" 0   #f  120)
   (list 'ef_poison_immunity           'poison-immunity-exec nil                 nil              nil                 "add-hook-hook"      "I" 0   #f  -1)
   (list 'ef_temporary_poison_immunity 'poison-immunity-exec nil                 nil              nil                 "add-hook-hook"      "I" 0   #f  60)
   (list 'ef_sleep                     'sleep-exec           nil                 'sleep-rm        nil                 "start-of-turn-hook" "S" 0   #f  60)
   (list 'ef_light                     nil                   'light-apply        'light-rm        'light-apply        "start-of-turn-hook" "L" 0   #t  60)
   (list 'ef_torchlight                nil                   'torchlight-apply   'torchlight-rm   'torchlight-apply   "start-of-turn-hook" "T" 0   #f  60)
   (list 'ef_great_light               nil                   'great-light-apply  'great-light-rm  'great-light-apply  "start-of-turn-hook" "L" 0   #t  120)
   (list 'ef_protection                nil                   'protection-apply   'protection-rm   'protection-apply   "start-of-turn-hook" "P" 0   #t  10)
   (list 'ef_charm                     nil                   'charm-apply        'charm-rm        'charm-apply        "start-of-turn-hook" "C" 0   #f   5)
   (list 'ef_invisibility              nil                   'invisibility-apply 'invisibility-rm 'invisibility-apply "start-of-turn-hook" "N" 0   #t  10)
   (list 'ef_permanent_invisibility    nil                   'invisibility-apply 'invisibility-rm 'invisibility-apply "start-of-turn-hook" "N" 0   #t  -1)
   (list 'ef_slime_split               'slime-split-exec     nil                 nil              'slime-split-exec   "on-damage-hook"     ""  0   #f  -1)
   (list 'ef_spider_calm               nil                   'spider-calm-apply  'spider-calm-rm  nil                 "start-of-turn-hook" ""  0   #f  60) 
   (list 'ef_drunk                     'drunk-exec           'drunk-apply        'drunk-rm        nil                 "keystroke-hook"     "A" 0   #t  60)
   (list 'ef_disease                   'disease-exec         nil                 nil              nil                 "start-of-turn-hook" "D" 0   #f  -1)
   (list 'ef_fire_immunity             nil                   nil                 nil              nil                 "nil-hook"           "F" 0   #f  -1)
   (list 'ef_temporary_fire_immunity   nil                   nil                 nil              nil                 "nil-hook"           "F" 0   #f  60)
   (list 'ef_grow_head                 'grow-head-exec       nil                 nil              'grow-head-exec     "on-damage-hook"     ""  0   #f  -1)
   ))

(map (lambda (effect) (apply mk-effect effect)) effects)

;;----------------------------------------------------------------------------
;; Effect Test Procedures
;;----------------------------------------------------------------------------

(define (is-poisoned? kobj)
  (in-list? ef_poison (kern-obj-get-effects kobj)))

(define (is-paralyzed? kobj)
  (in-list? ef_paralyze (kern-obj-get-effects kobj)))

(define (is-diseased? kobj)
  (in-list? ef_disease (kern-obj-get-effects kobj)))

(define (is-asleep? kobj)
  (in-list? ef_sleep (kern-obj-get-effects kobj)))

(define (is-charmed? kobj)
  (in-list? ef_charm (kern-obj-get-effects kobj)))

(define (is-disabled? kobj)
  (let ((effects (kern-obj-get-effects kobj)))
    (println "effects=" effects)
    (if (null? effects)
        #f
        (foldr (lambda (x effect)
                 (or x
                     (in-list? effect effects)))
               #f
               (list ef_paralyze ef_sleep ef_charm ef_ensnare)))))

(define (not-disabled? kobj)
  (not (is-disabled? kobj)))

;; ----------------------------------------------------------------------------
;; Effect Application Procedures
;; ----------------------------------------------------------------------------

;; Used by spells:
(define (apply-poison obj)
  (kern-obj-add-effect obj ef_poison nil)
  obj)

;; Used by species that are inherently immune:
(define (apply-poison-immunity kobj)
  (kern-obj-add-effect kobj ef_poison_immunity nil)
  kobj)

(define (apply-sleep kobj)
  (kern-char-set-sleep kobj #t)
  (kern-obj-add-effect kobj ef_sleep nil)
  kobj)

(define (apply-slime-split kobj)
  (kern-obj-add-effect kobj ef_slime_split nil)
  kobj)

(define (make-invisible kobj)
  (kern-obj-add-effect kobj ef_permanent_invisibility nil)
  kobj)

(define (apply-acid kchar)
  (let ((arms (kern-char-get-arms kchar)))
    (if (not (null? arms))
        (let ((ktype (random-select arms)))
          (kern-log-msg "Acid dissolves 1 " (kern-type-get-name ktype) 
                        " held by " (kern-obj-get-name kchar))
          (kern-char-unready kchar ktype)
          (kern-obj-remove-from-inventory kchar ktype 1)))))

;; ----------------------------------------------------------------------------
;; Container traps
;;
;; Container traps trigger when somebody (the actor) opens the container (the
;; subject). A trap may effect the actor, the subject, the contents of the
;; subject, or the place or tiles where the event takes place. A container trap
;; receives the actor and the subject as parameters.
;; ----------------------------------------------------------------------------

;; Simple trap - applies an effect to the actor
(define (mk-simple-trap effect)
  (lambda (actor subject)
    (effect actor)))

(define (test-trap actor subject)
  (kern-obj-apply-damage actor "ouch" 1))

(define (burn obj)
  (if (and (kern-obj-is-being? obj)
           (not (has-fire-immunity? obj)))
      (begin
        (kern-log-msg "Burning!")
        (kern-obj-apply-damage obj "burning" (kern-dice-roll "2d3+2")))))

(define (great-burn obj)
  (if (not (has-fire-immunity? obj))
      (kern-obj-apply-damage obj "burning" (kern-dice-roll "10d8+20"))))

(define (slip obj)
  (let ((mmode (kern-obj-get-mmode obj)))
    (if (eqv? mmode mmode-walk)
        (if (< (kern-dice-roll "1d20") 5)
            (let ((dir (kern-obj-get-dir obj)))
              (if (not (and (= 0 (car dir))
                            (= 0 (cadr dir))))
                  (begin
                    (kern-obj-move obj (- (car dir)) (- (cadr dir)))
                    (kern-log-msg "Slipped!")
                    (kern-obj-apply-damage obj "slipped" (kern-dice-roll "1d4")))))))))

(define (apply-lightning obj)
  (kern-obj-apply-damage obj "shocked" (kern-dice-roll "2d8")))

(define (lightning-trap actor subject)
  (kern-log-msg "Lightning trap!")
  (apply-lightning actor))

(define (spike-trap actor subject)
  (kern-log-msg "Spike trap!")
  (kern-obj-apply-damage actor "ouch" (kern-dice-roll "1d6")))

(define (burn-trap actor subject)
  (kern-log-msg "Fire trap!")
  (burn actor))

(define (poison-trap actor subject)
  (kern-log-msg "Poison trap!")
  (apply-poison actor))

(define (sleep-trap actor subject)
  (kern-log-msg "Sleep trap!")
  (apply-sleep actor))

(define (bomb-trap actor subject)
  (define (hit loc)
    (map burn (kern-get-objects-at loc))
    (if (terrain-ok-for-field? loc)
        (kern-obj-put-at (kern-mk-obj F_fire 1) loc)))
  (kern-log-msg "Bomb trap!")
  (burn actor)
  (shake-map 10)
  (map hit (get-8-neighboring-tiles (kern-obj-get-location subject)))
  (kern-obj-remove subject))

(define (self-destruct-trap actor subject)
  (shake-map 5)
  (kern-log-msg "Self-destruct trap!")
  (kern-obj-remove subject))

;; Explosion trap - shakes the screen and damages all surrounding objects

;; Burst trap - splatters the surrounding scene with dangerous fields

;; Self-destruct trap - rolls to destroy contents

;;----------------------------------------------------------------------------
;; Misc stuff -- not sure where to put this
(define (douse ktarg)
  (kern-obj-remove-effect ktarg ef_torchlight))

(define (wind-trap ktarg)
  (kern-log-msg "A gust of wind!")
  (douse ktarg)
  #f ;; prevents removal of trigger
  )
