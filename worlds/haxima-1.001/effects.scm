;; ----------------------------------------------------------------------------
;; effects.scm - generic effects procedures used in multiple places
;; ----------------------------------------------------------------------------

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
  (kern-char-set-sleep kchar #t))

(define (sleep-rm fgob kchar)
  (kern-char-set-sleep kchar #f))

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
  (kern-obj-remove-effect kobj ef_light)
  (kern-obj-dec-light kobj minor-light-amount))

(define (light-apply fgob kobj)
  (kern-obj-inc-light kobj minor-light-amount))

(define (great-light-rm fgob kobj)
  (kern-obj-remove-effect kobj ef_great_light)
  (kern-obj-dec-light kobj major-light-amount))

(define (great-light-apply fgob kobj)
  (kern-obj-inc-light kobj major-light-amount))

;; ----------------------------------------------------------------------------
;; Protection
;;
;; Used by the In Sanct spell.
;; ----------------------------------------------------------------------------
(define (protection-rm fgob kchar)
  (kern-obj-remove-effect kchar ef_protection)
  (kern-char-add-defense kchar -10))

(define (protection-apply fgob kchar)
  (kern-char-add-defense kchar 10))

;; ----------------------------------------------------------------------------
;; Charm
;;
;; Used by the An Xen Exe spell. This effect was the first to use its own
;; non-nil gob.
;; ----------------------------------------------------------------------------
(define (charm-mk align) (list align))

(define (charm-align charm) (car charm))

(define (charm-rm charm kchar)
  (kern-obj-remove-effect kchar ef_charm)
  (kern-char-uncharm kchar))

(define (charm-apply charm kchar)
  (kern-char-charm kchar (charm-align charm)))

;; ----------------------------------------------------------------------------
;; Invisibility
;;
;; Used by the Sanct Lor spell. Note: the kernel's kern-obj-set-visible proc
;; increments/decrements a visibility counter, naturally handling cumulative
;; invisibility effects.
;; ----------------------------------------------------------------------------
(define (invisibility-rm fgob kchar)
  (kern-obj-remove-effect kchar ef_invisibility)
  (kern-obj-set-visible kchar #t))

(define (invisibility-apply fgob kchar)
  (kern-obj-set-visible kchar #f))

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
          (display "not wilderness")(newline)
          (if (> (kern-dice-roll "2d20") 20)
              (let ((clone (kern-obj-clone kobj)))
                (kern-print "Slime divides!\n")
                (kern-char-set-alignment clone (kern-obj-get-alignment kobj))
                (slime-init clone)
                (kern-obj-put-at clone (pick-loc loc clone)))
              (begin (display "roll failed")(newline))
              )))))

;; ----------------------------------------------------------------------------
;; Effects Table
;; ----------------------------------------------------------------------------
(define (mk-effect tag exec apply rm hook sym ddc cum dur)
  (kern-mk-effect tag "undef" "undef" exec apply rm hook sym ddc s_null cum dur))

(define effects
  (list
   ;;    tag                           exec proc             apply proc          rm proc          hook                 sym ddc cum dur
   ;;    ============================= ============          ==========          =========        ==================== === === === ===
   (list 'ef_poison                    'poison-exec          nil                 nil              "start-of-turn-hook" "P" 0   #f  120)
   (list 'ef_poison_immunity           'poison-immunity-exec nil                 nil              "add-hook-hook"      "I" 0   #f  -1)
   (list 'ef_temporary_poison_immunity 'poison-immunity-exec nil                 nil              "add-hook-hook"      "I" 0   #f  60)
   (list 'ef_sleep                     'sleep-exec           nil                 'sleep-rm        "start-of-turn-hook" "S" 0   #f  60)
   (list 'ef_light                     nil                   'light-apply        'light-rm        "start-of-turn-hook" "L" 0   #t  60)
   (list 'ef_great_light               nil                   'great-light-apply  'great-light-rm  "start-of-turn-hook" "L" 0   #t  120)
   (list 'ef_protection                nil                   'protection-apply   'protection-rm   "start-of-turn-hook" "P" 0   #t  10)
   (list 'ef_charm                     nil                   'charm-apply        'charm-rm        "start-of-turn-hook" "C" 0   #f  10)
   (list 'ef_invisibility              nil                   'invisibility-apply 'invisibility-rm "start-of-turn-hook" "N" 0   #t  10)
   (list 'ef_slime_split               'slime-split-exec     nil                 nil              "on-damage-hook"     ""  0   #f  -1)
   ))

(map (lambda (effect) (apply mk-effect effect)) effects)

;; ----------------------------------------------------------------------------
;; Effect Application Procedures
;; ----------------------------------------------------------------------------

;; Used by spells:
(define (apply-poison obj)
  (kern-obj-add-effect obj ef_poison nil))

;; Used by species that are inherently immune:
(define (apply-poison-immunity kobj)
  (kern-obj-add-effect kobj ef_poison_immunity nil))

(define (apply-sleep kobj)
  (kern-char-set-sleep kobj #t)
  (kern-obj-add-effect kobj ef_sleep nil))

(define (apply-slime-split kobj)
  (kern-obj-add-effect kobj ef_slime_split nil))

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
  (kern-obj-apply-damage obj "burning" 10))

(define (apply-lightning obj)
  (kern-obj-apply-damage obj "shocked" 15))

(define (lightning-trap actor subject)
  (kern-log-msg "Lightning trap!")
  (apply-lightning actor))

;; Explosion trap - shakes the screen and damages all surrounding objects

;; Burst trap - splatters the surrounding scene with dangerous fields

;; Self-destruct trap - rolls to destroy contents

