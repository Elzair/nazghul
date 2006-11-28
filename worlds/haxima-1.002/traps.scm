;;----------------------------------------------------------------------------
;; traps.scm -- Traps that can be placed on chests, doors, etc. Most of the
;; procedures which apply the effects of these trap can be found over in
;; effects.scm, where they are shared in common.
;;
;; Traps are not kernel objects and do not have kernel types; they are entirely
;; an invention of the script. In order for a trap to exist in the game world
;; it must be attached to a kernel object like a door or a chest.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Trap Implementaion
;;----------------------------------------------------------------------------

;; Define what a trap "type" is. Each type has a name and the procedure to call
;; when the trap is triggered. The procedure should be of the form
;;
;;  (lambda (<kchar> <kobj>) ...)
;;
;; Where kchar is the kernel object for the character that triggered the trap,
;; and kobj is the kernel object which the trap was attached to.
;;
(define (mk-trap-type namestr proc-tag) (list 'trap-type namestr proc-tag))
(define (trap-type-name ttype) (cadr ttype))
(define (trap-type-proc ttype) (caddr ttype))

;; Define what a trap is. A trap has a type and some state variables. Currently
;; the only state variable is a "detected" flag, which is set if the player has
;; detected the trap. The avoidance is hard-coded currently, but depends on if
;; the trap has already been detected. The detection and fumble difficulties
;; are also hard-coded (these are used when s)earching trapped objects).
(define (mk-trap type) (list 'trap type #f #f))
(define (trap-type trap) (cadr trap))
(define (trap-detected? trap) (caddr trap))
(define (trap-set-detected! trap val) (set-car! (cddr trap) val))
(define (trap-tripped? trap) (cadddr trap))
(define (trap-set-tripped! trap val) (set-car! (cdddr trap) val))
(define (trap-name trap) (trap-type-name (trap-type trap)))
(define (trap-avoid-dc trap) (if (trap-detected? trap) 10 20))
(define (trap-detect-dc trap) 18)
(define (trap-fumble-dc trap) 12)

;; Trigger a trap. The trap parm is one of our scripted traps conforming to the
;; above, kobj is the kernel object the trap is applied to, and kchar is the
;; kernel character object that triggered the trap. This proc will
;; automatically use the character's thiefly skill to roll to avoid the trap.
(define (trap-trigger trap kobj kchar)
  (let ((roll (kern-dice-roll "1d20"))
        (bonus (occ-thief-dice-roll kchar))
        (ttype (trap-type trap))
        (avoid (trap-avoid-dc trap))
        (already-tripped? (trap-tripped? trap))
        )
    (trap-set-detected! trap #t)
    (trap-set-tripped! trap #t)
    (cond (already-tripped? nil)
          ((or (= roll 20)
               (> (+ roll bonus) avoid))
           (kern-log-msg (kern-obj-get-name kchar) 
                         " ^c+gavoids^c- a " 
                         (trap-type-name ttype) 
                         " trap!"))
          (else
           (kern-log-msg (kern-obj-get-name kchar) " ^c+rtrips^c- a "
                         (trap-type-name ttype)
                         " trap!")
           (apply (eval (trap-type-proc (trap-type trap)))
                  (list kchar kobj))))))

;; S)earch a trap. Roll to detect. If the roll is bad then the trap is
;; triggered (whether or not it was already detected). If the roll is good then
;; the trap is detected.
(define (trap-search trap kobj kchar)
  (let ((roll (kern-dice-roll "1d20"))
        (bonus (occ-thief-dice-roll kchar))
        (ttype (trap-type trap))
        )
    (cond ((and (not (trap-detected? trap))
                (or (= roll 20) 
                    (> (+ roll bonus) 
                       (trap-detect-dc trap))))
           (kern-log-msg (kern-obj-get-name kchar) 
                         " ^c+gfinds^c- a " 
                         (trap-type-name ttype) 
                         " trap!")
           (trap-set-detected! trap #t))
          ((or (= roll 1)
               (< (+ roll bonus) (trap-fumble-dc trap)))
           (trap-trigger trap kobj kchar)
           ))))

;;----------------------------------------------------------------------------
;; Trap Types
;;----------------------------------------------------------------------------
(define (lightning-trap-proc actor subject) (apply-lightning actor))
(define (burn-trap-proc actor subject) (burn actor))
(define (poison-trap-proc actor subject) (apply-poison actor))
(define (sleep-trap-proc actor subject) (apply-sleep actor))

(define (spike-trap-proc actor subject)
  (kern-obj-apply-damage actor "ouch" 
                         (kern-dice-roll "1d6")))

(define (bomb-trap-proc actor subject)
  (define (hit loc)
    (map burn (kern-get-objects-at loc))
    (if (terrain-ok-for-field? loc)
        (kern-obj-put-at (kern-mk-obj F_fire 1) loc)))
  (shake-map 10)
  (map hit 
       (get-8-neighboring-tiles (kern-obj-get-location subject))))

(define (self-destruct-trap-proc actor subject)
  (shake-map 3)
  (kern-obj-put-at (kern-mk-field F_fire 10)
                   (kern-obj-get-location subject))
  (kern-obj-put-at (kern-mk-obj sulphorous_ash 1)
                   (kern-obj-get-location subject))
  (ifccall subject 'self-destruct)
  )

(define lightning-trap (mk-trap-type "lightning" 'lightning-trap-proc))
(define burn-trap (mk-trap-type "burn" 'burn-trap-proc))
(define poison-trap (mk-trap-type "poison" 'poison-trap-proc))
(define sleep-trap (mk-trap-type "sleep" 'sleep-trap-proc))
(define spike-trap (mk-trap-type "spike" 'spike-trap-proc))
(define bomb-trap (mk-trap-type "bomb" 'bomb-trap-proc))
(define self-destruct-trap (mk-trap-type "self-destruct" 'self-destruct-trap-proc))
