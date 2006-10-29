;;----------------------------------------------------------------------------
;; traps.scm -- Traps that can be placed on chests, doors, etc. Most of the
;; procedures which apply the effects of these trap can be found over in
;; effects.scm, where they are shared in common.
;;
;; Traps are not kernel objects and do not have kernel types, they are entirely
;; an invention of the script. In order for a trap to exist in the game world
;; it must be attached to a kernel object like a door or a chest.
;;----------------------------------------------------------------------------

;; Define what a trap "type" is. Each type has a name and the symbol for a
;; procedure to call when the trap is triggered. The procedure should be of the
;; form
;;
;;  (lambda (<kchar> <kobj>) ...)
;;
;; Where kchar is the kernel object for the character that triggered the trap,
;; and kobj is the kernel object which the trap was attached to. The symbol for
;; the procedure is stored in the trap (instead of the procedure itself) so
;; that traps can be saved and reloaded.
;;
(define (mk-trap-type namestr procsym) (list 'trap-type namestr procsym))
(define (trap-type-name ttype) (cadr ttype))
(define (trap-type-proc ttype) (caddr ttype))

;; Define what a trap is. A trap has a type and some state variables. Currently
;; the only state variable is a "detected" flag, which is set if the player has
;; detected the trap.
(define (mk-trap type) (list 'trap type #f))
(define (trap-type trap) (cadr trap))
(define (trap-detected? trap) (caddr trap))
(define (trap-set-detected! trap val) (set-car! (cddr trap) val))


;; Legacy trap procedures
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
  )

(define (self-destruct-trap actor subject)
  (shake-map 5)
  (kern-log-msg "Self-destruct trap!")
  (kern-obj-remove subject))

;; Explosion trap - shakes the screen and damages all surrounding objects

;; Burst trap - splatters the surrounding scene with dangerous fields

;; Self-destruct trap - rolls to destroy contents
