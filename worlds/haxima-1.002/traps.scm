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
(define (mk-trap-type namestr proc) (list 'trap-type namestr proc))
(define (trap-type-name ttype) (cadr ttype))
(define (trap-type-proc ttype) (caddr ttype))

;; Define what a trap is. A trap has a type and some state variables. Currently
;; the only state variable is a "detected" flag, which is set if the player has
;; detected the trap.
(define (mk-trap type) (list 'trap type #f))
(define (trap-type trap) (cadr trap))
(define (trap-detected? trap) (caddr trap))
(define (trap-set-detected! trap val) (set-car! (cddr trap) val))

;; Trigger a trap. The trap parm is one of our scripted traps conforming to the
;; above, kobj is the kernel object the trap is applied to, and kchar is the
;; kernel character object that triggered the trap. This proc will
;; automatically use the character's thiefly skill to roll to avoid the trap.
(define (trap-trigger trap kobj kchar)
  (let* ((thief-dice (string-append "1d" 
                                    (number->string (occ-ability-thief kchar))))
         (roll (kern-dice-roll "1d20"))
         (bonus (kern-dice-roll thief-dice))
         (ttype (trap-type trap))
        )
    (cond ((or (= roll 20)
               (> (+ roll bonus) 20))
           (kern-log-msg (kern-obj-get-name kchar) 
                         " avoids a " 
                         (trap-type-name ttype) 
                         " trap!"))
          (else
           (println trap)
           (kern-log-msg (kern-obj-get-name kchar) " trips a "
                         (trap-type-name ttype)
                         " trap!")
           (apply (eval (trap-type-proc (trap-type trap)))
                  (list kchar kobj))))))

;;----------------------------------------------------------------------------
;; Trap Types
;;----------------------------------------------------------------------------
(define lightning-trap
  (mk-trap-type "lightning"
                (lambda (actor subject)
                  (kern-log-msg "Lightning trap!")
                  (apply-lightning actor))))

(define spike-trap
  (mk-trap-type "spike"
                (lambda (actor subject)
                  (kern-log-msg "Spike trap!")
                  (kern-obj-apply-damage actor "ouch" 
                                         (kern-dice-roll "1d6")))))

(define burn-trap 
  (mk-trap-type "burn"
                (lambda (actor subject)
                  (kern-log-msg "Fire trap!")
                  (burn actor))))

(define poison-trap
  (mk-trap-type "poison"
                (lambda (actor subject)
                  (kern-log-msg "Poison trap!")
                  (apply-poison actor))))

(define sleep-trap
  (mk-trap-type "sleep"
                (lambda (actor subject)
                  (kern-log-msg "Sleep trap!")
                  (apply-sleep actor))))

(define bomb-trap
  (mk-trap-type 
   "bomb"
   (lambda (actor subject)
     (define (hit loc)
       (map burn (kern-get-objects-at loc))
       (if (terrain-ok-for-field? loc)
           (kern-obj-put-at (kern-mk-obj F_fire 1) loc)))
     (kern-log-msg "Bomb trap!")
     (burn actor)
     (shake-map 10)
     (map hit 
          (get-8-neighboring-tiles (kern-obj-get-location subject))))))

;; FIXME: this doesn't actually self-destruct any more
(define self-destruct-trap
  (mk-trap-type "self-destruct"
                (lambda (actor subject)
                  (shake-map 5)
                  (kern-log-msg "Self-destruct trap!")
                  (kern-obj-remove subject))))

;; Explosion trap - shakes the screen and damages all surrounding objects

;; Burst trap - splatters the surrounding scene with dangerous fields

;; Self-destruct trap - rolls to destroy contents
