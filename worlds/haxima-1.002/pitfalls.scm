;;----------------------------------------------------------------------------
;; pitfall - "A source of danger or difficulty not easily foreseen and avoided"
;;
;; Traps may be attached to doors (see doors.scm) or containers (see
;; container.scm), but I also want some objects that are _just_ traps. They
;; start out invisible, but when detected they become visible. When a character
;; steps on one that is still hidden, the 'step' signal handler rolls and uses
;; the character's thiefly skill to determine if the character detects and
;; avoids the trap before it trips.
;;----------------------------------------------------------------------------
(define (mk-pitfall name ddc dmg usedc) (list 'pitfall name ddc dmg #f #t usedc))
(define (pitfall-name pfall) (cadr pfall))
(define (pitfall-detect-dc pfall) (caddr pfall))
(define (pitfall-damage pfall) (cadddr pfall))
(define (pitfall-detected? pfall) (list-ref pfall 4))
(define (pitfall-set-detected! pfall val) (list-set-ref! pfall 4 val))
(define (pitfall-known-to-npc? pfall) (list-ref pfall 5))
(define (pitfall-set-known-to-npc! pfall val) (list-set-ref! pfall 5 val))
(define (pitfall-use-dc pfall) (list-ref pfall 6))

;; The step handler runs whenever a character (kchar) steps on the pitfall
;; object (kobj). If the pitfall has already been detected then no harm
;; done. If the character is an NPC, and the pitfall was not put there by the
;; player, then the NPC avoids it automatically. Otherwise the character gets a
;; roll to avoid the pitfall. Whether avoided or not, the pitfall will be
;; detected and made visible.
(define (kpitfall-step-handler kobj kchar)
  (let ((pfall (kobj-gob-data kobj)))
    (if (and (not (pitfall-detected? pfall))
             (or (is-player-party-member? kchar)
                 (not (pitfall-known-to-npc? pfall))
                 ))
        (let ((roll (kern-dice-roll "1d20"))
              (bonus (occ-thief-dice-roll kchar)))
          (kern-obj-remove-effect kobj ef_permanent_invisibility)
          (pitfall-set-detected! pfall #t)
          (cond ((or (= roll 20)
                     (> (+ roll bonus)
                        (pitfall-detect-dc pfall)))
                 (kern-log-msg (kern-obj-get-name kchar) 
                               " ^c+gavoids^c- " 
                               (pitfall-name pfall) 
                               "!")
                 )
                (else
                 (kern-log-msg (kern-obj-get-name kchar) " ^c+rtrips^c- "
                               (pitfall-name pfall)
                               "!")
                 (kern-obj-apply-damage 
                  kchar
                  "ouch" 
                  (kern-dice-roll (pitfall-damage pfall)))))))))

;; This is a helper for kpitfall-use-handler. It checks if the location in the
;; current place has the right passability class for a pitfall to be concealed
;; on it.
(define (terrain-ok-for-pitfall? loc)
  (let ((pclass (kern-terrain-get-pclass (kern-place-get-terrain loc))))
    (foldr (lambda (a b) (or a (= pclass b)))
           #f
           (list pclass-grass pclass-trees pclass-forest))))

(define (mk-pitfall-from-ktype ktype)
  (cond ((eqv? ktype t_caltrops) (mk-caltrops))
        ((eqv? ktype t_beartrap) (mk-beartrap))))

;; The use handler runs when the player directs a character (kchar) to use a
;; pitfall type (ktype) from inventory. This prompts the player to select a
;; tile. If the tile has the right passability and is unoccupied, this creates
;; a new instance and conceals it on the tile.
;;
;; Update: this is dangerous. Roll against the kchar's thiefly skill to
;; determine if they succeed or just manage to hurt themselves.
(define (kpitfall-use-handler ktype kchar)
  (if (not (has-skill? kchar sk_arm_trap))
      result-lacks-skill
      (let ((loc (kern-ui-target (kern-obj-get-location kchar) 1)))
        (cond ((null? loc) 
               (kern-log-msg "Abort!")
               result-no-target
               )
              ((not (terrain-ok-for-pitfall? loc)) 
               (kern-log-msg "Wrong terrain type!")
               result-not-here
               )
              ((occupied? loc) 
               (kern-log-msg "Somebody is there!")
               result-not-here
               )
              (else
               (let* ((kobj (mk-pitfall-from-ktype ktype))
                      (pfall (kobj-gob-data kobj))
                      )
                 (cond ((null? kobj) 
                        (kern-log-msg "Script error: unknown type")
                        #f)
                       ((not (check-roll (pitfall-use-dc pfall)
                                         (occ-thief-dice-roll kchar)))
                        (kern-log-msg "^c+rOOPS...^c- " 
                                      (kern-obj-get-name kchar) 
                                      " fumbles "
                                      (pitfall-name pfall) "!")
                        (kpitfall-step-handler kobj kchar)
                        result-failed
                        )
                       (else
                        (kern-log-msg (kern-obj-get-name kchar)
                                      " plants "
                                      (pitfall-name pfall)
                                      "!")
                        (kern-obj-put-at kobj loc)
                        (kern-obj-remove-from-inventory kchar ktype 1)
                        (pitfall-set-known-to-npc! pfall #f)
                        result-ok
                        ))))))))

(define ktrap-ifc
  (ifc obj-ifc
       (method 'step kpitfall-step-handler)
       (method 'use kpitfall-use-handler)
       ))

(kern-mk-sprite-set 'ss_pitfalls 32 32 8 8 0 0 "pitfalls.png")
(kern-mk-sprite 's_caltrops ss_pitfalls 1 0 #f 0)
(kern-mk-sprite 's_beartrap ss_pitfalls 1 1 #f 0)

(mk-obj-type 't_caltrops "caltrops" s_caltrops layer-mechanism ktrap-ifc)
(mk-obj-type 't_beartrap "beartrap" s_beartrap layer-mechanism ktrap-ifc)

(define (mk-caltrops)
  (let ((kobj (kern-mk-obj t_caltrops 1)))
    (kern-obj-add-effect kobj ef_permanent_invisibility nil)
    (bind kobj (mk-pitfall "a caltrops" 18 "1d10" dc-easy))))

(define (mk-beartrap)
  (let ((kobj (kern-mk-obj t_beartrap 1)))
    (kern-obj-add-effect kobj ef_permanent_invisibility nil)
    (bind kobj (mk-pitfall "a beartrap" 16 "2d10" dc-challenging))))
