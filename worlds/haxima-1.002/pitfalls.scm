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
(define (mk-pitfall name ddc dmg) (list 'pitfall name ddc dmg #f))
(define (pitfall-name pfall) (cadr pfall))
(define (pitfall-detect-dc pfall) (caddr pfall))
(define (pitfall-damage pfall) (cadddr pfall))
(define (pitfall-detected? pfall) (list-ref pfall 4))
(define (pitfall-set-detected! pfall val) (list-set-ref! pfall 4 val))

;; The step handler runs whenever a character (kchar) steps on the pitfall
;; object (kobj). If the pitfall has already been detected then no harm
;; done. Otherwise, the character gets a roll to avoid the pitfall. Whether
;; avoided or not, the pitfall will be detected and made visible.
(define (kpitfall-step-handler kobj kchar)
  (let ((pfall (kobj-gob-data kobj)))
    (println pfall)
    (if (not (pitfall-detected? pfall))
        (let ((roll (kern-dice-roll "1d20"))
              (bonus (occ-thief-dice-roll kchar)))
          (kern-obj-set-visible kobj #t)
          (pitfall-set-detected! pfall #t)
          (cond ((or (= roll 20)
                     (> (+ roll bonus)
                        (pitfall-detect-dc pfall)))
                 (kern-log-msg (kern-obj-get-name kchar) 
                               " avoids " 
                               (pitfall-name pfall) 
                               "!")
                 )
                (else
                 (kern-log-msg (kern-obj-get-name kchar) " trips "
                               (pitfall-name pfall)
                               "!")
                 (kern-obj-apply-damage 
                  kchar
                  "ouch" 
                  (kern-dice-roll (pitfall-damage pfall)))))))))

(define ktrap-ifc
  (ifc nil
       (method 'step kpitfall-step-handler)
       ))

(kern-mk-sprite-set 'ss_pitfalls 32 32 8 8 0 0 "pitfalls.png")
(kern-mk-sprite 's_caltrops ss_pitfalls 1 0 #f 0)
(kern-mk-sprite 's_beartrap ss_pitfalls 1 1 #f 0)

(mk-obj-type 't_caltrops "caltrops" s_caltrops layer-mechanism ktrap-ifc)
(mk-obj-type 't_beartrap "beartrap" s_beartrap layer-mechanism ktrap-ifc)

(define (mk-caltrops)
  (bind (make-invisible (kern-mk-obj t_caltrops 1))
        (mk-pitfall "a caltrops" 18 "1d10")))

(define (mk-beartrap)
  (bind (make-invisible (kern-mk-obj t_beartrap 1))
        (mk-pitfall "a beartrap" 16 "2d10")))
