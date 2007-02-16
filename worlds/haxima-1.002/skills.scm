;;----------------------------------------------------------------------------
;; Skill procedures
;;
;; Skill procedures should not do any requirements-checking because the kernel
;; checks all requirements before allowing them to be called. Skill procedures
;; should always return (ie, evaluate to) one of the standard result-* codes
;; (eg, result-ok, result-no-target, etc... see naz.scm).

(define (skill-unlock kactor)
  (cast-ui-ranged-any powers-unlock
                      kactor 1 (occ-ability-thief kactor)
                      (mk-ifc-query 'unlock)))

(define (skill-jump kactor)
  (cast-ui-ranged-loc powers-jump
                      kactor
                      2
                      0))

(define (skill-detect-trap kactor)
  (cast-ui-ranged-any powers-detect-traps
                      kactor 1 (occ-ability-thief kactor)
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 'get-traps)))
                      ))

;; fixme: max-cost should account for actor's current ap
(define (skill-sprint kactor)
  (let* ((origin (kern-obj-get-location kactor))
         (kplace (loc-place origin))
         (sprint-max-range 5)
         (sprint-max-cost 6)
        )
    (define (too-far? origin dest)
      (let ((path (line (loc-x origin) (loc-y origin) 
                        (loc-x dest) (loc-y dest))))
        (let ((cost (foldr (lambda (d xy)
                             (+ d 
                                (kern-place-get-movement-cost (mk-loc kplace
                                                                      (car xy) 
                                                                      (cdr xy)) 
                                                              kactor)
                                ))
                           0
                           path)))
          (> cost sprint-max-cost))))
    (define (checkloc x y)
      (let ((dest (mk-loc kplace x y)))
        (and (kern-place-is-passable dest kactor)
             (kern-in-los? origin dest)
             (not (too-far? origin dest))
        )))
    (cast-ui-template-loc powers-sprint
                          kactor
                          (kern-mk-templ origin sprint-max-range 'checkloc)
                          0)))

;;----------------------------------------------------------------------------
;; Skill declarations
;;
;; (kern-mk-skill <tag>
;;                <name>
;;                <description>
;;                <ap-consumed>
;;                <mp-consumed>
;;                <can-use-in-wilderness?>
;;                <is-passive?>
;;                <yusage-proc>
;;                <yusage-special-check-proc>
;;                <list-of-required-tools>
;;                <list-of-required-consumables>)

(define sk_unlock 
  (kern-mk-skill "Unlock" "Unlock a door with a picklock"
                 1 2 #f #f
                 'skill-unlock nil 
                 (list t_picklock) 
                 nil
                 ))

(define sk_jump
  (kern-mk-skill "Jump" "Jump over impassable terrain"
                 2 1 #f #f
                 'skill-jump nil
                 nil
                 nil))

(define sk_detect_trap
  (kern-mk-skill "Detect Trap" "Check if a door or chest is trapped"
                 2 2 #f #f
                 'skill-detect-trap nil
                 nil
                 nil))

(define sk_arm_trap
  (kern-mk-skill "Arm Trap" "Allows character to arm traps"
                 2 2 #f #t
                 nil
                 nil
                 nil))

(define sk_sprint
  (kern-mk-skill "Sprint" "Move quickly, in a straight line, for a short distance"
                 1 1 #f #f
                 'skill-sprint
                 nil
                 nil))

;;----------------------------------------------------------------------------
;; Skill Set declarations

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list (list 1 sk_unlock)
                                    (list 1 sk_jump)
                                    (list 1 sk_detect_trap)
                                    (list 1 sk_arm_trap)
                                    (list 1 sk_sprint)
                                    )))

(define sks_wanderer sks_wrogue)
