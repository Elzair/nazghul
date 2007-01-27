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

;;----------------------------------------------------------------------------
;; Skill declarations

(define sk_unlock 
  (kern-mk-skill "Unlock" "Unlock a door with a picklock"
                 1 2 #f
                 'skill-unlock nil 
                 (list t_picklock) 
                 nil
                 ))

(define sk_jump
  (kern-mk-skill "Jump" "Jump over impassable terrain"
                 2 1 #f
                 'skill-jump nil
                 nil
                 nil))

(define sk_detect_trap
  (kern-mk-skill "Detect Trap" "Check if a door or chest is trapped"
                 2 2 #f
                 'skill-detect-trap nil
                 nil
                 nil))

;;----------------------------------------------------------------------------
;; Skill Set declarations

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list (list 1 sk_unlock)
                                    (list 1 sk_jump)
                                    (list 1 sk_detect_trap)
                                    )))

(define sks_wanderer sks_wrogue)
