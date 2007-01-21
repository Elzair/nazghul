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
                      
  

;;----------------------------------------------------------------------------
;; Skill declarations

(define sk_unlock 
  (kern-mk-skill "Unlock" "Unlock a non-magical door or chest" 
                 1 2 
                 'skill-unlock nil 
                 (list t_picklock) 
                 (list (list t_sword 1))
                 ))

(define sk_jump
  (kern-mk-skill "Jump" "Jump over tiles"
                 2 1
                 'skill-jump nil
                 nil
                 nil))

;;----------------------------------------------------------------------------
;; Skill Set declarations

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list (list 2 sk_unlock)
                                    )))

(define sks_wanderer
  (kern-mk-skill-set "Wanderer" (list (list 1 sk_unlock)
                                      (list 1 sk_jump)
                                    )))
