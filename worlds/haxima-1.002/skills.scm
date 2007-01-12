;;----------------------------------------------------------------------------
;; Skill procedures

(define (skill-unlock kchar)
  (cond ((has? kchar t_picklock 1) 
         (cast-ui-ranged-any powers-unlock
                             caster 1 (occ-ability-thief caster)
                             (mk-ifc-query 'unlock)))
        (else
         (kern-log-msg (kern-obj-get-name kchar)
                       " has no picklock!"))))


;;----------------------------------------------------------------------------
;; Skill declarations

(define sk_unlock 
  (kern-mk-skill "Unlock" "Unlock a non-magical door or chest" 
                 1 2 
                 'skill-unlock nil 
                 (list t_picklock) 
                 nil))

;;----------------------------------------------------------------------------
;; Skill Set declarations

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list (list 2 sk_unlock)
                                    )))

(define sks_wanderer
  (kern-mk-skill-set "Wanderer" (list (list 1 sk_unlock)
                                    )))
