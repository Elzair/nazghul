(define (skill-unlock kchar)
  (cond ((has? kchar t_picklock 1) 
         (cast-ui-ranged-any powers-unlock
                             caster 1 (occ-ability-thief caster)
                             (mk-ifc-query 'unlock)))
        (else
         (kern-log-msg (kern-obj-get-name kchar)
                       " has no picklock!"))))


(define (mk-skill name proc lvl)
  (kern-mk-skill name proc lvl))

(mk-skill "Unlock" 'skill-unlock 2)