;; Keys are a bit unusual in that each unique key must be a unique object
;; type. This is because player inventory consists only of object types, not
;; instances, and to use a key it must be in player inventory.

(define (mk-key-type tag name sprite)
  (mk-reusable-item 
   tag name sprite norm
   (lambda (ktype kuser)
     (let ((ktarg (ui-target (kern-obj-get-location kuser)
                             1 
                             (mk-ifc-query 'use-key))))
       (cond ((null? ktarg) 
              result-no-target)
             (else
              (ifccall ktarg 'use-key ktype)
              result-ok))))))

(define (mk-key type)
  (kern-mk-obj type 1))

;; List all the key types. This doesn't *need* to be done here, but like any
;; type, it must be done in a file that is kern-loaded, which means it can't be
;; done in a place file. So by convention let's list them here.
(mk-key-type 't_brundegardt_tower_4_key "weathered key" s_picklock)
(mk-key-type 't_stewardess_chest_key    "shiny key"     s_picklock)
