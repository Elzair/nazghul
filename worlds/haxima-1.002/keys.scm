;; Keys are a bit unusual in that each unique key must be a unique object
;; type. This is because player inventory consists only of object types, not
;; instances, and to use a key it must be in player inventory.

(define (mk-key-type tag sprite)
  (mk-reusable-item 
   tag "key" sprite 1
   (lambda (ktype kuser)
     (let ((ktarg (ui-target (kern-obj-get-location kuser)
                             1 
                             (mk-ifc-query 'use-key))))
       (cond ((null? ktarg) (kern-log-msg "No effect!"))
             (else
              (ifccall ktarg 'use-key ktype)))))))

(define (mk-key type)
  (kern-mk-obj type 1))

;; List all the key types. This doesn't *need* to be done here, but like any
;; type, it must be done in a file that is kern-loaded, which means it can't be
;; done in a place file. So by convention let's list them here.
(mk-key-type 't_brundegardt_tower_4_key s_picklock)

(println "made key: " t_brundegardt_tower_4_key)