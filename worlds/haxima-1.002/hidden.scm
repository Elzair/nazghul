;;----------------------------------------------------------------------------
;; hidden -- wrapper object; when s)earched it creates the revelaed object and
;; removes itself

(define (hidden-mk objtype-tag quan) (list objtype-tag quan))
(define (hidden-objtype-tag hidden) (car hidden))
(define (hidden-quan hidden) (cadr hidden))

(define (hidden-search khidden ksearcher)
  (println "hidden-search")
  (let* ((hidden (kobj-gob-data khidden))
         (kobj (kern-mk-obj (eval (hidden-objtype-tag hidden))
                            (hidden-quan hidden))))
    (kern-obj-put-at kobj
                     (kern-obj-get-location khidden))
    (kern-log-msg "You find something!")
    (kern-obj-remove khidden)))

(define hidden-ifc
  (ifc nil
       (method 'search hidden-search)))

(mk-obj-type 't_hidden nil nil layer-none hidden-ifc)

(define (mk-hidden objtype-tag quan)
  (bind (kern-mk-obj t_hidden 1)
        (hidden-mk objtype-tag quan)))

(define (is-hidden? kobj)
  (eqv? (kern-obj-get-type kobj)
        t_hidden))
