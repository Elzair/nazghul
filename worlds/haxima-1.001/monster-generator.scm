
(define (mk-gen-ifc threshold party alignment vehicle)
  (mk-ifc nil 
          nil
          (lambda (gen)
            (if (> (modulo (random-next) 100) threshold)
                (kern-obj-put-at (kern-mk-party party alignment vehicle)
                                 (kern-obj-get-location gen))))))

;; Note: if you try to create this interface on-the-fly, as in the commented
;; out part below, then you will eventually get a runtime crash. The reason is
;; that the procedure created by the lambda in mk-gen-ifc will not be referred
;; to by any environment variable, so the gc will deallocate it.
(define goblin-gen-ifc 
  (ifc '()
       (method 'exec (lambda (kobj)
                       (if (> (modulo (random-next) 100) 99)
                           (kern-obj-put-at (kern-mk-party t_goblin_horde
                                                           align-monster
                                                           '())
                                            (kern-obj-get-location kobj)))))))


;; A monster generator
(mk-obj-type 't_goblin_generator "goblin generator" nil layer-none 
             goblin-gen-ifc)

