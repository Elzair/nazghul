;; ----------------------------------------------------------------------------
;; tools.scm -- "usable" stuff that isn't a book, scroll or potion
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_tools 32 32 3 3 0 0 "tools.png")

(kern-mk-sprite 's_torch    ss_tools 1 0 #f 0)
(kern-mk-sprite 's_picklock ss_tools 1 1 #f 0)
(kern-mk-sprite 's_gem      ss_tools 1 2 #f 0)
(kern-mk-sprite 's_shovel   ss_tools 1 3 #f 0)

;; torch -- use two in-lor spells
(mk-usable-item 't_torch "torch" s_torch 1
                (lambda (kobj kuser) 
                  (in-lor kuser) 
                  (in-lor kuser)))

;; picklock
(mk-usable-item 't_picklock "picklock" s_picklock 2
                (lambda (kobj kuser)
                  (let ((ktarg (ui-target (kern-obj-get-location kuser)
                                          1 
                                          (mk-ifc-query 'unlock))))
                    (if (null? ktarg)
                        (begin
                          (kern-log-msg "No effect!")
                          nil)
                        (begin
                          (if (> (kern-dice-roll "1d20") 11)
                              (send-signal kuser ktarg 'unlock)
                              (kern-log-msg "Picklock broke!"))
                          #t)))))

;; gem -- use peer spell
(mk-usable-item 't_gem "gem" s_gem 2
                (lambda (kgem kuser)
                  (in-quas-wis nil kuser)))

;;----------------------------------------------------------------------------
;; shovel & buried object generator
;;----------------------------------------------------------------------------
(define (buried-mk objtype-tag quan) (list objtype-tag quan))
(define (buried-objtype-tag buried) (car buried))
(define (buried-quan buried) (cadr buried))

(define (buried-digup kburied)
  (display "buried-digup")(newline)
  (let* ((buried (kobj-gob-data kburied))
         (kobj (kern-mk-obj (eval (buried-objtype-tag buried))
                            (buried-quan buried))))
    (kern-obj-put-at kobj
                     (kern-obj-get-location kburied))
    (kern-log-msg "You dig up something!")
    (kern-obj-remove kburied)))

(define buried-ifc
  (ifc nil
       (method 'digup buried-digup)))

(mk-obj-type 't_buried nil nil layer-none buried-ifc)

(define (mk-buried objtype-tag quan)
  (bind (kern-mk-obj t_buried 1)
        (buried-mk objtype-tag quan)))

(define (is-buried? kobj)
  (eqv? (kern-obj-get-type kobj)
        t_buried))

(mk-usable-item 't_shovel "shovel" s_shovel 2
                (lambda (kshovel kuser)
                  (let ((ktarg (filter is-buried?
                                       (kern-get-objects-at 
                                        (kern-obj-get-location kuser)))))
                    (if (null? ktarg)
                        (kern-log-msg "Nothing buried here!")
                        (begin
                          (signal-kobj (car ktarg) 'digup (car ktarg) nil)
                          )))))
