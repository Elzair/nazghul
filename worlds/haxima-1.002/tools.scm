;; ----------------------------------------------------------------------------
;; tools.scm -- "usable" stuff that isn't a book, scroll or potion
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_tools 32 32 3 3 0 0 "tools.png")

(kern-mk-sprite 's_torch    ss_tools 1 0 #f 0)
(kern-mk-sprite 's_picklock ss_tools 1 1 #f 0)
(kern-mk-sprite 's_gem      ss_tools 1 2 #f 0)
(kern-mk-sprite 's_shovel   ss_tools 1 3 #f 0)
(kern-mk-sprite 's_pick     ss_tools 1 4 #f 0)
(kern-mk-sprite 's_sextant  ss_tools 1 5 #f 0)

;; torch -- use two in-lor spells
(mk-usable-item 't_torch "torch" s_torch 1
                (lambda (kobj kuser) 
                  (kern-obj-add-effect kuser ef_torchlight nil)
                  result-ok))

;; picklock
(define (pick-lock-ok kuser ktarg)
  (send-signal kuser ktarg 'unlock)
  #t)
(define (pick-lock-failed kuser ktool)
  (kern-log-msg "Picklock broke!")
  (kern-obj-remove-from-inventory kuser ktool 1)
  #t
  )
(define (pick-lock-bonus kuser)
  (if (eqv? (kern-char-get-occ kuser)
            oc_wrogue)
      (floor (+ (kern-char-get-level kuser) (/ (kern-char-get-dexterity kuser) 3)))
      (floor (/ (+ (kern-char-get-level kuser) (kern-char-get-dexterity kuser)) 4)))
      )
(mk-reusable-item 't_picklock "picklock" s_picklock 2
                (lambda (kobj kuser)
                  (let ((ktarg (ui-target (kern-obj-get-location kuser)
                                          1 
                                          (mk-ifc-query 'unlock))))
                    (if (null? ktarg)
                        (begin
                          (kern-log-msg "No effect!")
                          nil)
                        (let ((roll (kern-dice-roll "1d20"))
							  (bonus (kern-dice-roll (string-append "1d" (number->string (pick-lock-bonus kuser)))))
								)
                          (println "rolled " roll " + " bonus)
                          (if (= roll 20)
                              (pick-lock-ok kuser ktarg)
                              (if (> (+ roll bonus ) 
                                     15)
                                  (pick-lock-ok kuser ktarg)
                                  (pick-lock-failed kuser kobj)
                                  )))))))

;; gem -- use peer spell
(mk-usable-item 't_gem "gem" s_gem 2
                (lambda (kgem kuser)
                  (in-quas-wis nil kuser)))

;; sledge-hammer -- shatter rocks
(mk-reusable-item 't_pick "pick" s_pick 2
                  (lambda (ktool kuser)
                    (let ((loc (kern-ui-target (kern-obj-get-location kuser)
                                               1)))
                      (if (null? loc)
                          nil
                          (let ((kter (kern-place-get-terrain loc)))
                            (if (eqv? kter t_boulder)
                                (begin
                                  (kern-log-msg (kern-obj-get-name kuser)
                                                " pulverizes a boulder!")
                                  (kern-place-set-terrain loc t_grass)
                                  (if (> (kern-dice-roll "1d20") 16)
                                      (begin
                                        (kern-log-msg "The pick shatters!")
                                        (kern-obj-remove-from-inventory kuser ktool 1))))
                                (kern-log-msg "No effect!")))))))
                          
;; sextant -- gives location
(mk-reusable-item 't_sextant "sextant" s_sextant 2
                  (lambda (ktool kuser)
                    (let ((loc (kern-obj-get-location kuser)))
                      (if (kern-place-is-wilderness? (loc-place loc))
                          (kern-log-msg "You are at cartesian coords [" 
                                        (caddr loc) " " (cadr loc) "]")
                          (begin
                            (kern-log-msg "Usable only in the wilderness!")
                            nil)))))

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

(mk-reusable-item 't_shovel "shovel" s_shovel 2
                (lambda (kshovel kuser)
                  (let ((ktarg (filter is-buried?
                                       (kern-get-objects-at 
                                        (kern-obj-get-location kuser)))))
                    (if (null? ktarg)
                        (kern-log-msg "Nothing buried here!")
                        (begin
                          (signal-kobj (car ktarg) 'digup (car ktarg) nil)
                          )))))
