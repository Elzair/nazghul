;; ----------------------------------------------------------------------------
;; tools.scm -- "usable" stuff that isn't a book, scroll or potion
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_tools 32 32 1 3 0 0 "tools.png")

(kern-mk-sprite 's_torch    ss_tools 1 0 #f 0)
(kern-mk-sprite 's_picklock ss_tools 1 1 #f 0)
(kern-mk-sprite 's_gem      ss_tools 1 2 #f 0)

;; torch -- use two in-lor spells
(mk-usable-item 't_torch "torch" s_torch 1
                (lambda (kobj kuser) 
                  (in-lor kobj kuser) 
                  (in-lor kobj kuser)))

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
