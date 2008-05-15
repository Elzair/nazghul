;; Defines the basic stuff for the haxima quest system

;; Create a new quest.
;;
;; title - a string that will be shown in the quest log listing and at the top of the quest pane
;; descr - a string that will be shown in the quest pane
;; assign - a proc that will run when the quest is assigned, of the form (assign quest target),
;;     where 'quest' is the thing being created right here and 'target' is the (scheme) object 
;;     the quest is being assigned to. Iff 'assign' returns #t then the quest will be added to
;;     the target's list of quests.
;; status - a proc that will be called by the ztats pane, of the form (status quest). It should
;;     return a string describing how far along the quest is towards completion. Once the quest
;;     is completed this proc will no longer be called.
;; payload - whatever you want for your particular quest.
;;
(define (qst-mk title descr assign status . payload)
  (list 'quest title descr assign status #f payload))
(define (qst-title qst) (list-ref qst 1))
(define (qst-descr qst) (list-ref qst 2))
(define (qst-assign qst target) 
  (println "qst-assign")
  (if (apply (list-ref qst 3) (list qst target))
      (let ((qlst (find-field 'quests target)))
        (println "qlst=" qlst)
        (if (null? qlst)
            (append-field! target 'quests qst)
            (append! qlst (list qst)))
        (println "post target=" target))))
(define (qst-status qst) (apply (list-ref qst 4) (list qst)))
(define (qst-done? qst) (list-ref qst 5))
(define (qst-done! qst) (list-set-ref! qst 5 #t))
(define (qst-payload qst) (list-ref qst 6))
