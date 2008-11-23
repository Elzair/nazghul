;; Defines the basic stuff for the haxima quest system

;; Create a new quest.
;;
;; title - a string that will be shown in the quest log listing and at the top
;; of the quest pane
;;
;; tag - an optional tag (preferably unique) that can be used to retrieve the quest.
;;
;; descr - a list of strings (ie paragraph) that will be shown in the quest pane
;;
;; assign - an optional symbol[1] for a proc that will run when the quest is assigned;
;; of the form (assign quest target), where 'quest' is the thing being created right
;; here and 'target' is the (scheme) object the quest is being assigned to. Iff
;; 'assign' returns #t then the quest will be added to the target's list of
;; quests.
;;
;; status - an optional symbol [1] for a proc that will be called by the ztats pane, of
;; the form (status quest), when the quest details are shown in the quest log.
;; It is called before the description is written, so it may alter that if required.
;; The method should return a list of strings to be appended to the description, or nil
;; Note that this should not be used to update the icon or inprog/done/failed status, as
;; they are used in the preceeding panel.
;;
;; icon - symbol [1] for sprite to use for the quest UI
;;
;; payload - whatever you want for your particular quest (this is an optional
;; number of parms)
;;
;; (* optional = use nil to ignore)
;;
;; Example:
;;
;;   (qst-mk "Find 6 Foozles" 
;;           '( 
;;              "If you find 6 Foozles, Mr. Squeejie will give you an enchanted toothpick."
;;				"" 
;;              "Seek them out in distant Foozleburg"
;;            )
;;           'find-foozle-assign
;;           'find-foozle-status
;;			 's_quest_foozles
;;           0 ; payload tracks num foozles found so far
;;           )
;;
;; Notes:
;;
;; [1] The symbol of a proc named foo is 'foo. You must use a symbol because
;; the name of the procedure must be saved as part of an object's gob. It would
;; be nice if you could just pass in a lambda, but saving and reloading lambda
;; closures is left as an exercise for the advanced reader. BTW, this rule
;; applies within the payload lists as well.
;;
(define (qst-mk title tag descr assign status icon . payload)
  (if (or (not (symbol? assign))
          (not (symbol? status)))
      (error "qst-mk: 'assign' and 'status' must be the symbols for procedures (ie, not the procedures themselves)"))
  (list 'quest title tag descr assign status 'inprogress icon payload))
  
(define (qst-title qst) (list-ref qst 1))

(define (qst-tag qst) (list-ref qst 2))

(define (qst-descr qst) (list-ref qst 3))

(define (qst-assign qst target) 
  (println "qst-assign")
  (apply (eval (list-ref qst 4)) 
         (list qst target)))
         
(define (qst-status qst)
	(let ((statfn (list-ref qst 5)))
		(if (not (null? statfn))
			(apply (eval statfn) (list qst))
		))
)

(define (qst-done? qst)
  ;;(println "qst-done? qst=" qst)
  (list-ref qst 6))
  
(define (qst-done! qst result)
	;;(kern-log-msg "^c+gYou have completed the quest ^c+w" (qst-title qst) "^c-!^c-")
	(if (not (equal? (list-ref qst 6) result))
		(begin
		  	(list-set-ref! qst 6 result)
		  	(qst-bump! qst)
		)
	))

(define (qst-complete? qst)
	(equal? (list-ref qst 6) 'complete))
	
(define (qst-complete! qst)
  (qst-done! qst 'complete))
  
(define (qst-failed? qst)
	(equal? (list-ref qst 6) 'failed))

(define (qst-failed! qst)
  (qst-done! qst 'failed))
	
(define (qst-icon qst) (list-ref qst 7))
  
(define (qst-payload qst) (list-ref qst 8))

(define (quest-assign qst)
  (println "quest-assign")
  (let ((target (gob (kern-get-player))))
    (if (and (notnull? qst)
             (notnull? target)
             (qst-assign qst target))
        (begin
        	(quest-insert qst)
          ;;(tbl-append! target 'quests qst)
          ;;(println "quest-assign: " target)
          ;;(kern-log-msg "^c+gYou have a new quest: " (qst-title qst) "^c-")
          ))))
          
(define (quest-assigned? qst)
	(println "quest-assigned?")
	(let* ((target (gob (kern-get-player)))
			(qstlist (tbl-get target 'quests))
			)
		(if (or (null? qst)
				(null? qstlist)
				)
			#f
			(in-list? qst qstlist)
      	)
	))
      
;; first item, if any, else nil 
(define (safe-car alist)
	(cond ((null? alist)
		nil)
		((pair? alist)
		(car alist))
		(#t alist))) 
          
(define (quest-get tag)
	(safe-car
		(filter 
			(lambda (quest) (eq? (qst-tag quest) tag))
			(tbl-get (gob (kern-get-player)) 'quests)
		)
	))

(define (quest-remove qst)
	;; (cons a nil) = a; (cons nil b) != b;
	(define (quest-remove-helper qstlist)
		(if (null? qstlist) nil
			(let ((qhead (safe-car qstlist)))
				(println "rem? " (eq? qhead qst) " " )
				(if (eq? qhead qst)
					(cdr qstlist)
					(cons
						qhead
						(quest-remove-helper (cdr qstlist))
					)
				)
			)
		))
	(let* ((target (gob (kern-get-player)))
			(trimmed  (quest-remove-helper (tbl-get target 'quests) qst))
			)
		(if (null? trimmed)
			(tbl-rm! target 'quests)
			(tbl-set! target 'quests trimmed)
			)
	))
	
(define (qst-set-title! qst title) (list-set-ref! qst 1 title))
(define (qst-set-descr! qst descr) (list-set-ref! qst 3 descr))
(define (qst-set-icon! qst icon) (list-set-ref! qst 7 icon))

;; bump the quest to the top of its appropriate list
(define (qst-bump! quest)
	(define (qst-bump-base! qst)
		(if (quest-assigned? qst)
			(begin
				(quest-remove qst)
				(quest-insert qst)
			)
		))
	;; if we have a parent quest, bump that first
	(let ((parent (quest-tbl-get quest 'qparent)))
		(if (not (null? parent))
			(let ((pqst (quest-get parent)))
				(if (not (null? pqst))
					(qst-bump! pqst)
				))
		))
	(qst-bump-base! quest)
	;; if we have children, bump them
	(let ((childlist (quest-tbl-get quest 'qchildren)))
		(println childlist)
		(map (lambda (entry)
				(let ((cqst (quest-get entry)))
					(if (not (null? cqst))
						(qst-bump-base! cqst)
						)
					))
			 childlist)
		)
	)
	
(define (quest-insert qst)
	(let* ((target (gob (kern-get-player)))
			(targlist (tbl-get target 'quests))
			(inserttype (qst-done? qst))
			(parent (quest-tbl-get qst 'qparent))
			)
		(define (insert-here? testee)
			(cond ((eq? inserttype 'inprogress) #t)
				((eq? inserttype (qst-done? testee)) #t)
				((eq? 'failed (qst-done? testee)) #t)
				(#t #f))
			)
		(define (quest-insert-helper qstlist)
			(if (null? qstlist) (list qst)
				(let ((qhead (safe-car qstlist)))
					(if (insert-here? qhead)
						(cons qst qstlist)
						(cons
							qhead
							(quest-insert-helper (cdr qstlist))
						)
					)
				)
			))
		(define (quest-insertchild-helper qstlist)
			(if (null? qstlist) (list qst)
				(let ((qhead (safe-car qstlist)))
					(if (or (not (equal? parent (quest-tbl-get qhead 'qparent)))
							(insert-here? qhead))
						(cons qst qstlist)
						(cons
							qhead
							(quest-insertchild-helper (cdr qstlist))
						)
					)
				)
			))
		(define (quest-insert-findparent qstlist)
			(if (null? qstlist) (nil)
				(let ((qhead (safe-car qstlist)))
					(if (equal? parent (qst-tag qhead))
						(cons
							qhead 
							(quest-insertchild-helper (cdr qstlist))
						)
						(cons
							qhead
							(quest-insert-findparent (cdr qstlist))
						)
					)
				)
			))
		(cond ((null? targlist) (tbl-append! target 'quests qst))
			((null? parent) (tbl-set! target 'quests (quest-insert-helper targlist)))
			(#t (tbl-set! target 'quests (quest-insert-findparent targlist)))
			)
	))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special handling for quests with tbl payloads

(define (quest-tbl? quest)
	(let ((qpayload (qst-payload quest)))
		(cond ((not (pair? qpayload)) #f)
			 ((not (pair? (car qpayload))) #f)
			(#t (is-tbl? (car qpayload)))
		)
	))
	
(define (quest-tbl-get quest tag)
	(let ((qpayload (qst-payload quest)))
		(cond ((not (pair? qpayload)) nil)
			 ((not (pair? (car qpayload))) nil)
			((not (is-tbl? (car qpayload))) nil)
			(#t (tbl-get (car qpayload) tag))
		)
	))
