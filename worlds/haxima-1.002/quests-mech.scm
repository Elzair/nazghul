(define (quest-assign-always target)
	#t)
	
(define (quest-status-from-payload quest)
	"In progress"
	)

(define (quest-status-inprogress quest)
	"In progress"
	)

(define (quest-data-get tag)
	(let* ((questdata (tbl-get (gob (kern-get-player)) 'questdata))
			)
			(tbl-get questdata tag)
		)
	)

(define (quest-data-assign-once tag)
	(let ((questentry (quest-data-get tag)))
		(if (not (quest-assigned? questentry))
			(quest-assign questentry)
		)
	))	
	
;; assuming quest uses a tbl for payload, updates a key/value	
(define (quest-data-update tag key value)
	(let* ((qpayload (car (qst-payload (quest-data-get tag))))
			(updatehook (tbl-get qpayload 'on-update))
			)
		(if (not (equal? (tbl-get qpayload key) value))
			(begin
				(tbl-set! qpayload key value)
				(if (not (null? updatehook))
					((eval updatehook))
				)
				(qst-bump! (quest-data-get tag))
			))
	))
	
;; updates as per quest-data-update, but additionally triggers a passed in function
(define (quest-data-update-with tag key value callback)
	(let* ((qpayload (car (qst-payload (quest-data-get tag))))
			(updatehook (tbl-get qpayload 'on-update))
			)
		(if (not (equal? (tbl-get qpayload key) value))
			(begin
				(tbl-set! qpayload key value)
				(callback)
				(if (not (null? updatehook))
					((eval updatehook))
				)		
				(qst-bump! (quest-data-get tag))
			))
	))

(define (quest-data-descr! tag descr)
	(qst-set-descr! (quest-data-get tag) descr)
	)

(define (quest-data-icon! tag icon)
	(qst-set-icon! (quest-data-get tag) icon)
	)

(define (grant-xp-fn amount)
	(lambda () 
		(kern-char-add-experience (car (kern-party-get-members (kern-get-player))) amount)
	))
	
(define (grant-party-xp-fn amount)
	(lambda ()
		(let* ((party (kern-party-get-members (kern-get-player)))
				(xp-each (ceiling (/ amount (length party)))))
			(map (lambda (kchar) (kern-char-add-experience kchar xp-each)) party)
		)
	))
	
;;-------------------------------------------------------
;; Reconcile active and pregenned quests at game load to simplify
;; ingame tracking
	
(kern-add-hook 'new_game_start_hook 'reconcile-quests)

(define (reconcile-quests kplayer)
	(let ((questlist
					(tbl-get (gob
						(kern-get-player)) 'quests))
				(questdata
					(tbl-get (gob 
						(kern-get-player)) 'questdata))
			)
		(map 
			(lambda (quest)
				(let ((tag (qst-tag quest)))
					(if (and (not (null? tag))
							(not (null? (tbl-get questdata tag))))
						(tbl-set! questdata tag quest))
				))
		questlist)
	))
