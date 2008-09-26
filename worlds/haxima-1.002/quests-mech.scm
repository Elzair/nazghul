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

	
;;----------------------------------------------------
;; this is a collection place for updates to quests
;;
		
;;---------------
;; whereami

(define (quest-whereami-update)
	(let* ((quest (quest-data-get 'questentry-whereami))
			(quest-tbl (car (qst-payload quest)))
			(qp-shard (tbl-get quest-tbl 'shard))
			(qp-wanderer (tbl-get quest-tbl 'wanderer)))

		(qst-set-descr! quest
		
(if (not (null? (tbl-get quest-tbl 'nossifer)))

			(list
			"You have found yourself on the Shard, a small"
			"fragment of a world, that floats surrounded by"
			"a great void."
			""
			"The demon lord Nossifer arranged for your"
			"arrival, to set in motion events that would"
			"allow his own passage to the Shard."
			""
			"Now the Demon Gate is open, and the only"
			"thing left in Nossifer's way is you..."
		)
		
			(append
;;1 where
(cond ((null? qp-shard)
		(list
			"You have found yourself in a world you have no"
			"knowledge of, with barest impressions of what"
			"might have gone before."
			""
		))
	((equal? 1 qp-shard)
		(list
			"You have found yourself in a world you have no"
			"knowledge of. The inhabitants refer to it as"
			"the Shard."
			""
		))
	(#t (list
			"You have found yourself on the Shard, a small"
			"fragment of a world, that floats surrounded by"
			"a great void."
			""
		))
	)


;; how
(if (and (null? qp-wanderer) (null? qp-shard))
		"Where are you?"
		nil
	)
			
(cond ((null? qp-wanderer)
		(list
			"How and why are you here?"
			"And what are you going to do now?"
		))
	((equal? 1 qp-wanderer)
		(list
			"Others like you have in the past been found"
			"stumbling into this world. The inhabitants"
			"know you as 'Wanderers'."
			""
			"Now you are here, what are you going to do?"
		))
	(#t (list
			"Wanderers like yourself, who are occasionally"
			"stumbled upon this world, have in the past been"
			"responsible for great deeds."
			""
			"How will you make your place?"
		))
	)

				)
			)
		)
	))

;;-----------------------
;; calltoarms

(define (quest-calltoarms-update)
	(let* ((quest (quest-data-get 'questentry-calltoarms))
			(quest-tbl (car (qst-payload quest)))
			(header (list
					"You have recieved an urgent message to contact"
					"someone called the Enchanter as soon as"
					"possible."
					"")))
		(define (tbl-flag? tag) (not (null? (tbl-get quest-tbl tag))))
		(qst-set-descr! quest
		
(cond ((tbl-flag? 'done)
		(list
			"You have allied yourself with the Enchanter,"
			"one of the Wise who watch over the shard."
		))
	((tbl-flag? 'talked)
		(append header
		(list
			"You have met with the Enchanter, but you and"
			"he did not come to any agreement."
		)))
	((tbl-flag? 'tower)
		(append header
		(list
			"You have found the Enchanter's Tower in the"
			"Fens. However, getting inside could be more"
			"difficult than reaching it."
		)))
	((tbl-flag? 'directions)
		(append header
		(list
			"The Enchanter's Tower may be found in the"
			"Fens, a swampland north of the town of"
			"Trigrave, in the western part of the"
			"Shard."
		)))
	(#t
		(append header
		(list
			"The message suggests that you ask the"
			"caretaker of the clearing that you arrived in
			"for directions."
		)))
)

		)
	))

;; give some xp for reaching the tower
(define (quest-calltoarms-tower kplace kplayer)
	(quest-data-update-with 'questentry-calltoarms 'tower 1 (grant-xp-fn 5))
	)



