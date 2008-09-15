(define (quest-assign-always target)
	#t)
	
(define (quest-status-from-payload quest)
	"In progress"
	)

(define (quest-status-inprogress quest)
	"In progress"
	)

(define (quest-data-get title)
	(let* ((questdata (tbl-get (gob (kern-get-player)) 'questdata))
			(questinprog (quest-get title))
			)
		(if (null? questinprog)
			(tbl-get questdata title)
			questinprog
		)
	))

(define (quest-update-text title decription)
	(let ((quest quest-data-get title))
	))