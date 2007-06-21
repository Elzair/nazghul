(define (get-player-name kbeing)
      (begin
        (kern-log-msg "Speak your name, Seeker")
		(let
      		((reply (kern-conv-get-string kbeing)))
            (if (equal? reply "")
				nil
            	(begin
            		(kern-log-msg (string-append "Hail, " reply "!"))
                 	(kern-being-set-name kbeing reply)
					(kern-map-flash 1)
               )
            )
        ) 
		#f
		))
		
(define (one-off-message kbeing message messageid)
	(begin
		(kern-log-msg message)
		(map (lambda (trigobj)
			(if (equal? (length (gob trigobj)) 3)
				(if (equal? (caddr (gob trigobj)) messageid)
					(kern-obj-remove trigobj)
				))
			)
			(kplace-get-objects-of-type (car (kern-obj-get-location kbeing)) t_step_trig))
		#f
		))

(define (get-gamestart-data kbeing key)
	(let ((rdata (get-roomdata (car (kern-obj-get-location kbeing)))))
		(list-ref rdata key)
    ))
	
(define (set-gamestart-data kbeing key value)
	(println (get-roomdata (car (kern-obj-get-location kbeing))))
	(let* ((rdata (get-roomdata (car (kern-obj-get-location kbeing))))
			(curdat (list-tail rdata key)))
		(set-car! curdat value)
    ))

(define (initstats kbeing)
	(let ((rdata (get-roomdata (car (kern-obj-get-location kbeing)))))
		(kern-char-set-strength     kbeing (/ (- 24    (list-ref rdata 0)  (list-ref rdata 1)) 2))
		(kern-char-set-dexterity    kbeing (/ (- (+ 12 (list-ref rdata 0)) (list-ref rdata 2)) 2))
		(kern-char-set-intelligence kbeing (/ (+       (list-ref rdata 1)  (list-ref rdata 2)) 2))

		(kern-log-msg "Strength: "     (number->string (kern-char-get-strength     kbeing)) ", "
			      "Dexterity: "    (number->string (kern-char-get-dexterity    kbeing)) ", "
			      "Intelligence: " (number->string (kern-char-get-intelligence kbeing))
			      )

	))
	
(define (set-stat-info kbeing key value)
	(set-gamestart-data kbeing key value)
	(initstats kbeing)
	(kern-sound-play sound-moongate-enter)
	(kern-map-flash 1)
	#f
	)
	
(mk-obj-type 't_start_portal "path forward" s_moongate_full layer-mechanism step-trig-ifc)	

(define (mk-start-portal proc-tag . args)
  (bind (kern-mk-obj t_start_portal 1)
        (trig-mk proc-tag args)))

(define (start-cutscene kplayer startgate)
	(kern-char-set-sleep ch_wanderer #t)
	(kern-log-enable #t)
	(kern-log-msg "A dark gate rises in a quiet clearing...")
	(moongate-animate black-gate blackgate-stages)
	(kern-sleep 2000)	
	(kern-log-enable #f)
	)

(define (mk-start-cutscene kplayer startgate)
	(lambda () (start-cutscene kplayer startgate)))
	
(define (start-actual-game kplayer)
  (initstats kplayer)
    
  (kern-log-enable #f)

  (kern-obj-set-sprite (eval (get-gamestart-data kplayer 3)) s_grass)
   (kern-map-repaint)
	
  (kern-obj-relocate kplayer (list p_moongate_clearing 11 12) (mk-start-cutscene kplayer (get-gamestart-data kplayer 3)))

  (kern-log-enable #t)
  (kern-log-msg "Then closes without a trace...")
  (moongate-animate black-gate (reverse blackgate-stages))
  (kern-sleep 1000)
  
  (kern-log-msg "You lie dreaming for a while, of another life...")
  (kern-sleep 2000)

  (kern-log-enable #f)
  (kern-char-set-sleep ch_wanderer #f)
  (kern-player-set-follow-mode)
  (kern-log-enable #t)  
  (kern-log-msg "...then awaken to a strange new world.")
  (kern-log-msg "To the southwest you see a cave.")

  )
	