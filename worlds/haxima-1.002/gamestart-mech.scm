
  
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
	(if (is-player-party-member? kbeing)
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
			)))
		
(define (gamestart-statue-clean kbeing messageid)
	(map (lambda (trigobj)
		(if (equal? (length (gob trigobj)) 3)
			(if (equal? (caddr (gob trigobj)) messageid)
				(kern-obj-remove trigobj)
			))
		)
	(kplace-get-objects-of-type (car (kern-obj-get-location kbeing)) t_step_trig))
	)
				
(define (gamestart-statue-speak kbeing speaker messageid)
	(if (is-player-party-member? kbeing)
		(begin
			(kern-log-msg "A statue suddenly speaks to you!")
			(kern-conv-begin (eval speaker))
			))
			#f
		)

(define (get-gamestart-data kbeing key)
	(let ((rdata (get-roomdata (car (kern-obj-get-location kbeing)))))
		(list-ref rdata key)
    ))
    
(define (gamestart-field-circle ftype loc x y count)
	(define (sign n)
		(cond ((> n 0) 1)
				((< n 0) -1)
				(#t 0))
		)
	(define (is-my-field? kobj) (eqv? ftype (kern-obj-get-type kobj)))
	(define (gamestart-field-circle-elem xp yp toshow tocheck)
		(let* ((xf (+ x xp))
				(yf (+ y yp))
				(show (not (< (* toshow 8) (* tocheck count))))
				(fields (filter is-my-field? (kern-get-objects-at (mk-loc loc xf yf))))
				)
			(cond ((null? fields) nil)
				(else
					(kern-obj-remove (car fields))))
			(if show
				(kern-obj-put-at (kern-mk-obj ftype 1) (mk-loc loc xf yf))
				)
			(if (> tocheck 1)
				(let (
					(ntoshow (if show (- toshow 1) toshow))
					(nxp (sign (- xp yp)))
					(nyp (sign (+ xp yp)))
					)
				(gamestart-field-circle-elem nxp nyp ntoshow (- tocheck 1))
				))
			))	
	(let* ((sa (kern-dice-roll "1d3-2"))
			(sb (if (> 1 (kern-dice-roll "1d2")) 1 -1))
			(xy (kern-dice-roll "1d2"))
			(xp (if (> xy 1) sa sb))
			(yp (if (> xy 1) sb sa))
			)
			(gamestart-field-circle-elem xp yp count 8)
	)
)
    	
(define (gamestart-reset-lamps kbeing)
		(let ((str (floor (+ (/ (* (- (kern-char-get-strength kbeing) 10) 7) 12) 1)))
				(dex (floor (+ (/ (* (- (kern-char-get-dexterity kbeing) 10) 7) 12) 1)))
				(int (floor (+ (/ (* (- (kern-char-get-intelligence kbeing) 10) 7) 12) 1)))
				(place (eval 'p_char_setup)))
			(gamestart-field-circle F_fire_perm place 4 10 str)
			(gamestart-field-circle F_acid_perm place 9 8 dex)
			(gamestart-field-circle F_energy_perm place 14 10 int)
		(kern-map-repaint)
	))
	
(define (gamestart-light-lamps kbeing unused messageid)
	(if (is-player-party-member? kbeing)
		(begin
				(gamestart-reset-lamps kbeing)
	(map (lambda (trigobj)

		(if (equal? (length (gob trigobj)) 3)
			(if (equal? (caddr (gob trigobj)) messageid)
				(kern-obj-remove trigobj)
			))
		)
	(kplace-get-objects-of-type (car (kern-obj-get-location kbeing)) t_step_trig))
	)
	
	)
	#f)

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
	
(mk-obj-type 't_start_portal "path forward" s_blackgate_full layer-mechanism step-trig-ifc)	

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
  (quest-remove (quest-data-get 'questentry-charcreate))
  (quest-assign (quest-data-get 'questentry-whereami))
  )
	