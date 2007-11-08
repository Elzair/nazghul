;;------------------------------------------------------------
;; *special* mirror

(define (mag-mirror-active? mir)
		(car (list-ref (gob mir) 3))
	)

(define (mag-mirror-active! mir val)
		(set-car! (list-ref (gob mir) 3) val)
	)

(define (mag-mirror-have-target mir)
		(let ((loc (list-ref (gob mir) (if (mag-mirror-active? mir) 1 2))))
			(if (null? (portal-place loc))
				#f
				#t
				))
	)

(define (mag-mirror-target-loc mir)
		(portal-coords (list-ref (gob mir) (if (mag-mirror-active? mir) 1 2)))
	)
	
(define (mag-mirror-background mir)
		(eval (list-ref (gob mir) 0))
	)
	
(define (mag-mirror-handler mir)
		(list-ref (gob mir) 4)
	)
	

(define mag-mirror-ifc
  (ifc '()
       (method 'handle 
               (lambda (kmirror kuser)
					((eval (mag-mirror-handler kmirror)) kmirror kuser)
                 ))
       (method 'step
               (lambda (kmirror kuser)
                 ))
       (method 'remote-sensor
               (lambda (kmirror kuser) 
                 (let* ((mirror-loc (kern-obj-get-location kmirror))
                        (target-loc (mag-mirror-target-loc kmirror))
                        (character (get-char-at target-loc)))
                   (if (null? character)
                       (kern-obj-set-sprite kmirror (mk-composite-sprite (list s_mirror_bg (mag-mirror-background kmirror) s_mirror_fg)))
                       (kern-obj-set-sprite kmirror (mk-composite-sprite (list s_mirror_bg (kern-obj-get-sprite character) (mag-mirror-background kmirror) s_mirror_fg))))
                   (kern-map-set-dirty)
                   )
				))
	   (method 'on
               (lambda (kmirror kuser)
					(mag-mirror-active! kmirror #t)
					(if (mag-mirror-have-target kmirror)
						(send-signal kuser kmirror 'remote-sensor))
                 ))
		(method 'off
               (lambda (kmirror kuser)
					(mag-mirror-active! kmirror #f)
					(if (mag-mirror-have-target kmirror)
						(send-signal kuser kmirror 'remote-sensor))
                 ))
       (method 'init
               (lambda (kmirror)
					(kern-obj-set-pclass kmirror pclass-wall)
                 ))
		(method 'exec
                 (lambda (kclock)
                   (kern-sound-play-ambient sound-clock (kern-obj-get-location kclock))
                   ))	
		(method 'on-entry
                 (lambda (kclock)
                   (kern-sound-play-ambient sound-clock (kern-obj-get-location kclock))
                   ))	
       ))

(mk-obj-type 't_mag_mirror "mirror"
	'()
	layer-mechanism mag-mirror-ifc)
	
(define (mk-mag-mirror background-tag target-loc-a target-loc-i active handler)
	(let ((kmirror (kern-mk-obj t_mag_mirror 1)))
		(bind kmirror (list background-tag target-loc-a target-loc-i (list active) handler))
		(kern-obj-set-sprite kmirror (mk-composite-sprite (list s_mirror_bg (eval background-tag) s_mirror_fg)))
		kmirror))
		
(define doormirhandler 
			(lambda (kmirror kuser)
				(if (mag-mirror-active? kmirror)
					(let ((target-loc (mag-mirror-target-loc kmirror)))
						(kern-log-msg (kern-obj-get-name kuser) " steps through the mirror!")
						(kern-obj-relocate kuser target-loc nil))
					(let ((target-loc (kern-obj-get-location kmirror))
							(clone (kern-obj-clone kuser)))
						(println (kern-char-get-arms kuser))
						(map (lambda (ktype)
							(kern-obj-add-to-inventory clone ktype 1))
							(kern-char-get-arms kuser))
						(kern-char-arm-self clone)
						(kern-char-set-level clone (kern-char-get-level kuser))
						(kern-being-set-base-faction clone faction-monster)
						(kern-char-set-ai clone 'spell-sword-ai)
						(kern-obj-put-at clone target-loc)
						(kern-log-msg (kern-obj-get-name kuser) "'s reflection steps through the mirror!")
						)
				)))

(define testmirhandler 
			(lambda (kmirror kuser)
				(kern-log-msg (kern-obj-get-name kuser) " spots "
					(if (mag-mirror-active? kmirror)
							(let* ((target-loc (mag-mirror-target-loc kmirror))
									(character (get-char-at target-loc)))
								(if (null? character)
									"nothing"
									(kern-obj-get-name character)
								))
							(kern-obj-get-name kuser))
						" in the mirror"
						(if (mag-mirror-active? kmirror) "!" "")
						)
				))

;;---------------------------
;; send an ifc to multiple targets

(define (send-to-all kuser tags signal)
  (if (defined? (car tags))
      (send-signal kuser (eval (car tags)) signal))
  (if (not (equal? (cadr tags) '()))
      (send-to-all kuser (cdr tags) signal)
      ))

(define sig-splitter-ifc
  (ifc '() (method 'remote-sensor
                   (lambda (ksensor kuser)
                       (send-to-all kuser (gob ksensor) 'remote-sensor)
                     ))
			(method 'on
                   (lambda (ksensor kuser)
                       (send-to-all kuser (gob ksensor) 'on)
                     ))
			(method 'off
                   (lambda (ksensor kuser)
                       (send-to-all kuser (gob ksensor) 'off)
                     ))
			(method 'init tblit-init)
       ))

(mk-obj-type 't_sig_splitter nil nil layer-mechanism sig-splitter-ifc)

(define (mk-sig-splitter target-tags)
  (bind (kern-mk-obj t_sig_splitter 1)
        target-tags))
		
;;---------------------
;; reversible terrain mod

(define bim-secret-ifc
  (ifc '()  (method 'on
                   (lambda (ksensor kuser)
						(if (not (equal? (safe-eval (car (caddr (gob ksensor)))) '()))
							(kern-place-set-terrain
								(portal-coords (caddr (gob ksensor)))
								(eval (car (gob ksensor))))
							)
                     ))
			(method 'off
                   (lambda (ksensor kuser)
						(if (not (equal? (safe-eval (car (caddr (gob ksensor)))) '()))
							(kern-place-set-terrain
								(portal-coords (caddr (gob ksensor)))
								(eval (cadr (gob ksensor))))
							)
                     ))
			(method 'init tblit-init)
       ))

(mk-obj-type 't_bim_secret nil nil layer-mechanism bim-secret-ifc)

(define (mk-bim-secret terrain-on terrain-off target)
  (bind (kern-mk-obj t_bim_secret 1)
	(list terrain-on terrain-off target)))

;;-------------------
;; moving bookshelf -should use bim?

(define moving-shelf-ifc
  (ifc '()  (method 'search
                   (lambda (shelf)
					   (kern-log-msg "You find a hidden mechanism!")
                     ))
			(method 'handle
                   (lambda (shelf kuser)
                       (kern-log-msg "The shelf moves!")
					   (let ((data (gob shelf)))
							(set-car! data (not (car data)))
							(send-signal kuser (eval (list-ref data 3))
								(if (car data) 'on 'off))
							(kern-obj-relocate shelf 
								(portal-coords (if (car data) (cadr data) (caddr data)))
								nil))
                     ))
		   (method 'init
				   (lambda (shelf)
						(kern-obj-set-pclass shelf pclass-wall)
					 ))
		))

(mk-obj-type 't_moving_shelf "set of shelves"
	s_bookshelf layer-mechanism moving-shelf-ifc)

(define (mk-moving-shelf loc-open loc-closed trigger)
  (bind (kern-mk-obj t_moving_shelf 1)
	(list #f loc-open loc-closed trigger)))

(mk-reusable-item 
 't_mans_note "note" s_scroll norm
 (lambda (kletter kuser)
   (kern-ui-page-text
		"Short Note"
		"Hey Enchanter:"
		""
		"This room is _awfully_ dusty..."
		"Clean up more often!"
		"   -- the MAN"
   )))
   
   
;;--------------------------
;; magic clock. runs backwards. casts time stop


(define mag-clock-ifc
  (let ((mod
			(lambda (input modulus)
				(- input (* (floor (/ input modulus)) modulus))
			)))
  (ifc '()
		(method 'handle 
			(lambda (kclock kuser)
				(kern-log-msg "The clock hands stop moving!")
				(powers-timestop kuser kuser 30)
			))
		(method 'xamine 
			(lambda (kclock kuser)
				(let* ((time (kern-get-time))
						(hour (floor (/ (* 12 (- 60 (time-minute time))) 60)))
						(hour (number->string
							(if (< 1 hour) hour	12)))
						(min (- 6 (mod (time-minute time) 6)))
						(min (if (> min 5)
								"00"
								(number->string (* 10 min)))))
					(kern-log-msg "The clock reads " hour ":" min)
				)))
		(method 'step
			(lambda (kmirror kuser)
				))
		(method 'update-gfx
			(lambda (kclock)
				(let* ((time (kern-get-time))
						(min-hand (clock-get-hand (- 60 (time-minute time))))
						(hour-hand (clock-get-hand (floor (/ (- 65 (time-minute time)) 10)))))
					(kern-obj-set-sprite kclock (mk-composite-sprite (list s_clock_body hour-hand min-hand)))
				)))
		(method 'init
            (lambda (kmirror)
				(kern-obj-set-pclass kmirror pclass-wall)
            ))	
       )))

(mk-obj-type 't_mag_clock "clock"
	(mk-composite-sprite (list s_clock_body s_clock_hand_n s_clock_spin))
	layer-mechanism mag-clock-ifc)

(define (mk-mag-clock)
	(let ((kclock (kern-mk-obj t_mag_clock 1)))
		(kern-obj-add-effect kclock ef_graphics_update nil)
		(bind kclock nil)
		kclock))
