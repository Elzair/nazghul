;; Implements the Quest Log pane of the Ztats UI

(define (zqug-mk) (list nil nil 0 0 0))
(define (zqug-dims! gob dims) (set-car! gob dims))
(define (zqug-dims gob) (list-ref gob 0))
(define (zqug-party! gob kparty) (set-car! (cdr gob) kparty))
(define (zqug-party gob) (list-ref gob 1))
(define (zqug-cur-entry gob) (list-ref gob 2))
(define (zqug-cur-entry! gob val) (list-set-ref! gob 2 val))
(define (zqug-max-entry! gob val) (list-set-ref! gob 3 val))
(define (zqug-max-entry gob) (list-ref gob 3))
(define (zqug-top-entry gob) (list-ref gob 4))
(define (zqug-top-entry! gob val) (list-set-ref! gob 4 val))

(define sprite-offset-x 40)
(define sprite-offset-y (/ (- 32 kern-ascii-h) 2))
(define sprite-lineheight 40)

(define (zqug-store-max-entry! zqug)
  (let ((pgob (gob (zqug-party zqug))))
    (if (null? pgob)
        0
        (let ((qlst (tbl-get pgob 'quests)))
          (if (null? qlst)
              0
              (zqug-max-entry! zqug (1- (length qlst)))
              )))))

(if use-quest-pane

(kern-ztats-add-pane

 ;; enter
 (lambda (zqug kparty dir dims)
   (kern-status-set-title "Quest Log")
   (zqug-dims! zqug dims)
   (zqug-party! zqug kparty)
   (zqug-store-max-entry! zqug)
   )

 ;; scroll
 (lambda (zqug dir)
   (let* ((top (zqug-top-entry zqug))
          (cur (zqug-cur-entry zqug))
          (max (zqug-max-entry zqug))
          (winh (/ (rect-h (zqug-dims zqug)) sprite-lineheight))
          (midwin (/ winh 2))
          (maxtop (- (1+ max) winh))
         )

     (define (up n top cur)
       (cond ((and (> cur 0) (> n 0))
              (if (and (> top 0)
                       (< cur (- max midwin)))
                  (up (1- n) (1- top) (1- cur))
                  (up (1- n) top (1- cur)))
              )
             (else
              (zqug-top-entry! zqug top)
              (zqug-cur-entry! zqug cur)
              )))

     (define (down n top cur)
       (cond ((and (< cur max) (> n 0))
              (if (and (< top maxtop)
                       (>= cur midwin))
                  (down (1- n) (1+ top) (1+ cur))
                  (down (1- n) top (1+ cur)))
              )
             (else
             (println "down" top " " cur)
              (zqug-top-entry! zqug top)
              (zqug-cur-entry! zqug cur)
              )))

     (cond ((= dir scroll-up) (up 1 top cur) #t)
           ((= dir scroll-down) (down 1 top cur) #t)
           ((= dir scroll-pageup) (up winh top cur) #t)
           ((= dir scroll-pagedown) (down winh top cur) #t)
           ((= dir scroll-top)
            (zqug-top-entry! zqug 0)
            (zqug-cur-entry! zqug 0)
            #t)
           ((= dir scroll-bottom)
            (zqug-top-entry! zqug maxtop)
            (zqug-cur-entry! zqug max)
            #t)
           (else #f)
           )))

 ;; paint
 (lambda (zqug)
   (let* ((dims (zqug-dims zqug))
          (pgob (gob (zqug-party zqug)))
          (winh (/ (rect-h dims) kern-ascii-h))
          (top (zqug-top-entry zqug))
          (cur (zqug-cur-entry zqug))
          )

	(define (scrnprn qlst entry line)
		(if (and (notnull? qlst)
	        	(< line winh))
	  		 (if (< entry top)
	       		(scrnprn (cdr qlst) (1+ entry) line)
	       		(let* ((rect (rect-crop-down dims (* line sprite-lineheight)))
							(icon (safe-eval (qst-icon (car qlst))))
							(colorplus (cond ((qst-complete? (car qlst)) "^c+g")
										((qst-failed? (car qlst)) "^c+r")
										(#t "^c+w")))
							(rect (if (null? (quest-tbl-get (car qlst) 'qparent))
										rect
										(rect-crop-offset rect 12 0)))
						)
					(if (> (rect-h rect) 0)
						(begin
							(if (not (null? icon))
								(kern-screen-draw-sprite rect 0 icon)
							)
							(kern-screen-print (rect-offset rect sprite-offset-x sprite-offset-y) 0 colorplus (qst-title (car qlst)) "^c-" )
							(if (!= entry cur)
								(kern-screen-shade rect 128)
							)
							(scrnprn (cdr qlst) (1+ entry) (1+ line))
						))
				))
		))
     
     (if (null? pgob)
         (scrnprn "No Quests!")
         (let ((qlst (tbl-get pgob 'quests)))
           (if (not qlst)
               (scrnprn "No Quests Yet! (But keep trying!)")
               (scrnprn qlst 0 0)
               )))))

 ;; select proc - run the ztats quest applet
 (lambda (zqug)

   ;; ztats quest applet gob
   (define (zqag-mk) (list nil))
   (define (zqag-dims! zqag val) (set-car! zqag val))
   (define (zqag-dims zqag) (car zqag))

   (let* ((pgob (gob (zqug-party zqug)))
          (qlst (tbl-get pgob 'quests))
			 (offset-page 0)
				)
     (if qlst
         (let ((qst (list-ref qlst (zqug-cur-entry zqug))))

				(qst-status qst)

           ;; paint proc - render the quest details pane
           (define (paint zqag)
             (let* ((rect (zqag-dims zqag))
	             (icon (safe-eval (qst-icon qst)))
	             (line-offset sprite-lineheight)
                 (display-lines-available (floor (/ (- (rect-h rect) sprite-lineheight kern-ascii-h) kern-ascii-h)))
	             (max-offset (length (qst-descr qst)))
	             )
               (kern-screen-erase rect)
            	(if (not (null? icon))
             		(kern-screen-draw-sprite rect 0 icon)
             	)

               (kern-screen-print (rect-down rect sprite-offset-y) kern-sp-centered "^c+c" (qst-title qst) "^c-")
           
               ;; set offset to sane values
               (if (> offset-page (- max-offset display-lines-available))
                     (set! offset-page (- max-offset display-lines-available))
               )
               (if (< offset-page 0)
                     (set! offset-page 0)
               )
               
					(let ((offset-loop offset-page))
				(map (lambda (line)
					(if (and (eqv? offset-loop 0) (< (+ line-offset kern-ascii-h kern-ascii-h) (rect-h rect)))
						(begin
					(set! line-offset (+ line-offset kern-ascii-h))
					(kern-screen-print (rect-down rect line-offset) 0 line )
						)
						(set! offset-loop (- offset-loop 1))
					))
					(qst-descr qst)
				))		

               (kern-screen-update rect)
               ))
           
           (kern-applet-run
            
            ;; run proc - paint & push a keyhandler that exits when player hits ESC
            (lambda (zqag dims)
              ;;(kern-status-set-title "Quest") //doesnt work right
              (zqag-dims! zqag dims)
              (paint zqag)
              (kern-event-run-keyhandler
               (lambda (key mod)	
                 (cond ((or (= key kern-key-esc)
									(= key kern-key-space)
									(= key kern-key-return)
									(= key kern-key-enter))
                        #t)
								((= key kern-key-up)
									(if (> offset-page 0)
										(set! offset-page (- offset-page 1))
									)
									(paint zqag)
									#f
								)
								((= key kern-key-down)
									(set! offset-page (+ offset-page 1))
									(paint zqag)
									#f
								)
								((or (= key kern-key-pgup) (= key kern-key-kp-pgup))
									(set! offset-page (- offset-page (- (floor (/ (rect-h dims) kern-ascii-h)) 7)))
									(if (< offset-page 0)
										(set! offset-page 0)	
									)
									(paint zqag)
									#f
								)
								((or (= key kern-key-pgdn) (= key kern-key-kp-pgdn))
									(set! offset-page (+ offset-page (- (floor (/ (rect-h dims) kern-ascii-h)) 7)))
									(paint zqag)
									#f
								)
                       (else 
                        #f)))))
      
            ;; paint
            paint
          
            ;; zqa gob
            (zqag-mk)
            )))))

 ;; zqu gob
 (zqug-mk))
 
)
