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

(define (zqug-store-max-entry! zqug)
  (let ((pgob (gob (zqug-party zqug))))
    (if (null? pgob)
        0
        (let ((qlst (tbl-get pgob 'quests)))
          (if (not qlst)
              0
              (zqug-max-entry! zqug (1- (length qlst)))
              )))))
 
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
          (winh (/ (rect-h (zqug-dims zqug)) kern-ascii-h))
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
               (let ((rect (rect-down dims (* line kern-ascii-h))))
                 (kern-screen-print rect 0 (qst-title (car qlst)))
                 (if (!= entry cur)
                     (kern-screen-shade rect 128))
                 (scrnprn (cdr qlst) (1+ entry) (1+ line))
                 ))))
     
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
          (qlst (tbl-get pgob 'quests)))
     (if qlst
         (let ((qst (list-ref qlst (zqug-cur-entry zqug))))
           (println "qst" qst)

           ;; paint proc - render the quest details pane
           (define (paint zqag)
             (let ((rect (zqag-dims zqag)))
               (kern-screen-erase rect)
               (kern-screen-print rect kern-sp-centered "^c+c" (qst-title qst) "^c-")
               (kern-screen-print (rect-down rect kern-ascii-h) 0 (qst-descr qst))
               (kern-screen-print (rect-down rect (* 2 kern-ascii-h)) 0 
                                  "^c+GCompleted: "
                                  (if (qst-done? qst)
                                      "^cgYes^c-"
                                      "^cyNo^c-"))
               (kern-screen-update rect)
               ))
           
           (kern-applet-run
            
            ;; run proc - paint & push a keyhandler that exits when player hits ESC
            (lambda (zqag dims)
              (kern-status-set-title "Quest")
              (zqag-dims! zqag dims)
              (paint zqag)
              (kern-event-run-keyhandler
               (lambda (key mod)
                 (cond ((= key kern-key-esc)
                        #t)
                       (else
                        #f)))))
      
            ;; paint
            paint
          
            ;; zqa gob
            (zqag-mk)
            )))))

 ;; zqu gob
 (zqug-mk))
