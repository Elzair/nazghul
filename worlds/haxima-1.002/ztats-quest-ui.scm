;; Implements the Quest Log pane of the Ztats UI

(define (zqug-mk) (list nil nil))
(define (zqug-set-rect! gob x y w h) (set-car! gob (list x y w h)))
(define (zqug-get-rect gob) (car gob))
(define (zqug-set-party! gob kparty) (set-car! (cdr gob) kparty))
(define (zqug-get-party gob) (cadr gob))

(kern-ztats-add-pane
 (lambda (zqug kparty dir x y w h)
   (kern-ztats-set-title "Quest Log")
   (zqug-set-rect! zqug x y w h)
   (zqug-set-party! zqug kparty)
   )
 (lambda (zqug dir)
   #f)

 (lambda (zqug)
   (let* ((dims (zqug-get-rect zqug))
          (pgob (gob (zqug-get-party zqug))))

     (define (scrnprn qlst line)
       (if (notnull? qlst)
           (begin
             (kern-screen-print (list (car dims) line (caddr dims) (cadddr dims))
                                0
                                (qst-title (car qlst)))
             (scrnprn (cdr qlst) (+ 16 line))
             )))

     (if (null? pgob)
         (scrnprn "No Quests!")
         (let ((qlst (find-field 'quests pgob)))
           (if (null? qlst)
               (scrnprn "No Quests Yet! (But keep trying!)")
               (scrnprn (cdr qlst) (cadr dims))
               )))))
 (zqug-mk))
