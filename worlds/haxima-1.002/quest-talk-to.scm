(define (qst-talk-to-assign qst target) 
  #t)

(define (qst-talk-to-status qst) 
  "Incomplete")

(define (qst-talk-to-mk kchar-tag)
  (let ((kchar (safe-eval kchar-tag)))
    (if (notnull? kchar)
        (qst-mk (string-append "Talk to " (kern-obj-get-name kchar))
                (string-append "Find and talk to " (kern-obj-get-name kchar))
                'qst-talk-to-assign
                'qst-talk-to-status
                'talk-to kchar-tag))
    ))

(define (qst-is-talk-to? qst)
  (let ((payload (qst-payload qst)))
    (and (pair? payload)
         (eq? (car payload)
              'talk-to))))

(define (qst-talk-to-kchar qst)
  (safe-eval (cadr (qst-payload qst))))

(define (qst-talk-to-finish qst)
  (kern-log-msg "^c+gYou have completed the quest ^c+w" (qst-title qst) "^c-!^c-")
  (qst-done! qst)
  )
  

(kern-add-hook 'conv_start_hook
               (lambda (kpc knpc)
                 (let ((qlst (find-field 'quests (gob (kern-get-player)))))
                   (for-each (lambda (qst)
                               (if (and (not (qst-done? qst))
                                        (qst-is-talk-to? qst)
                                        (equal? (qst-talk-to-kchar qst)
                                             knpc))
                                   (qst-talk-to-finish qst)))
                             (cdr qlst)))))