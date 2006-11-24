;; The Eye of Brune is a special mechanism that shows a map of the entire Shard
;; when handled successfully.

(define (eye-of-brune-handle keye khandler)
  (kern-obj-set-sprite keye s_eye_open)
  (kern-log-msg "*** STENTORIAN VOICE ***")
  (kern-log-msg "WHAT WOULDST THOU BE?")
  (let ((answer (kern-conv-get-reply khandler)))
    (cond ((eq? answer 'vigi)
           (kern-log-msg "THEN SEE!")
           (let ((kimage (kern-image-load "map.png")))
             (kern-map-set-image kimage)
             (kern-print "Hit a key when done gazing...\n")
             (ui-waitkey)
             (kern-map-set-image nil)
             (kern-image-free kimage)))
          (else
           (kern-log-msg "WOE TO THE CARELESS, THE FORGETFUL AND THE IMPIOUS!")
           (apply-lightning khandler)
           (kern-char-set-intelligence khandler
                                       (- (kern-char-get-base-intelligence khandler) 
                                          1))
           (kern-log-msg (kern-obj-get-name khandler) " loses intelligence!")
           )))
  (kern-obj-set-sprite keye s_eye_closed)
  )

(define eye-of-brune-ifc
  (ifc nil
       (method 'handle eye-of-brune-handle)
       ))

(mk-obj-type 't_eye_of_brune "Eye of Brune" s_eye_closed layer-mechanism eye-of-brune-ifc)
