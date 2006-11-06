(define (trig-mk proc-tag args) (cons proc-tag args))
(define (trig-proc trg) (eval (car trg)))
(define (trig-args trg) (cdr trg))
(define (trig-invoke trg . more-args)
  (println "more-args: " more-args)
  (println "trig-args: " (trig-args trg))
  (apply (trig-proc trg) 
         (append more-args (trig-args trg))))

;;----------------------------------------------------------------------------
;; Step trigger -- executes a named procedure whan a character steps on it.
;; The procedure should expect a kernel being as the first arg followed by
;; the optional args.
;;----------------------------------------------------------------------------
(define (step-trig-exec ktrig kbeing)
  (let ((trg (gob ktrig)))
    (if (trig-invoke trg kbeing)
        (kern-obj-remove ktrig))))

(define step-trig-ifc
  (ifc '()
       (method 'step step-trig-exec)))

(mk-obj-type 't_step_trig nil nil layer-mechanism step-trig-ifc)

(define (mk-step-trig proc-tag . args)
  (bind (make-invisible (kern-mk-obj t_step_trig 1))
        (trig-mk proc-tag args)))

;;-----------------------------------------------------------------------------
;; Sense trigger -- just like a step trigger, but responds to the 'sense signal
;; instead, which is sent anytime a character enters or leaves its tile
;;-----------------------------------------------------------------------------
(define sense-trig-ifc
  (ifc '()
       (method 'sense step-trig-exec)))

(mk-obj-type 't_sense_trig nil nil layer-mechanism sense-trig-ifc)

(define (mk-sense-trig proc-tag . args)
  (bind (make-invisible (kern-mk-obj t_sense_trig 1))
        (trig-mk proc-tag args)))

;;----------------------------------------------------------------------------
;; Procedure for use with step or sense triggers. kchar is the character which
;; caused the trigger by stepping on the tile (or off it, in the case of a
;; sense trigger). target-tag is the object which will receive the message
;; sigval.
;;
;; Example:
;;
;;   (put (kern-tag 'p1 (mk-portcullis)) 3 4)
;;   (put (mk-sense-trig 'generic-trig-exec 'p1 'signal) 10 23)
;;
;; Whenever anybody enters tile (10, 23), the portcullis at (3, 4) will open in
;; response to the "signal" message. . When they leave, it will close again.
;;----------------------------------------------------------------------------
(define (generic-trig-exec kchar target-tag sigval)
  (send-signal kchar (eval target-tag) sigval)
  #f)

;;----------------------------------------------------------------------------
;; 'on trigger -- object which executes a named procedure when it gets the 'on
;; signal from something.
;;----------------------------------------------------------------------------
(define (on-trig-exec ktrig)
  (let ((trg (gob ktrig)))
    (if (trig-invoke trg ktrig)
        (kern-obj-remove ktrig))))

(define on-trig-ifc
  (ifc '()
       (method 'on on-trig-exec)))

(mk-obj-type 't_on_trig nil nil layer-mechanism on-trig-ifc)

(define (mk-on-trig proc-tag args)
  (bind (make-invisible (kern-mk-obj t_on_trig 1))
        (trig-mk proc-tag args)))

;;------------------------------------------------------------------------
;; sensor pad - sends a remote-sensor ifc call when it detects someone
;;	enters or leaves its tile
;;------------------------------------------------------------------------

(define char-sensor-ifc
  (ifc '() (method 'sense
                   (lambda (ksensor kuser)
                     (send-signal kuser (eval (gob ksensor)) 'remote-sensor)
                     ))
       ))

(mk-obj-type 't_char_sensor nil nil layer-mechanism char-sensor-ifc)

(define (mk-char-sensor target-tag)
  (bind (make-invisible (kern-mk-obj t_char_sensor 1))
        target-tag))

;;----------------------------------------------------------------------------
;; Terrain-changer -- procedure for a step trigger to set the terrain at (x, y)
;; to kter. kbeing triggered the step. This returns #t so that it is used only
;; once.
;;
(define (terrain-changer kbeing x y kter)
  (kern-place-set-terrain (list (get-place kbeing) x y)
                          (eval kter))
  #t)