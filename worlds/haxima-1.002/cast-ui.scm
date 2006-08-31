;; ui-target-char -- return the first available object for which 'pred' returns
;; true at a user-specified location on the map
(define (ui-target origin range pred)
  (define (select-from seq)
    (cond ((null? seq) 
           nil)
          (else (car seq))))
  (let ((coords (kern-ui-target origin range)))
    (cond ((null? coords) nil)
          (else (select-from (filter pred (kern-get-objects-at coords)))))))

(define (ui-get-direction)
  (kern-ui-direction))

(define (ui-get-adjacent origin pred)
  (define (select-from seq)
    (cond ((null? seq) 
           (kern-print "Nothing!\n") 
           nil)
          (else
           (car seq))))
  (let ((dir (ui-get-direction)))
    (if (null? dir) nil
        (select-from (filter pred (kern-get-objects-at (loc-offset origin dir)))))))

(define (ui-waitkey)
  (kern-ui-waitkey))

(define (mk-ifc-query signal)
  (lambda (kobj) (kobj-can? kobj signal)))

(define (neighbors loc)
  (let ((place (car loc))
        (x (cadr loc))
        (y (caddr loc)))
    (list (list place x (- y 1))
          (list place x (+ y 1))
          (list place (- x 1) y)
          (list place (+ x 1) y))))

(define (get-target-kchar caster range)
  (let ((loc (get-target-loc caster range)))
    (if (null? loc)
        nil
        (get-being-at loc))))

(define (user-cast-ranged-targeted-spell kchar range proc)
  (let ((ktarg (get-target-kchar kchar range)))
    (if (null? ktarg)
        result-no-target
        (begin
          (proc kchar ktarg)
          result-ok))))

;;--------------------------------------------------------------
;; Need to sort this stuff out
;;--------------------------------------------------------------

(define (cast-field-spell caster field-type)
  (let ((coords (kern-ui-target (kern-obj-get-location caster) 1)))
    (cond ((null? coords) nil)
          (else
           (kern-obj-put-at (kern-mk-obj field-type 1) coords)))))

(define (cast-teleport-spell caster dir)
  (let ((coords (loc-offset (kern-obj-get-location caster) dir)))
    (cond ((null? coords) (kern-print "You sense nothing there!\n"))
          ((not (passable? coords caster)) (kern-print "You sense it is impassable!\n"))
          (else (kern-obj-relocate caster coords nil)))))

(define (cast-signal-spell caster signal target)
  (cond ((null? target) result-no-target)
        (else 
         ((kobj-ifc target) signal target caster)
         result-ok
         )))

(define (cast-bimodal caster proc)
  (define (cast-it target)
    (cond ((null? target) result-no-target)
          (else (proc target))))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
  
(define (user-cast-spell-on-party-member caster proc)
  (define (cast-it target)
    (cond ((null? target) result-no-target)
          (else (proc caster target)
                result-ok)))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
	  
(define (user-cast-ranged-targeted-spell kchar range proc)
  (let ((ktarg (get-target-kchar kchar range)))
    (if (null? ktarg)
        result-no-target
        (begin
          (proc kchar ktarg)
          result-ok))))  
	  
	  
	  
(define (cast-ui-target-party-member caster range)
	(let ((loc (kern-obj-get-location caster)))
		(if (kern-place-is-wilderness? (loc-place loc))
			(kern-ui-select-party-member)
			(ui-target loc range obj-is-char?))))
	   
(define (cast-ui-target-char caster range)
	(let ((loc (get-target-loc caster range)))
		(if (null? loc)
			nil
			(get-being-at loc))))
			
			
			

(define (cast-ui-dospell target effect caster power)
	(if (null? target)
		(begin 
			result-no-target
			)
		(begin
			(effect caster target power)
			result-ok)))
			
			
			
			
(define (cast-ui-basic-member-spell effect caster power)
	(cast-ui-dospell
		(cast-ui-target-party-member caster 2)
		effect caster power))
		
(define (cast-ui-basic-ranged-spell effect caster range power)
	(cast-ui-dospell
		(cast-ui-target-char caster range)
		effect caster power))

