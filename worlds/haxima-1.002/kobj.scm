;; ----------------------------------------------------------------------------
;; kobj.scm - utilities related to kernel-objects
;; ----------------------------------------------------------------------------
(define (kobj-ktype kobj) (kern-obj-get-type kobj))
(define (kobj-ifc kobj) ( kern-type-get-gifc (kobj-ktype kobj)))
(define (kobj-gob kobj) (kern-obj-get-gob kobj))
(define (kobj-gob-data kobj) (gob-data (kobj-gob kobj)))

;; gob -- convenience accessor for getting the interesting part of a gob from
;; the kernel object
(define (gob kobj) (gob-data (kobj-gob kobj)))

(define (signal-kobj kobj sig . args)
  ;;(display "signal-kobj")(newline)
  (if (not (null? (kobj-ifc kobj)))
  	(apply (kobj-ifc kobj) (cons sig args))
  ))

(define (kobj-can? kobj signal)
  (let ((gifc (kobj-ifc kobj)))
    (cond ((null? gifc) #f)
          (else (gifc 'can signal)))))
	  
(define (ktype-can? ktype signal)
  (let ((gifc (kern-type-get-gifc  ktype)))
    (cond ((null? gifc) #f)
          (else (gifc 'can signal)))))
  
(define (kobj-place kobj)
  (loc-place (kern-obj-get-location kobj)))

(define (can-be-dropped? obj loc max_difficulty)
	(let ((mcost (kern-place-get-movement-cost loc obj)))
		(and (not (eqv? mcost 0))
			(< mcost max_difficulty))))
			