;; ----------------------------------------------------------------------------
;; kobj.scm - utilities related to kernel-objects
;; ----------------------------------------------------------------------------
(define (kobj-ktype kobj) (kern-obj-get-type kobj))
(define (kobj-ifc kobj) ( kern-type-get-gifc (kobj-ktype kobj)))
(define (kobj-gob kobj) (kern-obj-get-gob kobj))
(define (kobj-gob-data kobj) (gob-data (kobj-gob kobj)))

(define (signal-kobj kobj sig . args)
  (apply (kobj-ifc kobj) (cons sig args)))

(define (kobj-can? kobj signal)
  (let ((gifc (kobj-ifc kobj)))
    (cond ((null? gifc) #f)
          (else (gifc 'can signal)))))
  
