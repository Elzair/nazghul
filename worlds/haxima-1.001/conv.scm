(define (generic-hail knpc kpc)
  (kern-conv-say knpc "Well met"))

(define (generic-unknown knpc kpc)
  (kern-conv-say knpc "I can't help thee with that"))

(define (generic-bye knpc kpc)
  (kern-conv-say knpc "Farewell")
  (kern-conv-end))

(define basic-conv
  (ifc '()
       (method 'hail generic-hail)
       (method 'default generic-unknown)
       (method 'bye generic-bye)
       ))

;; Helper
(define (say knpc . msg) (kern-conv-say knpc msg))
