(define (zone-place-tag z) (car z))
(define (zone-x z) (cadr z))
(define (zone-y z) (caddr z))
(define (zone-w z) (cadddr z))
(define (zone-h z) (list-ref z 4))

;; Moongate clearing
(define (mk-zone x y w h) (list 'p_moongate_clearing x y w h))
(define mgc-roadbend  (mk-zone 10 20 3 3))
(define mgc-cave      (mk-zone  1 23 6 3))