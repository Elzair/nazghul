;; This defines the gob for the player party in haxima.
(define (mk-player-gob) (list #f))
(define (player-found-warritrix? gob) (car gob))
(define (player-found-warritrix! gob) (set-car! gob #t))
(define (player-gob) (gob (kern-get-player)))
