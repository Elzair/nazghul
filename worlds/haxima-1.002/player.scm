;; This defines the gob for the player party in haxima.
(define (player-gob) (gob (kern-get-player)))
(define (mk-player-gob) (list 
                         #f ;; found warritix
                         #f ;; trial of stewardess done
                         ))
(define (player-found-warritrix?) (list-ref (player-gob) 0))
(define (player-found-warritrix!) (set-car! (list-tail (player-gob) 0) #t))
(define (player-stewardess-trial-done?) (list-ref (player-gob) 1))
(define (player-stewardess-trial-done!) (set-car! (list-tail (player-gob) 1) #t))
