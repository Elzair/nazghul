;; This defines the gob for the player party in haxima.
;; This sort of thing would probably be best stored in quest flags.
(define (player-gob) (gob (kern-get-player)))
(define (mk-player-gob) (list 
                         #f ;; found warritix
                         #f ;; trial of stewardess done
                         ))
(define (player-found-warritrix?) (not (null? (quest-data-getvalue 'questentry-warritrix 'found))))
(define (player-stewardess-trial-done?) (not (null? (quest-data-getvalue 'questentry-warritrix 'avenged))))

