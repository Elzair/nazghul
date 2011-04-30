;;============================================================================
;; This defines the gob for the player party in haxima.

;;----------------------------------------------------------------------------
;; Generic functions

(define (player-has? key)
  (not (null? (tbl-get (gob (kern-get-player)) key))))

(define (player-get key)
  (tbl-get (gob (kern-get-player)) key))

(define (player-set! key val)
  ;(tbl-set! (gob (kern-get-player)) key val))
  (tbl-set! (gob (kern-get-player)) key val))

;; Update older versions of saved games with new player gob fields.
(define (player-reconcile-gob kplayer)
  (if (not (player-has? 'rep))
      (player-set! 'rep 0))
  (if (not (player-has? 'rapsheet))
      (player-set! 'rapsheet '()))
  ;; update the dtable
  (kern-mk-dtable
   ;;      non pla men cgb acc mon tro spd out gnt dem fgb prs gla plaout              
   (list   2   0   0   0  -1  -2  -2  -2   0  -2  -2   0   0   0   0) ;; none
   (list   0   2   2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2   2   2) ;; player
   (list  -1   2   2  -1  -2  -2  -2  -2  -2  -2  -2  -2   2   2  -2) ;; men
   (list  -1  -2  -2   2  -1  -2   0  -2  -2  -1  -2  -2   0  -2  -2) ;; cave goblin
   (list  -1  -2  -1  -1   2  -2  -1  -1  -2  -1  -2  -2   0  -2  -2) ;; accursed
   (list  -2  -2  -2  -2  -2   2  -2   0  -2   0  -2   0   0  -2  -2) ;; monsters
   (list  -2  -2  -2   0  -1  -2   2  -2  -2  -1  -2  -1   0  -2  -2) ;; hill trolls
   (list  -2  -2  -2  -2  -1   0  -2   2  -2  -1  -2   0   0  -2  -2) ;; wood spiders
   (list   0  -2  -2  -2  -2  -2  -2  -2   2  -2  -2  -1   0  -2   0) ;; outlaws
   (list  -2  -2  -2  -1  -1   0  -1  -1  -2   2  -2  -1   0  -2  -2) ;; gint
   (list  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2  -2   0  -2  -2) ;; demon
   (list   0  -2  -2  -2  -2   0  -2   0  -1  -1  -2   2   0  -2  -2) ;; forest goblin
   (list   0   2   2   0   0   0   0   0   0   0   0   0   2   2   2) ;; prisoners
   (list  -1   2   2  -1  -2  -2  -2  -2  -2  -2  -2  -2   2   2  -2) ;; glasdrin
   (list   0   2  -2  -2  -2  -2  -2  -2   0  -2  -2  -2   2  -2   2) ;; player-outlaw
  ))

(kern-add-hook 'new_game_start_hook 'player-reconcile-gob)


;;----------------------------------------------------------------------------
;; Specialized queries

(define (player-found-warritrix?) 
  (not (null? (quest-data-getvalue 'questentry-warritrix 'found))))

(define (player-stewardess-trial-done?) 
  (not (null? (quest-data-getvalue 'questentry-warritrix 'avenged))))
