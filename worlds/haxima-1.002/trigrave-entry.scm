;; ----------------------------------------------------------------------------
;; trigrave-entry.scm
;;
;; This file defines the on-entry procedure executed whenever the player enters
;; Trigrave. The purpose of this procedure is to ensure the inn doors are all
;; re-locked when the player enter. This procedure may be extended to add other
;; functions.
;;
;; ----------------------------------------------------------------------------

(define (trigrave-entry kplace kplayer)
  (kern-log-enable #f) ;; disable log messages
  (send-signal nil (eval 'trigrave-inn-room-1-door) 'lock)
  (send-signal nil (eval 'trigrave-inn-room-2-door) 'lock)
  (send-signal nil (eval 'trigrave-inn-room-3-door) 'lock)
  (send-signal nil (eval 'trigrave-inn-room-4-door) 'lock)
  (kern-log-enable #t) ;; re-enable log messages
  #t)
