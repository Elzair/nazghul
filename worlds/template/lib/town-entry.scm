;; ----------------------------------------------------------------------------
;; trigrave-entry.scm
;;
;; This file defines the on-entry procedure executed whenever the player enters
;; Trigrave. The purpose of this procedure is to ensure the inn doors are all
;; re-locked when the player enter. This procedure may be extended to add other
;; functions.
;;
;; ----------------------------------------------------------------------------

(define all-inn-room-doors
  (list 'trigrave-inn-room-1-door
        'trigrave-inn-room-2-door
        'trigrave-inn-room-3-door
        'trigrave-inn-room-4-door

        'oparine-inn-room-1-door        
        'oparine-inn-room-2-door        
        'oparine-inn-room-3-door        
        'oparine-inn-room-4-door        
        'oparine-inn-room-5-door 

        'white-stag-door

        'glasdrin-inn-room-1-door        
        'glasdrin-inn-room-2-door        

        'bole-inn-room-door

        ))

(define (lock-door door-tag)
  (send-signal nil (eval door-tag) 'lock)
  )

;; lock all inn room doors in all towns
(define (lock-inn-room-doors kplace kplayer)
  (kern-log-enable #f) ;; disable log messages
  (map lock-door all-inn-room-doors)
  (kern-log-enable #t) ;; re-enable log messages
  #t)
