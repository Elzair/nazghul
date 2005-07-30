;;----------------------------------------------------------------------------
;; Zones for Gregor's Hut
;;
;; A zone is a rectangle formatted as:
;;      (upper-left-corner-x upper-left-corner-y width height)
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_trigrave x y w h))
(define trigrave-chants-bed      (mk-zone 12  6   1  1))
(define trigrave-forge           (mk-zone 25  4   5  5))
(define trigrave-jims-bed        (mk-zone 25 11   1  1))
(define trigrave-tavern-hall     (mk-zone 18 23   8  6))
(define trigrave-tavern-kitchen  (mk-zone 27 25   3  5))
(define trigrave-tavern-bed      (mk-zone 27 22   1  1))
(define trigrave-tavern-table-1a (mk-zone 19 23   1  1)) 
(define trigrave-tavern-table-1d (mk-zone 21 23   1  1)) 
(define trigrave-tavern-table-3a (mk-zone 19 27   1  1)) 
(define trigrave-inn-counter     (mk-zone  5  4   9  3))
(define trigrave-gwens-bed       (mk-zone 12  2   1  1))
(define trigrave-gwens-room      (mk-zone 11  2   2  3))
(define trigrave-inn-room-1      (mk-zone  2  6   2  2))
(define trigrave-inn-room-2      (mk-zone  2  9   2  2))
(define trigrave-inn-room-3      (mk-zone 11  6   2  2))
(define trigrave-inn-room-4      (mk-zone 11  9   2  2))
(define trigrave-east-west-road  (mk-zone  0 15  32  3))
(define trigrave-earls-bed       (mk-zone  2  9   1  1))
(define trigrave-earls-counter   (mk-zone  2 24   5  1))
(define trigrave-earls-room      (mk-zone  2  9   2  2))
