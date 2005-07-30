;;----------------------------------------------------------------------------
;; Zones for Enchanter's Tower
;;
;; A zone is a rectangle formatted as:
;;      (upper-left-corner-x upper-left-corner-y width height)
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_enchanters_tower x y w h))
(define enchtwr-main-entrance    (mk-zone 31 40 10   8))
(define enchtwr-dining-room      (mk-zone 28 23  8   5))
(define enchtwr-dining-room-1    (mk-zone 29 26  1   1))
(define enchtwr-dining-room-2    (mk-zone 31 26  1   1))
(define enchtwr-kitchen          (mk-zone 30 18  4   3))
(define enchtwr-pantry           (mk-zone 31 14  3   2))
(define enchtwr-fountain         (mk-zone 38 29  6   6))
(define enchtwr-garden           (mk-zone 46 20  9   9))
(define enchtwr-bedroom-1        (mk-zone 47 36  2   3))
(define enchtwr-bedroom-2        (mk-zone 52 36  2   3))
(define enchtwr-bedroom-3        (mk-zone 52 44  2   3))
(define enchtwr-bedroom-4        (mk-zone 47 44  2   3))
(define enchtwr-workshop         (mk-zone 13 38  5   6))
(define enchtwr-campsite         (mk-zone 20 29  6   6))
(define enchtwr-guardhouse-1     (mk-zone  3  3  3   3))
(define enchtwr-guardhouse-2     (mk-zone 59  3  3   3))
(define enchtwr-guardhouse-3     (mk-zone  3 59  3   3))
