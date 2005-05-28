;;----------------------------------------------------------------------------
;; Zones for Gregor's Hut
;;
;; A zone is a rectangle formatted as:
;;      (upper-left-corner-x upper-left-corner-y width height)
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_gregors_hut x y w h))
(define gh-all             (mk-zone 0  0  32 32))
(define gh-living-room     (mk-zone 10 16 7  4))
(define gh-ilyas-room      (mk-zone 7  18 2  2))
(define gh-ilyas-bed       (mk-zone 7  18 1  1))
(define gh-gregors-room    (mk-zone 7  15 2  2))
(define gh-gregors-bed     (mk-zone 7  15 1  1))
(define gh-kitchen         (mk-zone 10 10 7  5))
(define gh-storeroom       (mk-zone 18 10 2  5))
(define gh-stable          (mk-zone 18 16 2  4))
(define gh-pasture         (mk-zone 21 10 5  10))
(define gh-graveyard       (mk-zone 29 29 3  3))
