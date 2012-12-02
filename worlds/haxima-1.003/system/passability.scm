;; The passability system, or "how terrain influences movement".

;; Movement modes. Each mode has a tag, a string description and an index into
;; the passability table. The movement modes are also used when dropping
;; objects, so even inanimate objects have movement modes.
(kern-mk-mmode 'mm_walk     "walking"  0)
(kern-mk-mmode 'mm_smallobj "dropping" 1)

;; Passability classes of terrain. Each value denotes a row in the passability
;; table. 0 is reserved for "none". The names are meant to be canonical in the
;; sense that a terrain that is not grass might still specify pclass-grass if
;; it works exactly like grass.
(define pclass-grass     1)
(define pclass-deep      2)
(define pclass-mountains 3)

;; Passability difficulty levels. The engine treats 255 as impassable.
(define fast     (* 0.66 base-move-ap))  ;; 0.66 (2/3)
(define s-fast   (* 0.8 base-move-ap))  ;; 'slightly fast' 0.8
(define norm     base-move-ap)  ;; 1.0
(define s-hard   (* 1.5 base-move-ap))  ;; 1.5
(define hard     (* 2 base-move-ap))  ;; 2.0
(define v-hard   (* 3 base-move-ap))  ;; 3.0
(define no-drop  100)  ;; special (not related to speed-human)
(define cant     255)  ;; special

;; Movement cost table. Each column is a movement mode. Each row is a
;; passability class. The value is the relative cost of movement, where 0 means
;; impassable, but otherwise smaller numbers mean less cost (and faster
;; movement).
(kern-mk-ptable 
 ;;	walk	smallobj
 (list	cant	cant ) ;; none 
 (list  norm    norm ) ;; grass
 (list  cant    cant ) ;; deep
 (list  cant    cant ) ;; mountain
 )

