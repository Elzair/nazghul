(define (mk-loc place x y) (list place x y))

(define (loc-place loc) (car loc))
(define (loc-x loc) (cadr loc))
(define (loc-y loc) (caddr loc))

(define (loc-op a b op)
  (mk-loc (loc-place a)
          (op (loc-x a) (loc-x b))
          (op (loc-y a) (loc-y b))))
(define (loc-sum a b) (loc-op a b +))
(define (loc-diff a b) (loc-op a b +))
(define (loc-distance a b)
  (kern-get-distance a b))

;; ----------------------------------------------------------------------------
;; loc-grid-distance -- return the distance needed to walk between two points
;;
;; REVISIT: this has a form almost identical to the loc-adjacent? proc below
;;
;; ----------------------------------------------------------------------------
(define (loc-grid-distance a b)
  (let ((place (loc-place a)))
    (if (kern-place-is-wrapping? place)
        (let ((w (kern-place-get-width place)))
          (+ (mdist (loc-x a) (loc-x b) w)
             (mdist (loc-y a) (loc-y b) w)))
        (+ (abs (- (loc-x a) (loc-x b)))
           (abs (- (loc-y a) (loc-y b)))))))

(define (loc-closer? a b c)
  (< (loc-grid-distance a c)
     (loc-grid-distance b c)))

;; Convert a location vector to "normal" form
(define (loc-norm loc)
  (define (norm a)
    (cond ((> a 0) 1)
          ((< a 0) -1)
          (else 0)))
  (mk-loc (loc-place loc)
          (norm (loc-x loc))
          (norm (loc-y loc))))

;; ----------------------------------------------------------------------------
;; loc-enum-rect -- given a rectangular region of a place return a flat list of
;; all locations in that rectangle. Useful in conjunction with map.
;; ----------------------------------------------------------------------------
(define (loc-enum-rect place x y w h)
  (define (enum-row x w)
    (if (= 0 w)
        nil
        (cons (mk-loc place x y) 
              (enum-row (+ x 1) (- w 1)))))
  (if (= 0 h)
      nil
      (append (enum-row x w)
              (loc-enum-rect place x (+ y 1) w (- h 1)))))

;; ----------------------------------------------------------------------------
;; loc-adjacent? -- check if two locations are adjacent neighbors in one of the
;; four cardinal directions. This assumes that the locations are in the same
;; place and that they have been wrapped if necessary.
;; ----------------------------------------------------------------------------
(define (loc-adjacent? a b)
  (define (check dx dy)
    (or (and (= 1 dx) (= 0 dy))
        (and (= 0 dx) (= 1 dy))))
  (let ((place (loc-place a)))
    (if (kern-place-is-wrapping? place)
        (let ((w (kern-place-get-width place)))
          (check (mdist (loc-x a) (loc-x b) w)
                 (mdist (loc-y a) (loc-y b) w)))
        (check (abs (- (loc-x a) (loc-x b)))
               (abs (- (loc-y a) (loc-y b)))))))

(define (mk-lvect dx dy dz) (list dx dy dz))
(define (lvect-dx lvect) (car lvect))
(define (lvect-dy lvect) (cadr lvect))
(define (lvect-dz lvect) (caddr lvect))

;; Convert a direction code to a location vector
(define (direction-to-lvect dir)
  (cond ((= dir east)      (mk-lvect  1  0  0))
        ((= dir west)      (mk-lvect -1  0  0))
        ((= dir north)     (mk-lvect  0 -1  0))
        ((= dir south)     (mk-lvect  0  1  0))
        ((= dir northwest) (mk-lvect -1 -1  0))
        ((= dir northeast) (mk-lvect  1 -1  0))
        ((= dir southwest) (mk-lvect -1  1  0))
        ((= dir southeast) (mk-lvect  1 -1  0))
        ((= dir up)        (mk-lvect  0  0  1))
        ((= dir down)      (mk-lvect  0  0 -1))
        ))  

(define (loc-offset loc dir)
  (define (get-place place dz)
    (cond ((= dz 0) place)
          (else (kern-place-get-neighbor place dir))))
  (let* ((vec (direction-to-lvect dir))
         (place (get-place (loc-place loc) (lvect-dz vec))))
    (cond ((null? place) nil)
          (else
           (mk-loc place
                   (+ (loc-x loc) (lvect-dx vec))
                   (+ (loc-y loc) (lvect-dy vec)))))))
