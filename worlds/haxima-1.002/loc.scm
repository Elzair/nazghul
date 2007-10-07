(define (mk-loc place x y) (list place x y))
(define (loc-mk place x y) (list place x y))

(define (loc-place loc) (car loc))
(define (loc-x loc) (cadr loc))
(define (loc-y loc) (caddr loc))

;; eval-loc -- given a loc where the place is a tag, return one where the place
;; is bound to the kernel object referred to by the tag
(define (eval-loc loc)
  (mk-loc (eval (loc-place loc))
          (loc-x loc) 
          (loc-y loc)))

(define (loc-op a b op)
  (mk-loc (loc-place a)
          (op (loc-x a) (loc-x b))
          (op (loc-y a) (loc-y b))))
(define (loc-sum a b) (loc-op a b +))
(define (loc-distance a b)
  (kern-get-distance a b))
(define (loc-city-block-distance a b)
  (+ (abs (- (loc-x a) 
             (loc-x b)))
     (abs (- (loc-y a)
             (loc-y b)))))


(define (loc-addx loc dx)
  (mk-loc (loc-place loc) (+ (loc-x loc) dx) (loc-y loc)))

(define (loc-addy loc dy)
  (mk-loc (loc-place loc) (loc-x loc) (+ (loc-y loc) dy)))

;; loc-opposite-x -- return a list of locs AWAY from the given loc in the given
;; x direction
(define (loc-opposite-x loc dx)
  (if (= 0 dx)
      nil
      (list (loc-addx loc dx)
            (loc-addx (loc-addy loc -1) dx)
            (loc-addx (loc-addy loc  1) dx))))

;; loc-opposite-y -- return a list of locs AWAY from the given loc in the given
;; y direction
(define (loc-opposite-y loc dy)
  (if (= 0 dy)
      nil
      (list (loc-addy loc dy)
            (loc-addy (loc-addx loc -1) dy)
            (loc-addy (loc-addx loc  1) dy))))

;; loc-wrap -- wrap location around edges of a wrapping map
(define (loc-wrap loc)
  (let ((place (loc-place loc)))
    (if (not (kern-place-is-wrapping? place))
        loc
        (mk-loc place
                (modulo (loc-x loc) (kern-place-get-width place))
                (modulo (loc-y loc) (kern-place-get-height place))))))

;; loc-add -- vector sum of locations (auto wraps)
(define (loc-add . locs)
  ;;(println "loc-add " locs)
  (if (null? locs)
      nil
      (loc-wrap (mk-loc (loc-place (car locs))
                        (foldr (lambda (dx loc) (+ dx (loc-x loc))) 0 locs)
                        (foldr (lambda (dy loc) (+ dy (loc-y loc))) 0 locs)))))

;; loc-diff -- vector difference of two locations; on wrapping maps there are
;; two solutions, the shortest is returned
(define (loc-diff a b)
  (let ((place (loc-place a)))
    (if (kern-place-is-wrapping? place)
        (let ((w (kern-place-get-width place)))
          (mk-loc place 
                  (mdist (loc-x a) (loc-x b) w)
                  (mdist (loc-y a) (loc-y b) w)))
        (mk-loc place
                (- (loc-x b) (loc-x a))
                (- (loc-y b) (loc-y a))))))


(define (loc-to-cardinal-dir loc)
  (let ((x (loc-x loc))
        (y (loc-y loc)))
    (if (> x 0)
        ;; eastern half
        (if (> y 0)
          ;; southeast quarter
          (if (> x y)
              east
              south)
          ;; northeast quarter
          (if (> x (abs y))
              east
              north))
        ;; western half
        (if (> y 0)
            ;; southwest quarter
            (if (> (abs x) y)
                west
                south)
            ;; northwest quarter
            (if (> (abs x) (abs y))
                west
                north)))))
          
          

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

;; loc-canonical -- return "canonical" form of vector (ie, one of the four
;; cardinal directions)
(define (loc-canonical loc)
  (define (norm a)
    (cond ((> a 0) 1)
          ((< a 0) -1)
          (else 0)))
  (mk-loc (loc-place loc)
          (norm (loc-x loc))
          (norm (loc-y loc))))

;; loc-sdiv -- scalar division
(define (loc-sdiv loc s)
  (mk-loc (loc-place loc)
          (/ (loc-x loc s))
          (/ (loc-y loc s))))

;; loc-smul -- scaler multiplication
(define (loc-smul loc s)
  ;;(println "loc-smul " loc " " s)
  (mk-loc (loc-place loc)
          (* (loc-x loc) s)
          (* (loc-y loc) s)))

;; loc-norm -- convert loc to normal form (where at least one component has
;; length 1)
(define (loc-norm loc)
  ;;(println "loc-norm " loc)
  (let ((s (min (abs (loc-x loc)) 
                     (abs (loc-y loc)))))
    (if (<= s 1)
        loc
        (loc-sdiv loc s))))

(define (loc-zero? loc)
  (and (= 0 (loc-x loc))
       (= 0 (loc-y loc))))

(define (loc-equal? a b)
  (and (eqv? (loc-place a) (loc-place b))
       (= (loc-x a) (loc-x b))
       (= (loc-y a) (loc-y b))))

;; convert loc to short directional vector
(define (loc-to-delta loc)
  (if (loc-zero? loc)
      loc
      (if (> (abs (loc-x loc)) (abs (loc-y loc)))
          (mk-loc (loc-place loc) (if (> (loc-x loc) 0) 1 -1) 0)
          (mk-loc (loc-place loc) 0 (if (> (loc-y loc) 0) 1  -1)))))

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

;; Helper procedure. Checks if location b is a neighbor of location a as judged
;; by is-adjacent?.
(define (loc-adjacent-generic? a b is-adjacent?)
  (let ((place (loc-place a)))
    (if (kern-place-is-wrapping? place)
        (let ((w (kern-place-get-width place)))
          (is-adjacent? (mdist (loc-x a) (loc-x b) w)
                 (mdist (loc-y a) (loc-y b) w)))
        (is-adjacent? (abs (- (loc-x a) (loc-x b)))
               (abs (- (loc-y a) (loc-y b)))))))

;; Checks if location b is one of the 4 neighbors of location a
(define (loc-4-adjacent? a b)
  (loc-adjacent-generic? a 
                         b 
                         (lambda (dx dy)     
                           (or (and (= 1 dx) (= 0 dy))
                               (and (= 0 dx) (= 1 dy))))))

;; Checks if location b is one of the 8 neighbors of location a
(define (loc-8-adjacent? a b)
  (loc-adjacent-generic? a
                        b
                        (lambda (dx dy)
                          (and (<= 1 dx) (<= 1 dy)))))

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
  ;;(println "    loc-offset:" loc "," dir)
  (define (get-place place dz)
    ;;(println "     get-place:" place "," dz)
    (cond ((= dz 0) place)
          (else (kern-place-get-neighbor place dir))))
  (let* ((vec (direction-to-lvect dir))
         (place (get-place (loc-place loc) (lvect-dz vec))))
    (cond ((null? place) nil)
          (else
           (mk-loc place
                   (+ (loc-x loc) (lvect-dx vec))
                   (+ (loc-y loc) (lvect-dy vec)))))))
                   
;; cardinal directions to lists
                   
; order is N W E S
(define (cardinal-dir-num dir)
	(/ (- dir 1) 2))

(define (get-cardinal-ref avector dir)
	;;(println "gcrc " avector)
	(vector-ref avector
		(cardinal-dir-num dir))
		)

(define (get-cardinal-lref alist dir)
	;;(println "gcrl " alist)
	(list-ref alist
		(cardinal-dir-num dir))
		)

		