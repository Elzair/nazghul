(define (mk-loc place x y) (list place x y))

(define (loc-place loc) (car loc))
(define (loc-x loc) (cadr loc))
(define (loc-y loc) (caddr loc))

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
