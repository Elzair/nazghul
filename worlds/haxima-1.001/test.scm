(load "init.scm")
(load "naz.scm")

(define (rl) (load "test.scm"))

(define (here? val)
  (display "here?")(newline)
  (eqv? val 5))

(define (next val)
  (list (+ val 1) (- val 1)))

(define queue (list 0))
(define visited (list nil))
(define (unvisited loc)
  (not (memq loc visited)))
