;;--------------------------------------------------------------
;; This stuff needs to be somewhere more generic
;;--------------------------------------------------------------

	;; ----------------------------------------------------------------------------
	;; terrain-ok-for-field? -- check if the terrain at a given location will allow
	;; a field to be dropped on it. Terrains with passability class equivalent to
	;; Grass, trees and forest are ok, everything else is not.
	;; ----------------------------------------------------------------------------
(define (terrain-ok-for-field? loc)
  (let ((pclass (kern-terrain-get-pclass (kern-place-get-terrain loc))))
    (foldr (lambda (a b) (or a (= pclass b)))
           #f
           (list pclass-grass pclass-trees pclass-forest))))

(define pi (* 2 (acos 0)))
		   
(define (xy->angle x y)
	(if (equal? x 0)
		(cond ((> y 0) (atan 999999))
			((< y 0) (atan -999999))
			(else 0))
		(if (< x 0)
			(+ (atan (/ y x)) pi)
			(atan (/ y x)))))

(define (cone-in-range x y range)
	(< (+ (* x x) (* y y)) (* range range)))
		
(define (angle-wrap angle)
	(cond ((< angle 0) (angle-wrap (+ angle (* 2 pi))))
		((> angle (* 2 pi)) (angle-wrap (- angle (* 2 pi))))
		(else angle)))
		
(define (angle-diff baseangle testangle)
	(- (angle-wrap (- testangle baseangle pi)) pi))

(define (cone-in-angle x y minangle maxangle)
	(let ((tangle (xy->angle x y)))
		(if (< (angle-diff minangle maxangle) 0)
			(or (>= (angle-diff minangle tangle) 0)
				(<= (angle-diff maxangle tangle) 0))
			(and (>= (angle-diff minangle tangle) 0)
				(<= (angle-diff maxangle tangle) 0))
			)))
	
(define (cone-get-edge x y inlist)
	(cons (list y x) 
	(cons (list x y) inlist)))
	
(define (cone-get-initial n inlist)
	(cone-get-edge (- 0 n) 0
	(cone-get-edge n 0 inlist)))
	
(define (cone-get-sides n m inlist)
	(if (< m n)
		(cone-get-sides n (+ m 1)
			(cone-get-edge n m
			(cone-get-edge (- 0 n) m
			(cone-get-edge n (- 0 m)
			(cone-get-edge (- 0 n) (- 0 m) inlist))))
		)
		inlist))

(define (cone-get-corners n inlist)
	(cons (list n n)
	(cons (list (- 0 n) (- 0 n) )
	(cone-get-edge (- 0 n) n inlist))))
	
(define (cone-get-box n)
	(cone-get-sides n 1
	(cone-get-corners n
	(cone-get-initial n nil))))
			
(define (cone-check-cell origin minangle maxangle range proc cell)
	(let* ((x (car cell))
			(y (cadr cell))
			(loc (list (car origin) (+ (cadr origin) x) (+ y (caddr origin)))))
		(if (and (cone-in-range x y range)
					(cone-in-angle x y minangle maxangle)
					(kern-is-valid-location? loc)
					(kern-in-los? origin loc)
					(not (kern-place-blocks-los? loc)))
				(proc loc)
				)))
			
			
(define (cone-handle-box origin minangle maxangle range proc list)
	(if (null? list)
		(println "donebox")
		(begin		
			(cone-check-cell origin minangle maxangle range proc (car list))
			(cone-handle-box origin minangle maxangle range proc (cdr list))
		)))

			
(define (cone-area-slice n origin minangle maxangle range proc)
	(if (< n range)
		(begin
			(cone-handle-box origin minangle maxangle range proc
				(cone-get-box n))
			(cone-area-slice (+ n 1) origin minangle maxangle range proc)
		)))
			
(define (cone-area-effect origin angle range width proc)
	(let ((minangle (angle-wrap (- angle (/ width 2))))
		(maxangle (angle-wrap (+ angle (/ width 2)))))
	(cone-area-slice 1 origin minangle maxangle range proc)
    ))
	
(define (cone-simple caster range proc)
	(let ((origin (kern-obj-get-location caster))
		(target (get-target-loc caster range)))
		(if (null? target)
			#f
			(let ((x (- (cadr target) (cadr origin))) 
				(y (- (caddr target) (caddr origin)))) 
			(cone-area-effect origin (xy->angle x y) range (/ pi 2) proc))
		)))
		
(define (mk-basic-cone-proc objfx field-type leaveprob caster)
	(define (dropfield loc)
		(if (kern-obj-put-at (kern-mk-obj field-type 1) loc)))
	(define (is-my-field? kobj) (eqv? field-type (kern-obj-get-type kobj)))
	(define (cleanfields loc)
		(if	(or (not (terrain-ok-for-field? loc))
				(> (kern-dice-roll "1d100") leaveprob))
			(let ((fields (filter is-my-field? (kern-get-objects-at loc))))
				(cond ((null? fields) nil)
					(else
						(kern-obj-remove (car fields)))))))
	(lambda (loc)
			(if (not (null? objfx))
				(map objfx (kern-get-objects-at loc)))
			(if (not (null? field-type))
				(begin
					(dropfield loc)
					(kern-map-repaint)
					(cleanfields loc)
				))))

(define (mkdice dice size)
	(let ((numstr (if (number? dice)
						(number->string dice)
						dice))
			(sizestr (if (number? size)
						(number->string size)
						size)))
			(string-append numstr "d" sizestr)))
			
(define (cast-missile-proc kchar ktarg ktype)
  (kern-fire-missile ktype
                     (kern-obj-get-location kchar)
                     (kern-obj-get-location ktarg)))

;;--------------------------------------------------------------
;; Shared utilities
;;--------------------------------------------------------------


;;--------------------------------------------------------------
;; Spells
;;--------------------------------------------------------------


;todo add area effect for high powered users?
(define (powers-awaken caster ktarg power)
	(kern-obj-remove-effect ktarg ef_sleep)
	(kern-char-set-sleep ktarg #f))
	
(define (powers-cure-poison caster ktarg power)
	(kern-obj-remove-effect ktarg ef_poison)
	(if (< (kern-dice-roll "1d25") power)
		(kern-obj-remove-effect ktarg ef_disease))
	)
	
;todo should the messages be in the ui part?
(define (powers-heal kchar ktarg power)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
	(kern-obj-heal ktarg 
		(+ 2 (kern-dice-roll "1d10")
			(kern-dice-roll (mkdice 2 power)))))
	
(define (powers-light caster ktarg power)
	(let ((lightadd 
			(kern-dice-roll
				(mkdice 5 power))))
		(light-apply-new ktarg (+ 400 (* 5 power)))))
		  
(define (powers-magic-missile-range power)
	(+ 4 (floor (/ power 3))))
		  
;todo messages out?
(define (powers-magic-missile kchar ktarg power)
	(kern-log-msg (kern-obj-get-name kchar)
			" fires magic missile at "
			(kern-obj-get-name ktarg))
	(if (cast-missile-proc kchar ktarg t_arrow)
		(let* (
			(apower 
				(ceiling (- 
					(/ power 2)
					(/ (occ-ability-magicdef ktarg) 10)
					)))
			(damagedice (string-append 
				(number->string (if (> apower 0) apower 1))
				"d3")))
		(kern-obj-inflict-damage ktarg
							 "magic" (kern-dice-roll damagedice) kchar))))

;todo duration based on power?
(define (powers-spider-calm caster ktarg power)
	(kern-obj-add-effect ktarg ef_spider_calm nil))
	
	