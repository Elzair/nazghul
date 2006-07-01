;;----------------------------------------------------------------------------
;; occupations
(define (mk-occ tag name hit def dam arm xp)
  (kern-mk-occ tag name 1.0 0 0 0 0 hit def dam arm xp)
  (kern-occ-set-gob (eval tag) (list nil nil nil)))

;;            /           /         / t  / f / e /   /
;;           /           /         / i  / e / g / r /
;;          /           / e       / h  / d / a / o /
;;         / g         / m       / -  / - / m / m /
;;        / a         / a       / o  / o / a / r / p
;;       / t         / n       / t  / t / d / a / x
(mk-occ 'oc_wizard   "wizard"   -1  -1  -1  -1   4)
(mk-occ 'oc_warrior  "warrior"   1   0   1   0   4)
(mk-occ 'oc_wright   "wright"    0   1   0   1   4)
(mk-occ 'oc_wrogue   "wrogue"    1   1   0   0   4)
(mk-occ 'oc_wanderer "wanderer"  5   5   5   5   8)
(mk-occ 'oc_ranger   "ranger"    1   1   0   0   4)

(define (occ-get-abil kocc ability)
	(list-ref (kern-occ-get-gob kocc) ability))
		
(define (occ-set-abil kocc ability calc)
	(set-car! 
		(list-tail 
			(kern-occ-get-gob kocc) 
			ability)
		calc))

;---------------------------------
;Thiefliness

(define (occ-ability-thief kchar)
	(let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 0)))
		(if (null? occ-abil)
			(floor 
				(/ (+ (kern-char-get-level kchar) 
					(kern-char-get-dexterity kchar)) 4))
				(occ-abil kchar)
			)))


(occ-set-abil oc_wrogue 0
	(lambda (kchar)
		(floor 
			(+ (kern-char-get-level kchar)
				(/ (kern-char-get-dexterity kchar) 3)))
	))

(let ((partskill
	(lambda (kchar)
		(floor 
			(+ (/ (kern-char-get-level kchar) 2)
				(/ (kern-char-get-dexterity kchar) 4)
			)))))
	(occ-set-abil oc_wright 0 partskill)
	(occ-set-abil oc_ranger 0 partskill)
	(occ-set-abil oc_wanderer 0 partskill)
	)

