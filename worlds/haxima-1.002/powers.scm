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
	
(define (cone-do-simple caster target range proc)
	(let* ((origin (kern-obj-get-location caster))
			(x (- (cadr target) (cadr origin))) 
			(y (- (caddr target) (caddr origin))))
		(cone-area-effect origin (xy->angle x y) range (/ pi 2) proc)
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

(define (mk-basic-cone-proc objfx field-type leaveproc)
	(define (dropfield loc)
		(if (kern-obj-put-at (kern-mk-obj field-type 1) loc)))
	(define (is-my-field? kobj) (eqv? field-type (kern-obj-get-type kobj)))
	(define (cleanfields loc)
		(let ((fields (filter is-my-field? (kern-get-objects-at loc)))
				(duration (leaveproc)))
			(cond ((null? fields) nil)
				(else
					(kern-obj-remove (car fields))))
			(if	(and (terrain-ok-for-field? loc)
					(> duration 0))
				(kern-obj-put-at (kern-mk-field field-type duration) loc))
				))
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


(define (contest-of-skill offense defense)
	(let ((oprob (+ offense 1))
		 (tprob (number->string (+ offense defense 2))))
		(if (< (kern-dice-roll (string-append "1d" tprob))
				oprob)
                    (begin
                      (kern-log-msg "^c+gSpell succeeds!^c-")
                      #t)
                    (begin
                      (kern-log-msg "^c+rSpell resisted!^c-")
                      #f)
                    )))


;;--------------------------------------------------------------
;; Spells
;;--------------------------------------------------------------


;todo add area effect for high powered users?
(define (powers-awaken caster ktarg power)
	(kern-obj-remove-effect ktarg ef_sleep)
	(kern-char-set-sleep ktarg #f))
	
(define (powers-blink-range power)
	(+ 3 power))
	
(define (powers-blink caster ktarg power)
	(if (kern-place-is-passable ktarg caster)
		(kern-obj-relocate caster ktarg nil)
		(kern-log-msg "Blink Failed: Impassable terrain")
	))
	
(define (powers-blink-party-range power)
	(* power 0.75))
	
(define (powers-blink-party caster ktarg power)
	(if (kern-place-is-passable ktarg (kern-char-get-party caster))
		(kern-obj-relocate (kern-char-get-party caster) ktarg nil)
		(kern-log-msg "Blink Failed: Impassable terrain")
	))

(define (powers-charm-range power)
	(+ 3 (/ power 3)))
	
;failed charm pisses off target
(define (powers-charm caster target power)
	(if (contest-of-skill
			(+ power 1)
			(occ-ability-magicdef target))
		(kern-obj-add-effect target 
							ef_charm 
							(charm-mk (kern-being-get-current-faction caster)))
			)
		(kern-harm-relations target caster))

(define (powers-clone-range power)
	(+ 1 (/ power 7)))

;todo: nerf this! (does anything stop you cloning the final boss?)
;otoh that it creates a level 1 clone makes it practically useless anyway
(define (powers-clone caster target power)
	(let* ((clone (kern-obj-clone target))
		(loc (pick-loc (kern-obj-get-location target) clone)))
			(kern-being-set-base-faction clone (kern-being-get-current-faction caster))
			(kern-obj-put-at clone loc)
		))
	
(define (powers-cone-flamespray caster ktarg power)
	(let ((damage (mkdice 2 (min (floor (+ 2 (/ power 2))) 10))))
		(define (flambe-all kobj)
			(if (and (is-being? kobj)
					(not (has-fire-immunity? kobj)))
				(kern-obj-inflict-damage kobj "burning" (kern-dice-roll damage) caster)
				))
		(cone-do-simple caster ktarg 3.3
			(mk-basic-cone-proc flambe-all F_fire (lambda () 0))
			)))

(define (powers-cone-basic-leaveproc balance width)
	(lambda ()
		(- (kern-dice-roll (mkdice 1 width)) balance)))
	
(define (powers-cone-basic-range power)
	(+ 7 (/ power 3)))

(define (powers-cone-energy caster ktarg power)
	(let ((damage (mkdice (floor (/ power 2)) 3)))
		(define (energize-all kobj)
			(if (is-being? kobj)
				(kern-obj-inflict-damage kobj "shocked" (kern-dice-roll damage) caster)
				))
		(cone-do-simple caster ktarg (powers-cone-basic-range power)
			(mk-basic-cone-proc energize-all F_energy 
				(powers-cone-basic-leaveproc 40 (+ 30 (* 4 power)))
			))))

(define (powers-cone-fire caster ktarg power)
	(let ((damage (mkdice (floor (/ power 2)) 3)))
		(define (burn-all kobj)
			(if (is-being? kobj)
				(begin
					(if	(has-fire-immunity? kobj)
						(kern-obj-inflict-damage kobj "burning" (kern-dice-roll damage) caster)
						(kern-harm-relations kobj caster)
				))))
		(cone-do-simple caster ktarg (+ 2 (powers-cone-basic-range power))
			(mk-basic-cone-proc burn-all F_fire 
				(powers-cone-basic-leaveproc 30 (+ 20 (* 5 power)))
			))))

(define (powers-cone-poison caster ktarg power)
	(let ((damage (mkdice 1 (floor (/ power 4)))))
		(define (poison-all kobj)
			(if (is-being? kobj)
				(begin
					(apply-poison kobj)
					(if	(is-poisoned? kobj)
						(kern-obj-inflict-damage kobj "poison" (kern-dice-roll damage) caster)
						(kern-harm-relations kobj caster))
					(kern-harm-relations kobj caster)
					(kern-harm-relations kobj caster)
					(kern-harm-relations kobj caster)
				)))
		(cone-do-simple caster ktarg (powers-cone-basic-range power)
			(mk-basic-cone-proc poison-all F_poison 
				(powers-cone-basic-leaveproc 60 (+ 40 (* 3 power)))
			))))

(define (powers-cone-sleep caster ktarg power)
	(let ((damage (mkdice 1 (floor (/ power 4)))))
		(define (sleep-all kobj)
			(if (is-being? kobj)
				(begin
					(kern-harm-relations kobj caster)
					(if (contest-of-skill
							(+ power 8)
							(occ-ability-magicdef kobj))
						(apply-sleep kobj))
				)))
		(cone-do-simple caster ktarg (powers-cone-basic-range power)
			(mk-basic-cone-proc sleep-all F_sleep 
				(powers-cone-basic-leaveproc 40 (+ 30 (* 4 power)))
			))))

;todo limit to some range?
(define (powers-confuse caster unused power)
	(define (confuse kchar)
		(if (contest-of-skill
				power
				(+ (occ-ability-magicdef kchar) 2))
			(kern-being-set-base-faction kchar (random-faction))
			))
	(map confuse (all-hostiles caster)))	
	
(define (powers-cure-poison caster ktarg power)
	(kern-obj-remove-effect ktarg ef_poison)
	(if (< (kern-dice-roll "1d25") power)
		(kern-obj-remove-effect ktarg ef_disease))
	)
	
(define (kobj-is-trapped? kobj)
  (or (kern-obj-is-trapped? kobj)
      (ifccall kobj 'is-trapped?)))

;todo currently only checks topmost item
(define (powers-detect-traps caster ktarg power)
	(if (kobj-is-trapped? ktarg)
            (kern-log-msg (kern-obj-get-name caster)
                          " detects a trap on "
                          (kern-obj-get-name ktarg)
                          "!")
            (kern-log-msg (kern-obj-get-name caster)
                          " does not detect any traps")))

;again, a bit of range for powerful users?
(define (powers-dispel-field caster ktarg power)
   (kern-print "Dispelled field!\n")
   (kern-obj-remove ktarg)
   (kern-map-repaint))	

(define (kobj-remove-trap kobj)
  (cond ((handles? kobj 'rm-traps) (ifccall kobj 'rm-traps))
        (else (kern-obj-remove-trap kobj))))

;todo currently only checks topmost item
(define (powers-disarm-traps caster ktarg power)
  (if (kobj-is-trapped? ktarg)
      (begin
        (kern-log-msg (kern-obj-get-name caster)
                      " disarms a trap on "
                      (kern-obj-get-name ktarg)
                      "!")
        (kobj-remove-trap ktarg))
      ))
				
;todo limit range?
;check critter is viewable before message?
(define (powers-fear caster unused power)			
	(define (repel kchar)
		(kern-log-msg (kern-obj-get-name kchar) " flees in terror!")
		(kern-char-set-fleeing kchar #t)
		)
	(define (try-repel kchar)
		(if (contest-of-skill
				(+ power 8)
				(occ-ability-magicdef kchar))
			(repel kchar)))
	(map try-repel (all-hostiles caster)))
				
;todo
; fields would be a lot more useful if a wall was created instead of one square
;   (length based on caster strength of course)
; I need a 'line' utility anyway, perhaps a ui along the lines of (select center point) (select end point)
;   -> draw line from centre to end and opposite side
(define (powers-field-range power)
	(+ 1 (/ power 5)))

(define (powers-field-energy caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_energy (+ 20 (kern-dice-roll (mkdice 2 power)))) ktarg))

(define (powers-field-fire caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_fire (+ 20 (kern-dice-roll (mkdice 1 power)))) ktarg))
	
(define (powers-field-poison caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_poison (+ 10 (kern-dice-roll (mkdice 1 power)))) ktarg))

(define (powers-field-sleep caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_sleep (+ 15 (kern-dice-roll (mkdice 1 power)))) ktarg))

(define (powers-fireball-range power)
	(+ 3 (/ power 3)))
	
(define (powers-fireball caster ktarg apower)
	(define (fireball-damage-dice power)
		(if (> power 3) (string-append (number->string  (floor (/ power 2))) "d3")
				"1d3"))
	(define (is-my-field? kobj) (eqv? F_fire (kern-obj-get-type kobj)))
	(define (cleanfields kplace x y)
		(let ((kloc (mk-loc kplace x y)))
			(if (kern-is-valid-location? kloc)
				(let ((fields (filter is-my-field? (kern-get-objects-at kloc))))
					(cond ((null? fields) nil)
						(else
							(kern-obj-remove (car fields))))))))
	(define (do-fireball-hit kplace x y damdf damdi)
		(define (fire-damage kobj)
			(if (kern-obj-is-char? kobj)
				(begin
					(kern-log-msg "Burning!")
					(if (not (has-fire-immunity? kobj))
						(kern-obj-inflict-damage kobj "burning" (kern-dice-roll damdf) caster)
						(if (not (null? damdi)
							(kern-obj-inflict-damage kobj "impact" (kern-dice-roll damdi) caster)))
				))
				(kern-obj-apply-damage kobj "burning" (kern-dice-roll damdf))
				)
		)
		(let ((kloc (mk-loc kplace x y)))
			(if (kern-is-valid-location? kloc)
				(begin
					(kern-obj-put-at (kern-mk-obj F_fire 1) kloc)
					(kern-map-repaint)
					(for-each fire-damage
						(kern-get-objects-at kloc))
				))))
	(let* ((targchar (get-being-at ktarg))
		(damf (fireball-damage-dice apower))
		(dami (if (> apower 5) (fireball-damage-dice (/ apower 3)) nil)))
		(define (do-fireball-effect kplace x y)
			(do-fireball-hit kplace x y damf dami)
			(if (> apower 10) (let ((apower (- apower 5))
					(damf (fireball-damage-dice apower))
					(dami (if (> apower 5) (fireball-damage-dice (/ apower 3)) nil)))
				(do-fireball-hit kplace (+ x 1) y damf dami)
				(do-fireball-hit kplace (- x 1) y damf dami)
				(do-fireball-hit kplace x (+ y 1) damf dami)
				(do-fireball-hit kplace x (- y 1) damf dami)
			(if (> apower 10) (let ((apower (- apower 5))
					(damf (fireball-damage-dice apower))
					(dami (if (> apower 5) (fireball-damage-dice (/ apower 3)) nil)))
				(do-fireball-hit kplace (+ x 1) (+ y 1) damf dami)
				(do-fireball-hit kplace (- x 1) (+ y 1) damf dami)
				(do-fireball-hit kplace (+ x 1) (- y 1) damf dami)
				(do-fireball-hit kplace (- x 1) (- y 1) damf dami)
				(cleanfields kplace (+ x 1) (+ y 1))				
				(cleanfields kplace (- x 1) (+ y 1))				
				(cleanfields kplace (+ x 1) (- y 1))				
				(cleanfields kplace (- x 1) (- y 1))				
			))
			(cleanfields kplace (+ x 1) y)				
			(cleanfields kplace (- x 1) y)				
			(cleanfields kplace x (+ y 1))				
			(cleanfields kplace x (- y 1))				
			))
			(cleanfields kplace x y)
		)
		(if (null? targchar)
			(kern-log-msg (kern-obj-get-name caster)
							" hurls a fireball")
			(kern-log-msg (kern-obj-get-name caster)
							" hurls a fireball at "
						(kern-obj-get-name targchar)))
		(temp-ifc-set 
			(lambda (kmissile kplace x y)
				(do-fireball-effect kplace x y)
			)
		)
		(kern-fire-missile t_mfireball
                     (kern-obj-get-location caster)
                     ktarg))
	)

;todo high power should go to user specified gate
(define (powers-gate-travel caster ktarg power)
	(println "vas-rel-por")
	(define (rmgate kobj)
		(moongate-close gate)
		(kern-obj-remove gate))
	(let ((gate (summon-moongate 'ord)))
		(println " gate=" gate)
		(kern-obj-put-at gate ktarg)
		(moongate-open gate)
		))
		  
(define (powers-great-light caster ktarg power)
	(let ((lightadd 
			(kern-dice-roll
				(mkdice 5 power))))
		(light-apply-new ktarg (+ 6000 (* 50 power)))))

;todo should the messages be in the ui part?
(define (powers-great-heal kchar ktarg power)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a great healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
	(kern-obj-heal ktarg 
		(+ 10 power (kern-dice-roll "2d20")
			(kern-dice-roll (mkdice 4 power)))))

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
	
;todo vary duration with power
(define (powers-invisibility kchar ktarg power)
	(kern-obj-add-effect ktarg ef_invisibility nil))

;todo hack in something for xp & hostility
(define (powers-kill kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts kill at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_deathball))
	
(define (powers-light caster ktarg power)
	(let ((lightadd 
			(kern-dice-roll
				(mkdice 5 power))))
		(light-apply-new ktarg (+ 400 (* 5 power)))))
		  
(define (powers-lock caster ktarg power)
	((kobj-ifc ktarg) 'lock ktarg caster)
	)

(define (powers-lock-magic caster ktarg power)
	((kobj-ifc ktarg) 'magic-lock ktarg caster)
	)
	
(define (powers-locate caster ktarg power)
	(let ((loc (kern-obj-get-location caster)))
		(kern-log-msg "You are in " (kern-place-get-name (car loc)) " at [" (caddr loc) " " (cadr loc) "]"))
	)

(define (powers-magic-missile-range power)
	(+ 4 (/ power 3)))
		  
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

(define (powers-negate-magic caster ktarg power)
	(kern-add-magic-negated (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1))))))

(define (powers-poison-range power)
	(+ 3 (/ power 3)))

;todo contest to resist? to-hit roll required? power based initial damage?
;note instant hostility - you cant just cause someone to slowly die and say
;sorry afterwards
(define (powers-poison caster ktarg power)
	(define (do-poison-effect kmissile kplace x y)
		(let* ((loc (mk-loc kplace x y))
				(targchar (get-being-at loc)))
			(if (not (null? targchar))
				(begin
					(if (contest-of-skill
							(+ power 10)
							(occ-ability-dexdefend targchar))
						(apply-poison targchar))
					(kern-harm-relations targchar caster)
					(kern-harm-relations targchar caster)
					(kern-harm-relations targchar caster)
					(kern-harm-relations targchar caster)
					))
		))
	(temp-ifc-set do-poison-effect)
	(kern-log-msg (kern-obj-get-name caster)
				" hurls poison missile at "
				(kern-obj-get-name ktarg))
	(cast-missile-proc caster ktarg t_mpoison_bolt)
	)

;todo duration based on power?
(define (powers-protect caster ktarg power)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (kern-obj-add-effect caster ef_protection nil)
        (kern-obj-add-effect party ef_protection nil)
        )
    ))

;todo duration based on power?
(define (powers-protect-vs-fire caster ktarg power)
	(kern-obj-add-effect ktarg ef_temporary_fire_immunity nil))

;todo duration based on power?
(define (powers-protect-vs-poison caster ktarg power)
	(kern-obj-add-effect ktarg ef_temporary_poison_immunity nil))

;todo duration based on power?
(define (powers-protect-vs-poison-all caster ktarg power)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (kern-obj-add-effect caster ef_temporary_poison_immunity nil)
        (kern-obj-add-effect party ef_temporary_poison_immunity nil)
        )
    ))

(define (powers-quickness caster dir power)
	(kern-add-quicken (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1))))))

;note is different scenarios, could have other uses
(define (powers-raise-lost-area caster loc power)
	(let ((kobjs (filter can-raise-vessel? 
					(kern-get-objects-at loc))))
		(if (not (null? kobjs))
			(let ((kgen (car kobjs)))                
				(signal-kobj kgen 'raise kgen caster)
			))))

;resurrect should have side effects, diminishing with power
(define (powers-resurrect caster ktarg power)
	(kern-char-resurrect ktarg)
	(apply-sleep ktarg))
				
(define (powers-reveal caster ktarg power)
	(kern-add-reveal (* power 4)))

;todo sleep single target

;todo limit to some range?
(define (powers-sleep-area caster ktarg power)
	(let ((hostiles (all-hostiles caster)))
		(define (trysleep target)
			(if (contest-of-skill
				(+ power 3)
				(occ-ability-magicdef target))
			(apply-sleep target)))
                (cond ((null? hostiles) result-no-hostiles)
                      (else
                       (map apply-sleep hostiles)
                       result-ok))))

;todo duration based on power?
(define (powers-spider-calm caster ktarg power)
	(kern-obj-add-effect ktarg ef_spider_calm nil))

(define (powers-summon targetloc quantity typegen levelgen faction)
	(define (run-loop count done)
		(println " run-loop " count)
		(if (<= count 0) done
			(let* ((knpc (spawn-npc (typegen) (levelgen)))
					(loc (pick-loc targetloc knpc)))
				(println " loc=" loc)
				(if (null? loc) 
					(begin
						(kern-obj-dec-ref knpc)
						done)
					(begin
						(kern-being-set-base-faction knpc faction)
						(kern-obj-set-temporary knpc #t)
						(kern-obj-put-at knpc loc)
						(run-loop (- count 1) 1)
					)))))
	(run-loop quantity 0))
	
(define (powers-summon-simple-levelgen power)
	(lambda ()
		(+ (floor (+ (* power 0.2) 1
			(kern-dice-roll
				(mkdice 3 (floor (+ (* power 0.2) 1))))
			)))))

(define (powers-summon-single-type type)
	(lambda ()
		type
	))

;todo enable remote summoning for high power?
(define (powers-summon-snake caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(powers-summon-single-type 'snake)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))	

;todo enable remote summoning for high power?
(define (powers-summon-insect caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 2 spower))
			(powers-summon-single-type 'insect)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))

;todo enable remote summoning for high power?
(define (powers-summon-undead caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(lambda () 
                (random-select (list 'skeletal-warrior 'skeletal-spear-thrower 'ghast)))
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))
	
(define (powers-summon-slime caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(powers-summon-single-type 'green-slime)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))
		 
	
(define (powers-telekinesis-range power)
	(+ (/ power 3) 1))
	
;todo damage/knock away critters?
;should fail on no handler squares rather than aborting?
(define (powers-telekinesis caster ktarg power)
	((kobj-ifc ktarg) 'handle ktarg caster))
	
(define (powers-timestop caster dir power)
	(kern-add-time-stop (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1))))))
	
; a few things needed here:
;	check for visibility before messages
;	no player specific messages
;	only hits hostiles
;   area of effect based on power
;	'turned' as an effect? [so it shows on description] or maybe fleeing should show...
(define (powers-turn-undead caster unused power)
  (define (is-undead-char? kobj)
    (and (obj-is-char? kobj)
         (species-is-undead? (kern-char-get-species kobj))))
  (define (repel kchar)
	(if (contest-of-skill
			(+ power 3)
			(occ-ability-magicdef kchar))
		(kern-char-set-fleeing kchar #t)))
  (let ((all-kobjs (kern-place-get-objects (car (kern-obj-get-location caster)))))
    (cond ((null? all-kobjs) 
           (kern-print "Odd, Nobody here!\n")
           )
          (else (let ((all-undead-combatants (filter is-undead-char? all-kobjs)))
                  (cond ((null? all-undead-combatants) 
                         (kern-print "No undead here!\n")
                         )
                        (else (map repel all-undead-combatants)
                              )))))))
	
;todo limit to some (large) range?
(define (powers-tremor caster unused power)
	(let ((damdice (mkdice 1 power))
		(foes (all-hostiles caster)))
	(define (tremor kchar)
		;;(println "tremor")
		(cond ((kern-char-is-asleep? kchar) (kern-char-set-sleep kchar #f))
			((> (kern-dice-roll "1d4") 1)
				(kern-map-set-jitter #t)
				(kern-map-repaint)
				(kern-char-set-sleep kchar #t)
				(kern-obj-inflict-damage kchar "knocked down" (kern-dice-roll damdice) caster))
				(else nil)))
	(define (loop n kchar)
		;;(println "loop:" n)
		;;(println "foes:" foes)
		(if (not (= n 0))
			(begin
			(map tremor kchar)
			(loop (- n 1) kchar))))
	(define (wakeup kchar) (kern-char-set-sleep kchar #f))
  ;;(println "in-vas-por-ylem: entry")
    ;;(println "in-vas-por-ylem:" foes)
    (kern-log-enable #f)
    (map kern-obj-inc-ref foes)
    (shake-map 20)
    (loop (+ 1 (floor (/ power 4))) foes)
    (kern-map-repaint)
    (map wakeup foes)
    (map kern-obj-dec-ref foes)
    (map wakeup (kern-place-get-beings (loc-place (kern-obj-get-location caster))))
	(kern-log-enable #t)
	))

(define (powers-unlock caster ktarg power)
	((kobj-ifc ktarg) 'unlock ktarg caster))

(define (powers-unlock-magic caster ktarg power)
	((kobj-ifc ktarg) 'magic-unlock ktarg caster))
	
(define (powers-view caster ktarg power)
	(kern-map-set-peering #t)
	(kern-map-repaint)
	(kern-print "Hit a key when done gazing...\n")
	(ui-waitkey)
	(kern-map-set-peering #f)
	(kern-map-repaint))

(define (powers-web-range power)
	(+ 3 (/ power 3)))
	
;note defense is dodge, not magicdef
(define (powers-web caster target power)
	(define (do-web-effect kplace x y)
		(let* ((loc (mk-loc kplace x y))
				(targchar (get-being-at loc)))
			(if (not (null? targchar))
				(begin
					(if (contest-of-skill
							power
							(occ-ability-dexdefend targchar))
						(ensnare targchar))
						(kern-harm-relations kobj caster)))
			(if (and (< (kern-dice-roll "1d20") power)
					(terrain-ok-for-field? loc))
				(kern-obj-put-at (kern-mk-obj web-type 1) loc))
		))
	(let ((targchar (get-being-at target)))	
		(if (null? targchar)
			(kern-log-msg (kern-obj-get-name caster)
							" hurls a web")
			(kern-log-msg (kern-obj-get-name caster)
							" hurls a web at "
						(kern-obj-get-name targchar)))
		(temp-ifc-set 
			(lambda (kmissile kplace x y)
				(do-web-effect kplace x y)
			))
		(kern-fire-missile t_mweb
                     (kern-obj-get-location caster)
                     target)
	))

(define (powers-wind-change caster dir power)
	(kern-set-wind dir (kern-dice-roll (mkdice (* 2 power) 6))))
	
(define (powers-xray caster dir power)
	(kern-add-xray-vision (kern-dice-roll
		(mkdice 10 (floor (+ (/ power 3) 1))))))
		