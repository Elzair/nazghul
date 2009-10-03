;;--------------------------------------------------------------
;; This stuff needs to be somewhere more generic
;;--------------------------------------------------------------

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
					)
				(proc loc)
				)))
			
			
(define (cone-handle-box origin minangle maxangle range proc list)
	(if (not (null? list))
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

(define (powers-field-generic loc f_type duration proc)
	(let* ((finduration (if (< duration 1) 1 duration))
			(afield (kern-mk-field f_type finduration)))
		(if (can-be-dropped? afield loc cant)
			(begin
				(kern-obj-put-at afield loc)
				(kern-map-repaint)
				(if (not (null? proc))
					(for-each proc (kern-get-objects-at loc))
				)
				;; remove fields on semi-bad locations
				(if (or (< duration 1)
						(not (can-be-dropped? afield loc no-drop)))
					(kern-obj-remove afield)
				)
				(kern-map-repaint)
			))
	))
		
(define (mk-basic-cone-proc origin objfx field-type leaveproc)
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
		(if (kern-in-los? origin loc)
			(if (null? field-type)
				(if (not (null? objfx))
					(map objfx (kern-get-objects-at loc))
				)
				(powers-field-generic loc field-type (leaveproc) objfx)
			))
	))
	
(define (mk-cone-proc-sfx origin objfx sfx field-type leaveproc)
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
		(kern-sound-play-at sfx origin)
		(if (kern-in-los? origin loc)
			(if (null? field-type)
				(if (not (null? objfx))
					(map objfx (kern-get-objects-at loc))
				)
				(powers-field-generic loc field-type (leaveproc) objfx)
			))
	))
				
;; todo- inc these in line-cell to simplify?
(define (line-do-proc proc location)
	(if (kern-is-valid-location? location)
		(proc location 1)
		#f
	))
		
(define (line-diag place x y dx dy proc)
	(let* ((curx (floor x))
			(cury (floor y))
			(newx (floor (+ x (/ dx 2))))
			(newy (floor (+ y (/ dy 2))))
			(location (loc-mk place newx newy)))
		(if (or (not (equal? newx curx))
					(not (equal? newy cury)))
			(if (kern-is-valid-location? location)
				(proc location 0.5)
				#f
			)
			#t
		)
	))
		
(define (line-cell place x y dx dy endx endy proc)
	(let ((curx (floor x))
			(cury (floor y)))
		(if (and 
				(if (equal? (abs dx) 1) (line-diag place x y 0 (* dy 1.0000001) proc) (line-diag place x y (* dx 1.0000001) 0 proc))
				(line-do-proc proc (loc-mk place curx cury))
				(not (and (equal? curx endx) (equal? cury endy)))
				(if (equal? (abs dx) 1) (line-diag place (+ x dx) (+ y dy) 0 (* dy -0.9999999) proc) (line-diag place (+ x dx) (+ y dy) (* dx -0.9999999) 0 proc))
				)
			(line-cell place (+ x dx) (+ y dy) dx dy endx endy proc))
	))
				
;; todo will fail on looping maps
(define (line-draw place startx starty stopx stopy proc)
	(if (and (equal? startx stopx)
				(equal? starty stopy))
		(line-do-proc proc (loc-mk place startx starty))
		(let* ((xdif (- stopx startx))
				(ydif (- stopy starty))
				(div (if (> (abs xdif) (abs ydif)) (abs xdif) (abs ydif)))
				(dx (/ xdif div))
				(dy (/ ydif div)))
			(line-cell place (+ startx 0.5) (+ starty 0.5) dx dy stopx stopy proc)
		)))
			
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
    (println "oprob=" oprob " tprob=" tprob " offense=" offense " defense=" defense)
    (if (< (kern-dice-roll (string-append "1d" tprob))
           oprob)
          #t
          #f
        )))


;;--------------------------------------------------------------
;; Spells
;;--------------------------------------------------------------


;todo add area effect for high powered users?
(define (powers-awaken caster ktarg power)
	(kern-obj-remove-effect ktarg ef_sleep)
	(kern-char-set-sleep ktarg #f)
	result-ok)
	
(define (powers-blink-range power)
	(+ 3 power))
	
(define (powers-blink caster ktarg power)
	(if (kern-place-is-passable ktarg caster)
		(kern-obj-relocate caster ktarg nil)
		(kern-log-msg "Blink Failed: Impassable terrain")
	)
	result-ok)
	
(define (powers-blink-party-range power)
	(cond ((< power 20) (* power 0.75))
		(else 15)))
	
(define (powers-blink-party caster ktarg power)
	(if (kern-place-is-passable ktarg (kern-char-get-party caster))
		(kern-obj-relocate (kern-char-get-party caster) ktarg nil)
		(kern-log-msg "Blink Failed: Impassable terrain")
	)
	result-ok)

(define (powers-charm-range power)
	(+ 3 (/ power 3)))
	
; (Only) failed charm pisses off target
(define (powers-charm caster target power)
	(cond
         ((has-charm-immunity? target)
          (msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " immune to charm")
          )
         ((contest-of-skill
           (+ power 1)
           (occ-ability-magicdef target))
          (let ((tloc (kern-obj-get-location target)))
            (kern-obj-add-effect target 
                                 ef_charm 
                                 (charm-mk (kern-being-get-current-faction caster)))
            (kern-map-flash-sprite s_heart (loc-x tloc) (loc-y tloc))
					(msg-log-visible tloc (kern-obj-get-name target) " is charmed")
                                        )
          )
         (else 
          (msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " resists charm")
          (kern-harm-relations target caster)
          )
         )
	result-ok
        )

;; Weaker than charm, this turns the target's alignment to be that of
;; monsters. The monster faction is hostile to most others, so the player can
;; use it against outlaws, cave goblins, etc.
(define (powers-beastly-illusion caster target power)
  (cond ((has-charm-immunity? target)
         (msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " resists illusion")
         )
        ((contest-of-skill (+ power 1) (occ-ability-magicdef target))
         (let ((tloc (kern-obj-get-location target)))
           (kern-obj-add-effect target ef_charm (charm-mk faction-monster))
           (kern-map-flash-sprite s_heart (loc-x tloc) (loc-y tloc))
           (msg-log-visible tloc (kern-obj-get-name target) " is deluded")
           ))
        (else (msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " resists illusion"))
        )
  (kern-harm-relations target caster)
  result-ok
  )

(define (powers-clone-range power)
	(+ 1 (/ power 7)))

(define (powers-clone caster target power)
	(let* ((clone (kern-obj-clone target))
				(loc (pick-loc (kern-obj-get-location target) clone)))
		(kern-being-set-base-faction clone (kern-being-get-current-faction caster))
		;; clone has equipment of original
		(map (lambda (ktype)
			(kern-obj-add-to-inventory clone ktype 1))
			(kern-char-get-arms target)
		)
		(kern-char-arm-self clone)
		;; clone level based on of weaker of caster or original
		(if (> (kern-char-get-level target) (kern-char-get-level caster))
			(kern-char-set-level clone (+ 1 (* (kern-char-get-level caster) 0.75)))
			(kern-char-set-level clone (+ 1 (* (kern-char-get-level target) 0.75)))
		)
		;; clone may not have more hp/mana than original
		(if (> (kern-char-get-hp clone) (kern-char-get-hp target))
			(kern-char-set-hp clone (kern-char-get-hp target)))
		(if (> (kern-char-get-mana clone) (kern-char-get-mana target))
			(kern-char-set-mana clone (kern-char-get-mana target)))
		;;(kern-char-set-ai clone 'spell-sword-ai)
		(kern-obj-put-at clone loc)
	)
	result-ok)
	
(define (powers-cone-flamespray caster ktarg power)
	(let ((damage (mkdice 2 (min (floor (+ 2 (/ power 2))) 10))))
		(define (flambe-all kobj)
			(if (and (is-being? kobj)
					(not (has-fire-immunity? kobj)))
				(kern-obj-inflict-damage kobj "burning" (kern-dice-roll damage) caster)
				))
		(cone-do-simple caster ktarg 3.3
			(mk-basic-cone-proc (kern-obj-get-location caster) flambe-all F_fire (lambda () 0))
			))
			result-ok)

(define (powers-cone-basic-leaveproc balance width)
	(lambda ()
		(- (kern-dice-roll (mkdice 1 width)) balance)))
	
;; this may need to be limited...
(define (powers-cone-basic-range power)
	(+ 3 (/ power 3)))
	
(define (powers-cone-fire-range power)
	(+ 5 (/ power 3)))
	
(define (powers-cone-energy caster ktarg power)
	(let ((damage (mkdice (floor (/ power 2)) 3)))
		(define (energize-all kobj)
			(if (is-being? kobj)
				(kern-obj-inflict-damage kobj "shocked" (kern-dice-roll damage) caster)
				))
		(cone-do-simple caster ktarg (powers-cone-basic-range power)
			(mk-basic-cone-proc (kern-obj-get-location caster) energize-all F_energy 
				(powers-cone-basic-leaveproc 40 (+ 30 (* 4 power)))
			)))
			result-ok)

;; check for: no unintended victims
;;    at least 2 fire vulnerable targets
(define (powers-cone-fire-test caster targloc power)
	;;(println "test cone fire")
	(let ((viable-targets (list 0))
			(shot-ok (list #t)))
		(define (checktarg kobj)
			(if (is-being? kobj)
				;; test for hostility and known (ie permanent) fire resistance
				(if (is-hostile? kobj caster)
					(if (not (has-effect? kobj ef_fire_immunity))
						(set-car! viable-targets (+ (car viable-targets) 1))
					)
					(set-car! shot-ok #f)
				)
			)
		)			
		(cone-do-simple caster targloc (powers-cone-fire-range power)
			(mk-basic-cone-proc (kern-obj-get-location caster) checktarg nil nil)
			)
		;;(println "tested cone fire " (car shot-ok) " " (car viable-targets))
		(and (car shot-ok)
			(> (car viable-targets )1))
	))
			
(define (powers-cone-fire caster targloc power)
	(let ((damage (mkdice (floor (/ power 2)) 3)))
		(define (burn-all kobj)
			(if (and (is-being? kobj)
					(not (has-fire-immunity? kobj)))
				(begin
					(kern-obj-inflict-damage kobj "burning" (kern-dice-roll damage) caster)
					(kern-harm-relations kobj caster)
				)
			))
		(cone-do-simple caster targloc (powers-cone-fire-range power)
			(mk-cone-proc-sfx (kern-obj-get-location caster) burn-all sound-fireblast F_fire 
				(powers-cone-basic-leaveproc 30 (+ 20 (* 5 power)))
			)
		))
	result-ok
)

(define (powers-cone-poison caster ktarg power)
	(let ((damage (mkdice 1 (floor (/ power 4)))))
		(define (poison-all kobj)
			(if (is-being? kobj)
				(begin
					(apply-poison kobj)
					(if (is-poisoned? kobj)
						(begin
							(kern-harm-relations kobj caster)
							(kern-harm-relations kobj caster)
							(kern-harm-relations kobj caster)
							(kern-harm-relations kobj caster)
							(kern-obj-inflict-damage kobj "poison" (kern-dice-roll damage) caster)
						)
				))))
		(cone-do-simple caster ktarg (powers-cone-basic-range power)
			(mk-basic-cone-proc (kern-obj-get-location caster) poison-all F_poison 
				(powers-cone-basic-leaveproc 60 (+ 40 (* 3 power)))
			)))
			result-ok)

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
			(mk-basic-cone-proc (kern-obj-get-location caster) sleep-all F_sleep 
				(powers-cone-basic-leaveproc 40 (+ 30 (* 4 power)))
			)))
			result-ok)

;todo limit to some range?
(define (powers-confuse caster unused power)
	(define (confuse kchar)
		(if (contest-of-skill
				power
				(+ (occ-ability-magicdef kchar) 2))
			(kern-being-set-base-faction kchar (random-faction))
			))
	(map confuse (all-hostiles caster))
	result-ok)
	
(define (powers-cure-poison caster ktarg power)
	(kern-obj-remove-effect ktarg ef_poison)
	(if (< (kern-dice-roll "1d25") power)
            (kern-obj-remove-effect ktarg ef_disease))
	result-ok)
	
;todo currently only checks topmost item
(define (powers-detect-traps caster ktarg power)
  (let ((traps (ifccall ktarg 'get-traps)))
    (cond ((null? traps)
           (kern-log-msg (kern-obj-get-name caster)
                         " does not detect any traps")
           )
          (else
           (map (lambda (trap)
                  (trap-set-detected! trap #t)
                  (kern-log-msg (kern-obj-get-name caster)
                                " detects a " (trap-name trap) " trap!")
                  )
                traps)
           )))
  result-ok)

;again, a bit of range for powerful users?
(define (powers-dispel-field caster ktarg power)
   (kern-print "Dispelled field!\n")
   (kern-obj-remove ktarg)
   (kern-map-repaint)
   result-ok)
   
;; todo saving throw vs caster power for different effects?
(define (powers-dispel-magic caster ktarg power)
	(effects-dispel-magic ktarg)
	result-ok)

(define (powers-disarm-traps kchar ktarg power)
  (let (
        (traps (filter (lambda (trap) 
                          (and (trap-detected? trap) 
                               (not (trap-tripped? trap))))
                        (ifccall ktarg 'get-traps)))
        )
    ;; Check if any unprocessed traps remaining
    (cond ((null? traps) 
           result-no-effect
           )
          ((not (handles? ktarg 'rm-traps)) 
           (kern-log-msg "Traps can't be removed!")
           result-no-effect
           )
          (else
           ;; Roll to succeed
           (let* (
                  (trap (car traps))
                  (dc (trap-avoid-dc trap))
                  (roll (kern-dice-roll "1d20"))
                  (bonus (kern-dice-roll (string-append "1d" (number->string power))))
                  )
             (cond ((or 
                     (= roll 20) 
                     (> (+ roll bonus) dc)
                     )
                    ;; Success - disarm the trap
                    (kern-log-msg (kern-obj-get-name kchar) " ^c+gdisarms^c- a " (trap-name trap) " trap!")
                    (trap-set-tripped! trap #t)
                    result-ok
                    )
                   (else
                    ;; Failure - trip the trap (kchar will get another roll
                    ;; to avoid the damage)
                    (trap-trigger trap ktarg kchar)
                    result-failed
                    )))))))


;todo limit range?
(define (powers-fear caster unused power)			
	(define (repel kchar)
		(msg-log-visible (kern-obj-get-location kchar) (kern-obj-get-name kchar) " flees in terror!")
		(kern-map-flash-sprite s_magicflash (loc-x tloc) (loc-y tloc))
		(kern-char-set-fleeing kchar #t)
		)
	(define (try-repel kchar)
		(if (contest-of-skill
				(+ power 8)
				(occ-ability-magicdef kchar))
			(repel kchar)))
	(map try-repel (all-hostiles caster))
	result-ok)

;todo
; fields would be a lot more useful if a wall was created instead of one square
;   (length based on caster strength of course)
; I need a 'line' utility anyway, perhaps a ui along the lines of (select center point) (select end point)
;   -> draw line from centre to end and opposite side
(define (powers-field-range power)
	(if (> power 30)
		7
		(+ 1 (/ power 5))
	))

(define (powers-field-length power)
	(+ 1 (/ power 4)))
	
(define (powers-field-wall start stop f_type duration leng proc)
	(let ((lengremaining (list leng)))
		(define (put-field location delta)
			(powers-field-generic location f_type duration proc)
			(set-car! lengremaining (- (car lengremaining) delta))
			(> (car lengremaining) 0)
			)
		(line-draw (loc-place start) (loc-x start) (loc-y start) (loc-x stop) (loc-y stop) put-field)
	))
	
(define (powers-field-fire-wall caster start stop power)
	(define (do-burn kobj)
		(if (and (kern-obj-is-char? kobj)
				(not (has-fire-immunity? kobj)))
			(kern-obj-inflict-damage kobj "burning" (kern-dice-roll "2d3+2") caster)
		))
	(powers-field-wall start stop F_fire (+ 20 (kern-dice-roll (mkdice 1 power))) (powers-field-length power) do-burn)
	result-ok)
	
(define (powers-field-energy-wall caster start stop power)
	(define (do-burn kobj)
		(if (kern-obj-is-char? kobj)
			(kern-obj-inflict-damage kobj "shocked" (kern-dice-roll "2d8") caster)
		))
	(powers-field-wall start stop F_energy (+ 20 (kern-dice-roll (mkdice 2 power))) (powers-field-length power) do-burn)
	result-ok)

(define (powers-field-poison-wall caster start stop power)
	(define (do-burn kobj)
		(if (and (kern-obj-is-char? kobj)
				(not (has-poison-immunity? kobj)))
			(begin
				(apply-poison kobj)
				(kern-harm-relations kobj caster)
				(kern-harm-relations kobj caster)
			)
		))
	(powers-field-wall start stop F_poison (+ 10 (kern-dice-roll (mkdice 1 power))) (powers-field-length power) do-burn)
	result-ok)

(define (powers-field-sleep-wall caster start stop power)
	(define (do-burn kobj)
		(if (and (kern-obj-is-char? kobj)
				(not (has-sleep-immunity? kobj)))
			(begin
				(kern-harm-relations kobj caster)			
				(apply-sleep kobj)
			)
		))
	(powers-field-wall start stop F_sleep (+ 15 (kern-dice-roll (mkdice 1 power))) (powers-field-length power) do-burn)
	result-ok)
	
(define (powers-field-energy caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_energy (+ 20 (kern-dice-roll (mkdice 2 power)))) ktarg)
	result-ok)

(define (powers-field-fire caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_fire (+ 20 (kern-dice-roll (mkdice 1 power)))) ktarg)
	result-ok)
	
(define (powers-field-poison caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_poison (+ 10 (kern-dice-roll (mkdice 1 power)))) ktarg)
	result-ok)

(define (powers-field-sleep caster ktarg power)
	(kern-obj-put-at (kern-mk-field F_sleep (+ 15 (kern-dice-roll (mkdice 1 power)))) ktarg)
	result-ok)
	
(define (powers-field-energy-weak caster ktarg power)
	(powers-field-generic ktarg F_energy (+ 5 (kern-dice-roll (mkdice 1 (ceiling (/ power 2))))) apply-lightning)
	result-ok)

(define (powers-field-fire-weak caster ktarg power)
	(powers-field-generic ktarg F_fire (+ 5 (kern-dice-roll (mkdice 1 (ceiling (/ power 3))))) burn)
	result-ok)
	
(define (powers-field-poison-weak caster ktarg power)
	(powers-field-generic ktarg F_poison (+ 3 (kern-dice-roll (mkdice 1 (ceiling (/ power 3))))) apply-poison)
	result-ok)

(define (powers-field-sleep-weak caster ktarg power)
	(powers-field-generic ktarg F_sleep (+ 4 (kern-dice-roll (mkdice 1 (ceiling (/ power 3))))) apply-sleep)
	result-ok)

(define (powers-fireball-range power)
	(+ 3 (/ power 3)))
	
;; returns true if the location is ok
(define (powers-fireball-collateral-check caster targloc apower)
	;;(println "fireball check")
	(let ((place (loc-place targloc))
			(x (loc-x targloc))
			(y (loc-y targloc)))
		(define (checkloc kloc)
			(null? 
				(filter
					(lambda (kobj)
						(and (kern-obj-is-char? kobj)
							(not (is-hostile? kobj caster))
							)
					)
					(kern-get-objects-at kloc)
				)
			))
		(define (checkoff xoff yoff)
			(let ((kloc (mk-loc place (+ x xoff) (+ y yoff))))
				(if (kern-is-valid-location? kloc)
					(checkloc kloc)
					#t
				)
			))
		(and (checkoff 0 0)
			(or (<= apower 10)
				(and (checkoff 1 0)
					(checkoff -1 0)
					(checkoff 0 1)
					(checkoff 0 -1)
				))
			(or (<= apower 15)
				(and (checkoff 1 1)
					(checkoff 1 -1)
					(checkoff -1 1)
					(checkoff -1 -1)
				))
			)
	))
	
(define (powers-fireball caster ktarg apower)
	;;(println "fireball")
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
                		 (if (not (null? damdi))
                      	(kern-obj-inflict-damage kobj "impact" (kern-dice-roll damdi) caster))
                  	))
                ;;(kern-obj-apply-damage kobj "burning" (kern-dice-roll damdf))
                ))
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
			(kern-sound-play-at sound-explode (mk-loc kplace x y))
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
			(lambda (kmissile kuser ktarget kplace x y)
				(do-fireball-effect kplace x y)
			)
		)
		(kern-sound-play-at sound-missile (kern-obj-get-location caster))
		(kern-sound-play-at sound-missile ktarg)
		(kern-fire-missile t_mfireball
                     (kern-obj-get-location caster)
                     ktarg))
	result-ok)

;todo high power should go to user specified gate
(define (powers-gate-travel caster ktarg power)
  ;; Fix for bug 1738251, which involved summoning gates over magically locked
  ;; doors: check passability. Use the passability of the caster as a
  ;; reasonable estimate for the passability of the gate.
  (if (not (kern-place-is-passable ktarg caster))
      result-not-here
      (let ((gate (summon-moongate 'ord)))
        (kern-obj-put-at gate ktarg)
        (moongate-open gate)
        result-ok)))
		  
(define (powers-great-light caster ktarg power)
	(let ((lightadd 
			(kern-dice-roll
				(mkdice 5 power))))
		(light-apply-new ktarg (+ 6000 (* 50 power))))
		result-ok)

;todo should the messages be in the ui part?
(define (powers-great-heal kchar ktarg power)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a great healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
  (kern-obj-heal ktarg 
                 (+ 10 power (kern-dice-roll "2d20")
                    (kern-dice-roll (mkdice 4 power))))
  result-ok)

;todo should the messages be in the ui part?
(define (powers-heal kchar ktarg power)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
  (kern-obj-heal ktarg 
                 (+ 2 (kern-dice-roll "1d10")
                    (kern-dice-roll (mkdice 2 power))))
  result-ok)
	
;todo vary duration with power
(define (powers-invisibility kchar ktarg power)
	(kern-obj-add-effect ktarg ef_invisibility nil)
	result-ok)

;todo hack in something for xp & hostility
(define (powers-kill kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts kill at "
                (kern-obj-get-name ktarg))
	(kern-sound-play-at sound-missile (kern-obj-get-location kchar))
	(kern-sound-play-at sound-missile (kern-obj-get-location ktarg))
  (cast-missile-proc kchar ktarg t_deathball)
  result-ok)
	
(define (powers-light caster ktarg power)
	(let ((lightadd 
			(kern-dice-roll
				(mkdice 5 power))))
		(light-apply-new ktarg (+ 400 (* 5 power))))
		result-ok)
		  
(define (powers-lightning-range power)
	(+ 3 (/ power 2.5)))
	
;; todo will fail on looping maps
(define (powers-lightning-collateral-check caster targloc apower)
	;;(println "checkzap")
	(let* ((range (powers-lightning-range apower))
			(casterloc (kern-obj-get-location caster))
			(targrange (+ 1 (kern-get-distance targloc casterloc)))
			(rangemult (if (> targrange 0) (ceiling (/ range targrange)) 0))
			(dx (* rangemult (- (loc-x targloc) (loc-x casterloc))))
			(dy (* rangemult (- (loc-y targloc) (loc-y casterloc))))
			(endx (+ (loc-x casterloc) dx))
			(endy (+ (loc-y casterloc) dy))
			(shot-ok (list #t))
			(range-ok (> range targrange))
			)
		(define (check-loc location delta)
			(cond ((equal? location casterloc) #t)
				((> (kern-get-distance location casterloc) range) #f)
				((null? (filter
							(lambda (kobj)
								(and (kern-obj-is-char? kobj)
									(not (is-hostile? kobj caster))
									)
							)
							(kern-get-objects-at location)
						))
					#t
					)
				(else (set-car! shot-ok #f) #f)
			))
		(if (and range-ok (> rangemult 0))
			(begin
				(line-draw (loc-place targloc) (loc-x casterloc) (loc-y casterloc) endx endy check-loc)
				(car shot-ok)
			)
			#f
		)
	))
	
(define (powers-lightning caster targloc apower)
	;;(println "zap")
  (let ((targets (list nil))
        (dam (mkdice (floor (+ 1 (/ apower 3))) 4))
        )
    (temp-ifc-set 
     (lambda (kmissile kuser ktarget kplace x y unused)
       (let (
             (targchar (get-being-at (mk-loc kplace x y)))
             )
         (if (not (null? targchar))
             (set-car! targets (cons targchar (car targets)))
             ))
       #t	
       ))
    (kern-sound-play sound-lightning)
    (kern-fire-missile-to-max t_lightning_bolt (powers-lightning-range apower)
                              (kern-obj-get-location caster)
                              targloc
                              )    
    (if (not (null? (car targets)))
        (map
         (lambda (zappee)
           (kern-log-msg (kern-obj-get-name zappee) " shocked!")
           (kern-obj-inflict-damage zappee "shocked" (kern-dice-roll dam) caster)						
           )
         (car targets)
         ))
    )
  result-ok)
		
(define (powers-lock caster ktarg power)
	((kobj-ifc ktarg) 'lock ktarg caster)
	result-ok)

(define (powers-lock-magic caster ktarg power)
	((kobj-ifc ktarg) 'magic-lock ktarg caster)
	result-ok)
	
(define (powers-locate caster ktarg power)
	(let ((loc (kern-obj-get-location caster)))
		(kern-log-msg "You are in " (kern-place-get-name (car loc)) 
                              " at [x=" (cadr loc) " y=" (caddr loc) "]"))
	result-ok)

(define (powers-magic-missile-range power)
	(+ 4 (/ power 3)))
		  
;todo messages out?
(define (powers-magic-missile kchar ktarg power)
	(kern-sound-play-at sound-missile (kern-obj-get-location kchar))
	(kern-sound-play-at sound-missile (kern-obj-get-location ktarg))
	(kern-log-msg (kern-obj-get-name kchar)
			" fires magic missile at "
			(kern-obj-get-name ktarg))
	(if (cast-missile-proc kchar ktarg t_magicarrow_p)
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
							 "magic" (kern-dice-roll damagedice) kchar)))
	result-ok)

(define (powers-negate-magic caster ktarg power)
	(kern-add-magic-negated (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1)))))
	result-ok)

(define (powers-paralyse caster ktarg power)
  	(if (and (can-paralyze? ktarg)
  				(contest-of-skill
						(+ power 5)
						(occ-ability-magicdef ktarg)))
        (kern-obj-add-effect ktarg ef_paralyze nil))
	result-ok)
		
(define (powers-poison-range power)
	(+ 3 (/ power 3)))
	
;todo contest to resist? to-hit roll required? power based initial damage?
;note instant hostility - you cant just cause someone to slowly die and say
;sorry afterwards
(define (powers-poison-effect caster ktarg power)
	(if (and (kern-obj-is-char? ktarg)
			(not (null? ktarg)))
		(begin
                  (if (contest-of-skill
                       power
                       (occ-ability-dexdefend ktarg))
                      (apply-poison ktarg)
                      (kern-log-msg (kern-obj-get-name ktarg) " avoids poison!")
		)))
	)

(define (powers-poison caster ktarg power)
	(define (do-poison-effect kmissile kuser ktarget kplace x y dam)
		(on-hit-target ktarget dam 
              				(lambda (obj) (powers-poison-effect kuser obj (+ power 10))))
		(on-hit-nontarget ktarget (loc-mk kplace x y) dam 
              				(lambda (obj) (powers-poison-effect kuser obj power)))
              			)
	(temp-ifc-set do-poison-effect)
	(kern-log-msg (kern-obj-get-name caster)
				" hurls poison missile at "
				(kern-obj-get-name ktarg))
	(kern-harm-relations ktarg caster)
	(kern-harm-relations ktarg caster)
	(kern-harm-relations ktarg caster)
	(kern-harm-relations ktarg caster)
	(kern-sound-play-at sound-missile (kern-obj-get-location caster))
	(kern-sound-play-at sound-missile (kern-obj-get-location ktarg))
	(cast-missile-proc caster ktarg t_mpoison_bolt)
	result-ok)

;todo duration based on power?
(define (powers-protect caster ktarg power)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (kern-obj-add-effect caster ef_protection nil)
        (kern-obj-add-effect party ef_protection nil)
        )
    )
	result-ok)

;todo duration based on power?
(define (powers-protect-vs-fire caster ktarg power)
	(kern-obj-add-effect ktarg ef_temporary_fire_immunity nil)
	result-ok)

;todo duration based on power?
(define (powers-protect-vs-poison caster ktarg power)
	(kern-obj-add-effect ktarg ef_temporary_poison_immunity nil)
	result-ok)

;todo duration based on power?
(define (powers-protect-vs-poison-all caster ktarg power)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (kern-obj-add-effect caster ef_temporary_poison_immunity nil)
        (kern-obj-add-effect party ef_temporary_poison_immunity nil)
        )
    )
	result-ok)

(define (powers-quickness caster dir power)
	(kern-add-quicken (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1)))))
	result-ok)

;note is different scenarios, could have other uses
(define (powers-raise-lost-area caster loc power)
	(let ((kobjs (filter can-raise-vessel? 
					(kern-get-objects-at loc))))
		(if (not (null? kobjs))
			(let ((kgen (car kobjs)))                
				(signal-kobj kgen 'raise kgen caster)
			)))
	result-ok)

;resurrect should have side effects, diminishing with power
(define (powers-resurrect caster ktarg power)
  (cond ((is-dead? ktarg)
         (kern-char-resurrect ktarg)
         (apply-sleep ktarg)
         result-ok)
        (else
         result-no-effect))
	result-ok)
				
(define (powers-reveal caster ktarg power)
	(kern-add-reveal (* power 4))
	result-ok)


(define (powers-sleep-target-range power)
	(+ (/ power 3) 3))

(define (powers-sleep-apply target power)
	(if (contest-of-skill power (occ-ability-magicdef target))
			(begin
				(msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " slept")
				(apply-sleep target))
			(begin 
				(msg-log-visible (kern-obj-get-location target) (kern-obj-get-name target) " resists sleep"))
	))
	
(define (powers-sleep-target caster ktarg power)
	(powers-sleep-apply ktarg (+ power 6))
	(kern-harm-relations ktarg caster)
  result-ok)

;todo limit to some range?
(define (powers-sleep-area caster ktarg power)
	(let ((hostiles (all-hostiles caster)))
		(define (trysleep target)
			(powers-sleep-apply target (+ power 3))
			)
       (cond ((null? hostiles) result-ok)
             (else
              (map trysleep hostiles)
              result-ok))))
                       
(define (powers-smoke-range power)
	(+ 3 (/ power 3)))
                   
(define (powers-smoke-field caster ktarg apower)
	(fields-smoke-apply (loc-place ktarg) (loc-x ktarg) (loc-y ktarg) apower)
	result-ok
	)
                       
;todo duration based on power?
(define (powers-spider-calm caster ktarg power)
	(kern-obj-add-effect ktarg ef_spider_calm nil)
	result-ok)

(define (powers-summon targetloc quantity typegen levelgen faction)
	(define (run-loop count done)
		(if (<= count 0) done
			(let* ((knpc (spawn-npc (typegen) (levelgen)))
					(loc (pick-loc targetloc knpc)))
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
(define (powers-summon-medium-size caster ktarg power type-tag)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(powers-summon-single-type type-tag)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))

;todo enable remote summoning for high power?
(define (powers-summon-snake caster ktarg power)
  (powers-summon-medium-size caster ktarg power 'snake)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-spider caster ktarg power)
  (powers-summon-medium-size caster ktarg power 'giant-spider)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-wolf caster ktarg power)
  (powers-summon-medium-size caster ktarg power 'wolf)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-small caster ktarg power type-tag)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 2 spower))
			(powers-summon-single-type type-tag)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	))

;todo enable remote summoning for high power?
(define (powers-summon-insect caster ktarg power)
  (powers-summon-small caster ktarg power 'insect)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-rat caster ktarg power)
  (powers-summon-small caster ktarg power 'rat)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-bat caster ktarg power)
  (powers-summon-small caster ktarg power 'bat)
	result-ok)

;todo enable remote summoning for high power?
(define (powers-summon-undead caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(lambda () 
                (random-select (list 'skeletal-warrior 'skeletal-spear-thrower 'ghast)))
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	)
	result-ok)
	
(define (powers-summon-slime caster ktarg power)
  (let ((spower (floor (+ (/ power 4) 1))))
	(powers-summon (kern-obj-get-location caster)
			(kern-dice-roll (mkdice 1 spower))
			(powers-summon-single-type 'green-slime)
			(powers-summon-simple-levelgen power)
			(kern-being-get-base-faction caster))
	)
	result-ok)
		 
	
(define (powers-telekinesis-range power)
	(+ (/ power 3) 1))
	
;todo damage/knock away critters?
;should fail on no handler squares rather than aborting?
(define (powers-telekinesis caster ktarg power)
  ((kobj-ifc ktarg) 'handle ktarg caster)
  result-ok)
	
(define (powers-timestop caster dir power)
	(kern-add-time-stop (kern-dice-roll
		(mkdice 3 (floor (+ (/ power 3) 1)))))
	result-ok)
	
; a few things needed here:
;	check for visibility before messages
;	no player specific messages
;	only hits hostiles
;   area of effect based on power
;	'turned' as an effect? [so it shows on description] or maybe fleeing should show...
(define (powers-turn-undead caster unused power)
	(define (is-undead-char? kobj)
		(and (obj-is-char? kobj)
		(species-is-undead? (kern-char-get-species kobj)))
		)
	(define (repel kchar)
		(if (contest-of-skill
				(+ power 3)
				(occ-ability-magicdef kchar))
			(let ((tloc (kern-obj-get-location kchar)))
				(kern-map-flash-sprite s_magicflash (loc-x tloc) (loc-y tloc))
				(msg-log-visible tloc (kern-obj-get-name kchar) " turned")
				(kern-char-set-fleeing kchar #t)
			)
			(begin
				(msg-log-visible (kern-obj-get-location kchar) (kern-obj-get-name kchar) " resists")
			)
		)
	)
	(let* ((all-kobjs (all-hostiles caster))
		(all-undead-combatants (filter is-undead-char? all-kobjs)))
			(map repel all-undead-combatants)
	)
	result-ok)
	
;todo limit to some (large) range?
(define (powers-tremor caster unused power)
	(let ((damdice (mkdice 1 power))
		(foes (all-hostiles caster)))
	(define (tremor kchar)
		(cond ((kern-char-is-asleep? kchar) (kern-char-set-sleep kchar #f))
			((> (kern-dice-roll "1d4") 1)
				(kern-map-set-jitter #t)
				(kern-map-repaint)
				(kern-char-set-sleep kchar #t)
				(kern-obj-inflict-damage kchar "knocked down" (kern-dice-roll damdice) caster))
				(else nil)))
	(define (loop n kchar)
		(if (not (= n 0))
			(begin
			(map tremor kchar)
			(loop (- n 1) kchar))))
	(define (wakeup kchar) (kern-char-set-sleep kchar #f))
    (kern-log-enable #f)
    (map kern-obj-inc-ref foes)
    (shake-map 20)
    (loop (+ 1 (floor (/ power 4))) foes)
    (kern-map-repaint)
    (map wakeup foes)
    (map kern-obj-dec-ref foes)
    (map wakeup (kern-place-get-beings (loc-place (kern-obj-get-location caster))))
	(kern-log-enable #t)
	)
	result-ok)

(define (powers-unlock caster ktarg power)
  (println "power:" power)
  (let ((dc ((kobj-ifc ktarg) 'get-unlock-dc ktarg caster)))
    (println "dc:" dc)
    (if (= 0 dc) 
        result-no-effect
        (let ((roll (kern-dice-roll "1d20"))
              (bonus (kern-dice-roll (string-append "1d" (number->string power)))))
          (println "roll:" roll)
          (println "bonus:" bonus)
          (cond ((or (= roll 20) 
                     (> (+ roll bonus ) dc))
                 (if ((kobj-ifc ktarg) 'unlock ktarg caster)
                     result-ok
                     result-no-effect))
                (else result-failed))))))

(define (powers-unlock-magic caster ktarg power)
  (if ((kobj-ifc ktarg) 'magic-unlock ktarg caster)
      result-ok
      result-no-effect))
	
(define (powers-view caster ktarg power)
	(kern-map-center-camera (kern-obj-get-location caster))
	(kern-map-set-peering #t)
	(kern-map-repaint)
	(kern-print "Hit a key when done gazing...\n")
	(ui-waitkey)
	(kern-map-set-peering #f)
	(kern-map-repaint)
	result-ok)

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
		(kern-sound-play-at sound-missile (kern-obj-get-location caster))
		(kern-sound-play-at sound-missile target)
		(kern-fire-missile t_mweb
                     (kern-obj-get-location caster)
                     target)
	)
	result-ok)

(define (powers-wind-change caster dir power)
	(kern-set-wind dir (+ 10 (kern-dice-roll (mkdice (* 2 power) 6))))
	result-ok)
	
(define (powers-xray caster dir power)
	(kern-add-xray-vision (kern-dice-roll
		(mkdice 10 (floor (+ (/ power 3) 1)))))
	result-ok)
		
;; vttjo - "Vectors to tiles jumped over"
(define (powers-jump-vttjo dx dy)
    (cond ((= dx 2)
           (cond ((= dy -1) (list (cons 1 0) (cons 1 -1)))
                 ((= dy 0)  (list (cons 1 0)))
                 ((= dy 1)  (list (cons 1 0) (cons 1 1)))
                 (else nil)))
          ((= dx 1)
           (cond ((= dy -2) (list (cons 0 -1) (cons 1 -1)))
                 ((= dy 2)  (list (cons 0 1) (cons 1 1)))
                 (else nil)))
          ((= dx 0)
           (cond ((= dy -2) (list (cons 0 -1)))
                 ((= dy 2) (list (cons 0 1)))
                 (else nil)))
          ((= dx -1)
           (cond ((= dy -2) (list (cons 0 -1) (cons -1 -1)))
                 ((= dy 2) (list (cons 0 1) (cons -1 1)))
                 (else nil)))
          ((= dx -2)
           (cond ((= dy -1) (list (cons -1 0) (cons -1 -1)))
                 ((= dy 0)  (list (cons -1 0)))
                 ((= dy 1)  (list (cons -1 0) (cons -1 1)))
                 (else nil)))
          (else nil)))

(define (powers-jump caster ktarg power)
  (let ((cloc (kern-obj-get-location caster)))

    ;; special case: when jumping 1 (or fewer tiles) use normal movement mode
    (define (jump-one)
      (cond ((not (kern-place-move-is-passable? cloc ktarg caster))
             (kern-log-msg "Jump failed: blocked!")
             result-no-effect)
            (else
             (kern-obj-relocate caster ktarg nil)
             result-ok)))

    (cond ((not (kern-place-is-passable ktarg caster))
           (kern-log-msg "Jump Failed: Impassable terrain")
           result-no-effect)
          (else
           (let* ((vect (loc-diff cloc ktarg))
                  (dx (loc-x vect))
                  (dy (loc-y vect))
                  (kplace (loc-place (kern-obj-get-location caster))))
             (cond ((and (<= (abs dx) 1) (<= (abs dy) 1))
                    (jump-one))
                   (else
                    ;; normal case: jump of more than 1 tile
                    (kern-obj-set-mmode caster mmode-jump)
                    (let* ((vttjo (powers-jump-vttjo dx dy))
                          (result
                           (cond ((foldr (lambda (val vtt)
                                           (or val
                                               (not (kern-place-is-passable 
                                                     (mk-loc kplace 
                                                             (+ (car vtt) (loc-x cloc))
                                                             (+ (cdr vtt) (loc-y cloc)))
                                                     caster))))
                                         #f vttjo)
                                  (kern-log-msg "Jump failed: blocked!")
                                  result-no-effect)
                                 (else
                                  (kern-obj-relocate caster ktarg nil)
                                  (kern-obj-add-effect caster ef_fatigue nil)
                                  result-ok))))
                      (kern-obj-set-mmode caster nil)
                      result))))))))

(define (powers-sprint caster ktarg power)
  ;; hokay... first let's get the path from here to there
  (let* ((origin (kern-obj-get-location caster))
         (kplace (loc-place origin))
         (path (line (loc-x origin) (loc-y origin) 
                     (loc-x ktarg) (loc-y ktarg)))
         )
    ;; and now, for each point on the path, let's move the dude there and apply
    ;; any terrain/field effects. The way should be passable (unless we do
    ;; something weird like along the way trigger a mech which throws up a
    ;; wall, in which case I guess that's an advantage of having the sprint
    ;; skill ;) Note that the dude may die along the way due to tile effects,
    ;; so keep a ref count just to be safe and check for death in the move-dude
    ;; function.
    (define (move-dude ok xy)
      (and ok
           (let ((loc (loc-mk kplace (car xy) (cdr xy))))
             (cond ((or (not (passable? loc caster))
                        (occupied? loc))
                    (println loc " impassable")
                    #f
                    )
                   ((not (kern-char-is-dead? caster))
                    (kern-obj-relocate caster loc nil)
                    (kern-map-repaint)
                    (kern-place-apply-tile-effects kplace caster)
                    #t
                    )
                   ))))
    (kern-obj-inc-ref caster)
    (foldr move-dude #t (cdr path))
    (kern-obj-dec-ref caster)
    )
  (kern-obj-add-effect caster ef_fatigue nil)
  result-ok)

;; Roll to even make the attempt, then roll to see if you get stuck.
(define (powers-wriggle caster ktarg power)
  (kern-obj-set-mmode caster mmode-wriggle)
  (cond ((not (kern-place-move-is-passable? (kern-obj-get-location caster)
                                            ktarg caster))
         (kern-log-msg "Wriggle failed: blocked!")
         (kern-obj-set-mmode caster nil)
         result-not-here)
        (else
         (kern-obj-relocate caster ktarg nil)
         (kern-obj-set-mmode caster nil)
         (cond ((passable? (kern-obj-get-location caster) caster)
                (kern-log-msg "(Was that really necessary?)")
                result-ok
                )
               ((not (check-roll dc-avoid-stuck (occ-thief-dice-roll caster)))
                (kern-obj-add-effect caster ef_stuck nil)
                result-failed
                )
               (else
                result-ok
                )))))

(define (powers-butcher caster ktarg power)
  (if ((kobj-ifc ktarg) 'butcher ktarg caster)
      result-ok
      result-no-effect))

(define (powers-pickpocket kactor ktarg power)
  (cond ((contest-of-skill power (occ-ability-thief ktarg))
         (let ((ktype (kern-ui-select-item ktarg)))
           (cond ((null? ktype) result-no-effect)
                 (else
                  (kern-obj-remove-from-inventory ktarg ktype 1)
		  (if (ktype-can? ktype 'receive)
			((kern-type-get-gifc ktype) 'receive ktype kactor)
		  )
                  (kern-obj-add-to-inventory kactor ktype 1)
                  result-ok
                  ))))
        (else
         (harm-relations kactor ktarg)
         result-failed
         )))
  