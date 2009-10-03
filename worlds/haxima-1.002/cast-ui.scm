;; ui-target-char -- return the first available object for which 'pred' returns
;; true at a user-specified location on the map
(define (ui-target origin range pred)
  (define (select-from seq)
    (cond ((null? seq) 
           nil)
          (else (car seq))))
  (let ((coords (kern-ui-target origin range)))
    (cond ((null? coords) nil)
          (else (select-from (filter pred (kern-get-objects-at coords)))))))

(define (ui-get-direction)
  (kern-ui-direction))

(define (ui-get-adjacent origin pred)
  (define (select-from seq)
    (cond ((null? seq) 
           (kern-print "Nothing!\n") 
           nil)
          (else
           (car seq))))
  (let ((dir (ui-get-direction)))
    (if (null? dir) nil
        (select-from (filter pred (kern-get-objects-at (loc-offset origin dir)))))))

(define (ui-waitkey)
  (kern-ui-waitkey))

(define (mk-ifc-query signal)
  (lambda (kobj) (kobj-can? kobj signal)))

(define (neighbors loc)
  (let ((place (car loc))
        (x (cadr loc))
        (y (caddr loc)))
    (list (list place x (- y 1))
          (list place x (+ y 1))
          (list place (- x 1) y)
          (list place (+ x 1) y))))

(define (get-target-kchar caster range)
  (let ((loc (get-target-loc caster range)))
    (if (null? loc)
        nil
        (get-being-at loc))))

;;--------------------------------------------------------------
;; Need to sort this stuff out
;;--------------------------------------------------------------

(define (cast-field-spell caster field-type)
  (let ((coords (kern-ui-target (kern-obj-get-location caster) 1)))
    (cond ((null? coords) nil)
          (else
           (kern-obj-put-at (kern-mk-obj field-type 1) coords)))))

(define (cast-teleport-spell caster dir)
  (let ((coords (loc-offset (kern-obj-get-location caster) dir)))
    (cond ((null? coords) (kern-print "You sense nothing there!\n"))
          ((not (passable? coords caster)) (kern-print "You sense it is impassable!\n"))
          (else (kern-obj-relocate caster coords nil)))))

(define (cast-signal-spell caster signal target)
  (cond ((null? target) result-no-target)
        (else 
         ((kobj-ifc target) signal target caster)
         result-ok
         )))

(define (cast-bimodal caster proc)
  (define (cast-it target)
    (cond ((null? target) result-no-target)
          (else (proc target))))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
  
(define (user-cast-spell-on-party-member caster proc)
  (define (cast-it target)
    (cond ((null? target) result-no-target)
          (else (proc caster target)
                result-ok)))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
	  
(define (user-cast-ranged-targeted-spell kchar range proc)
  (let ((ktarg (get-target-kchar kchar range)))
    (if (null? ktarg)
        result-no-target
        (begin
          (proc kchar ktarg)
          result-ok))))  
	  
(define (cast-ui-target-party-member caster range)
	(let ((loc (kern-obj-get-location caster)))
		(if (kern-place-is-wilderness? (loc-place loc))
			(kern-ui-select-party-member)
			(ui-target loc range obj-is-char?))))
	   
(define (cast-ui-target-char caster range)
	(let ((loc (get-target-loc caster range)))
		(if (null? loc)
			nil
			(get-being-at loc))))
			
(define (cast-ui-target-any caster range checkproc)
	(ui-target (kern-obj-get-location caster)
		range
		checkproc))

(define (cast-ui-dospell target effect caster power)
  (cond ((null? target) result-no-target)
        ((kern-in-los? (kern-obj-get-location caster) (kern-obj-get-location target))
         ;; note in this case we should let the effect procedure determine the
         ;; result (fixme: need to go through and ensure all the powers return
         ;; a result-* codee)
         (effect caster target power))
        (else
         (kern-log-msg "Not in LOS!")
         result-no-target)))


(define (cast-ui-dospell-loc target effect caster power)
	(cond ((null? target) result-no-target)
		((kern-in-los? (kern-obj-get-location caster) target)
			(effect caster target power))
		(else
			(kern-log-msg "Not in LOS!")
			result-no-target)))

(define (cast-ui-dospell-nolos target effect caster power)
	(if (null? target)
		(begin 
			result-no-target
			)
		(begin
			(effect caster target power)
			result-ok)))
			
			
			
(define (cast-ui-basic-member-spell effect caster power)
	(cast-ui-dospell
		(cast-ui-target-party-member caster 2)
		effect caster power))
		
(define (cast-ui-basic-ranged-spell effect caster range power)
	(cast-ui-dospell
		(cast-ui-target-char caster range)
		effect caster power))

(define (cast-ui-ranged-loc effect caster range power)
	(cast-ui-dospell-loc
		(kern-ui-target (kern-obj-get-location caster) range)
		effect caster power))
		
(define (cast-ui-ranged-loc-nolos effect caster range power)
	(cast-ui-dospell-nolos
		(kern-ui-target (kern-obj-get-location caster) range)
		effect caster power))
		
(define (cast-ui-ranged-any effect caster range power targetcheck)
	(cast-ui-dospell
		(cast-ui-target-any caster range targetcheck)
		effect caster power))
		
(define (cast-ui-field effect caster range power)
	(let ((target (kern-ui-target (kern-obj-get-location caster) range)))
		(cond ((null? target) result-no-target)
			((not (terrain-ok-for-field? target)) result-no-effect)
			((kern-in-los? (kern-obj-get-location caster) target)
				(effect caster target power)
				result-ok)
			(else 
				(kern-log-msg "Not in LOS!")
				result-no-target))))	

(define (cast-ui-wall effect caster range power)
	(let ((target (kern-ui-target (kern-obj-get-location caster) range)))
		(cond ((null? target) result-no-target)
			((not (kern-in-los? (kern-obj-get-location caster) target)) (kern-log-msg "Not in LOS!") result-no-target)
			(else 
				(let ((targetb (kern-ui-target (kern-obj-get-location caster) range)))
					(cond ((and (not (null? targetb)) (not (kern-in-los? (kern-obj-get-location caster) target))) (kern-log-msg "Not in LOS!") result-no-target)
						(else
							(effect caster target (if (null? targetb) target targetb) power)
							result-ok
						))
				)))
	))

(define (cast-ui-template-loc effect caster template power)
  (cast-ui-dospell-loc
   (kern-ui-target-generic (kern-obj-get-location caster)
                           (kern-obj-get-location caster) ;; fixme: cursor-loc
                           template
                           nil ;; fixme: suggested-locs
                           nil ;; fixme: cursor-move-cb
                           nil ;; fixme: cursor-select-cb
                           nil ;; fixme: gob
                           )
   effect caster power))

(define (cast-ui-party-spell effect caster power)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (effect caster caster power)
        (foldr (lambda (final-result target)
                 (let ((result (effect caster target power)))
                   (if (= result-ok final-result)
                       final-result
                       result)))
               result-no-effect
               (kern-party-get-members party)))))

;;----------------------------------------------------------------------------
;; First Circle
;;----------------------------------------------------------------------------

(define (an-nox caster)
	(cast-ui-basic-member-spell powers-cure-poison
		caster (occ-ability-whitemagic caster)))

(define (an-zu  caster)
	(cast-ui-basic-member-spell powers-awaken
		caster (occ-ability-whitemagic caster)))

(define (grav-por caster)
	(cast-ui-basic-ranged-spell powers-magic-missile
		caster 
		(powers-magic-missile-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (in-lor caster)
	(powers-light caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (an-xen-bet  caster)
  (powers-spider-calm caster caster (occ-ability-whitemagic caster))
  result-ok)

(define (mani caster)
	(cast-ui-basic-member-spell powers-heal
		caster (occ-ability-whitemagic caster)))
		
;todo currently only checks topmost item
(define (wis-sanct caster)
  (cast-ui-ranged-any powers-detect-traps
                      caster 1 (ceiling (/ (+ (occ-ability-whitemagic caster)
                                              (occ-ability-thief caster))
                                           2))
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 'get-traps)))
                      ))

;todo currently only checks topmost item
(define (an-sanct-ylem caster)
  (cast-ui-ranged-any powers-disarm-traps
                      caster 1 (ceiling (/ (+ (occ-ability-whitemagic caster)
                                              (occ-ability-thief caster))
                                           2))
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 'rm-traps)))
                      ))
	
(define (ylem-an-ex  caster)
	(cast-ui-ranged-loc powers-web caster
		(powers-web-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))
		
(define (bet-ylem-hur caster)
	(cast-ui-ranged-loc powers-smoke-field caster
		(powers-smoke-range (occ-ability-whitemagic caster))
		(occ-ability-whitemagic caster)))
	
	
;;----------------------------------------------------------------------------
;; Second Circle
;;----------------------------------------------------------------------------
(define (an-sanct  caster)
	(cast-ui-ranged-any powers-unlock
                            caster 1 (ceiling (/ (+ (occ-ability-whitemagic caster)
                                                    (occ-ability-thief caster))
                                                 2))
                            (mk-ifc-query 'unlock)))

(define (sanct  caster)
	(cast-ui-ranged-any powers-lock
		caster 1 (occ-ability-whitemagic caster)
		(mk-ifc-query 'lock)))
		
(define (sanct-nox  caster)
	(cast-ui-basic-member-spell powers-protect-vs-poison
		caster (occ-ability-whitemagic caster)))

(define (an-xen-corp caster)
	(powers-turn-undead caster nil (occ-ability-blackmagic caster))
	result-ok)

(define (in-wis caster)
  (powers-locate caster nil nil)
  result-ok)

(define (in-bet-xen  caster)
  (let ((power (occ-ability-whitemagic caster)))
    (cond ((< power 7) (powers-summon-insect caster caster power))
          ((< power 8) (powers-summon-rat caster caster power))
          (else (powers-summon-bat caster caster power)))
    result-ok))

(define (rel-hur  caster)
  (let ((dir (ui-get-direction)))
    (cond ((or (null? dir)
               (= here dir))
           result-no-target)
          (else 
           (powers-wind-change caster dir (occ-ability-whitemagic caster))
           result-ok
           ))))

(define (in-nox-por  caster)
 	(cast-ui-basic-ranged-spell powers-poison
		caster 
		(powers-poison-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))
  
(define (bet-flam-hur caster)
 	(cast-ui-ranged-loc-nolos powers-cone-flamespray
		caster 
		4
		(occ-ability-blackmagic caster)))
		
(define (xen-zu caster)
	(cast-ui-basic-ranged-spell powers-sleep-target
		caster 
		(powers-sleep-target-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

;;----------------------------------------------------------------------------
;; Third Circle
;;----------------------------------------------------------------------------
(define (in-flam-grav caster)
  (cast-ui-wall powers-field-fire-wall caster (powers-field-range (occ-ability-whitemagic caster)) (occ-ability-whitemagic caster)))

(define (in-nox-grav  caster)
  (cast-ui-wall powers-field-poison-wall caster (powers-field-range (occ-ability-whitemagic caster)) (occ-ability-whitemagic caster)))

(define (in-zu-grav  caster)
  (cast-ui-wall powers-field-sleep-wall caster (powers-field-range (occ-ability-whitemagic caster)) (occ-ability-whitemagic caster)))
  
(define (vas-flam  caster)
	(cast-ui-ranged-loc powers-fireball caster
		(powers-fireball-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (vas-lor  caster)
	(powers-great-light caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (in-flam-sanct caster)
	(cast-ui-basic-member-spell powers-protect-vs-fire
			caster (occ-ability-whitemagic caster)))

(define (in-nox-sanct caster)
	(cast-ui-basic-member-spell powers-protect-vs-poison
			caster (occ-ability-whitemagic caster)))

(define (vas-an-nox caster)
  (cast-ui-party-spell powers-cure-poison
                       caster (occ-ability-whitemagic caster)))

(define (an-ort-xen caster)
	(cast-ui-basic-member-spell powers-dispel-magic
		caster (occ-ability-whitemagic caster)))

;;----------------------------------------------------------------------------
;; Fourth Circle
;;----------------------------------------------------------------------------
(define (an-grav  caster)
	(cast-ui-ranged-any powers-dispel-field
		caster 1 (occ-ability-whitemagic caster)
		is-field?))
		
;leaving alone at the moment (not used)
(define (uus-por  caster)
  (cast-teleport-spell caster up))

;leaving alone at the moment (not used)
(define (des-por  caster)
  (cast-teleport-spell caster down))

(define (in-sanct-grav  caster)
  (cast-ui-wall powers-field-energy-wall caster (powers-field-range (occ-ability-whitemagic caster)) (occ-ability-whitemagic caster)))

(define (in-sanct  caster)
	(powers-protect caster caster (occ-ability-whitemagic caster))
		result-ok)

(define (wis-quas  caster)
	(powers-reveal caster caster (occ-ability-blackmagic caster))
	result-ok)

;; bet-por -- single character blink
(define (bet-por caster)
	(cast-ui-ranged-loc powers-blink caster
		(powers-blink-range (occ-ability-whitemagic caster))
		(occ-ability-whitemagic caster)))
		
(define (vas-sanct-nox  caster)
	(powers-protect-vs-poison-all caster caster (occ-ability-whitemagic caster))
		result-ok)

(define (ort-grav  caster)
	(cast-ui-ranged-loc powers-lightning caster
		(powers-lightning-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))
		
;;----------------------------------------------------------------------------
;; Fifth Circle
;;----------------------------------------------------------------------------
(define (in-ex-por  caster)
	(cast-ui-ranged-any powers-unlock-magic
		caster 1 (occ-ability-whitemagic caster)
		(mk-ifc-query 'magic-unlock)))

(define (an-ex-por  caster)
	(cast-ui-ranged-any powers-lock-magic
		caster 1 (occ-ability-whitemagic caster)
		(mk-ifc-query 'magic-lock)))

(define (in-zu  caster)
        (powers-sleep-area caster caster (occ-ability-blackmagic caster)))

(define (vas-mani  caster)
	(cast-ui-basic-member-spell powers-great-heal
		caster (occ-ability-whitemagic caster)))
		
(define (rel-tym  caster)
	(powers-quickness caster caster (occ-ability-whitemagic caster))
	result-ok)
	

(define (kal-xen caster)
  (let ((power (occ-ability-whitemagic caster)))
    (cond ((< power 9) (powers-summon-snake caster caster power))
          ((< power 10) (powers-summon-spider caster caster power))
          (else (powers-summon-wolf caster caster power)))
    result-ok))
	
;; ----------------------------------------------------------------------------
;; Sixth Circle
;; ----------------------------------------------------------------------------
(define (in-an  caster) 
	(powers-negate-magic caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (in-rel-por caster)
	(cast-ui-ranged-any powers-telekinesis
		caster
		(powers-telekinesis-range (occ-ability-whitemagic caster))
		(occ-ability-whitemagic caster)
		kern-obj-is-mech?))

(define (wis-an-ylem caster) 
	(powers-xray caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (an-xen-ex  caster)
	(cast-ui-basic-ranged-spell powers-charm
		caster 
		(powers-charm-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (rel-xen-quas caster)
  (cast-ui-basic-ranged-spell powers-beastly-illusion
                              caster 
                              (powers-charm-range (occ-ability-blackmagic caster))
                              (occ-ability-blackmagic caster))
  )

(define (in-vas-por-ylem  caster)
	(powers-tremor caster caster (occ-ability-blackmagic caster))
	result-ok)

(define (quas-an-wis  caster)
	(powers-confuse caster caster (occ-ability-blackmagic caster))
	result-ok)

;; vas-uus-ylem -- special spell which raises a sunken ship
(define (vas-uus-ylem caster)
	(cast-ui-ranged-loc powers-raise-lost-area caster
		1 (occ-ability-whitemagic caster)))

;; vas-por -- whole party blink
(define (vas-por caster)
	(cast-ui-ranged-loc powers-blink-party caster
		(powers-blink-party-range (occ-ability-whitemagic caster))
		(occ-ability-whitemagic caster)))

;; ----------------------------------------------------------------------------
;; Seventh Circle
;; ----------------------------------------------------------------------------

(define (in-nox-hur  caster)
 	(cast-ui-ranged-loc-nolos powers-cone-poison
		caster 
		(powers-cone-basic-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (in-zu-hur  caster)
 	(cast-ui-ranged-loc-nolos powers-cone-sleep
		caster 
		(powers-cone-basic-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (in-quas-corp  caster)
	(powers-fear caster caster (occ-ability-blackmagic caster))
	result-ok)

(define (in-quas-wis  caster)
	(powers-view caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (sanct-lor  caster)
	(cast-ui-basic-member-spell powers-invisibility
			caster (occ-ability-whitemagic caster)))

(define (in-quas-xen  caster)
	(cast-ui-basic-ranged-spell powers-clone
		caster 
		(powers-clone-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

;; ----------------------------------------------------------------------------
;; Eighth Circle
;; ----------------------------------------------------------------------------

(define (in-flam-hur caster)
 	(cast-ui-ranged-loc-nolos powers-cone-fire
		caster 
		(+ 2 (powers-cone-basic-range (occ-ability-blackmagic caster)))
		(occ-ability-blackmagic caster)))

(define (in-vas-grav-corp  caster)
 	(cast-ui-ranged-loc-nolos powers-cone-energy
		caster 
		(powers-cone-basic-range (occ-ability-blackmagic caster))
		(occ-ability-blackmagic caster)))

(define (an-tym  caster)
	(powers-timestop caster caster (occ-ability-whitemagic caster))
	result-ok)

(define (kal-xen-corp caster)
	(powers-summon-undead caster caster (occ-ability-blackmagic caster))
	result-ok)

(define (xen-corp  caster)
	(let ((range (+ 1 (floor (/ (occ-ability-blackmagic caster) 3)))))
	(user-cast-ranged-targeted-spell caster range cast-kill-proc)))

(define (in-mani-corp  caster)
	(cast-ui-dospell-nolos
		(kern-ui-select-party-member)
			powers-resurrect caster (occ-ability-whitemagic caster)))
			
(define (vas-rel-por  caster)
	(let* ((loc (kern-obj-get-location caster))
			(range (if 
				(kern-place-is-wilderness? (loc-place loc))
				1
				2)))
		(cast-ui-ranged-loc powers-gate-travel
			caster 
			range
			(occ-ability-whitemagic caster))))
	
(define (kal-xen-nox caster)
	(powers-summon-slime caster caster (occ-ability-whitemagic caster))
	result-ok)
