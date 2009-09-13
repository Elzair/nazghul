;;----------------------------------------------------------------------------
;; Ability "class"
;;----------------------------------------------------------------------------

(define default-aap 50)

(define (mk-ability name level mana ap rng proc)
  (list name level mana (* ap default-aap) proc rng))

(define (ability-name ability) (car ability))
(define (ability-level-required ability) (cadr ability))
(define (ability-mana-cost ability) (caddr ability))
(define (ability-ap-cost ability) (cadddr ability))
(define (ability-proc ability) (list-ref ability 4))
(define (ability-range ability) (list-ref ability 5))

(define (can-use-ability? ability kchar)
  ;;(println " can-use-ability?" display ability)
  (and (<= (kern-get-magic-negated) 0)
       (>= (kern-char-get-mana kchar)
           (ability-mana-cost ability))
       (>= (kern-char-get-level kchar)
           (ability-level-required ability))))

(define (use-ability ability kchar . args)
  (let ((result (apply (ability-proc ability) (cons kchar args))))
  	 (if result
  	 	(begin
		  (kern-char-dec-mana kchar (ability-mana-cost ability))
		  (kern-obj-dec-ap kchar (ability-ap-cost ability))
  	 	)
  	 )
    (if (<= (kern-char-get-mana kchar) 0)
        (kern-log-msg (kern-obj-get-name kchar) " is exhausted!"))
    result))


;;----------------------------------------------------------------------------
;; Ability procedures
;;----------------------------------------------------------------------------

(define (vampiric-touch-proc kchar ktarg)
  (let ((amount (min (* (kern-dice-roll "1d3")
                        (kern-char-get-level kchar))
                     (kern-char-get-hp ktarg))))
    (kern-obj-inc-ref ktarg)
    (kern-obj-apply-damage ktarg "life drained" amount)
    (kern-obj-heal kchar amount)
    (kern-log-msg (kern-obj-get-name kchar)
                  " drains life from "
                  (kern-obj-get-name ktarg)
                  "!")
    (kern-obj-dec-ref ktarg))
  #t)

(define (disease-touch-proc kchar ktarg)
  (if (kern-obj-add-effect ktarg ef_disease nil)
      (kern-log-msg (kern-obj-get-name kchar)
                    " inflicts "
                    (kern-obj-get-name ktarg)
                    " with Disease!"))
  #t)

(define (disarm kchar ktarg)
  (let ((readied (kern-char-get-readied-weapons ktarg)))
    (if (null? readied)
        #f
        (if (> (kern-char-get-level kchar)
               (+ (kern-dice-roll "1d3-1")
                  (kern-char-get-level ktarg)))
            (let ((ktype (random-select readied)))
              (kern-log-msg (kern-obj-get-name kchar)
                            " disarms "
                            (kern-obj-get-name ktarg))
              (kern-char-unready ktarg ktype)
              (kern-obj-remove-from-inventory ktarg ktype 1)
              (kern-obj-put-at (kern-mk-obj ktype 1)
                               (kern-obj-get-location ktarg))
              )
            (kern-log-msg  (kern-obj-get-name kchar)
                           " fails to disarm "
                           (kern-obj-get-name ktarg))
            #t))))

(define (heal-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
	(kern-obj-heal ktarg 
		(+ 2 (kern-dice-roll "1d10")
			(kern-dice-roll (string-append "2d" (number->string (occ-ability-whitemagic kchar)))))))

(define (great-heal-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a great healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
  (kern-obj-heal ktarg (kern-dice-roll "4d20+20")))

;;----------------------------------------------------------------------------
;; field spells
(define (cast-field-proc kchar loc ktype)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts "(kern-type-get-name ktype) "!")
  (kern-obj-put-at (kern-mk-obj ktype 1) loc))
  
(define (cast-fire-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_fire))
  
(define (cast-poison-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_poison))
  
(define (cast-sleep-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_sleep))
  
(define (cast-energy-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_energy))
  
;;----------------------------------------------------------------------------
;; missile spells

;; cast-magic-missile-proc -- damage goes up with level of caster
(define (cast-magic-missile-proc kchar ktarg)
	(powers-magic-missile kchar ktarg (occ-ability-blackmagic kchar)))

(define (cast-poison-missile-proc kchar ktarg)
   	(powers-poison kchar ktarg (occ-ability-blackmagic kchar)))

(define (cast-fireball-proc kchar ktarg)
	(let ((target (kern-obj-get-location ktarg))
			(power (occ-ability-blackmagic kchar)))
		(and (powers-fireball-collateral-check kchar target power)
			(powers-fireball kchar target power))
	))

(define (cast-kill-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts kill at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_deathball))

(define (cast-acid-missile-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " hurls acid missile at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_acid_bolt))

(define (web-spew-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " spews web at "
                (kern-obj-get-name ktarg))
  (define (spew-in-dir dir)
    (define (ensnare-loc loc)
      (kern-obj-put-at (kern-mk-obj web-type 1) loc))
    (let ((loc (kern-obj-get-location kchar)))
      (cast-wind-spell2 loc
                        ensnare-loc
                        dir
                        (/ (kern-char-get-level kchar) 2))))
  (let* ((v (loc-diff (kern-obj-get-location kchar)
                      (kern-obj-get-location ktarg)
                      ))
         (dir (loc-to-cardinal-dir v)))
    (spew-in-dir dir)))

(define (teleport-proc kchar loc)
  (kern-log-msg (kern-obj-get-name kchar)
                " teleports")
  (kern-obj-relocate kchar loc nil))

(define (fire-wind-proc3 kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " blasts fire at "
                (kern-obj-get-name ktarg))
  (define (spew-in-dir dir)
    (define (ensnare-loc loc)
      (kern-obj-put-at (kern-mk-obj F_fire 1) loc))
    (let ((loc (kern-obj-get-location kchar)))
      (cast-wind-spell2 loc
                        ensnare-loc
                        dir
                        4)))
  (let* ((v (loc-diff (kern-obj-get-location kchar)
                      (kern-obj-get-location ktarg)
                      ))
         (dir (loc-to-cardinal-dir v)))
    (spew-in-dir dir)))
    
(define (fire-wind-proc kchar ktarg)
	;;(println "flamewind")
	(let ((target (kern-obj-get-location ktarg))
			(power (occ-ability-blackmagic kchar)))
		(and (powers-cone-fire-test kchar target power)
			(begin 
				;;(println "flamewind2")
				(kern-log-msg (kern-obj-get-name kchar)
	                " blasts fire at "
	                (kern-obj-get-name ktarg))
		   	(powers-cone-fire kchar target power)
		  ))
	))

(define (lightning-bolt-proc kchar ktarg)
	(let ((target (kern-obj-get-location ktarg))
			(power (occ-ability-blackmagic kchar)))
		(and (powers-lightning-collateral-check kchar target power)
			(begin (kern-log-msg (kern-obj-get-name kchar)
	                " streams lightning at "
	                (kern-obj-get-name ktarg))
		   	(powers-lightning kchar target power)
		   ))
	))
  

;;----------------------------------------------------------------------------
;; summoning
(define (cast-summon-proc kchar gen-npct quantity)
  (define (run-loop count)
    (cond ((<= count 0) 0)
          (else
           (let* ((lvl (+ (kern-dice-roll "1d2") (/ (kern-char-get-level kchar) 2)))
                  (knpc (spawn-npc (gen-npct) lvl))
                  (loc (pick-loc (kern-obj-get-location kchar) knpc))
                  )
             (cond ((null? loc) 
                    (kern-obj-dec-ref knpc)
                    0)
                   (else
                    (kern-being-set-base-faction knpc (kern-being-get-base-faction kchar))
                    (kern-obj-set-temporary knpc #t)
                    (kern-obj-put-at knpc loc)
                    (+ 1 (run-loop (- count 1)))))))))
  (cond ((> (run-loop quantity)
            0)
         (kern-log-msg (kern-obj-get-name kchar) " summons help")
         #t)
        (else
         (kern-log-msg (kern-obj-get-name kchar) " fails to summon help")
         #f)))

(define (summon-skeleton-proc kchar)
  ;;(println "summon-skeleton-proc")
  (cast-summon-proc kchar
                    (lambda () 
                      (random-select (list 'skeletal-warrior 'skeletal-spear-thrower)))
                    (/ (kern-char-get-level kchar) 2)
                    ))
                    
(define (summon-slime-proc kchar)
  ;;(println "summon-slime-proc")
  (cast-summon-proc kchar
                    (lambda () 'green-slime)
                    (/ (kern-char-get-level kchar) 2)
                    ))

(define (summon-demon-proc kchar)
  (cast-summon-proc kchar
                    (lambda () 'demon)
                    1))

(define (summon-wolf-proc kchar)
  (cast-summon-proc kchar
                    (lambda () 'wolf)
                    1))

(define (summon-ratling-proc kchar)
  (cast-summon-proc kchar
                    (lambda () 'ratling-swarmer)
                    (* (kern-char-get-level kchar) 3)
                    ))

;;----------------------------------------------------------------------------
;; enslave -- aka charm
(define (enslave-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " enslaves "
                (kern-obj-get-name ktarg))
  (kern-obj-add-effect ktarg 
                       ef_charm 
                       (charm-mk (kern-being-get-current-faction kchar))))

;;----------------------------------------------------------------------------
;; chomp-deck -- convert deck terrain into shallow water terrain
(define (chomp-deck-proc kchar loc)
  (cond ((not (is-deck? (kern-place-get-terrain loc))) #f)
  		  ((not (null? (get-being-at loc))) #f)
        (else
         (kern-place-set-terrain loc t_shallow)
         (msg-log-visible (kern-obj-get-location kchar) (kern-obj-get-name kchar) " chomps through the deck!")
         (map kern-obj-remove
         	(kern-get-objects-at loc))
         (if (kern-place-is-combat-map? (loc-place loc))
				(let* ((vehicle (kern-party-get-vehicle (kern-get-player))))
					(if (not (null? vehicle))
						(begin 
						   (shake-map 10)
							(kern-obj-apply-damage vehicle "breakage" (floor (/ (kern-obj-get-hp vehicle) 7)))
						)
					)
				)
			)
         #t)))

(define (deck-to-sludge-proc kchar loc)
  (cond ((not (is-deck? (kern-place-get-terrain loc))) #f)
        (else
         (kern-place-set-terrain loc t_shallow_sludge)
         (kern-log-msg (kern-obj-get-name kchar) " chomps through the deck!")
         #t)))

;;----------------------------------------------------------------------------
;; narcotize -- mass sleep
(define (narcotize-proc kchar)
  (let ((hostiles (all-hostiles kchar)))
    (cond ((null? hostiles) #f)
          (else
           (kern-log-msg (kern-obj-get-name kchar)
                         " beckons slumber to its foes")
           (map (lambda (ktarg)
                  (if (> (- (+ (kern-dice-roll "1d20") 
                               (kern-char-get-level kchar)) 
                            (kern-char-get-level ktarg))
                         12)
                      (begin
                        (apply-sleep ktarg)
                        (kern-log-msg (kern-obj-get-name ktarg) " succumbs!")
                        )
                      (kern-log-msg (kern-obj-get-name ktarg) " resists!")))
                hostiles)
           #t))))

;;----------------------------------------------------------------------------
;; turn invisible
(define (turn-invisible-proc kchar)
  (kern-log-msg (kern-obj-get-name kchar)
                " vanishes!")
  (kern-obj-add-effect kchar ef_invisibility nil))

;;----------------------------------------------------------------------------
;; Ability declarations
;;
;;  L = level
;;  M = mana
;;  A = action points
;;  R = range
;;
;;----------------------------------------------------------------------------

;;                                      name                  L M A R proc
(define vampiric-touch      (mk-ability "vampiric touch"      3 3 2 1 vampiric-touch-proc))
(define disease-touch       (mk-ability "disease touch"       6 6 1 1 disease-touch-proc))
(define disarm              (mk-ability "disarm"              4 2 2 1 disarm))
(define heal-ability        (mk-ability "heal"                1 1 1 2 heal-proc))
(define great-heal-ability  (mk-ability "great heal"          4 4 2 2 great-heal-proc))
(define cast-fire-field     (mk-ability "cast fire field"     3 3 2 1 cast-fire-field-proc))
(define cast-poison-field   (mk-ability "cast poison field"   3 3 2 1 cast-poison-field-proc))
(define cast-sleep-field    (mk-ability "cast sleep field"    3 3 2 1 cast-sleep-field-proc))
(define cast-energy-field   (mk-ability "cast energy field"   4 4 2 1 cast-energy-field-proc))
(define cast-magic-missile  (mk-ability "cast magic missile"  1 1 1 6 cast-magic-missile-proc))
(define cast-poison-missile (mk-ability "cast poison missile" 2 2 1 6 cast-poison-missile-proc))
(define cast-fireball       (mk-ability "cast fireball"       3 3 1 6 cast-fireball-proc))
(define cast-kill           (mk-ability "cast kill"           7 7 2 4 cast-kill-proc))
(define cast-acid-missile   (mk-ability "cast acid missile"   4 4 1 4 cast-acid-missile-proc))
(define web-spew            (mk-ability "spew web"            4 4 2 5 web-spew-proc))
(define teleport            (mk-ability "teleport"            6 6 2 0 teleport-proc))
(define summon-skeleton     (mk-ability "summon skeleton"     6 6 4 0 summon-skeleton-proc))
(define summon-slimes       (mk-ability "summon slimes"       2 2 3 0 summon-slime-proc))
(define summon-demon        (mk-ability "summon demon"        8 8 6 0 summon-demon-proc))
(define summon-wolves       (mk-ability "summon wolves"       4 4 2 0 summon-wolf-proc))
(define summon-ratlings     (mk-ability "summon ratlings"     1 2 4 0 summon-ratling-proc))
(define chomp-deck          (mk-ability "chomp deck"          2 4 3 1 chomp-deck-proc))
(define deck-to-sludge      (mk-ability "chomp deck"          1 1 1 1 deck-to-sludge-proc))
(define enslave             (mk-ability "enslave"             3 4 2 4 enslave-proc))
(define narcotize           (mk-ability "narcotize"           5 6 3 0 narcotize-proc))
(define cast-fire-wind      (mk-ability "fire wind"           6 6 2 9 fire-wind-proc))
(define turn-invisible      (mk-ability "turn invisible"      7 7 2 0 turn-invisible-proc))
(define cast-lightning-bolt (mk-ability "lightning bolt"      4 4 2 9 lightning-bolt-proc))

;;----------------------------------------------------------------------------
;; Abilities listed by various attributes
;;----------------------------------------------------------------------------

(define melee-spells
  (list cast-fire-field
        cast-sleep-field
        cast-poison-field
        cast-energy-field))

(define all-field-spells
  (list cast-fire-field
        cast-poison-field
        cast-sleep-field
        cast-energy-field
        ))

;; ranged-spells -- damaging spells which take a target kchar as an arg.
(define fireball-spell cast-fireball)
(define poison-missile-spell cast-poison-missile)
(define acid-missile-spell cast-acid-missile)
(define kill-spell cast-kill)
(define all-ranged-spells
  (list 
   cast-magic-missile
   poison-missile-spell
   fireball-spell
   cast-fire-wind
   cast-acid-missile
   cast-lightning-bolt
   ))

