;;----------------------------------------------------------------------------
;; occupations

;; Occupation gob fields (these are all procedures for evaluating a kchar's
;; ability at something):
;;
;; 0 thief
;; 1 white magic
;; 2 black magic
;; 3 magic defense
;; 4 strength-based attack
;; 5 dexterity-based attack
;; 6 dexterity-based defense
;; 7 crafting

(define (mk-occ tag name hit def dam arm xp skset)
  (kern-mk-occ tag name 1.0 0 0 0 0 hit def dam arm xp skset)
  (kern-occ-set-gob (eval tag) (list nil nil nil nil nil nil nil nil)))

;;            /           /         / t  / f / e /   /
;;           /           /         / i  / e / g / r /
;;          /           / e       / h  / d / a / o /
;;         / g         / m       / -  / - / m / m /
;;        / a         / a       / o  / o / a / r / p
;;       / t         / n       / t  / t / d / a / x
(mk-occ 'oc_wizard   "wizard"   -1  -1  -1  -1   4 nil)
(mk-occ 'oc_warrior  "warrior"   1   0   1   0   4 sks_warrior)
(mk-occ 'oc_wright   "wright"    0   1   0   1   4 sks_wright)
(mk-occ 'oc_wrogue   "wrogue"    1   1   0   0   4 sks_wrogue)
(mk-occ 'oc_wanderer "wanderer"  5   5   5   5   8 sks_wanderer)
(mk-occ 'oc_ranger   "ranger"    1   1   0   0   4 sks_ranger)

(define (occ-get-abil kocc ability)
	(if (null? kocc)
		nil
		(list-ref (kern-occ-get-gob kocc) ability)))
		
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

(define (occ-thief-dice-roll kchar)
  (kern-dice-roll (string-append "1d" 
                                 (number->string (occ-ability-thief kchar)))))

;-----------------------------
;Magic output

(define (occ-ability-whitemagic kchar)
	(let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 1)))
		(if (null? occ-abil)
			(floor 
				(+ (/ (kern-char-get-level kchar) 3)
					(/ (kern-char-get-intelligence kchar) 3)))
			(occ-abil kchar)
			)))
			
(define (occ-ability-blackmagic kchar)
	(let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 2)))
		(if (null? occ-abil)
			(floor 
				(/ (+ (kern-char-get-level kchar) 
					(kern-char-get-intelligence kchar)) 4))
			(occ-abil kchar)
			)))
			
(let (
	(highskill
		(lambda (kchar)
			(floor 
				(+ (kern-char-get-level kchar)
					(/ (kern-char-get-intelligence kchar) 2))
			)))
	(modskill
		(lambda (kchar)
			(floor 
				(+ (* (kern-char-get-level kchar) 0.75)
					(/ (kern-char-get-intelligence kchar) 3))
			)))
	(someskill
		(lambda (kchar)
			(floor 
				(+ (/ (kern-char-get-level kchar) 3)
					(/ (kern-char-get-intelligence kchar) 3))
			)))	
	(poorskill
		(lambda (kchar)
			(floor 
				(+ (/ (kern-char-get-level kchar) 4)
					(/ (kern-char-get-intelligence kchar) 4))
			)))
	(noskill
		(lambda (kchar)
			(floor 
				(+ (/ (kern-char-get-level kchar) 6)
					(/ (kern-char-get-intelligence kchar) 4))
			)))
	)
	(occ-set-abil oc_wizard 1 highskill)
	(occ-set-abil oc_wizard 2 highskill)
	(occ-set-abil oc_wanderer 1 modskill)
	(occ-set-abil oc_wanderer 2 modskill)
	(occ-set-abil oc_warrior 1 poorskill)
	(occ-set-abil oc_warrior 2 noskill)
	(occ-set-abil oc_ranger 2 someskill)
)

;-----------------------------
; Magic defense

(define (occ-ability-magicdef kchar)
	(let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 3)))
		(if (null? occ-abil)
			(floor 
				(+ (/ (kern-char-get-level kchar) 3)
					(/ (kern-char-get-intelligence kchar) 2)))
			(occ-abil kchar)
			)))
			
(let (
	(highskill
		(lambda (kchar)
			(floor 
				(+ (/ (kern-char-get-level kchar) 2)
					(/ (kern-char-get-intelligence kchar) 2))
			)))
	(modskill
		(lambda (kchar)
			(floor 
				(+ (* (kern-char-get-level kchar) 0.4)
					(/ (kern-char-get-intelligence kchar) 2))
			)))
	)
	(occ-set-abil oc_wizard 3 highskill)
	(occ-set-abil oc_wanderer 3 modskill)
)

;----------------------------
; Combat

(define (occ-ability-strattack kchar)
	(let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 4)))
		 (if (null? occ-abil)
                     (floor (/ (* (kern-char-get-level kchar)
                                  (+ (kern-char-get-strength kchar) 15))
                               30))
                     (occ-abil kchar)
                     )))

(let (
	(highskill
		(lambda (kchar)
                  (floor (/ (* (kern-char-get-level kchar)
                               (+ (kern-char-get-strength kchar) 10))
                            20))
                         ))
	(modskill
		(lambda (kchar)
                  (floor (/ (* (kern-char-get-level kchar)
                               (+ (kern-char-get-strength kchar) 12))
                            24))
			))
	(lowskill
		(lambda (kchar) 
                  (floor (/ (* (kern-char-get-level kchar)
				(+ (kern-char-get-strength kchar) 10))
                             30))
                         ))
	)
	(occ-set-abil oc_wizard 4 lowskill)
	(occ-set-abil oc_wright 4 modskill)
	(occ-set-abil oc_wanderer 4 modskill)
	(occ-set-abil oc_warrior 4 highskill)
	(occ-set-abil oc_ranger 4 modskill)
)


(define (occ-ability-dexattack kchar)
  (let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 5)))
    (if (null? occ-abil)
        (floor (/ (* (kern-char-get-level kchar)
                     (+ (kern-char-get-dexterity kchar) 15))
                  30))
        (occ-abil kchar)
        )))

(let (
      (highskill
       (lambda (kchar)
         (floor (/ (* (kern-char-get-level kchar)
                      (+ (kern-char-get-dexterity kchar) 10))
                   20))
         ))
      (modskill
       (lambda (kchar)
         (floor (/ (* (kern-char-get-level kchar)
                      (+ (kern-char-get-dexterity kchar) 12))
                   24))
         ))
      (lowskill
       (lambda (kchar) 
           (floor (/ (* (kern-char-get-level kchar)
                        (+ (kern-char-get-dexterity kchar) 10))
                     30))
           ))
      )
  (occ-set-abil oc_wizard 5 lowskill)
  (occ-set-abil oc_wrogue 5 modskill)
  (occ-set-abil oc_wanderer 5 modskill)
  (occ-set-abil oc_warrior 5 highskill)
  (occ-set-abil oc_ranger 5 modskill)
)

(define (occ-ability-strdamage kchar)
  (floor (* 2 (-  (log (+ 2 (occ-ability-strattack kchar)))))))

(define (occ-ability-dexdefend kchar)
  (let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 6)))
    (if (null? occ-abil)
        (floor (+ (/ (kern-char-get-level kchar) 2)
                  (/ (kern-char-get-dexterity kchar) 3)))
               (occ-abil kchar)
               )))

(let (
	(highskill
         (lambda (kchar)
           (floor (+ (* (kern-char-get-level kchar) 0.8)
                     (/ (kern-char-get-dexterity kchar) 2)))
           ))
	(modskill
         (lambda (kchar)
           (floor (+ (* (kern-char-get-level kchar) 0.6)
                     (* (kern-char-get-dexterity kchar) 0.4)))
           ))
	(lowskill
         (lambda (kchar) 
           (floor (+ (/ (kern-char-get-level kchar) 3)
                     (/ (kern-char-get-dexterity kchar) 4)))
                  ))
	)
	(occ-set-abil oc_wizard 6 lowskill)
	(occ-set-abil oc_wrogue 6 highskill)
	(occ-set-abil oc_wanderer 6 modskill)
	(occ-set-abil oc_warrior 6 modskill)
	(occ-set-abil oc_ranger 6 highskill)
)

(define (proc-stratt kchar)
	(* 1000 (occ-ability-strattack kchar)))

(define (proc-dexatt kchar)
	(* 1000 (occ-ability-dexattack kchar)))

(define (proc-strdam kchar)
	(* 1000 (occ-ability-strdamage kchar)))

(define (proc-dexdef kchar)
	(* 1000 (occ-ability-dexdefend kchar)))

;----------------------------
; Crafting

;; Note: I'm not sure this is really how we want to handle wright skills, but
;; it serves as a starting point. For one thing, we probably need to break the
;; skills up into different abilities. Eg, a wizard might not be able to skin a
;; deer as well as a ranger, but he can do a better job of whittling a magic
;; wand. Also, I just used all the core attributes and divided them
;; evenly. Different types of wright skills might emphasize dexterity over
;; intelligence, or vice versa, and etc.

(define (occ-ability-crafting kchar)
  (let ((occ-abil (occ-get-abil (kern-char-get-occ kchar) 7)))
    (if (null? occ-abil)
        (floor (/ (* (kern-char-get-level kchar)
                     (+ (/ (kern-char-get-dexterity kchar) 3)
                        (/ (kern-char-get-strength kchar) 3)
                        (/ (kern-char-get-intelligence kchar) 3)
                        15))
                  30))
        (occ-abil kchar)
        )))

(let (
      (highskill
       (lambda (kchar)
         (floor (/ (* (kern-char-get-level kchar)
                      (+ (/ (kern-char-get-dexterity kchar) 3)
                         (/ (kern-char-get-strength kchar) 3)
                         (/ (kern-char-get-intelligence kchar) 3)
                         10))
                   20))
         ))
      (modskill
       (lambda (kchar)
         (floor (/ (* (kern-char-get-level kchar)
                      (+ (/ (kern-char-get-dexterity kchar) 3)
                         (/ (kern-char-get-strength kchar) 3)
                         (/ (kern-char-get-intelligence kchar) 3)
                         12))
                   24))
         ))
      (lowskill
       (lambda (kchar) 
           (floor (/ (* (kern-char-get-level kchar)
                        (+ (/ (kern-char-get-dexterity kchar) 3)
                           (/ (kern-char-get-strength kchar) 3)
                           (/ (kern-char-get-intelligence kchar) 3)
                           10))
                     30))
           ))
      )
  (occ-set-abil oc_wizard 7 lowskill)
  (occ-set-abil oc_wrogue 7 highskill)
  (occ-set-abil oc_wanderer 7 modskill)
  (occ-set-abil oc_warrior 7 lowskill)
  (occ-set-abil oc_ranger 7 modskill)
)

;----------------------------
; Acrobatics

;; These are for physical feats like sprinting and wriggling through
;; bars. Piggyback on some of the other abilities for now.

(define (occ-ability-stracro kchar) (occ-ability-strattack kchar))
(define (occ-ability-dexacro kchar) (occ-ability-dexdef kchar))
