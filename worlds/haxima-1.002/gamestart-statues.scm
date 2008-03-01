(define (gsstatue-unknown knpc kpc)
  (say knpc "[The statue remains silent]"))

(define (gsstatue-bye knpc kpc)
  (say knpc "Be sure to see my brothers- you may have need of their abilities as well.")
  (kern-conv-end))

(define (gsstatue-hail knpc kpc)
  (say knpc "[A statue speaks to you]")
  (gamestart-statue-clean knpc "statspeak")
  )
    
(define gsstatue-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gsstatue-bye)
    	 (method 'hail gsstatue-hail)
       )
       )

(define (gsstatue-dostat knpc kpc iname iset iget dname dset dget initial)
	(define (gs-check-upper value)
		(if (> value 11)
			(begin
				(say knpc "I cannot raise your " iname " further")
				#t
				)
			#f))
	(define (gs-check-lower value)
		(if (< value 1)
			(begin
				(say knpc "You must not abuse your " dname " further")
				#t
				)
			#f))
	(define (gs-initialcheck)
		(say knpc "Do you wish to convert " dname " into " iname "?")
		(if (kern-conv-get-yes-no? kpc)
			#f #t)
		)
	(define (gs-repeatcheck)
		(say knpc "Continue?")
		(if (kern-conv-get-yes-no? kpc)
			#f #t)
		)
	(let ((ival (iget kpc))
			(dval (dget kpc))
			)
		(cond ((gs-check-upper ival))
			((gs-check-lower dval))
			((and initial (gs-initialcheck)) (say knpc "As you wish"))
			((and (not initial) (gs-repeatcheck)) (say knpc "As you wish"))
			(#t
				(iset kpc (+ ival 1))
				(dset kpc (- dval 1))
				(say knpc "[Your " iname " increases]")
				(say knpc "[Your " dname
					(cond ((< dval 3) " withers!]")
						((< dval 9) " diminishes]")
						(#t " wanes]")
					)
				)
				(gamestart-reset-lamps kpc)
				(gsstatue-dostat knpc kpc iname iset iget dname dset dget #f)
			)
		)	
	))
		       
;; Statue of intelligence

(define (gs-int-hail knpc kpc)
  (say knpc "Hail Seeker")
  (say knpc "I represent the force of reason, and can assist you in endeavors of magic or wit.")
  (gamestart-statue-clean knpc "statspeak")
  )
  
(define (gs-int-job knpc kpc)
	(say knpc "I represent the force of reason, and can assist you in endeavors of magic or wit.")
	)
  
(define (gs-int-assi knpc kpc)
	(say knpc "I can raise your intellect, but it will cost you some of your strength or dexterity")
	)

(define (gs-int-rais knpc kpc)
	(say knpc "You will need to say which attribute you want to suffer the penalty")
	)
		
;; expand on this as other abilities become available
(define (gs-int-inte knpc kpc)
	(say knpc "Intelligence contributes to your magic abilities, both casting spells, and resisting hostile magics cast upon you.")
	)
	
(define (gs-int-stre knpc kpc)
	(gsstatue-dostat knpc kpc "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence 
		"strength" kern-char-set-strength kern-char-get-base-strength #t)
	)
	
(define (gs-int-dext knpc kpc)
	(gsstatue-dostat knpc kpc "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence 
		"dexterity" kern-char-set-dexterity kern-char-get-base-dexterity #t)
	)

(define gs-int-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gsstatue-bye)
    	 (method 'hail gs-int-hail)
    	 (method 'job gs-int-job)
    	 
    	 (method 'assi gs-int-assi)
    	 (method 'pena gs-int-assi)
    	 (method 'attr gs-int-assi)
    	 
    	 (method 'magi gs-int-inte)
    	 (method 'reas gs-int-inte)
    	 (method 'inte gs-int-inte)
    	 (method 'int gs-int-inte)
    	 (method 'wit gs-int-inte)
    	 (method 'ende gs-int-inte)
    	 
    	 (method 'rais gs-int-rais)
    	 (method 'cost gs-int-rais)
    	 
    	 (method 'stre gs-int-stre)
    	 (method 'str gs-int-stre)
    	 (method 'dext gs-int-dext)
    	 (method 'dex gs-int-dext)
       )
       )
       
;; Statue of might

(define (gs-str-hail knpc kpc)
  (say knpc "Hail Seeker")
  (say knpc "I represent the power of physical strength. With my help, your foes will tremble before you!")
  (gamestart-statue-clean knpc "statspeak")
  )
  
(define (gs-str-job knpc kpc)
	(say knpc "I represent the power of physical strength. With my help, your foes will tremble before you!")
	)
  
(define (gs-str-help knpc kpc)
	(say knpc "I can help you raise your strength. Some intelligence or dexterity will be lost though.")
	)

(define (gs-str-rais knpc kpc)
	(say knpc "You will need to say which attribute you want to suffer the penalty")
	)
		
;; expand on this as other abilities become available
(define (gs-str-stre knpc kpc)
	(say knpc "Strength is of great use on the battlefield. The damage you inflict with most weapons will increase with your strength, and you will be better at battering through your opponents guard.")
	(say knpc "Without enough strength, you might not even be able to wield your equipment, or wear strong armour!")
	)
	
(define (gs-str-inte knpc kpc)
	(gsstatue-dostat knpc kpc "strength" kern-char-set-strength kern-char-get-base-strength 
		"intelligence" kern-char-set-intelligence kern-char-get-base-intelligence #t)
	)
	
(define (gs-str-dext knpc kpc)
	(gsstatue-dostat knpc kpc "strength" kern-char-set-strength kern-char-get-base-strength 
		"dexterity" kern-char-set-dexterity kern-char-get-base-dexterity #t)
	)

(define gs-str-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gsstatue-bye)
    	 (method 'hail gs-str-hail)
    	 (method 'job gs-str-job)
    	 
    	 (method 'help gs-str-help)
    	 (method 'pena gs-str-help)
    	 (method 'attr gs-str-help)
    	 
    	 (method 'phys gs-str-stre)
    	 (method 'stre gs-str-stre)
   	 (method 'str gs-str-stre)
    	 (method 'trem gs-str-stre)
    	 (method 'foes gs-str-stre)
    	 
    	 (method 'rais gs-str-rais)
    	 (method 'lost gs-str-rais)
    	 
    	 (method 'inte gs-str-inte)
    	 (method 'int gs-str-inte)
    	 (method 'dext gs-str-dext)
    	 (method 'dex gs-str-dext)
       )
       )
       
;; Statue of agility

(define (gs-dex-hail knpc kpc)
  (say knpc "Hail Seeker")
  (say knpc "I represent swiftness and accuracy in all things. With my help, you can gain many skills.")
  (gamestart-statue-clean knpc "statspeak")
  )
  
(define (gs-dex-job knpc kpc)
	(say knpc "I represent swiftness and accuracy in all things. With my help, you can gain many skills.")
	)
  
(define (gs-dex-help knpc kpc)
	(say knpc "I can help you raise your dexterity. It will, however, require you to sacrifice some intelligence or strength.")
	)

(define (gs-dex-rais knpc kpc)
	(say knpc "You will need to say which attribute you want to suffer the penalty")
	)
		
;; expand on this as other abilities become available
(define (gs-dex-dext knpc kpc)
	(say knpc "Dexterity is of great use to those whose livelihoods depend on subtlety. Picking locks, getting past traps, that sort of thing.")
	(say knpc "And brute force is not the only way to fight a battle- a quick hand allows you to more accurately strike. You will hit more often, and can often find a deadlier place to land a blow.")
	(say knpc "The swift are also better at getting out of the way of their opponents blows.")
	)
	
(define (gs-dex-inte knpc kpc)
	(gsstatue-dostat knpc kpc "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity 
		"intelligence" kern-char-set-intelligence kern-char-get-base-intelligence #t)
	)
	
(define (gs-dex-stre knpc kpc)
	(gsstatue-dostat knpc kpc "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity 
		"strength" kern-char-set-strength kern-char-get-base-strength #t)
	)

(define gs-dex-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gsstatue-bye)
    	 (method 'hail gs-dex-hail)
    	 (method 'job gs-dex-job)
    	 
    	 (method 'help gs-dex-help)
    	 (method 'gain gs-dex-help)
    	 (method 'pena gs-dex-help)
    	 (method 'attr gs-dex-help)
    	 
    	 (method 'dext gs-dex-dext)
    	 (method 'dex gs-dex-dext)
    	 (method 'skil gs-dex-dext)
    	 (method 'swif gs-dex-dext)
    	 (method 'accu gs-dex-dext)
    	 
    	 (method 'rais gs-dex-rais)
    	 (method 'sacr gs-dex-rais)
    	 
    	 (method 'stre gs-dex-stre)
    	 (method 'str gs-dex-stre)
    	 (method 'inte gs-dex-inte)
    	 (method 'int gs-dex-inte)
       )
       )
