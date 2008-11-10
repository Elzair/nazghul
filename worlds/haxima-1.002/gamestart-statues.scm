(define (gsstatue-unknown knpc kpc)
  (say knpc "[The statue remains silent]"))

(define (gsstatue-hail knpc kpc)
  (say knpc "[A statue speaks to you]")
  (gamestart-statue-clean knpc "statspeak")
  )
    
(define gsstatue-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
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
  (say knpc "Welcome, Seeker. I give wisdom to those who ask for it.")
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
  (say knpc "The wise weave spells of power, and resist malignant magicks, but fools are ensnared and easily deceived. "
       "Beware, Seeker! Evil beings of vast intelligence lie in wait along your path. "
       "You will need wisdom to overcome them. Is this your wish?")
  (if (yes? kpc)
      (say knpc "To grow wise you must give up strength or dexterity.")
      (say knpc "He who turns away from wisdom is the delight of daemons.")
      )
  )
	
(define (gs-int-stre knpc kpc)
  (say knpc "Take heed, Seeker! Physical strength will not avail you against the strongest of the dark powers.")
  (gsstatue-dostat knpc kpc "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence 
                   "strength" kern-char-set-strength kern-char-get-base-strength #t)
  )
      
	
(define (gs-int-dext knpc kpc)
  (say knpc "Consider well, Seeker! The straightest arrow will miss the unclean warlock, "
       "and the doors of the deep are locked with spells that cannot be picked.")
  (gsstatue-dostat knpc kpc "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence 
                   "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity #t)
  )

(define (gs-int-bye knpc kpc)
  (say knpc "Go now, Seeker. May your wards shield you, your summonings aid you, and your wrath rain fire and lightning on the unholy!")
  )

(define gs-int-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gs-int-bye)
       (method 'hail gs-int-hail)
       (method 'job gs-int-inte)
       (method 'inte gs-int-inte)
       (method 'int gs-int-inte)
       (method 'wis gs-int-inte)
       (method 'wisd gs-int-inte)
       (method 'stre gs-int-stre)
       (method 'str gs-int-stre)
       (method 'dext gs-int-dext)
       (method 'dex gs-int-dext)
       )
  )
       
;; Statue of might

(define (gs-str-hail knpc kpc)
  (say knpc "Hail, Seeker. I give strength to those who would be mighty.")
  (gamestart-statue-clean knpc "statspeak")
  )
  
;; expand on this as other abilities become available
(define (gs-str-stre knpc kpc)
	(say knpc "Gain strength, and you will split helms, crush bones, and batter the shields of your foes to splinters. "
             "You can bear the heaviest armour without staggering, and the blows of your enemies will glance off unheeded. "
             "Strength is not the most important thing to a warrior, it is the only thing! "
             "Would you be strong?")
        (if (yes? kpc)
            (say knpc "What will you sacrifice for strength: intelligence or dexterity?")
            (say knpc "Only the strong will survive where you must go.")
            ))
	
(define (gs-str-inte knpc kpc)
  (say knpc "A warrior needs some cunning, but might is paramount!")
  (gsstatue-dostat knpc kpc "strength" kern-char-set-strength kern-char-get-base-strength 
                   "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence #t)
  )
	
(define (gs-str-dext knpc kpc)
  (say knpc "Your friends may be impressed by feats of dexterity, "
       "but your foes will be persuaded by force of arms!")
  (gsstatue-dostat knpc kpc "strength" kern-char-set-strength kern-char-get-base-strength 
                   "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity #t)
  )

(define (gs-str-bye knpc kpc)
  (say knpc "Go now and smite the wicked."))

(define gs-str-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gs-str-bye)
       (method 'hail gs-str-hail)
       (method 'job gs-str-stre)
       (method 'stre gs-str-stre)
       (method 'inte gs-str-inte)
       (method 'int gs-str-inte)
       (method 'dext gs-str-dext)
       (method 'dex gs-str-dext)
       )
  )
       
;; Statue of agility

(define (gs-dex-hail knpc kpc)
  (say knpc "Well met, Seeker. Alas, I cannot make you a powerful brute nor an absent-minded mage like my adjacent cohorts, "
       "but allow me to profer dexterity.")
  (gamestart-statue-clean knpc "statspeak")
  )
  
(define (gs-dex-dext knpc kpc)
  (say knpc "Dexterity is the attribute of the subtle adventurer. Why beat down a door when one can pick the lock?\n\n"
       "Why ruin the eyesite on musty grimoires when treasure is easily found by stealth and craft?\n\n"
       "Why risk mussing one's tastefully chosen accoutrements in a melee when foes can be shot from afar with aplomb?\n\n"
       "And indeed, should a momentary lapse in judgment leave one cornered, speed and quick thinking will save the day. "
       "Do you agree?")
  (if (yes? kpc)
      (say knpc "Indeed. Dexterity is well worth the sacrifice of crass strength or boorish intelligence.")
      (say knpc "Ah. Well. Mind your wallet, friend. Thieves love to prey on the clumsy.")
      )
  )
	
(define (gs-dex-inte knpc kpc)
  (say knpc "If you ask me, intelligence is greatly overrated. "
       "I mean, who cares about raising the dead when one can loot their corpses?")
       (gsstatue-dostat knpc kpc "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity 
                        "intelligence" kern-char-set-intelligence kern-char-get-base-intelligence #t)
       )
	
(define (gs-dex-stre knpc kpc)
  (say knpc "Strength is impressive to the impressionable, "
       "but hauling all that armour and weaponry around looks like pointless hard work.")
  (gsstatue-dostat knpc kpc "dexterity" kern-char-set-dexterity kern-char-get-base-dexterity 
                   "strength" kern-char-set-strength kern-char-get-base-strength #t)
  )

(define (gs-dex-bye knpc kpc)
  (say knpc "Farewell, always keep your wits about you, "
       "never pass up an opportunity to sip from a flagon or kiss a pretty maid, "
       "and at all costs, Seeker, remember to die well!")
  )

(define gs-dex-conv
  (ifc '()
       ;; fundamentals
       (method 'default gsstatue-unknown)
       (method 'bye gs-dex-bye)
       (method 'hail gs-dex-hail)
       (method 'job gs-dex-dext)
       (method 'dex gs-dex-dext)
       (method 'dext gs-dex-dext)
       (method 'stre gs-dex-stre)
       (method 'str gs-dex-stre)
       (method 'inte gs-dex-inte)
       (method 'int gs-dex-inte)
       )
  )
