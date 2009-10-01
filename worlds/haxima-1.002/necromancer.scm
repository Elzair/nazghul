;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define necr-lvl 8)
(define necr-species sp_human)
(define necr-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define necr-bed nl-bed)
(define necr-mealplace nl-tbl)
(define necr-workplace nl-lab)
(define necr-leisureplace nl-lib)
(kern-mk-sched 'sch_necr
               (list 0  0 necr-bed          "sleeping")
               (list 7  0 necr-mealplace    "eating")
               (list 8  0 necr-workplace    "working")
               (list 12 0 necr-mealplace    "eating")
               (list 13 0 necr-workplace    "working")
               (list 18 0 necr-mealplace    "eating")
               (list 19 0 necr-leisureplace "idle")
               (list 22 0 necr-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (necr-mk) 
  (mk-quest))
(define (necr-quest gob) gob)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (necr-hail knpc kpc)
  (let ((quest (necr-quest (kobj-gob-data knpc))))
    (if (and (quest-offered? quest)
             (not (quest-done? quest))
             (in-inventory? kpc t_lich_skull)
             )
        (necr-meet-lich knpc kpc)
        (say knpc "[You meet a thin, pale wizard dressed all in black] "
             "(COUGH) Hello, Wanderer."))))

(define (necr-default knpc kpc)
  (say knpc "[He is seized by a fit of coughing]"))

(define (necr-heal knpc kpc)
  (begin
    (say knpc "\n[He is seized by a fit of coughing]\n"
	 "I'm fine [COUGH] [WHEEZE]")
    (prompt-for-key)

    (say knpc "Really.\n")
    (prompt-for-key)

    (say knpc "\nI am quite wel...[COUGH]")
    (prompt-for-key)

    (say knpc "\n  [WHEEZE]...")
    (prompt-for-key)

    (say knpc "[CHOKE]")
    (kern-sleep 100)
    (say knpc "[the slight figure slumps]")
    (kern-sleep 100)
    (say knpc "[GASP]")
    (kern-sleep 3000)
    (say knpc " ...")
    (prompt-for-key)

    (say knpc "\n[All is silence]")
    (prompt-for-key)

    (if (in-player-party? 'ch_mesmeme)
	(begin	
	  (say knpc "\n[The silence stretches on...]")
	  (kern-sleep 3000)
	  (aside kpc 'ch_mesmeme "[Looks at the slumped figure]\nFood now?")
	  (aside kpc 'ch_amy "EEEWWW!  Bad Gazer!")
	  (prompt-for-key)
	  )
	)

    (if (in-player-party? 'ch_nate)
	(begin
	  (say knpc "\n[The dead sound of the crypt presses on]")
	  (kern-sleep 3000)
	  (aside kpc 'ch_nate "Erm...")
	  (kern-sleep 100)
	  (aside kpc 'ch_nate "  should we maybe...")
	  (kern-sleep 1000)
	  (aside kpc 'ch_nate "  take his stuff?")
	  (kern-sleep 500)
	  (aside kpc 'ch_roland "Nay!  'Twould be dishonour!")
	  (prompt-for-key)
	  )
	)
    
    (if (in-player-party? 'ch_amy)
	(begin
	  (aside kpc 'ch_amy "I could maybe...dig a hole, somewhere?")
	  (prompt-for-key)
	  )
	)

    (kern-sleep 3000)
    (say knpc
	 "\n[His still form twitches]\n"
	 "[His arm gropes for his chest]\n"
	 "  ^c+bIN VAS MANI CORP XEN^c-\n"
	 "")
    ;; (vas-mani knpc)  ;; SAM: Alas, this invoked UI, and emitted extra messages
    (say knpc
	 "\n[He straightens, and breathes deeply]\n"
	 "As I was saying, I am quite well, thank you.")
    ))

(define (necr-name knpc kpc)
  (say knpc "I am the Necromancer."))

(define (necr-join knpc kpc)
  (say knpc "I am not an adventurer."))

(define (necr-job knpc kpc)
  (say knpc "I investigate the secrets of the dead."))

(define (necr-bye knpc kpc)
  (say knpc "[He waves you off in a fit of coughing]"))

;; L2
(define (necr-dead knpc kpc)
  (say knpc "Dead? No, not yet. You?")
  (if (yes? knpc)
      (say knpc "You look a bit firm for a spirit.")
      (say knpc "I didn't think so, but I talk to so many spirits I can't "
           "really tell any more.")))

(define (necr-coug knpc kpc)
  (say knpc "I smoked a pipe for years. Still do, sometimes."))

(define (necr-spir knpc kpc)
  (say knpc "Some spirits are so old they remember things before recorded "
       "history. (COUGH) If you want to know something, there's a spirit "
       "somewhere that knows it."))

;; Quest-related
(define (necr-meet-lich knpc kpc)
  (if (quest-done? (necr-quest (kobj-gob-data knpc)))
      (begin
        (say knpc "Ask the spirit of King Luximene! Can't you see him?")
        (if (no? kpc)
            (begin
              (say knpc "Oh, sorry. Let me fix that:\n"
		   "[He intones words of magic]"
		   "  ^c+bWIS QUAS^c-!")
              (wis-quas knpc))
            (say knpc "Well...")))
      (begin
        (say knpc "Ah! You have the skull of King Luximene! "
             "No doubt there's a good story to go along with this, "
             "but let's hear about that later. For now...")
        (kern-obj-remove-from-inventory kpc t_lich_skull 1)
        (say knpc "\n[He intones words of magic]\n"
	     "  ^c+bKAL AN XEN CORP^c-!\n"
	     "Luximene, come forth!")
        (kern-obj-put-at (mk-luximene)
                         (loc-offset (kern-obj-get-location knpc)
                                     south))
        (quest-done! (necr-quest (kobj-gob-data knpc)) #t)
        (say knpc "There! Do you see him?")
        (if (no? kpc)
            (begin
              (say knpc "Of course, his spirit is invisible to the uninitiated.\n"
                   "Let me fix that:\n"
		   "[He intones words of magic]"
		   "  ^c+bWIS QUAS^c-!")
              (wis-quas knpc))
            (say knpc "Ask him of the rune now.")))))

(define (necr-rune knpc kpc)
  (let ((quest (necr-quest (kobj-gob-data knpc))))
    (if (quest-offered? quest)
        (if (in-inventory? kpc t_lich_skull)
            (necr-meet-lich knpc kpc)
            (if (quest-done? quest)
                (begin
                  (say knpc "Ask the spirit of King Luximene! Can't you see him?")
                  (if (no? kpc)
                      (begin
                        (say knpc "Try a Reveal spell...\n"
			     "[He intones words of magic]\n"
			     "  ^c+bWIS QUAS^c-\n"
			     "Speak to his shade, ask of the RUNE.")
			(wis-quas knpc)
			(kern-conv-end)
			)
                      (begin
			(say knpc "Well then, ask his shade.")
			(kern-conv-end)
		      )
		  ))
                (say knpc "Bring me the skull of King Luximene the lich "
                     "and we can learn more.")))
        (if (not (any-in-inventory? kpc rune-types))
            (say knpc "I've encountered many runes. (COUGH) Bring me an example "
                 "of one and perhaps I can tell you of it.")
            (begin
              (say knpc "Hm. Yes. This rune reminds me of writings I once saw in "
                   "the tomb of King Luximene. (COUGH) If I could speak to his "
                   "spirit perhaps it would tell us more. Are you brave, Wanderer?")
              (if (no? kpc)
                  (say knpc "Me neither. More's the pity, for I am most curious "
                       "now about this rune. (COUGH)")
                  (begin
                    (say knpc "I thought so. King Luximene is a lich nowadays. "
                         "Most unruly! His tomb is in a crypt beneath Green Tower. "
                         "If you bring me his skull I can tame his spirit and speak "
                         "with him. (COUGH) You'll have to defeat him first, "
                         "of course, as well as his undead army. "
                         "Do you know how to repel the undead?")
                    (quest-offered! quest #t)
                    (if (yes? kpc)
                        (say knpc "A most useful spell when dealing with the angry dead.")
                        (say knpc "Learn ye [An Xen Corp].\n"
			     "A most useful spell.\n"
			     "Mix ^c+ggarlic^c- and ^c+gsulphurous ash^c-\n"
			     "to make it.\n"
                             "(COUGH)\n"
			     "I probably have some "
                             "around here.\n"
			     "You may borrow it."))
		    )))))))

(define (necr-absa knpc kpc)
  (say knpc "Ah, Absalot, ancient city of Wisdom, now fallen. "
       "Have you been there?")
  (if (yes? kpc)
      (say knpc "It is only a shameful ruin now.")
      (begin
        (say knpc "Are you an ally of Glasdrin?")
        (if (yes? kpc)
            (say knpc "[He coughs and mutters] Well, "
                 "there is nothing to see there now.")
            (say knpc "There is a secret way in. Ask the Alchemist, he knows.")))))

;; the wise
(define (necr-ench knpc kpc)
  (say knpc "The old fool thinks me one of the Accursed! "
       "He does not understand that his ways are not the only ways.")
       )

(define (necr-man knpc kpc)
  (say knpc "She comes to me sometimes when she needs information... "
       "and in return she sometimes... acquires things for me.")
  (quest-data-update 'questentry-the-man 'common 1)
  )

(define (necr-alch knpc kpc)
  (say knpc "A clever man. A deep man, good in his own way.")
  (quest-data-update 'questentry-alchemist 'common 1)
  )

(define (necr-engi knpc kpc)
  (say knpc "Smart fellow, and very curious, "
       "but always has to be making something. "
       "And as soon as he's done, he starts on another! "
       "(COUGH) Never time to reflect on anything worthy.")
       (quest-data-update 'questentry-engineer 'common 1)
       )

(define (necr-warr knpc kpc)
	(if (quest-data-assigned? 'questentry-wise)
		(begin
		  (say knpc "Alas, she is fallen. I have met her spirit in the void. "
		       "She was betrayed by the leadership of Glasdrin. "
		       "Would that the gods had not abandoned us, "
		       "and Vale, lord of vengeance, still stalked the Shard!")
		       (quest-data-update 'questentry-warritrix 'slain 1)
		 )
		 (say knpc "The hasn't been another knight so noble and so strong in "
		 "a long, long time.")
       ))

(define (necr-vale knpc kpc)
  (say knpc "An ancient god, known only to the dead now."))

(define (necr-wise knpc kpc)
  (say knpc "It is an ancient and worthy tradition that those "
       "who have mastered the ways of Warrior, Wizard, Wright "
       "and Wrogue should influence the affairs of the Shard. "
       "Long is the chain of that tradition, "
       "back to the oldest spirits in the void."))

(define (necr-accu knpc kpc)
  (say knpc "They are a wretched abomination! (COUGH) Their order "
       "began about 500 years ago, or at least those are the oldest "
       "spirits that speak of them. But I do not know their secrets "
       "because their spirits do not return to the void!")
  (prompt-for-key)
  (say knpc "[He looks troubled] "
       "I cannot find any among the dead who were Accursed in life. "
       "It is a most disturbing conundrum."))

(define (necr-gate knpc kpc)
  (say knpc "Ah, the fabled Demon Gate. I thought it was only a legend. "
       "Ask the Enchanter, he surely knows more of it than I. "
       "Meanwhile I will confer with the dead to see what I can discover."))

(define (necr-necr knpc kpc)
  (say knpc "[Cough] I specialize in magic relating to the dead."))

(define necr-conv
  (ifc basic-conv

       ;; basics
       (method 'default necr-default)
       (method 'hail necr-hail)
       (method 'bye necr-bye)
       (method 'job necr-job)
       (method 'name necr-name)
       (method 'join necr-join)
       (method 'heal necr-heal)
       
       (method 'dead necr-dead)
       (method 'coug necr-coug)
       (method 'spir necr-spir)
       (method 'rune necr-rune)
       (method 'absa necr-absa)
       (method 'ench necr-ench)
       (method 'man necr-man)
       (method 'alch necr-alch)
       (method 'engi necr-engi)
       (method 'warr necr-warr)
       (method 'vale necr-vale)
       (method 'wise necr-wise)
       (method 'accu necr-accu)
       (method 'gate necr-gate)
       (method 'demo necr-gate)
       (method 'necr necr-necr)
       ))

(define (mk-necromancer)
  (bind 
   (kern-mk-char 
    'ch_necr           ; tag
    "Necromancer"    ; name
    necr-species         ; species
    necr-occ              ; occ
    s_necromancer     ; sprite
    faction-men      ; starting alignment
    1 6 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    necr-lvl
    #f               ; dead
    'necr-conv         ; conv
    sch_necr         ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_staff)              ; readied
    )
   (necr-mk)))
