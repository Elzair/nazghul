;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define engineer-start-lvl 8)

(define voidship-parts
  (list 
  	(list t_power_core 1)
	(list sulphorous_ash 20)
	(list t_gem 10)
  ))

(define voidship-loc (mk-loc 'p_shard 50 3))

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Engineer's Tower Ground Floor"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_engineer
               (list 0  0  eng-workshop   "working")
               (list 1  0  eng-bed        "sleeping")
               (list 10 0  eng-workshop   "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (engineer-mk)
  (list #f 
        (mk-quest)))
(define (eng-met? gob) (car gob))
(define (eng-quest gob) (cadr gob))
(define (eng-met! gob val) (set-car! gob val))

;; ----------------------------------------------------------------------------
;; Voidship plans
;; ----------------------------------------------------------------------------
(mk-reusable-item 
 't_voidship_plans "Voidship Plans" s_lexicon norm
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "Voidship Plans"
   "Parts List:"
   " Sulphurous Ash (20)"
   " Gems (10)"
   " Power Core (1)"
   )))

;;----------------------------------------------------------------------------
;; Conv
;;
;; The Engineer is a Wright of great knowledge, and one of the Wise.
;; He dwells in an isolated workshop on a nearly inaccessible
;; void-mesa (or void-island) off the edge of the Shard.
;;----------------------------------------------------------------------------
(define (eng-hail knpc kpc)
  (say knpc "[You meet a thin man with wild, white hair. "
       "He doesn't seem to notice you at first] Oh, hello."))

(define (eng-name knpc kpc)
  (say knpc "Rudolph. I'm better known as the Engineer."))

(define (eng-job knpc kpc)
  (say knpc "Oh, this and that. I like to make things."))

(define (eng-default knpc kpc)
  (say knpc "I don't know. Ask the Enchanter."))

(define (eng-bye knpc kpc)
  (say knpc "[He seems to have forgotten about you already]"))

(define (eng-join knpc kpc)
  (say knpc "Too busy. Try the Warritrix, she likes adventures."))

(define (eng-warr knpc kpc)
  (say knpc "I understand she's one of the finest warriors to ever live, "
       "but I know for a fact she's one of the most noble. When she isn't "
       "off doing something incredibly brave and stupid you can find her "
       "at Glasdrin.")
    (quest-wise-subinit 'questentry-warritrix)
  	(quest-data-update 'questentry-warritrix 'general-loc 1)
       )

(define (eng-make knpc kpc)
  (say knpc "I work on all kinds of different things. Lately I've been "
       "interested in devices for traveling: gates, voidships, etc. "
       "Inventing is an obsession of mine."))

(define (eng-wand knpc kpc)
  (say knpc "You're a Wanderer? I've always wanted to meet one. "
       "Did you build your own gate?")
  (kern-conv-get-reply kpc)
  (say knpc "I've always wondered how it could be done. "
       "I have some theories, and I've designed a voidship "
       "to test some of them, but it isn't finished yet."))

(define (eng-void knpc kpc)
  (let* ((eng (kobj-gob-data knpc))
         (quest (eng-quest eng)))

    (define (remove-stuff)
      (map (lambda (ktype) 
             (kern-obj-remove-from-inventory kpc (car ktype) (cadr ktype)))
           voidship-parts))

	;;FIXME: the grammer here needs work
	
    (define (really-has-parts?)
      (display "really-has-parts?")(newline)
      (let ((missing (filter 
				(lambda (ktype)
					(let ((nrem (- (cadr ktype) (num-in-inventory kpc (car ktype)))))
						(cond 
							((> nrem 1)
								(begin
									(say knpc "We still need " nrem " " (kern-type-get-name (car ktype)) "s")
									#t))
							((> nrem 0)
								(begin
									(say knpc "We still need the " (kern-type-get-name (car ktype)))
									#t))
							(else #f))))
					voidship-parts)))
		
        (if (null? missing)
			#t
            #f)))

    (define (build-ship)
      (say knpc "Yes, it looks like you have everything. "
           "Well, let's get to work...")
      (remove-stuff)
      (kern-log-msg "[After a good deal of effort, cursing, trying, failing...]")
      (prompt-for-key)
      (kern-log-msg "[retrying, refailing, pacing up and down, tearing out "
           "fistfuls of hair...]")
      (prompt-for-key)
      (kern-log-msg "[more cursing, arguing, starting over, failing again...]")
      (prompt-for-key)
      (kern-log-msg "[retrying, refailing, weeping, gnashing of teeth...]")
      (prompt-for-key)
      (kern-log-msg "[...and so on and so forth...]")
      (prompt-for-key)
      (kern-log-msg "[...finally...]")
      (prompt-for-key)
      (kern-log-msg "[...oh, hell, now what?...]")
      (prompt-for-key)
      (kern-log-msg "[...but then...]")
      (kern-obj-relocate (mk-voidship) (eval-loc voidship-loc) nil)
      (kern-log-msg "[You both collapse with exhaustion] ")
      (say knpc
           "Well. That wasn't so bad. She's all yours now, waiting "
           "for you outside at the end of the dock. Good luck! ")
      (kern-log-msg "[He starts to snore]")
      (kern-obj-add-effect knpc ef_sleep nil)
      (quest-done! quest #t)
      (kern-conv-end))

    (define (missing-power-core?)
      (not (in-inventory? kpc t_power_core)))

    (define (has-plans)
      (say knpc "Ah, you've found the plans for my voidship! "
           "Do you have all the parts we need?")
      (if (kern-conv-get-yes-no? kpc)
          (if (really-has-parts?)
              (build-ship))
          (if (missing-power-core?)
              (say knpc "Somewhere across the chasm to the south lies a wrecked voidship. "
                   "If you can find a way to cross the chasm it might have a power core "
                   "you can salvage.")
              (say knpc "Well, what are you waiting for? Go get them.")
              )))
      
    (define (no-plans)
      (say knpc "A great void surrounds the Shard. I've designed a ship "
           "which should be able to cross the void, but it isn't "
           "finished yet. I've got the plans around here someplace. "
           "If you find them let me know.")
           (quest-data-update 'questentry-whereami 'shard 2)
           )

    (if (quest-done? quest)
        (say knpc "It's all finished.")
        (if (in-inventory? kpc t_voidship_plans)
            (has-plans)
            (no-plans)))))

(define (eng-gate knpc kpc)
  (say knpc "The moongates and Shrine Gate are a mystery. I'd love to figure "
       "them out. You know about the Demon Gate, right?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "I wish I knew if it really existed or not.")
      (say knpc "It's a legend. The Demon Gate opened from this world to "
           "others. Then somebody got paranoid, locked it shut, and "
           "threw away the key. Pity if it's true.")))
  
(define (eng-key knpc kpc)
  (say knpc "Supposedly the key is a set of Runes that were subsequently "
       "lost or scattered. Typical fairy-tale nonsense. But there may be "
       "a kernel of truth to it."))

(define (eng-wise knpc kpc)
  (say knpc "Anyone with the arrogance to call themselves Wise is probably "
       "an ass. Take the Stewardess of Glasdrin, for example."))

(define (eng-stew knpc kpc)
  (say knpc "She longs to be counted among the Wise, and doesn't care how "
       "much blood she has to spill to do it. You've heard of Absalot, I "
       "assume?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "A perfect example of abusive power. Sad.")
      (say knpc "The Stewardess conned the other cities and the Enchanter "
           "into massacring the citizens of Absalot. "
           "Supposedly Absalot was a hotbed of Accursed insurgency. If there "
           "was any evidence of the Accursed being there, they burned it to "
           "the ground with the rest of the city.")))

(define (eng-accu knpc kpc)
  (say knpc "Supposedly they're a cult of some sort engaging in evil "
       "practices. People who make it their business to meddle are very "
       "upset about them. I don't really give a fig."))

(define (eng-shri knpc kpc)
  (say knpc "I've studied the records about the Shrine Gate and investigated "
       "the site, of course. But there's nothing to indicate how it works or "
       "how to control it. I think the last time it opened was over one "
       "hundred years ago."))

(define (eng-rune knpc kpc)
  (say knpc "The legend of the Demon Gate insists that it was locked by a set "
       "of Runes. Some versions say the Runes were scattered so they could "
       "not be reassambled to open the Gate, others say they were lost "
       "through simple incompetence and bad luck. Of course, there's no "
       "telling if the Runes or even the Gate ever existed!"))

(define (eng-wiza knpc kpc)
  (say knpc "Wizards as a rule tend to be obsessed with power."))

(define (eng-wrog knpc kpc)
  (say knpc "Mosts Wrogues are simply pests. They're into everything."))

(define (eng-wrig knpc kpc)
  (say knpc "Wrights are my kind of people. They like to make things, to "
       "figure out how things work, and just want the freedom to follow "
       "their own interests."))

(define (eng-necr knpc kpc)
  (say knpc "Not a bad sort. I've conferred with him a time or two.")
  (quest-data-update 'questentry-necromancer 'nonevil 1)
  )

(define (eng-alch knpc kpc)
  (say knpc "A tricky bastard but I have to respect him."))

(define (eng-man knpc kpc)
  (say knpc "A Wrogue, but a helpful one. We get along."))

(define (eng-ench knpc kpc)
  (say knpc "Bit of a fanatic if you ask me. But it takes all kinds."))

(define eng-merch-msgs
  (list "Not now."
        "Let me show you some of my inventions..."
        nil
        nil
        "Always glad to help out a fellow tinkerer."
        "No problem."
   ))

(define eng-catalog
  (list
   ;; Various tools and devices
   (list t_picklock       10 "I happen to have some spare picklocks. They're handy for all kinds of things.")  ;; high prices, not his specialty
   (list t_shovel        100 "I hate to part with my shovel, you never know when it might come in handy.")  ;; high prices, not his specialty
   (list t_pick          100 "I've actually used this pick. Once. I suppose I can part with it.")  ;; high prices, not his specialty
   (list t_sextant       200 "This is one of my most successful inventions. With this sextant you don't neet the In Wis spell to find your location on the surface.")
   (list t_chrono        200 "It's a little PORTABLE CLOCK! Isn't it amazing? [He giggles with glee]")
   
   ;; A bit of oil and grease, for a grease-monkey:
   (list t_grease         20 "I've got plenty of grease. I use it for everything.")
   (list t_oil            10 "If I could figure out how to harness the explosive power of this oil, I'm sure I could make a useful engine.")  ;; high prices, not his specialty

   ;; Crossbows and bolts, as he likes intricate devices
   (list t_lt_crossbow    50 "Isn't this little crossbow cute? The little levers and actions are quite clever.")
   (list t_crossbow      100 "I'm a terrible shot, but I wanted to study crossbows to see if I could figure out how to shoot myself safely across the void.")
   (list t_hvy_crossbow  300 "I couldn't help making some improvements on the standard crossbow. This one works best if you mount it on something first.")
   (list t_trpl_crossbow 500 "What if a crossbow could fire more than one bolt at a time? I had to try it out, so I made this.") ;; a mechanism of his devising
   (list t_bolt            2 "I went through a lot of bolts while testing crossbows modifications, and I have a few crates left over.")
   ))

(define (eng-trade knpc kpc) (conv-trade knpc kpc "buy" eng-merch-msgs eng-catalog))

(define engineer-conv
  (ifc nil
       (method 'default eng-default)
       (method 'hail eng-hail)
       (method 'name eng-name)
       (method 'bye eng-bye)
       (method 'job eng-job)
       (method 'join eng-join)

       (method 'trad eng-trade)
       (method 'buy  eng-trade)
       (method 'inve eng-trade)

       (method 'make eng-make)
       (method 'thin eng-make)
       (method 'wand eng-wand)
       (method 'void eng-void)
       (method 'gate eng-gate)
       (method 'key eng-key)
       (method 'wise eng-wise)
       (method 'stew eng-stew)
       (method 'accu eng-accu)
       (method 'shri eng-shri)
       (method 'rune eng-rune)
       (method 'wiza eng-wiza)
       (method 'wrog eng-wrog)
       (method 'wrig eng-wrig)
       (method 'necr eng-necr)
       (method 'alch eng-alch)
       (method 'man eng-man)
       (method 'ench eng-ench)
       ))

(define (mk-engineer)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_engineer ;;..........tag
     "Engineer" ;;.......name
     sp_human ;;.....species
     oc_wright ;;.. .occupation
     s_companion_tinker ;;..sprite
     faction-men ;;..faction
     2 ;;...........custom strength modifier
     10 ;;...........custom intelligence modifier
     2 ;;...........custom dexterity modifier
     10 ;;............custom base hp modifier
     2 ;;............custom hp multiplier (per-level)
     20 ;;............custom base mp modifier
     5 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     0
     engineer-start-lvl  ;;..current level
     #f ;;...........dead?
     'engineer-conv ;;...conversation (optional)
     sch_engineer ;;.....schedule (optional)
     'townsman-ai ;;..........custom ai (optional)
     ;;..............container (and contents)
     (mk-inventory
      (list
       (list 1   t_dagger)
       (list 1   t_doom_staff)
       (list 1   t_trpl_crossbow)
       (list 100 t_bolt)
       (list 5   t_cure_potion)
       (list 5   t_heal_potion)
       ))
     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (engineer-mk)))
