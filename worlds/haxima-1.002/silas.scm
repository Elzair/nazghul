;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define silas-lvl 8)
(define silas-species sp_human)
(define silas-occ oc_wizard)
(define shrine-path-x 97)
(define shrine-path-y 5)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Old Absalot
;;----------------------------------------------------------------------------
(define silas-bed oa-bed1)
(define silas-mealplace oa-tbl3)
(define silas-workplace oa-temple)
(define silas-leisureplace oa-baths)
(kern-mk-sched 'sch_silas
               (list 0  0 silas-bed          "sleeping")
               (list 7  0 silas-mealplace    "eating")
               (list 8  0 silas-workplace    "working")
               (list 12 0 silas-mealplace    "eating")
               (list 13 0 silas-workplace    "working")
               (list 18 0 silas-mealplace    "eating")
               (list 19 0 silas-leisureplace "idle")
               (list 22 0 silas-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (silas-mk) 
  (list #f (mk-quest) #f))
(define (silas-set-will-help! gob) (set-car! gob #t))
(define (silas-will-help? gob) (car gob))
(define (silas-quest gob) (cadr gob))
(define (silas-met? gob) (caddr gob))
(define (silas-set-met! gob) (set-car! (cddr gob) #t))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Silas is the leader of the evil cult of the Accursed.
;; He is a wizard of considerable power.
;;----------------------------------------------------------------------------

;; Basics...
(define (silas-hail knpc kpc)
  (if (silas-met? (kobj-gob-data knpc))
      (say knpc "Hello again, Wanderer.")
      (begin
        (silas-set-met! (kobj-gob-data knpc))
        (say knpc "[You meet a charming older man] Hail, traveler. "
             "If I am not mistaken, you are a Wanderer. Am I right?")
        (if (yes? kpc)
            (say knpc "I knew it! I've actually been expecting you.")
            (say knpc "[He clearly does not believe you] "
                 "Really, you are too modest. "
                 "I've been expecting you for some time.")))))

(define (silas-default knpc kpc)
  (say knpc "I'm afraid I cannot help you with that."))

(define (silas-name knpc kpc)
  (say knpc "Forgive me, I am Silas. And you, my friend, "
       "need no introduction."))

(define (silas-ask-help knpc kpc)
  (cond ((yes? kpc)
         (say knpc "I can't tell you how gratified I am to hear that. "
              "With you on my side we will bring a new age of hope and "
              "peace to the Shard. When you are ready to begin, "
              "I have a quest for you.")
         (silas-set-will-help! (kobj-gob-data knpc))
         (make-allies knpc kpc)
         )
        (else
         (say knpc "Although success is most doubtful now, "
              "I shall have to do my best without you. "
              "It is a pity. You know, in the past, the appearance of a "
              "Wanderer always occurred at a pivotal moment in history. "
              "The actions -- or inactions -- of the Wanderer sometimes "
              "decided the fate of the world for centuries thereafter.")
         )))

(define (silas-join knpc kpc)
  (say knpc "[Chuckling] Actually, I was hoping to convince you to join ME. "
       "I have a most difficult job ahead of me, but with your help I know it "
       "can be done. Will you help me?")
  (silas-ask-help knpc kpc)
  )

(define (silas-job knpc kpc)
  (say knpc "My goal is to bring a new golden age to the Shard. "
       "Many obstacles lie before me, "
       "and I greatly need someone with your talents. "
       "Will you help me, Wanderer? I cannot do this without you.")
  (silas-ask-help knpc kpc)
  )

(define (silas-help knpc kpc)
  (say knpc "Will you help me in my task?")
  (silas-ask-help knpc kpc)
  )

(define (silas-bye knpc kpc)
  (say knpc "Farewell, Wanderer, and good fortune!"))

;; Tier 2
(define (silas-expe knpc kpc)
  (say knpc "Yes, I have heard of your coming. "
       "I knew that sooner or later you would appear. "
       "I have studied the history of the Wanderers quite extensively."))
(define (silas-hist knpc kpc)
  (say knpc "When the library above was burnt most of the archives were lost. "
       "I'm afraid the best record of the Wanderers is now in this feeble book. "
       "[He taps his head]"))
(define (silas-wand knpc kpc)
  (say knpc "All Wanderers arrive through the Shrine Gate, as you did. But did you know there is another Gate?")
  (if (yes? kpc)
      (say knpc "You are well-learned already! Of course I am speaking of the Demon Gate.")
      (say knpc "Indeed there is. It is known as the Demon Gate.")))

(define (silas-demo knpc kpc)
  (say knpc "For a long time the Demon Gate connected the Shard to other worlds. "
       "This was a golden age of progress and enlightenment, "
       "when Wizards from this world and others would freely mingle, "
       "sharing knowledge and commerce.")
  (prompt-for-key)
  (say knpc "But eventually, trouble began to brew. "
       "One of the other worlds began to conquer the others, "
       "using the gate to move its armies. For fear that the Shard also would "
       "be invaded our Wizards sealed the Demon Gate with eight locks, and divided the "
       "keys among themselves for safe-keeping."))

(define (silas-key knpc kpc)
  (if (any-in-inventory? kpc rune-types)
      (say knpc "The keys are special runes, like the one you already carry.")
      (say knpc "The keys are special runes. Unfortunately, they have been lost through the ages.")))

(define (silas-rune knpc kpc)
  (say knpc "I have made it my goal to recover all of the lost runes. Even if "
       "we choose to keep the Demon Gate locked forever, we should at least know that "
       "the runes are all accounted for and safely guarded. Don't you agree?")
  (if (yes? kpc)
      (say knpc "Of course, it is merely sound policy.")
      (say knpc "But consider if the runes were to fall into the wrong hands, "
           "or if some day we should need to evacuate the Shard, "
           "or if we needed to call on assistance from the other worlds? "
           "Surely it is good to have the option of opening the Demon Gate, "
           "even if we never intend to exercise it.")))

;; Accursed, Wise
(define (silas-accu knpc kpc)
  (say knpc "[He chuckles] Yes, I am Accursed. Unfortunately that name has come "
       "to be demonized by the popular imagination. All sorts of wild tales abound "
       "regarding our supposed activities. I assure you that none of them are true. But "
       "we cannot defend ourselves by revealing what we really do, for we are all sworn "
       "to secrecy."))

(define (silas-secr knpc kpc)
  (say knpc "The rites of the Accursed are kept secret to protect them from "
       "profane people, and to protect foolish people from attempting them! They are "
       "sacred and dangerous, and not intended for the uninitiated. But I assure you, "
       "they do not involve anything harmful to innocents!"))

(define (silas-wise knpc kpc)
  (say knpc "Ah, yes. The so-called Wise. You know, they are part of a "
       "venerable and useful tradition. At least, it was a useful tradition in bygone "
       "days. [Sigh] I fear that now the Wise are as often an impediment to the Shard as "
       "they are a help. Consider this whole business with Absalot."))
 
(define (silas-absa knpc kpc)
  (say knpc "By now you are no doubt familiar with the story. The Enchanter, in "
       "misguided jealousy of our secrets, is obsessed with eliminating the "
       "Accursed. The Stewardess of Glasdrin is hungry for power, and dreams of a "
       "military empire. She plans to conquer or raze the cities of the Shard one by "
       "one, Absalot was merely the first and most convenient target. She easily "
       "manipulated the evidence against Absalot and convinced the other cities and the "
       "Enchanter to destroy it. The rest is history."))

;; Philosophy
(define (silas-evil knpc kpc)
  (say knpc "What, really, is evil? Mind you, I am not saying that evil does "
       "not exist. I simply ask you to challenge your conventional notions of what is "
       "evil. If an authoritative figure tells us something is wrong, shall we take "
       "them at their word? Are the authorities not people like us, with their own "
       "agendas? Everyone acts from their own selfish motives, you know, whether or not "
       "they admit it.")
  (prompt-for-key)
  (say knpc "And indeed, Wanderer, that is as it should be! All should strive "
       "after their own interests, for that is the natural way of things. All Men are "
       "pieces in a game they cannot avoid playing. To deny the game, to deny one's own "
       "part in it, to accept the belief that one's own interests are not as important "
       "as those of another, these are all deceptions propogated by one's opponents."))

(define (silas-good knpc kpc)
  (say knpc "You ask me of good, and I ask you in return, what is it that you "
       "most desire? A thing has value only when it is desired by Men. How ironic, that "
       "Men are taught to suppress their desires, to quench them with forbearance, as "
       "if they were evil because they were desires, when in fact the opposite is true! "
       "I tell you a great secret, it is the desire of certain Men that all other Men "
       "quench their desires!"))

(define (silas-desi knpc kpc)
  (say knpc "Yes, as the spirit animates the flesh, so desire animates the "
       "spirit. Without it the spirit is listless and void. This is the first step of "
       "the Accursed, to acknowledge their own desires as what they are: the ultimate "
       "good, worthy of all sacrifice and all striving."))

(define (silas-sacr knpc kpc)
  (say knpc "The doctrine of the Accursed holds that sacrifice is the ladder of "
       "desire. Sacrifice is the inevitable outcome of choice, for not all things are "
       "possible! To make the object of desires possible, other possibilities must be "
       "denied. As the branch of a tree can be cut by shears, so each choice one makes "
       "shears off an entire branch of the future. That is what we mean by sacrifice: "
       "the pruning of the tree of possibilities."))

;; People
(define (silas-ench knpc kpc)
  (say knpc "'Tis a pity he is such a foe to the Accursed. But I'm afraid he is "
       "old and inflexible, and once he has decided something is evil there is no "
       "changing his mind. Unfortunately his notions of good and evil are misguided and "
       "unworkable."))

(define (silas-deni knpc kpc)
  (say knpc "Dennis is an earnest but frankly unimaginative young man."))

(define (silas-sele knpc kpc)
  (say knpc "Selene is a talented young woman. "
       "Unfortunately her powers will always be inhibited by her insatiable "
       "cruelty."))
 
;; Quest-related
(define (silas-ques knpc kpc)
  (let* ((gob (kobj-gob-data knpc))
        (quest (silas-quest gob)))

    (define (has-all-runes?)
      (all-in-inventory? kpc rune-types))

    (define (missing-only-s-rune?)
      (all-in-inventory? kpc
                         (filter (lambda (ktype)
                                   (not (eqv? ktype t_rune_s)))
                                 rune-types)))

    (define (give-last-rune)
      (say knpc "I see you have all save 1 of the runes. Please forgive me for "
           "a small deception, but I have hidden the last rune here in Old "
           "Absalot. Consider it one last test for you to find it.")
	(quest-data-update-with 'questentry-rune-s 'silasinfo 1 (quest-notify nil))
	(quest-data-assign-once 'questentry-rune-s)
	)

    (define (continue-quest) 
      (say knpc "I see you are still missing at least one rune. Don't give up, "
           "Wanderer! Ask among the Wise, delve into the deeps, search far and wide."))

    (define (end-quest)
      (quest-done! quest #t)
      (say knpc "Well done, Wanderer! You have collected all the lost runes, "
           "a feat worthy of legend. Well done, indeed!")
      (prompt-for-key)
      (say knpc "Now that all the runes are recovered, it is imperative that "
           "they be kept safe. Forgive me for being overly suspicious, but I do not trust "
           "their keeping to the Wise, not even to the Enchanter himself.")
      (prompt-for-key)
      (say knpc "I know what you are thinking, my friend, but perish the thought! "
           "I could not possibly keep them here with me. Nor do I want to. "
           "No, I have something much more daring in mind. ")
      (prompt-for-key)
      (say knpc "Wanderer, the time has come to grasp the nettle. Indeed, I "
           "sense that this is what brought you to the Shard. Take the keys, find the Gate, "
           "and unlock it. Face what lies within. Be bold, and you will usher in a New "
           "Age. Will you do this?")
      (if (yes? kpc)
          (say knpc "Then I will tell you a great secret hidden even from the "
               "Wise: the gods yet live, as do their foes. I know not which you will face. But "
               "this I know: their foes sealed the Gate. ")
          (say knpc "[His shoulders slump] It was not your burden to bear, "
               "Wanderer, for you are a stranger to this "
               "world. Keep the keys safe, at least, if you will not change your mind.")))
           
    (define (offer-quest)
      (say knpc "Wanderer, I have a most important task for you: find the eight "
           "rune-keys which lock the Demon Gate. Will you do this?")
      (if (yes? kpc)
          (begin
            (quest-accepted! quest #t)
            (cond
            	((has-all-runes?)
            		(say knpc "You already have the runes?")
            		(prompt-for-key)
            		(end-quest)
            		)
            	((missing-only-s-rune?)
            		(give-last-rune))
            	(#t
            		(say knpc "I know I can count on you. There is a most clever man, "
                 	"an Alchemist, who lives on Oparine. Perhaps you know of him already. He would "
                 	"be a good place to start."))
             ))
          (say knpc "It is imperative that we find them. I am disappointed, my "
               "friend, but no doubt you have your reasons.")))

    (if (silas-will-help? gob)
        (if (quest-done? quest)
            (say knpc "Seek the Demon Gate!")
            (if (quest-accepted? quest)
                (if (has-all-runes?)
                    (end-quest)
                    (if (missing-only-s-rune?)
                        (give-last-rune)
                        (continue-quest)))
                (offer-quest)))
        (say knpc "Join me, and there will be quests and glory in abundance. "
             "You will become the most famous Wanderer -- indeed, "
             "the greatest hero of the Shard -- for all time."))
    ))

(define (pissed-off-silas knpc kpc)
  (map (lambda (tag)
         (if (defined? tag)
             (let ((kchar (eval tag)))
               (if (is-alive? kchar)
                   (begin
                     (kern-being-set-base-faction kchar faction-accursed)
                     (kern-char-set-schedule kchar nil)
                     )))))
       (list 'ch_silas 'ch_dennis 'ch_selene))
  (make-enemies knpc kpc)
  )

(define (silas-noss knpc kpc)
  (say knpc "[His face freezes] How did you learn that name, friend?")
  (kern-conv-get-reply kpc)
  (say knpc "Have you been nosing around in other people's property?")
  (if (yes? kpc)
      (say knpc "How unfortunate. I'm afraid it is time for you to leave.")
      (say knpc "I think you have. I think you have been misbehaving, "
           "and now you are telling a fib. You are not welcome here. "
           "Leave at once."))
  (pissed-off-silas knpc kpc)
  (kern-conv-end)
  )

(define silas-conv
  (ifc basic-conv

       ;; basics
       (method 'default silas-default)
       (method 'hail silas-hail)
       (method 'bye  silas-bye)
       (method 'job  silas-job)
       (method 'name silas-name)
       (method 'join silas-join)

       (method 'absa silas-absa)
       (method 'accu silas-accu)
       (method 'demo silas-demo)
       (method 'deni silas-deni)
       (method 'denn silas-deni)
       (method 'desi silas-desi)
       (method 'ench silas-ench)
       (method 'evil silas-evil)
       (method 'expe silas-expe)
       (method 'gate silas-demo)
       (method 'good silas-good)
       (method 'help silas-help)
       (method 'hist silas-hist)
       (method 'key  silas-key)
       (method 'keys silas-key)
       (method 'noss silas-noss)
       (method 'ques silas-ques)
       (method 'rune silas-rune)
       (method 'sacr silas-sacr)
       (method 'secr silas-secr)
       (method 'sele silas-sele)
       (method 'task silas-job)
       (method 'wand silas-wand)
       (method 'wise silas-wise)
       ))

(define (mk-silas)
  (bind 
   (kern-mk-char 
    'ch_silas           ; tag
    "Silas"             ; name
    silas-species         ; species
    silas-occ              ; occ
    s_silas     ; sprite
    faction-men      ; starting alignment
    0 5 0            ; str/int/dex
    2 1              ; hp mod/mult
    2 1              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
	0
    silas-lvl
    #f               ; dead
    'silas-conv         ; conv
    sch_silas           ; sched
    'spell-sword-ai  ; special ai
    nil              ; container
    (list t_stun_wand) ; readied
    )
   (silas-mk)))
