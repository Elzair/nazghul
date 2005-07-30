;;----------------------------------------------------------------------------
;; gregor.scm - read-only data for Gregor the Charcoal Burner
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for...
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_gregor
               (list 0  0  gh-gregors-bed   "sleeping")
               (list 6  7  gh-graveyard     "idle")
               (list 7  0  mgc-roadbend     "idle")
               (list 13 0  gh-kitchen       "eating")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Gregor has no special flags or data.
;;----------------------------------------------------------------------------
(define (gregor-mk) nil)


;;----------------------------------------------------------------------------
;; The following are responses that are invoked by more than one keyword in the
;; conversation table.
;;----------------------------------------------------------------------------
(define (gregor-dead knpc kpc)
  (say knpc "Aye, it's a shame. My daughter and her husband both - "
       "killed by trolls."))

(define (gregor-charcoal knpc kpc)
  (say knpc "I take charcoal into town and sell it, "
       "and some folks come by my place to buy it."))

(define (gregor-hut knpc kpc)
  (say knpc "My hut's in the forest to the east. "
       "Just myself and my grandaughter living there now."))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define gregor-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Can't help you there.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Welcome, Wanderer.")))
       (method 'heal (lambda (knpc kpc) (say knpc "[cough] Well enough, my granddaughter helps take care of me.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Yep.")))
       (method 'job (lambda (knpc kpc) (say knpc "I'm a charcoal burner. I also care for this shrine.")))
       (method 'join (lambda (knpc kpc) (say knpc "Nope. Already got a job.")))
       (method 'name (lambda (knpc kpc) (say knpc "Gregor's my name.")))

       (method 'accu (lambda (knpc kpc) 
                       (say knpc "The Accursed trade their souls for power. "
                            "If not for the Wise they would overrun the Shard.")))     
       (method 'angr (lambda (knpc kpc) (say knpc "Just a legend. Mother of all wood spiders. "
                                             "Scares the kids, keeps them out o' the woods "
                                             "where they can get lost.")))
       (method 'band (lambda (knpc kpc) (say knpc "A band of rogues been raiding the shrine "
                                             "when I'm not around. They haven't attacked me, "
                                             "so they're probably just vagabonds, "
                                             "afraid of an old man's cudgel.")))
       (method 'burn gregor-charcoal)
       (method 'char gregor-charcoal)
       (method 'daug (lambda (knpc kpc) (say knpc "Aye, she was a near-witch like her mother. "
                                             "Had the knack, but not enough to be among the Wise.")))
       (method 'dead gregor-dead)
       (method 'folk (lambda (knpc kpc) (say knpc "There's homesteads scattered about in "
                                             "the woods and the foothills.")))
       (method 'fore (lambda (knpc kpc) (say knpc "Stay out of the deep woods. "
                                               "Bandits, spiders and worse live there.")))
       (method 'gate (lambda (knpc kpc) (say knpc "No one can predict when it will open, "
                                             "or if anything will come through if it does. "
                                             "I've heard of other gates in other parts of the land, "
                                             "and stories tell of others long forgotten now.")))
       (method 'gran (lambda (knpc kpc) (say knpc "I've a granddaughter name of Ilya.")))
       (method 'help (lambda (knpc kpc) (say knpc "There's always folks who need help. "
                                             "These are hard times in a hard land.")))
       (method 'hill (lambda (knpc kpc) (say knpc "Trolls are always a threat in the foothills, "
                                             "but more so of late.")))
       (method 'husb (lambda (knpc kpc) (say knpc "My son-in-law was a simple farmer. "
                                             "Why the trolls attacked I don't know. "
                                             "Maybe they were driven out of the hills "
                                             "by something else.")))
       (method 'hut gregor-hut)
       (method 'ilya (lambda (knpc kpc) (say knpc "Yep. She lives at my place now "
                                             "that her parents are dead.")))
       (method 'lake (lambda (knpc kpc) (say knpc "Exit this shrine and ye'll find yourself in a "
                                             "hidden valley. Head south and you'll see the Gray Lake "
                                             "to the west.")))
       (method 'offe (lambda (knpc kpc) (say knpc "There in the corner you'll find some chests. "
                                             "Take what's inside. Wanderers enter this world with little, "
                                             "and in the past some have done great good, "
                                             "so folks leave stuff in good will for the next one.")))
       (method 'pare gregor-dead)
       (method 'plac gregor-hut)
       (method 'shar (lambda (knpc kpc) (say knpc "That's what we call this land, Wanderer.")))
       (method 'shri (lambda (knpc kpc) (say knpc "This shrine is for those who come through the gate. "
                                             "Wanderers like yourself. "
                                             "Folks leave simple offerings here to help you on "
                                             "your journey.")))
       (method 'spid (lambda (knpc kpc) (say knpc "Some of the brutes in the deep part of the woods are "
                                             "big as oxen. Children of Angriss, we call those big monsters.")))
       (method 'town (lambda (knpc kpc) (say knpc "Trigrave is the closest town. Follow the "
                                             "road south and you can't miss it.")))
       (method 'trol (lambda (knpc kpc) (say knpc "Trolls eat folks. Even crack the "
                                             "bones and suck the marrow. Nothing left to "
                                             "bury.")))
       (method 'wand (lambda (knpc kpc) (say knpc "We call those who come through the gate Wanderers. "
                                             "No one knows where they come from or where they go. "
                                             "You are the first to come through in a long, long time.")))
       (method 'wise (lambda (knpc kpc) (say knpc "The Wise are both strong and - mostly - good. "
                                             "They help the land, as they can, and keep the Accursed "
                                             "at bay.")))
       (method 'witc (lambda (knpc kpc) (say knpc "Don't know of any witches in these parts any more. "
                                             "Used to be one south of the lake.")))
       ))

