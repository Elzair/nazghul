;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_gren
               (list 0  0  trigrave-gwens-bed        "sleeping")
               (list 6  0  trigrave-tavern-table-1a  "eating")
               (list 7  0  trigrave-forge            "working")
               (list 12 0  trigrave-tavern-table-1a  "eating")
               (list 7  0  trigrave-forge            "working")
               (list 18 0  trigrave-tavern-hall      "idle")
               (list 12 0  trigrave-gwens-bed        "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (gwen-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Gwen is the innkeeper for Trigrave.
;;----------------------------------------------------------------------------
(define (gwen-trade knpc kpc)
  (say knpc "I'm afraid that part of my script is missing right now"))

(define (gwen-sea knpc kpc)
  (say knpc "Follow the road south to reach the shores of "
       "the Gray Sea. Across it lies he Greater Shard, where "
       "one will find kingdoms and empires that put our local "
       "warlords to shame."))
(define (gwen-king knpc kpc)
  (say knpc "I have seen lands that our good Chanticleer has not even heard "
       "of! Perhaps someday we can speak of them more."))

(define gwen-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "I cannot help you with that")))
       (method 'hail (lambda (knpc kpc) (say knpc "Welcome")))
       (method 'bye (lambda (knpc kpc) (say knpc "Goodbye")))
       (method 'job (lambda (knpc kpc) (say knpc "I'm the innkeeper of Trigrave. Do you need a room?")
                            (if (kern-conv-get-yes-no? kpc)
                                (gwen-trade knpc kpc)
                                (say knpc "Perhaps another time"))))
       (method 'name (lambda (knpc kpc) (say knpc "I am Gwen")))
       (method 'trad gwen-trade)
       (method 'join (lambda (knpc kpc) (say knpc "Nay, I have the Inn to run")))

       (method 'inn  (lambda (knpc kpc)
                       (say knpc "I enjoy running the Inn because I get to hear "
                            "news from travelers.")))
       (method 'news (lambda (knpc kpc)
                       (say knpc "The news lately has been rumours of trouble "
                            "with the local warlords.")))
       (method 'trig (lambda (knpc kpc) 
                       (say knpc "This is a small town, far from civilization. "
                            "Many come here to escape.")))
       (method 'civi (lambda (knpc kpc) 
                       (say knpc "You will not find civilization on this side "
                            "of the Gray Sea, I am afraid, only warlords and "
                            "pirates.")))
       (method 'warl (lambda (knpc kpc)
                       (say knpc "Even Trigrave is ruled by a warlord, "
                            "Lord Froederick, but he troubles us little. It is "
                            "his rivals that threaten our peace.")))
       (method 'fred (lambda (knpc kpc)
                       (say knpc "Froederick never had bold plans for conquest. "
                            "He has always been content to defend his territory, "
                            "so he taxed the land little. But now he is weak and "
                            "old, and has no heir.")))
       (method 'riva (lambda (knpc kpc)
                       (say knpc "Lord Froederick's rivals are Lords Clovis and "
                            "Bruce. I fear the day we fall under the yoke of "
                            "either. Travelers from the west tell me Lord Bruce "
                            "has taken the pass through the Ork hills, and news "
                            "from the east speaks of spies from Lord Clovis "
                            "disguised as bandits in the woods.")))
       (method 'bruc (lambda (knpc kpc)
                       (say knpc "Travelers from the west tell me Lord Bruce "
                            "has taken the pass through the Ork hills. Once his "
                            "knights cross the pass nothing will stop them west "
                            "of the river.")))
       (method 'clov (lambda (knpc kpc)
                       (say knpc "News from the east is that Lord Clovis sends "
                            "his spies through the woods disguised as bandits. "
                            "No doubt they seek a place where he can use his famous "
                            "archers in a decisive ambush.")))
       
       (method 'sea gwen-sea)
       (method 'gray gwen-sea)
       (method 'king gwen-king)
       (method 'empi gwen-king)
       (method 'esca (lambda (knpc kpc)
                       (say knpc "Yes, if one wants to hide from an enemy or a "
                            "shameful act there is no place better than this "
                            "forgotten corner of the Shard.")))
       (method 'enem (lambda (knpc kpc) (say knpc "That is not your business.")))
       (method 'sham (lambda (knpc kpc) (say knpc "That is not your business.")))
       ))
