;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define trigrave-inn-room-price 10)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_gwen
               (list 0  0  trigrave-gwens-bed        "sleeping")
               (list 8  0  trigrave-tavern-table-1a  "eating")
               (list 9  0  trigrave-inn-counter      "working")
               (list 13 0  trigrave-tavern-table-1d  "eating")
               (list 14 0  trigrave-inn-counter      "working")
               (list 20 0  trigrave-tavern-table-1a  "eating")
               (list 21 0  trigrave-inn-counter      "working")
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
;; Gwen is the innkeeper for Trigrave. She is a gracious, mysterious woman.
;;----------------------------------------------------------------------------
(define (gwen-sea knpc kpc)
  (say knpc "Follow the road south to reach the shores of "
       "the Gray Sea. Across it lies the Greater Shard, where "
       "one will find kingdoms and empires that put our local "
       "warlords to shame."))
(define (gwen-king knpc kpc)
  (say knpc "I have seen lands that our good Chanticleer has not even heard "
       "of! Perhaps someday we can speak of them more."))

(define (gwen-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by my shop when I'm open. "
           "It's the Quiet Inn in the northwest corner of town.")
      (let ((door (eval 'trigrave-inn-room-1-door)))
        ;; is the room still open?
        (if (not (door-locked? (kobj-gob door)))
            ;; yes - remind player
            (say knpc "Your room is still open!")
            ;; no - ask if player needs a room
            (begin
              (say knpc "Do you need a room?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes - player wants a room
                  (begin
                    (say knpc 
                         "It will be " trigrave-inn-room-price " gold, "
                         "and you may use the room as "
                         "long as you are in town. Agreed?")
                    (if (kern-conv-get-yes-no? kpc)
                        ;; yes - player agrees to the price
                        (let ((gold (kern-player-get-gold)))
                          ;; does player have enough gold?
                          (if (>= gold trigrave-inn-room-price)
                              ;; yes - player has enough gold
                              (begin
                                (kern-player-set-gold 
                                 (- gold 
                                    trigrave-inn-room-price))
                                (say knpc "You're in room 1. Enjoy your stay!")
                                (send-signal knpc door 'unlock)
                                (kern-conv-end)
                                )
                              ;; no - player does not have enouvh gold)
                              (say knpc "Sorry, but you need more gold!")))
                        ;; no - player does not agree to the price
                        (say knpc 
                             "You won't find a better deal in Three "
                             "Corners!")))
                  ;; no - player does not want a room
                  (say knpc "Perhaps another time.")))))))

(define gwen-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) 
                          (say knpc "I cannot help you with that")))
       (method 'hail (lambda (knpc kpc) (say knpc "Welcome.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Goodbye.")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "I'm the innkeeper of Trigrave.")
                 (gwen-trade knpc kpc)))
       (method 'name (lambda (knpc kpc) (say knpc "I am Gwen.")))
       (method 'trad gwen-trade)
       (method 'join 
               (lambda (knpc kpc) 
                 (say knpc "My journeys are over, "
                      "but thank your for the offer.")))
       (method 'look 
               (lambda (knpc kpc)
                 (kern-print "You see an enchanting woman dressed in "
                             "gray. Belted to her waist is a long, thin "
                             "sword.\n")))


       (method 'bruc 
               (lambda (knpc kpc)
                 (say knpc "Travelers from the west tell me Lord Bruce "
                      "has taken the pass through the Ork hills. Once his "
                      "knights cross the pass nothing will stop them west "
                      "of the river.")))
       (method 'chant
               (lambda (knpc kpc)
                 (say knpc "That roguish bard is usually at the tavern.")))
       (method 'civi 
               (lambda (knpc kpc) 
                 (say knpc "You will not find civilization on this side "
                      "of the Gray Sea, I am afraid, only warlords and "
                      "pirates.")))
       (method 'clov 
               (lambda (knpc kpc)
                 (say knpc "News from the east is that Lord Clovis sends "
                      "his spies through the woods disguised as bandits. "
                      "No doubt they seek a place where he can use his famous "
                      "archers in an ambush.")))
       (method 'empi gwen-king)
       (method 'enem 
               (lambda (knpc kpc) (say knpc "That is not your business.")))
       (method 'esca 
               (lambda (knpc kpc)
                 (say knpc "If one wishes to hide from an enemy or a "
                      "shameful act there is no place better than this "
                      "forgotten corner of the Shard.")))
       (method 'froe 
               (lambda (knpc kpc)
                 (say knpc "Froederick never had bold plans for conquest. "
                      "He has always been content to defend his territory, "
                      "so he taxed the land little. But now he is weak and "
                      "old, and has no heir.")))
       (method 'gray gwen-sea)
       (method 'inn  
               (lambda (knpc kpc)
                 (say knpc "I enjoy running the Inn because I get to hear "
                            "news from travelers.")))
       (method 'king gwen-king)
       (method 'news (lambda (knpc kpc)
                       (say knpc "The news lately has been rumours of trouble "
                            "with the local warlords.")))
       (method 'riva 
               (lambda (knpc kpc)
                 (say knpc "Lord Froederick's rivals are Lords Clovis and "
                      "Bruce. I fear the day we fall under the yoke of "
                      "either.")))
       (method 'room gwen-trade)
       (method 'sea gwen-sea)
       (method 'sham 
               (lambda (knpc kpc) (say knpc "That is not your business.")))
       (method 'swor
               (lambda (knpc kpc) 
                 (say knpc "It was a gift from a friend.")))
       (method 'tave
               (lambda (knpc kpc)
                 (say knpc "The Lusty Jugs is in the south part of town.")))
       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "This is a small town, far from civilization. "
                      "Many come here to escape.")))
       (method 'warl 
               (lambda (knpc kpc)
                 (say knpc "Even Trigrave is ruled by a warlord, "
                      "Lord Froederick, but he troubles us little. It is "
                      "his rivals that threaten our peace.")))

       ))
