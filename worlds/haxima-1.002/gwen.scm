;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

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
(define (gwen-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by my shop when I'm open. "
           "It's the Gray Dove Inn in the northwest corner of town. "
           "I open at 9:00AM and close the counter at midnight.")
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
                             "You won't find a better deal in the Peninsula!")))
                  ;; no - player does not want a room
                  (say knpc "Perhaps another time.")))))))

(define (gwen-thie knpc kpc)
  (say knpc "Some recent travelers from Green Tower met a man in great haste heading east through the pass. You might try asking around in Green Tower.")
  (quest-data-update-with 'questentry-thiefrune 'tower 1 (quest-notify (grant-party-xp-fn 10)))
  )

(define gwen-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) 
                          (say knpc "I cannot help you with that")))
       (method 'hail
               (lambda (knpc kpc)
                 (kern-print "[You see an enchanting woman dressed in "
                             "gray. Belted to her waist is a long, thin "
                             "sword.]")
                 (say knpc "Welcome, traveler.")))
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
                      "but thank you for the offer.")))
       (method 'chan
               (lambda (knpc kpc)
                 (say knpc "That rapscallion is usually at the tavern. He "
                      "stumbles back to his room here late at night, drunk as a sailor.")))
       (method 'civi 
               (lambda (knpc kpc) 
                 (say knpc "There is not much civilization to speak of around here.")))
       (method 'earl
               (lambda (knpc kpc)
                 (say knpc "A sweet but befuddled old man. He keeps the shop "
                      "south of my Inn.")))
       (method 'enem 
               (lambda (knpc kpc) (say knpc "That is not your business.")))
       (method 'esca 
               (lambda (knpc kpc)
                 (say knpc "If one wishes to hide from an enemy or a "
                      "shameful act there is no place better than this "
                      "forgotten corner of the Shard.")))
       (method 'inn  
               (lambda (knpc kpc)
                 (say knpc "I enjoy running the Inn because I get to hear "
                            "news from travelers.")))
       (method 'jim
               (lambda (knpc kpc)
                 (say knpc "Handsome, but rather grim. He owns the "
                      "blacksmith shop on the east end.")))
       (method 'news (lambda (knpc kpc)
                       (say knpc "I've heard the Enchanter is missing something valuable.")))
       (method 'room gwen-trade)
       (method 'sham 
               (lambda (knpc kpc) (say knpc "That is not your business.")))
       (method 'swor
               (lambda (knpc kpc) 
                 (say knpc "It was a gift from a friend.")))
       (method 'tave
               (lambda (knpc kpc)
                 (say knpc "The Lusty Juggs is in the south part of town.")))
       (method 'thie gwen-thie)
       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "This is a small town, far from civilization. "
                      "Many come here to escape.")))

       ))
