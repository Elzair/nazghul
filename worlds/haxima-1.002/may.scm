;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define may-start-lvl  6)
(define inn-room-price 30)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Bole.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_may
               (list 0  0  bole-bed-may "sleeping")
               (list 6  0  bole-dining-hall "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (may-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; May is a female innkeeper, who lives in Bole.
;; Melvin is her (seventh) husband.
;;----------------------------------------------------------------------------
(define (may-trade knpc kpc)
  (say knpc "Talk to my husband Melvin in the kitchen."))

(define (may-hail knpc kpc)
  (say knpc "[You meet a stout older woman who looks at you keenly] "
       "I know you are trouble, but I welcome thee."))

(define (may-job knpc kpc)
  (say knpc "I run the tavern with my husband here in the Bole"))

(define (may-husband knpc kpc)
  (say knpc "My husband Melvin is a worthless drunk and a coward "
       "but he is a good enough cook. I have outlived six other husbands. "
       "I married him because I needed help running the inn, that is all."))

(define (may-other-husbands knpc kpc)
  (say knpc "My other husbands were all fools! A few, indeed, I loved. "
       "But they were fools and each met his own foolish death. "
       "I see that you are a fool, too, and will meet your own foolish "
       "death some day."))

(define (may-tavern knpc kpc)
  (say knpc "Melvin is the cook and I serve the guests. "
       "Perhaps you would care to buy a drink or some supper, "
       "or a room where you may rest."))

(define (may-guests knpc kpc)
  (say knpc "[She gives you a canny look] Aye, even now we have a strange "
       "woman and her... companion. But perhaps you were looking for "
       "someone else, eh?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (say knpc "Yes, I thought so. And perhaps this other person had "
             "something of interest to you?")
        (if (kern-conv-get-yes-no? kpc)
            (begin
              (say knpc "How odd, because that item was also of interest to "
                   "our current guests. Not that it is any of my business, "
                   "but one suspects that the woman and he whom you seek "
                   "arranged to meet here. Perhaps to make an... "
                   "exchange."))
            (say knpc "Well, there was another man here briefly. But he "
                 "left shortly before you arrived.")))
      (say knpc "Perhaps you are just out admiring the countryside then.")))

(define (may-woman knpc kpc)
  (say knpc "Yes, we don't often get such beautiful people in these parts. "
       "Poor Bill is quite smitten. And my fool of a husband ogles here "
       "whenever he gets a chance. [She leans in close and whispers] But "
       "if she isn't a sorceress, or one in training, then I'm a goose!"))

(define (may-companion knpc kpc)
  (say knpc "This woman travels with a great brute. I suspect he has ogre "
       "blood in his veins. Obviously he is her protector, though what bind "
       "she has over him I know not."))

(define (may-bill knpc kpc)
  (say knpc "Aye, Bill is a local boy. Not too bright, but he makes an "
       "honest living as a woodcutter. He eats here, and sometimes talks "
       "to the guests."))

(define (may-hackle knpc kpc)
  (say knpc "Hackle is a crazy but harmless old hedge-witch who lives across "
       "the stream. She has a knack for the healing arts but hasn't the wits "
       "for much else."))

(define (may-room knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "The Inn is closed. Come back in the morning at 0600.")
      (let ((door (eval 'bole-inn-room-door)))
        ;; is the room still open?
        (if (not (door-locked? (kobj-gob door)))
            ;; yes - remind player
            (say knpc "Your room is still open.")
            ;; no - ask if player needs a room
            (begin
              (say knpc "Would ye like a room?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes - player wants a room
                  (begin
                    (say knpc 
                         "It will be " inn-room-price " gold, "
                         "and you may use the room as "
                         "long as you are in town. Agreed?")
                    (if (kern-conv-get-yes-no? kpc)
                        ;; yes - player agrees to the price
                        (let ((gold (kern-player-get-gold)))
                          ;; does player have enough gold?
                          (if (>= gold inn-room-price)
                              ;; yes - player has enough gold
                              (begin
                                (kern-player-set-gold 
                                 (- gold 
                                    inn-room-price))
                                (say knpc "Good. Your room is down the hall "
                                     "on the left.")
                                (send-signal knpc door 'unlock)
                                (kern-conv-end)
                                )
                              ;; no - player does not have enouvh gold)
                              (say knpc "Ye have not the gold!")))
                        ;; no - player does not agree to the price
                        (say knpc 
                             "Sleep on the ground then. Mind the wolves.")))
                  ;; no - player does not want a room
                  (say knpc "Even one such as you must sometimes rest!")))))))
  
(define (may-thief knpc kpc)
  (say knpc "Ah... thou playest the thief-catcher. I suspected something of "
       "the kind. We HAVE had some odd guests here lately."))

(define (may-trouble knpc kpc)
  (say knpc "Ye have the hard, pitiless visage like the men of the Old Era. "
       "But I sense that thou art not unjust."))

(define may-conv
  (ifc nil
       (method 'default (lambda (knpc kpc) (say knpc "I can't help thee.")))
       (method 'hail may-hail)
       (method 'bye  (lambda (knpc kpc) (say knpc "Begone then for now.")))
       (method 'job  may-job)
       (method 'name (lambda (knpc kpc) (say knpc "I am called May.")))
       (method 'join (lambda (knpc kpc)
                       (say knpc "Don't flatter me with your foolishness.")))

       (method 'buy   may-trade)
       (method 'food  may-trade)
       (method 'drin  may-trade)
       (method 'supp  may-trade)
       (method 'trade may-trade)

       (method 'bill  may-bill)
       (method 'comp  may-companion)
       (method 'gues  may-guests)
       (method 'hack  may-hackle)
       (method 'husb  may-husband)
       (method 'inn   may-tavern)
       (method 'melv  may-husband)
       (method 'other may-other-husbands)
       (method 'run   may-tavern)
       (method 'room  may-room)
       (method 'six   may-other-husbands)
       (method 'tave  may-tavern)
       (method 'thie  may-thief)
       (method 'trou  may-trouble)
       (method 'woma  may-woman)

       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-may)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_may ;;......tag
     "May" ;;.......name
     sp_human ;;.....species
     nil ;;..........occupation
     s_townswoman ;;...sprite
     faction-men ;;..faction
     0 ;;............custom strength modifier
     1 ;;............custom intelligence modifier
     0 ;;............custom dexterity modifier
     0 ;;............custom base hp modifier
     0 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     0
     may-start-lvl  ;;..current level
     #f ;;...........dead?
     'may-conv ;;...conversation (optional)
     sch_may ;;.....schedule (optional)
     'townsman-ai ;;..........custom ai (optional)
     nil ;;..........container (and contents)
     (list t_dagger) ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (may-mk)))
