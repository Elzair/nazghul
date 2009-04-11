;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;
;; In Oparine
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_oscar
               (list 0  0  oparine-innkeepers-bed "sleeping")
               (list 8  0  bilge-water-seat-3     "eating")
               (list 9  0  cheerful-counter       "working")
               (list 12 0  bilge-water-seat-3     "eating")
               (list 13 0  cheerful-counter       "working")
               (list 21 0  bilge-water-seat-3     "eating")
               (list 22 0  bilge-water-hall       "idle")
               (list 23 0  oparine-innkeepers-bed "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (oscar-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Oscar is the innkeeper in Oparine.  He is a glum fellow, with a wooden leg.
;;----------------------------------------------------------------------------

;; Basics...
(define (oscar-hail knpc kpc)
  (say knpc "[You meet a glum man with a wooden leg] Hello."))

(define (oscar-default knpc kpc)
  (say knpc "I don't know."))

(define (oscar-name knpc kpc)
  (say knpc "I'm Oscar the Innkeeper."))

(define (oscar-join knpc kpc)
  (say knpc "I would just get in the way."))

(define (oscar-job knpc kpc)
  (say knpc "I'm the Innkeeper. "
       "I doubt you want to rent a room, "
       "but if you do let me know."))

(define (oscar-bye knpc kpc)
  (say knpc "Bye"))

;; Trade...
(define (oscar-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "The inn is open from 9 to 9, and I do have to eat sometime, "
           "so try me later.")
      (let ((door (eval 'oparine-inn-room-1-door)))
        ;; is the room still open?
        (if (not (door-locked? (kobj-gob door)))
            ;; yes - remind player
            (say knpc "Your room is still open.")
            ;; no - ask if player needs a room
            (begin
              (say knpc "Do you need a room?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes - player wants a room
                  (begin
                    (say knpc 
                         "That will be " oparine-inn-room-price " gold. "
                         "You can have the room as long as you're in town. "
                         "Do you still want it?")
                    (if (kern-conv-get-yes-no? kpc)
                        ;; yes - player agrees to the price
                        (let ((gold (kern-player-get-gold)))
                          ;; does player have enough gold?
                          (if (>= gold oparine-inn-room-price)
                              ;; yes - player has enough gold
                              (begin
                                (say knpc "Ok, you're in room 1, "
                                     "but you won't like it. "
                                     "Don't complain to me because I told you "
                                     "so.")
                                (kern-player-set-gold 
                                 (- gold 
                                    oparine-inn-room-price))
                                (send-signal knpc door 'unlock)
                                (kern-conv-end)
                                )
                              ;; no - player does not have enouvh gold)
                              (say knpc "Sorry, but you don't have the gold. "
                                   "This isn't a poorhouse, you know." )))
                        ;; no - player does not agree to the price
                        (say knpc 
                             "I knew you wouldn't.")))
                  ;; no - player does not want a room
                  (say knpc "I didn't think so. "
                       "I was just being polite.")))))))

;; Inn...
(define (oscar-inn knpc kpc)
  (say knpc "The former owner gave it the name. "
       "I didn't feel like changing it. Besides, that would be bad luck."))

(define (oscar-luck knpc kpc)
  (say knpc "The ghost in room 3 likes the name. I don't want the ghost of "
       "a wicked pirate mad at me.")
	(quest-data-assign-once 'questentry-ghertie)
	(quest-data-update 'questentry-ghertie 'ghertieloc 1))

(define (oscar-ghost knpc kpc)
  (say knpc "Ghastly Ghertie was murdered by her crew in room 3. "
       "She still haunts it so I don't rent it out. "
       "I wish she would pay her bill.")
	(quest-data-update 'questentry-ghertie 'ghertieid 1)
	(quest-data-update 'questentry-ghertie 'ghertieloc 1)
	(quest-data-assign-once 'questentry-ghertie))

;; Leg...
(define (oscar-leg knpc kpc)
  (say knpc "I tried to be a sailor but nobody would take me. "
       "So I cut off my leg to make myself look like a sailor. "
       "They still wouldn't take me. I miss that leg."))

;; Townspeople...
(define (oscar-opar knpc kpc)
  (say knpc "This is a port city. Most of my customers are travelers who "
       "disembark here before traveling north."))

(define (oscar-gher knpc kpc)
  (say knpc "Ghertie was a wicked pirate back before my time. "
       "Her own crew murdered her and left with her ship and her treasure.")
	(quest-data-assign-once 'questentry-ghertie))

(define (oscar-alch knpc kpc)
  (say knpc "His shop is next to the tavern. He's always looking for weird "
       "stuff to use in his experiments."))

(define (oscar-bart knpc kpc)
  (say knpc "Bart the shipwright has a shop just across the way. "
       "He usually gets pretty drunk at night. I can't keep up with him."))

(define (oscar-seaw knpc kpc)
  (say knpc "The sea witch is very beautiful but keeps to herself. "
       "She ignores me, of course."))

(define (oscar-henr knpc kpc)
  (say knpc "There's a real sailor. I'll never be like him."))

(define oscar-conv
  (ifc basic-conv

       ;; basics
       (method 'default oscar-default)
       (method 'hail oscar-hail)
       (method 'bye  oscar-bye)
       (method 'job  oscar-job)
       (method 'name oscar-name)
       (method 'join oscar-join)
       
       ;; trade
       (method 'trad oscar-trade)
       (method 'room oscar-trade)
       (method 'buy  oscar-trade)
       (method 'sell oscar-trade)

       ;; inn
       (method 'inn  oscar-inn)
       (method 'luck oscar-luck)
       (method 'ghos oscar-ghost)
       (method 'pira oscar-ghost)
       (method 'leg  oscar-leg)

       ;; town & people
       (method 'opar oscar-opar)
       (method 'alch oscar-alch)
       (method 'gher oscar-gher)
       (method 'ghas oscar-gher)
       (method 'henr oscar-henr)
       (method 'bart oscar-bart)
       (method 'sea  oscar-seaw)
       (method 'witc oscar-seaw)

       ))

(define (mk-oscar)
  (bind 
   (kern-mk-char 'ch_oscar           ; tag
                 "Oscar"             ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townsman          ; sprite
                 faction-men         ; starting alignment
                 1 1 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 1  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'oscar-conv         ; conv
                 sch_oscar           ; sched
                 'townsman-ai        ; special ai
                 nil                 ; container
                 (list t_dagger)     ; readied
                 )
   (oscar-mk)))
