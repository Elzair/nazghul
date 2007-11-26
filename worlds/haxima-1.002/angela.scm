;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;
;; In Glasdrin.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ange
               (list 0  0  ga-bed "sleeping")
               (list 6  0  ghg-s2     "eating")
               (list 7  0  gpi-counter       "working")
               (list 11 0  ghg-s2     "eating")
               (list 12 0  gpi-counter       "working")
               (list 17 0  ghg-s2     "eating")
               (list 18 0  gpi-counter "working")
               (list 23 0  ga-bed "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (ange-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Angela is a female innkeeper, who runs the Palisades Inn in Glasdrin.
;; She is a gracious, mysterious woman.
;;----------------------------------------------------------------------------

;; Basics...
(define (ange-hail knpc kpc)
  (say knpc "[You meet a charming lady] Welcome, traveler."))

(define (ange-default knpc kpc)
  (say knpc "I'm afraid I don't know."))

(define (ange-name knpc kpc)
  (say knpc "I am Angela. And you are?")
  (let ((name (kern-conv-get-string kpc)))
    (say knpc "I am most pleased to meet you, " name 
         ". I hope you enjoy your stay in Glasdrin.")))

(define (ange-join knpc kpc)
  (say knpc "You flatter me! It's been years since I've had a good adventure, "
       "but I must decline."))

(define (ange-job knpc kpc)
  (say knpc "I'm the Innkeeper of Glasdrin. "
       "If you'd like a room just ask!"))

(define (ange-bye knpc kpc)
  (say knpc "Farewell, traveler, I do hope you come back soon!"))

;; Trade...
(define (ange-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Oh, do please stop by when I'm at my counter! "
           "The Palisades is open from 7:00AM to 11:00PM. "
           "I hope to see you then!")
      (let ((door (eval 'glasdrin-inn-room-1-door)))
        ;; is the room still open?
        (if (not (door-locked? (kobj-gob door)))
            ;; yes - remind player
            (say knpc "Room 1 is yours until you leave town!")
            ;; no - ask if player needs a room
            (begin
              (say knpc "Do you need a room?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes - player wants a room
                  (begin
                    (say knpc 
                         "That will be " glasdrin-inn-room-price " gold. "
                         "The room is yours until you leave town. "
                         "Is this to your liking?")
                    (if (kern-conv-get-yes-no? kpc)
                        ;; yes - player agrees to the price
                        (let ((gold (kern-player-get-gold)))
                          ;; does player have enough gold?
                          (if (>= gold glasdrin-inn-room-price)
                              ;; yes - player has enough gold
                              (begin
                                (say knpc "Wonderful! You're in room 1, "
                                     "our very best! Enjoy your stay.")
                                (kern-player-set-gold 
                                 (- gold 
                                    glasdrin-inn-room-price))
                                (send-signal knpc door 'unlock)
                                (kern-conv-end)
                                )
                              ;; no - player does not have enouvh gold)
                              (say knpc "Oh dear, I'm afraid you don't have "
                                   "enough gold! I do hope when your fortunes "
                                   "improve you'll come back and stay." )))
                        ;; no - player does not agree to the price
                        (say knpc "I do wish you would stay, "
                             "I rarely get such intriguing guests!")))
                  ;; no - player does not want a room
                  (say knpc "Oh, I am so disappointed, "
                       "you seem like a very interesting guest! "
                       "Perhaps another time.")))))))

;; Inn...
(define (ange-inn knpc kpc)
  (say knpc "The Palisades is a very fine inn, if I say so myself."))

(define (ange-adve knpc kpc)
  (say knpc "As you may know, all citizens of Glasdrin must serve a tour of "
       "duty. I was a quartermaster, and once I even went with the paladins "
       "on patrol."))

(define (ange-patr knpc kpc)
  (say knpc "It was a lovely walk, and camping under the stars was grand. "
       "But then gints attacked and ruined the whole mood."))

(define (ange-gint knpc kpc)
  (say knpc "Gints are like stocky men but enormous and two-headed. "
       "They once were common in the mountains, but the vigilance of Glasdrin "
       "has driven them into caves. They are most fearsome brutes. "
       "I've seen them hurl grown, heavily armored men like missiles at their "
       "own comrades!"))

;; Townspeople...
(define (ange-glas knpc kpc)
  (say knpc "Glasdrin is a fair city, do you not think so?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Indeed.")
      (say knpc "No doubt you have seen magnificent cities in your travels. "
           "I suppose I love it because it is home.")))

(define (ange-patc knpc kpc)
  (say knpc "Patch is the best healer around. "
       "He can even call back the recently deceased, "
       "if their wounds are not too grievous."))

(define ange-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default ange-default)
       (method 'hail ange-hail)
       (method 'bye ange-bye)
       (method 'job ange-job)
       (method 'name ange-name)
       (method 'join ange-join)
       
       ;; trade
       (method 'trad ange-trade)
       (method 'room ange-trade)
       (method 'buy ange-trade)
       (method 'sell ange-trade)

       ;; inn
       (method 'inn  ange-inn)
       (method 'adve ange-adve)
       (method 'gint ange-gint)
       (method 'patr ange-patr)

       ;; town & people
       (method 'glas ange-glas)
       (method 'patc ange-patc)

       ))

(define (mk-angela)
  (bind 
   (kern-mk-char 'ch_angela          ; tag
                 "Angela"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townswoman        ; sprite
                 faction-glasdrin         ; starting alignment
                 0 1 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'ange-conv          ; conv
                 sch_ange            ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_dagger)))                 ; container
                 (list t_dagger
					         )                  ; readied
                 )
   (ange-mk)))
