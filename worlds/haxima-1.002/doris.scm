;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Green Tower.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_doris
               (list 0  0  doris-bed "sleeping")
               (list 8  0  white-stag-counter "working"))

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (doris-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Doris is the female innkeeper of the White Stag Lodge in Green Tower.
;;----------------------------------------------------------------------------
(define (doris-name kdoris kplayer)
  (say kdoris "Hi, I'm Doris, innkeeper of the White Stag Lodge."))

(define (doris-default)
  (say kdoris "Let me think... nope, can't help you there."))

(define (doris-join kdoris kplayer)
  (say kdoris "[laughing] No thanks! I've got my hands full with the lodge."))

(define (doris-doris knpc kpc)
  (say knpc "Yep. That's me."))

(define (doris-trade knpc kpc)
  (let ((door (eval 'white-stag-door))
        (price 15))
    ;; is the room still open?
    (if (not (door-locked? (kobj-gob door)))
        ;; yes - remind player
        (say knpc "Your room is still open, silly! You can use it as long as "
             "you're in town.")
        ;; no - ask if player needs a room
        (begin
          (say knpc "My room is " price " gold. I'll unlock the door and you "
               "can use it for as long as you are in town. Agreed?")
          (if (kern-conv-get-yes-no? kpc)
              ;; yes - player agrees to the price
              (let ((gold (kern-player-get-gold)))
                ;; does player have enough gold?
                (if (>= gold price)
                    ;; yes - player has enough gold
                    (begin
                      (kern-player-set-gold (- gold price))
                      (say knpc "Very well. Take the west passage. First room "
                           "at the end of the hall.")
                      (send-signal knpc door 'unlock)
                      (kern-conv-end)
                      )
                    ;; no - player does not have enouvh gold)
                    (say knpc "Seems you're short of funds. Why don't you "
                         "walk around and kill things until you loot enough "
                         "corpses? Or whatever you bold adventurers do. [She "
                         "smiles a bit too sweetly]")))
              ;; no - player does not want the room
              (say knpc "Maybe some other time, then"))))))

(define (doris-lodge knpc kpc)
  (say knpc "Yep. It's all mine. It used to belong to my Daddy, God rest his "
       "soul. Most of my customers are local folk, woodsman or travellers "
       "wandering through."))

(define (doris-daddy knpc kpc)
  (say knpc "Daddy hunted and trapped his way to a small fortune then he "
       "built this lodge. When he died, I got the lodge."))

(define (doris-local knpc kpc)
  (say knpc "The people here are varied, odd and secretive. They are also "
       "trustworthy, competent and interesting. You will not find better "
       "friends or worse enemies then the ones you make here."))

(define (doris-woodsman knpc kpc)
  (say knpc "Hunters, lumberjacks and foragers usually pass through here if "
       "they are in the area. Wealthier sport hunters from the city take "
       "lodging here, but usually those who work in the woods camp in the "
       "woods. They come here for a bit of drink, company and a decent meal."))

(define (doris-travelers knpc kpc)
  (say knpc "Yes, folk like yourself."))

(define (doris-gen knpc kpc)
  (say knpc "An old goblin-fighter. Something of a local legend. You can "
       "find him pottering around in the woods, and he usually stops in at "
       "night for a drink."))

(define (doris-deric knpc kpc)
  (say knpc "Yes, Deric. Well, if you've met Deric then you know Deric. He's "
       "competent enough but doesn't want to spend his life in the obscurity "
       "of a frontier post."))

(define (doris-shroom knpc kpc)
  (say knpc "Some say she is a witch, and that she knows some goblin magic. "
       "She certainly is handy when anyone falls ill. She often takes her "
       "meals here."))

(define (doris-abe knpc kpc)
  (say knpc "Some kind of scholar from Glasdrin. He spends all his time "
       "studying the ruins. Bookish fellow."))

(define (doris-abigail knpc kpc)
  (say knpc "She was orphaned, so I adopted her. I always wanted a child, "
       "but that was not to be. I worry about her future... I know she "
       "belongs among her own kind but I can't bear the thought of losing "
       "her."))

(define (doris-goblins knpc kpc)
  (say knpc "They trade with the townsfolk, I even have one as a customer "
       "from time-to-time. But they are forbidden by law to enter cities in "
       "large numbers, so they're always outnumbered. Most of them are hasty "
       "to conclude their business and get back into the woods."))

(define (doris-orphaned knpc kpc)
  (say knpc "Shroom brought her to me when she was just a baby. Said she "
       "found her in the woods besides her dead parents. I don't know what - "
       "or who - killed them, Shroom wouldn't say. Perhaps I don't want to "
       "know."))

(define (doris-hail knpc kpc)
  (say knpc "Welcome to the White Stag"))

(define (doris-bye knpc kpc)
  (say knpc "Come back anytime"))

(define (doris-default knpc kpc)
  (say knpc "Can't help you there"))

(define (doris-thie knpc kpc)
  (say knpc "I haven't had any odd guests lately. Check with Deric, who gets "
       "reports from ranger patrols. And Gen may have seen something in his "
       "wanderings, too."))

(define (doris-band knpc kpc)
  (say knpc "Travelers have been complaining about them! "
       "I wish Deric would do something, they're bad for business!"))

(define doris-conv
  (ifc green-tower-conv
       (method 'band      doris-band)
       (method 'hail      doris-hail)
       (method 'bye       doris-bye)
       (method 'default   doris-default)
       (method 'name      doris-name)
       (method 'room      doris-trade)
       (method 'defa      doris-default)
       (method 'job       doris-trade)
       (method 'join      doris-join)
       (method 'dori      doris-doris)
       (method 'buy       doris-trade)
       (method 'innk      doris-trade)
       (method 'whit      doris-lodge)
       (method 'stag      doris-lodge)
       (method 'lodg      doris-lodge)
       (method 'dadd      doris-daddy)
       (method 'loca      doris-local)
       (method 'wood      doris-woodsman)
       (method 'trav      doris-travelers)
       (method 'gen       doris-gen)
       (method 'deri      doris-deric)
       (method 'shro      doris-shroom)
       (method 'abe       doris-abe)
       (method 'abig      doris-abigail)
       (method 'orph      doris-orphaned)
       (method 'gobl      doris-goblins)
       (method 'thie      doris-thie)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-doris tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Doris"             ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townswoman   ; sprite
                 faction-men         ; starting alignment
                 0 1 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'doris-conv         ; conv
                 sch_doris           ; sched
                 'townsman-ai        ; special ai
                 nil                 ; container
                 (list t_dagger)     ; readied
                 )
   (doris-mk)))
