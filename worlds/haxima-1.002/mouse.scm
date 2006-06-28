;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define mouse-start-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (mouse-mk) (list #t))
(define (mouse-first-meeting? mouse) (car mouse))
(define (mouse-set-first-meeting! mouse val) (set-car! mouse val))

(define (mouse-meet-first-time knpc kpc)
  (mouse-set-first-meeting! (kobj-gob-data knpc) #f)

  (define (mouse-disappear)
    (say knpc "Oh, bother. Not again!")
    (kern-obj-add-effect knpc ef_invisibility nil)
    (kern-conv-end kpc)
    (kern-log-msg "<Hit any key to continue>")
    (kern-ui-waitkey)
    )
    
  (define (betray-player kchar)
    (kern-char-leave-player kchar)
    (kern-being-set-base-faction kchar faction-monster))

  (define (kathryn-alive-in-party)
    (betray-player ch_kathryn)
    (if (and (defined? 'ch_thud)
             (is-player-party-member? ch_thud))
        (if (is-alive? ch_thud)
            (say ch_kathryn "The fool has led us right to the Thief! "
                 "Quickly, Thud! Kill them all!"))
        (betray-player ch_thud))
    (say ch_kathryn "Fool! Now the thief is mine!")
    (mouse-disappear))


  (define (kathryn-alive-but-not-in-party)
    (let* ((gate-loc (mk-loc (loc-place (kern-obj-get-location knpc)) 7 2))
           (kgate (open-moongate gate-loc)))
      (warp-in ch_kathryn gate-loc south faction-monster)
      (say ch_kathryn "I knew this fool would lead me right to you, "
           "Thief!")
      (if (and (defined? 'ch_thud)
               (is-alive? ch_thud))
          (begin
            (warp-in ch_thud gate-loc west faction-monster)
            (say ch_kathryn "Thud, do what you do best!")
            (say ch_thud "KILL! KILL! KILL! KILL")))
      (close-moongate kgate))
    (mouse-disappear))

  (define (thud-alive-in-party)
    (say ch_thud "Thief here, but Red Lady dead! Thud kill all!")
    (mouse-disappear))

  (define (both-dead-in-party)
    (betray-player ch_thud)
    (say knpc "It appears I owe you a debt of gratitude. That sorceress and "
         "her brute were getting to be a nuisance!"))

  (define (kathryn-dead-in-party)
    (betray-player ch_kathryn)
    (if (and (defined? 'ch_thud)
             (is-player-party-member? ch_thud))
        (if (is-alive? ch_thud)
            (thud-alive-in-party)
            (both-dead-in-party))))

  (if (defined? 'ch_kathryn)
      (if (is-player-party-member? ch_kathryn)              
          (if (is-alive? ch_kathryn)
              (kathryn-alive-in-party)
              (kathryn-dead-in-party))
          (if (is-alive? ch_kathryn)
              (kathryn-alive-but-not-in-party)
              (if (and (defined? 'ch_thud)
                       (is-player-party-member? ch_thud))
                  (if (is-alive? ch_thud)
                      (thud-alive-in-party)
                      (both-dead-in-party)))))))
  

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define (mouse-hail knpc kpc)
  (let ((mouse (kobj-gob-data knpc)))
    (if (mouse-first-meeting? mouse)
        (mouse-meet-first-time knpc kpc)
        (say knpc "Ah, hello. Heh."))))

(define (mouse-default knpc kpc)
  (say knpc "Got me there."))

(define (mouse-name knpc kpc)
  (say knpc "I'm the Mouse."))

(define (mouse-join knpc kpc)
  (say knpc "Sorry, I'm not a team player. Heh."))

(define (mouse-job knpc kpc)
  (say knpc "I... uh... collect things."))


(define (mouse-coll knpc kpc)
  (say knpc "Some might even call me a thief."))

(define (mouse-thie knpc kpc)
  (say knpc "Until recently it was a good business. Somebody wants "
       "something, they pay me to get it. But then I was hired by this "
       "strange lady in red."))

(define (mouse-lady knpc kpc)
  (say knpc "The lady hired me to get something and then meet her to "
       "exchange it for payment. All very typical, you know? But instead "
       "of paying me, she tried to kill me!"))

(define (mouse-kill knpc kpc)
  (say knpc "That red lady and her brute were relentless! I can't thank you "
       "enough for getting rid of them, but I have a bad feeling they weren't "
       "working alone. I'll be hunted for the rest of my life unless I can "
       "get rid of this stupid rune!"))

(define (mouse-rune knpc kpc)
  (if (not (in-inventory? knpc t_rune_k))
      (say knpc "It's your problem now, buddy!")
      (begin

        (define (give-rune gold)
          (let* ((pgold (kern-player-get-gold)))
            (if (> pgold gold)
                (kern-obj-add-gold kpc (- 0 gold))
                (let ((price (min pgold gold)))
                  (say knpc "You don't have enough! Oh well, I'll just take "
                       "whatever you can give me for it.")
                  (kern-obj-add-gold kpc (- 0 price)))))
          (kern-obj-remove-from-inventory knpc t_rune_k 1)
          (kern-obj-add-to-inventory kpc t_rune_k 1))
        
        (say knpc "This rune I got for the red lady has been nothing but "
             "trouble since I first heard of it. I don't even know what "
             "it's good for! I'll give you a really good deal on it. Say, "
             "500 gold?")
        (if (kern-conv-get-yes-no? kpc)
            (give-rune 500)
            (begin
              (say knpc "Well, I guess I do owe you for saving me from "
                   "that red lady. How about 250 gold?")
              (if (kern-conv-get-yes-no? kpc)
                  (give-rune 250)
                  (begin
                    (say knpc "You drive a hard bargain, buddy. 100 gold?")
                    (if (kern-conv-get-yes-no? kpc)
                        (give-rune 100)
                        (begin
                          (say knpc "50?")
                          (if (kern-conv-get-yes-no? kpc)
                              (give-rune 50)
                              (begin
                                (say knpc "Look, just take it ok?")
                                (if (kern-conv-get-yes-no? kpc)
                                    (give-rune 0)
                                    (begin
                                      (say knpc "I'm desperate! Here, "
                                           "I'll pay YOU to take it! "
                                           "Just get it away from me!")
                                      (give-rune (- 0 100))))))
                          )))))))))
      
(define (mouse-bye knpc kpc)
  (say knpc "No offense, but I hope we never meet again."))

(define mouse-conv
  (ifc nil
       (method 'default mouse-default)
       (method 'hail mouse-hail)
       (method 'bye mouse-bye)
       (method 'job mouse-job)
       (method 'name mouse-name)
       (method 'join mouse-join)

       (method 'coll mouse-coll)
       (method 'kill mouse-kill)
       (method 'lady mouse-lady)
       (method 'rune mouse-rune)
       (method 'thie mouse-thie)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-mouse)
  (bind 
   (kern-char-force-drop
    (kern-char-arm-self
     (kern-mk-char 
      'ch_mouse ;;..tag
      "Mouse" ;;....name
      sp_human ;;.....species
      nil ;;..........occupation
      s_brigand ;;.....sprite
      faction-men ;;..faction
      0 ;;............custom strength modifier
      0 ;;............custom intelligence modifier
      10 ;;............custom dexterity modifier
      2 ;;............custom base hp modifier
      2 ;;............custom hp multiplier (per-level)
      1 ;;............custom base mp modifier
      1 ;;............custom mp multiplier (per-level)
      (max-hp sp_human nil mouse-start-lvl 0 0) ;;..current hit points
      0 ;;...........current experience points
      (max-mp sp_human nil mouse-start-lvl 0 0) ;;..current magic points
      mouse-start-lvl  ;;..current level
      #f ;;...........dead?
      'mouse-conv ;;...conversation (optional)
      nil ;;..........schedule (optional)
      nil ;;..........custom ai (optional)
      
      ;;..........container (and contents)
      (mk-chest nil
                (mk-contents
                             (add-content 1 t_rune_k)
                             (add-content 1 t_armor_leather)
                             (add-content 1 t_leather_helm)
                             (add-content 1 t_sword)
                             (add-content 1 t_bow)
                             (add-content 50 t_arrow)))
      
      nil ;;..........readied arms (in addition to the container contents)
      nil ;;..........hooks in effect
      ))
    #t)
   (mouse-mk)))
