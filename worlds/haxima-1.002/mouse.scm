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
(define (mouse-talked)
	(quest-data-update-with 'questentry-thiefrune 'talked 1 (quest-notify (grant-party-xp-fn 10)))
	)


(define (mouse-meet-first-time knpc kpc)

  (define (mouse-disappear)
    (say knpc "Oh, bother. Not again!")
    (kern-obj-add-effect knpc ef_invisibility nil)
    (kern-conv-end kpc)
    )

  (define (mouse-query)
    (say knpc "Hi. You weren't sent by the Red Lady, were you?")
    (if (yes? kpc)
        (mouse-disappear)
        (begin
        	(say knpc "Whew! You scared me for a minute.")
        	(mouse-talked)
        )
    ))

  (define (mouse-gratitude)
    (say knpc "Praise be to Alopex! The Red Lady is dead! "
         "You've done me a great favor.")
         (mouse-talked)
         )

  (define (kathryn-speech)
    (say ch_kathryn "Fool, you have led me right to the thief!")
    (kern-obj-set-conv ch_kathryn nil)
    (kern-being-set-base-faction ch_kathryn faction-monster))

  (define (thud-speech)
    (say ch_thud "Thief here! Kill! Kill! Kill!")
    (kern-obj-set-conv ch_thud nil)
    (kern-being-set-base-faction ch_thud faction-monster))

  (define (open-gate)
    (open-moongate (mk-loc (loc-place (kern-obj-get-location knpc)) 7 2)))
  
  (define (warp-in-kathryn kgate)
    (warp-in ch_kathryn 
             (kern-obj-get-location kgate)
             south
             faction-monster))

  (define (warp-in-thud kgate)
    (warp-in ch_thud 
             (kern-obj-get-location kgate)
             west
             faction-monster))

  (mouse-set-first-meeting! (kobj-gob-data knpc) #f)
  (if (defined? 'ch_kathryn)
      (if (is-alive? ch_kathryn)
          (if (is-player-party-member? ch_kathryn)

              ;; kathryn is alive in the party (if thud is defined then he must
              ;; be in the party, too; see kathryn.scm)
              (begin
                (kern-char-leave-player ch_kathryn)
                (kathryn-speech)
                (if (defined? 'ch_thud)
                    (begin
                      (if (is-alive? ch_thud)
                          (thud-speech))
                      (kern-char-leave-player ch_thud)))
                (mouse-disappear))

              ;; kathryn is alive but not in the party so gate her in, and
              ;; thud, too, if he's alive
              (let ((kgate (open-gate))
                    (use-thud? (and (defined? 'ch_thud)
                                    (is-alive? ch_thud))))
                (kern-sleep 1000)
                (warp-in-kathryn kgate)
                (if use-thud?
                    (warp-in-thud kgate))
                (kathryn-speech)
                (if use-thud?
                    (thud-speech))
                (kern-sleep 1000)
                (close-moongate kgate)
                (mouse-disappear)))

          ;; kathryn is dead
          (if (is-player-party-member? ch_kathryn)

              ;; but in the party so remove her and thud, too, if he's defined
              (begin
                (kern-char-leave-player ch_kathryn)
                (if (defined? 'ch_thud)
                    (if (is-alive? ch_thud)
                        (begin
                          (thud-speech)
                          (kern-char-leave-player ch_thud)
                          (mouse-disappear))
                        (begin
                          (kern-char-leave-player ch_thud)
                          (mouse-gratitude)))
                    (mouse-gratitude)))

              ;; kathryn is dead but not in the party (since she is not in the
              ;; party, thud cannot be either)
              (mouse-query)))

      ;; kathryn is undefined (so she could never have been in the party, and
      ;; thus neither could thud)
      (mouse-query)))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define (mouse-hail knpc kpc)
  (let ((mouse (kobj-gob-data knpc)))
    (if (mouse-first-meeting? mouse)
        (mouse-meet-first-time knpc kpc)
        (begin
        	(say knpc "Ah, hello. Heh.")
   	        (mouse-talked)
	    )
        )))

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
          (kern-obj-add-to-inventory kpc t_rune_k 1)
          (quest-data-update-with 'questentry-thiefrune 'recovered 1 (quest-notify (grant-party-xp-fn 50)))
          )
        
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

(define (mouse-alopex knpc kpc)
  (say knpc "Alopex? Oh, the old god of thieves. "
       "Or so I've heard."))

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
       (method 'alop mouse-alopex)
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
      6 ;;............custom dexterity modifier
      2 ;;............custom base hp modifier
      2 ;;............custom hp multiplier (per-level)
      1 ;;............custom base mp modifier
      1 ;;............custom mp multiplier (per-level)
      max-health ;;..current hit points
      -1 ;;...........current experience points
      max-health ;;..current magic points
      0
      mouse-start-lvl  ;;..current level
      #f ;;...........dead?
      'mouse-conv ;;...conversation (optional)
      nil ;;..........schedule (optional)
      nil ;;..........custom ai (optional)
      
      ;;..........container (and contents)
      (mk-inventory
       (list
        (list 1 t_rune_k)
        (list 1 t_armor_leather)
        (list 1 t_leather_helm)
        (list 1 t_sword)
        (list 1 t_bow)
        (list 50 t_arrow)))
      
      nil ;;..........readied arms (in addition to the container contents)
      nil ;;..........hooks in effect
      ))
    #t)
   (mouse-mk)))
