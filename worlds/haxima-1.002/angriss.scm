;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define angriss-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (angriss-mk)
  (mk-quest))

(define (angriss-quest angriss) angriss)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Angriss is Queen of the Spiders. Suspicious, jealous, alien.
;;----------------------------------------------------------------------------

;; Basics...
(define (angriss-hail knpc kpc)
  (say knpc "Hideous Soft One, who comes uninvited, be mindful of the bones "
       "of my former guests, lest you trip and find your woeful place "
       "among them."))

(define (angriss-default knpc kpc)
  (say knpc "I search the web of thought and find no answer."))

(define (angriss-name knpc kpc)
  (say knpc "I am Angriss to men, RuKa to the goblins, Hibliminos to the "
       "trolls, and Midwife to the dead."))

(define (angriss-join knpc kpc)
  (say knpc "Lips join a cup only to drain it. Do not tempt me, "
       "Soft One."))

(define (angriss-job knpc kpc)
  (say knpc "I drink, I breed, I fear, and I hate."))

(define (angriss-bye knpc kpc)
  (say knpc "Return where you belong, to the deeps of light above."))



(define (angriss-soft knpc kpc)
  (say knpc "You crush the grape to make the wine, I drink the wine "
       "that's crushed from Men."))

(define (angriss-drin knpc kpc)
  (say knpc "A river of blood flows to my lair, a wasteland of bones waxes "
       "there. Is not the purpose of the weak to feed the strong?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "It is my belief, but the weak must resist, for only struggle "
       "reveals who is truly weak and who is strong."))

(define (angriss-bree knpc kpc)
  (say knpc "The Soft Ones make one, maybe two, and grieve their deaths. "
       "I make a thousand, one survives, the Worthy One, the rest are dross I "
       "do not mourn. Is this not wise?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "It is our way."))
  
(define (angriss-fear knpc kpc)
  (say knpc "The Soft Ones are strange. They mourn their slain, AVENGE them. "
       "The day will come when they descend with numbers too great to "
       "slaughter, with wizards and soldiers, as they did to Absalot, "
       "and they will bring my doom. I have sensed it on the web of time."))

(define (angriss-hate knpc kpc)
  (say knpc "The weakness, the arrogance, the IMPURITY of the Soft Ones "
       "disgusts me. I loathe their squalid cities, their tramping armies, "
       "their insipid devices and adornments."))

(define (angriss-rune knpc kpc)

  (define (player-alone?)
    (display "player-alone?")(newline)
    (< (num-player-party-members) 
       2))

  (let ((quest (angriss-quest (kobj-gob-data knpc))))

    (define (choose-victim)
      (say knpc "Choose whom you will give to me.")
      (quest-done! quest #t)
      (let ((kchar (kern-ui-select-party-member)))
        (if (null? kchar)
            (say knpc "Do not toy with me.")
            (begin
              (say knpc "Take what you seek, and I will take what you have "
                   "given!")
              (prompt-for-key)
              (kern-char-leave-player kchar)
              (kern-being-set-base-faction kchar faction-none)
              (kern-obj-remove-from-inventory knpc t_rune_k 1)
              (kern-obj-add-to-inventory kpc t_rune_k 1)
              (improve-relations knpc kpc)
              (kern-conv-end)))))

    (define (offer-quest)
      (display "offer-quest")(newline)
      (if (player-alone?)
          (begin
            (say knpc "I know of what you seek. Bring me a sacrifice and it "
                 "will be yours. Agreed?")
            (if (kern-conv-get-yes-no? kpc)
                (begin
                  (quest-accepted! quest)
                  (improve-relations knpc kpc))
                (say knpc "Then it will remain mine.")))
          (begin
            (say knpc "I know of what you seek. Give me a sacrifice, "
                 "one from your party, and it will be yours. Agreed?")
            (if (kern-conv-get-yes-no? kpc)
                (choose-victim)
                (say knpc "Then it will remain mine.")))))
            

    (display "quest:")(display quest)(newline)
    (if (quest-done? quest)
        (say knpc "What you seek was already given to you.")
        (if (quest-accepted? quest)
            (if (player-alone?)
                (say knpc "You have not brought me a victim, "
                     "so you cannot have the rune.")
                (choose-victim))
            (offer-quest)))))


(define angriss-conv
  (ifc basic-conv

       ;; basics
       (method 'default angriss-default)
       (method 'hail angriss-hail)
       (method 'bye angriss-bye)
       (method 'job angriss-job)
       (method 'name angriss-name)
       (method 'join angriss-join)
       
       (method 'soft angriss-soft)
       (method 'drin angriss-drin)
       (method 'bree angriss-bree)
       (method 'fear angriss-fear)
       (method 'hate angriss-hate)
       (method 'rune angriss-rune)
       ))

;; Angriss's special abilities:
;; o spew web at great distance
;; o cast In Zu (sleep)
;; o vampiric touch
(define (angriss-ai kchar)

  (display "hp=")(display (kern-char-get-hp kchar))
  (display " mp=")(display (kern-char-get-mana kchar))
  (newline)

  (define (use-vampiric-touch-on-foes?)
    (display "use-vampiric-touch-on-foes?")(newline)
    (and (can-use-ability? vampiric-touch kchar)
         (foldr (lambda (val ktarg)
                  (or val
                      (use-ability vampiric-touch kchar ktarg)))
                #f
                (get-hostiles-in-range kchar 1))))

  (define (use-web-spew-on-foes?)
    (display "use-web-spew-on-foes?")(newline)
    (define (use-on? ktarg)
      (display "use-on?: ")(display ktarg)(newline)
      (and (not (is-ensnared? ktarg))
           (not (is-paralyzed? ktarg))
           (not (is-asleep? ktarg))
           (use-ability web-spew kchar ktarg)))
    (and (can-use-ability? web-spew kchar)
         (foldr (lambda (val ktarg)
                  (or val
                      (use-on? ktarg)))
                #f 
                (get-hostiles-in-range kchar
                                       (kern-char-get-level kchar)))))
         
  (or (and (wants-healing? kchar)
           (use-vampiric-touch-on-foes?))
      (use-web-spew-on-foes?)))

(define (mk-angriss)
  (bind 
   (kern-char-force-drop
    (kern-mk-char 
     'ch_angriss         ; tag
     "Angriss"           ; name
     sp_queen_spider     ; species
     nil                 ; occ
     s_purple_spider     ; sprite
     faction-wood-spider ; starting alignment
     20 0 20             ; str/int/dex
     10 5                ; hp mod/mult
     10 5                ; mp mod/mult
     (max-hp sp_spider nil angriss-lvl 10 5) ;;..current hit points
     0 ;;...........current experience points
     (max-mp sp_spider nil angriss-lvl 10 5) ;;..current magic points
     angriss-lvl
     #f                  ; dead
     'angriss-conv       ; conv
     nil                 ; sched
     'angriss-ai                 ; special ai
     
     ;;..........container (and contents)
     (mk-chest nil
               (mk-contents (add-content 1 t_rune_f)))
     nil                 ; readied
     )
    #t)
    (angriss-mk)))
