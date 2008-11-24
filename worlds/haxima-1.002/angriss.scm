;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define angriss-lvl 20)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; No schedule.  (Angriss' Lair)
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (angriss-mk)
  (list #f (mk-quest)))

(define (angriss-quest angriss) (cadr angriss))
(define (angriss-spoke? angriss) (car angriss))
(define (angriss-spoke! angriss) (set-car! angriss #t))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Angriss is Queen of the Spiders, dwelling in Angriss' Lair.
;; Suspicious, jealous, alien.
;;----------------------------------------------------------------------------

;; Basics...
(define (angriss-hail knpc kpc)
  (say knpc "Hideous Soft One, who\n"
       "comes uninvited, be quick\n"
       "in honor, or die."))

(define (angriss-default knpc kpc)
  (say knpc "[Still as a statue in her web, she gives no response]"))

(define (angriss-name knpc kpc)
  (say knpc "To Men, Angriss; to\n"
       "the Goblins, Ruka; to the\n"
       "Trolls, Hibliminos."))

(define (angriss-join knpc kpc)
  (say knpc "Do lips join a cup\n"
       "but to drain it dry? My kind\n"
       "to Men join the same."))

(define (angriss-job knpc kpc)
  (say knpc "Hunger."))

(define (angriss-bye knpc kpc)
  (say knpc "The empire of light\n"
       "Calls you back. Ascend the stair\n"
       "if you can reach it.\n"))


(define (angriss-soft knpc kpc)
  (say knpc "To drink the dark wine\n"
       "Men crush the grape. I drink the\n"
       "wine that's crushed from Men."))

(define (angriss-hung knpc kpc)
  (say knpc "A river of blood\n"
       "flows down to my lair, a pile\n"
       "of bones waxes there."))

(define (angriss-men knpc kpc)
  (say knpc "So proud in armor\n"
       "so soft in hidden aspect\n"
       "so sweet in folly."))

(define (angriss-gobl knpc kpc)
  (say knpc "A stealthy hunt, a\n"
       "terrible struggle, in the\n"
       "end the longest rest."))

(define (angriss-trol knpc kpc)
  (say knpc "Stone-thrower and rock\n"
       "beater, your fearsome bellow\n"
       "will become a scream."))

(define (angriss-choose knpc kpc)
  (say knpc "Choose whom you will give to me.")
  (let ((kchar (kern-ui-select-party-member))
        (quest (angriss-quest (kobj-gob-data knpc))))
    (if (null? kchar)
        (begin
          (say knpc "You toy with a monster, now flee.")
          (harm-relations knpc kpc)
          (harm-relations knpc kpc)
          (kern-conv-end))
        (if (is-dead? kchar)
            (begin
              (say knpc "FRESH meat I require!\n"
                   "Poison! Foul, is that dead blood!\n"
                   "Bring me another.")
              (kern-conv-end))
            (begin
              (say knpc "Honor is satisfied.\n")
              (if (not (quest-done? quest))
                  (quest-done! quest #t))
              (kern-char-leave-player kchar)
              (kern-being-set-base-faction kchar faction-none)
              (improve-relations knpc kpc)
              (kern-conv-end))))))


(define (angriss-rune knpc kpc)
  (let ((quest (angriss-quest (kobj-gob-data knpc))))
    (if (quest-done? quest)
        (begin
          (say knpc "The ancient secret,\n"
               "The key to a lock of hell.\n"
               "Take it, it is thine.")
          (kern-obj-remove-from-inventory knpc t_rune_f 1)
          (kern-obj-add-to-inventory kpc t_rune_f 1)
          (rune-basic-quest 'questentry-rune-f s_runestone_f)
         )
        (say knpc "I know what you seek.\n"
             "But I wonder, do you? First,\n"
             "satisfy honor."))))

(define (angriss-sacr knpc kpc)

  (define (player-alone?)
    (< (num-player-party-members) 
       2))

  (let ((quest (angriss-quest (kobj-gob-data knpc))))

    (define (refused)
      (say knpc "Flee from my presence.\n"
           "You may escape my wrath, but\n"
           "You will not return.")
      (harm-relations knpc kpc)
      (harm-relations knpc kpc)
      (kern-conv-end))

    (define (offer-quest)
      (display "offer-quest")(newline)
      (if (player-alone?)
          (begin
            (say knpc "You will find, if you\n"
                 "seek, a fool to join you. Bring\n"
                 "that fool unto me.\n"
                 "...Agreed?")
            (if (kern-conv-get-yes-no? kpc)
                (begin
                  (quest-accepted! quest)
                  (improve-relations knpc kpc)
                  (improve-relations knpc kpc))
                (refused)))
          (begin
            (say knpc "Among your party\n"
                 "Choose one as a sacrifice\n"
                 "and you will go free.\n"
                 "...Agreed?")
            (if (kern-conv-get-yes-no? kpc)
                (angriss-choose knpc kpc)
                (refused)))))
            
    (if (quest-done? quest)
        (say knpc "It is done.")
        (if (quest-accepted? quest)
            (if (player-alone?)
                (say knpc "Alone you come back.\n"
                     "Where is the sacrifice that\n"
                     "will keep you from doom?")
                (choose-victim))
            (offer-quest)))))


(define (angriss-hono knpc kpc)
  (say knpc "I demand honor\n"
       "and sacrifice. Give them to\n"
       "me, or flee, or die."))

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
       (method 'hung angriss-hung)
       (method 'rune angriss-rune)
       (method 'men angriss-men)
       (method 'gobl angriss-gobl)
       (method 'trol angriss-trol)
       (method 'sacr angriss-sacr)
       (method 'hono angriss-hono)
       ))

(define (angriss-ai kchar)
  (if (angriss-spoke? (kobj-gob-data kchar))
      (spider-ai kchar)
      (begin
        (angriss-spoke! (kobj-gob-data kchar))
        (kern-conv-begin kchar))))

(define (mk-angriss)
  (bind 
   (kern-char-force-drop
    (kern-mk-char 
     'ch_angriss         ; tag
     "Angriss"           ; name
     sp_queen_spider     ; species
     nil                 ; occ
     s_purple_spider     ; sprite
     faction-spider ; starting alignment
     20 0 20             ; str/int/dex
     10 5                ; hp mod/mult
     10 5                ; mp mod/mult
     max-health ;;..current hit points
     -1 ;;...........current experience points
     max-health ;;..current magic points
     0
     angriss-lvl
     #f                  ; dead
     'angriss-conv       ; conv
     nil                 ; sched
     'angriss-ai          ; special ai
     
     ;;..........container (and contents)
     (mk-inventory (list (list 1 t_rune_f)))
     nil                 ; readied
     )
    #t)
    (angriss-mk)))
