;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ghertie
               (list 0  0  cheerful-room-3      "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ghertie-mk) 
  (list (mk-quest)))

(define (ghertie-quest gob) (car gob))



;;----------------------------------------------------------------------------
;; Conv
;; 
;; Ghertie is the vengeful ghost of a murdered female pirate captain.
;; She haunts an inn room in Oparine.
;;----------------------------------------------------------------------------

;; Quest...
(define (ghertie-give-instr knpc kpc)
  (say knpc "Each member of my crew wears a "
       "cursed ring, with a skull for a signet. It cannot be "
       "removed without the finger. Jorn, Gholet and Meaney still "
       "live. Bring me their rings to fulfil your part of the "
       "bargain, and I will then fulfill mine.")
	(quest-data-update-with 'questentry-ghertie 'questinfo 1 (quest-notify nil))
)

(define (ghertie-update-quest knpc kpc)
  (let ((nrem (- 3 (num-in-inventory kpc t_skull_ring))))
    (if (= nrem 0)
        (begin
          (say knpc "I am avenged! Now I can rest... the Merciful "
               "Death lies at [" merciful-death-x ", " merciful-death-y
               "] by the sextant. But how you will pillage her when "
               "she lies at the bottom of the sea is your problem! "
               "[She vanishes with a cruel laugh]")
			(quest-data-update-with 'questentry-rune-c 'shiploc 1 (quest-notify nil))
			(quest-data-assign-once 'questentry-ghertie)
			(quest-data-update-with 'questentry-ghertie 'done 1 (grant-party-xp-fn 20))
			 (kern-conv-end)
          (kern-obj-remove knpc)
          (kern-map-set-dirty))
        (begin
          (say knpc "You still have " nrem " rings to collect. "
               "Have you forgotten my instructions?")
           (if (kern-conv-get-yes-no? kpc)
               (begin
                 (say knpc "If you were one of my crew I would have you "
                      "flogged for your carelessness!")
                 (ghertie-give-instr knpc kpc))
               (say knpc "Then why have you returned empty-handed? "
                    "If you fail in your oath I will flog your soul with my "
                    "own hand!"))))))

;; Basics...
(define (ghertie-hail knpc kpc)
  (let ((quest (ghertie-quest (kobj-gob-data knpc))))
		(quest-data-update 'questentry-ghertie 'ghertieloc 1)
		(quest-data-assign-once 'questentry-ghertie)
    (display "quest:")(display quest)(newline)
    (if (quest-accepted? quest)
        (ghertie-update-quest knpc kpc)        
        (say knpc "[You meet the ghost of a wild-looking woman] "
             "You dare disturb me? Be wary, I am in a black mood."))))

(define (ghertie-default knpc kpc)
  (say knpc "I care not for this line of talk."))

(define (ghertie-name knpc kpc)
	(quest-data-update 'questentry-ghertie 'ghertieid 1)
  (say knpc "I am Ghertrude."))

(define (ghertie-join knpc kpc)
  (say knpc "I am anchored here for eternity."))

(define (ghertie-job knpc kpc)
  (say knpc "I was a pirate when I was alive. Now I haunt this room."))

(define (ghertie-bye knpc kpc)
  (if (quest-accepted? (ghertie-quest (kobj-gob-data knpc)))
      (say knpc "Avenge me without delay!")
      (say knpc "Trust not your crew!")))

;; Pirate...
(define (ghertie-pira knpc kpc)
  (say knpc "I plunderd this coastline for years and amassed a fortune. "
       "Then, on the eve of my retirement, my crew betrayed me. "
       "They were like sons to me, the wretches!"))

(define (ghertie-betr knpc kpc)
  (say knpc "The cowards slew me in my sleep and stole my ship."))

(define (ghertie-ship knpc kpc)
  (say knpc "The Merciful Death was fast, limber and mean. "
       "I could not wish for a finer ship. My treasure was nothing, "
       "my life was doomed, but for stealing my ship I will never "
       "forgive my crew!"))

(define (ghertie-haun knpc kpc)
  (say knpc "I am bound to this spot. My first mate knew I would not rest "
       "in my grave, and with a curse he fixed me here so I could not "
       "pursue them with my vengeance! Ha! He did not know "
       "about the curse upon my ship!"))

(define (ghertie-curs knpc kpc)
  (say knpc "I had a curse put upon my ship. Should it ever be stolen it "
       "would steer itself to a particular place and sink! I alone know "
       "of its watery grave..."))

(define (ghertie-grav knpc kpc)
  (say knpc "Why should I tell you?"))

(define (ghertie-reve knpc kpc)
  (let ((quest (ghertie-quest (kobj-gob-data knpc))))
    (if (quest-accepted? quest)
        (say knpc "Yes, you have sworn to avenge me, "
             "why are you prattling?")
        (begin
          (say knpc "[She fixes you with an icy glare] "
               "You speak the word dearest to my dead heart. "
               "Are you offering to avenge me?")
	(quest-data-update 'questentry-ghertie 'revenge 1)
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (say knpc "Not all of my crew went down with the ship. "
                     "I have searched among the dead and found some missing. "
                     "Find and punish the survivors, "
                     "and I will tell you where my ship lies. "
                     "Do we have an oath?")
                (if (kern-conv-get-yes-no? kpc)
                    (begin
                      (say knpc "Agreed then, mortal. ")
                      (quest-accepted! quest #t)
                      (ghertie-give-instr knpc kpc))
                    (say knpc "It is wise you do not take such an oath "
                         "lightly, for I await oath-breakers on this side "
                         "of the divide.")))
              (begin
                (say knpc "Then do not toy with me, fool!")
                (kern-conv-end)))))))

(define (ghertie-fort knpc kpc)
  (say knpc "Gold, gems, magical items, weapons armor and runes.")
	(quest-data-assign-once 'questentry-rune-c)
	)

(define ghertie-conv
  (ifc basic-conv

       ;; basics
       (method 'default ghertie-default)
       (method 'hail ghertie-hail)
       (method 'bye ghertie-bye)
       (method 'job ghertie-job)
       (method 'name ghertie-name)
       (method 'join ghertie-join)
       
       ;; special
       (method 'pira ghertie-pira)
       (method 'betr ghertie-betr)
       (method 'crew ghertie-betr)
       (method 'ship ghertie-ship)
       (method 'haun ghertie-haun)
       (method 'curs ghertie-curs)
       (method 'grav ghertie-grav)
       (method 'fort ghertie-fort)
       (method 'reve ghertie-reve)

       ))

(define (mk-ghertie)
  (bind 
   (kern-mk-char 'ch_ghertie           ; tag
                 "Ghertie"             ; name
                 sp_ghast            ; species
                 oc_warrior                 ; occ
                 s_ghost               ; sprite
                 faction-men         ; starting alignment
                 0 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 6  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'ghertie-conv         ; conv
                 sch_ghertie           ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (ghertie-mk)))
