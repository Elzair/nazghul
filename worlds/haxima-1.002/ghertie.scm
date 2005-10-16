;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define trigrave-inn-room-price 20)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ghertie
               (list 0  0  cheerful-room-3      "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ghertie-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (ghertie-hail knpc kpc)
  (say knpc "[You meet the ghost of a wild-looking woman] "
       "You dare disturb me? Be wary, I am in a black mood."))

(define (ghertie-default knpc kpc)
  (say knpc "I care not for this line of talk."))

(define (ghertie-name knpc kpc)
  (say knpc "I am Ghertrude."))

(define (ghertie-join knpc kpc)
  (say knpc "I am anchored here for eternity."))

(define (ghertie-job knpc kpc)
  (say knpc "I was a pirate when I was alive. Now I haunt this room."))

(define (ghertie-bye knpc kpc)
  (say knpc "Trust not your crew!"))

;; Pirate...
(define (ghertie-pira knpc kpc)
  (say knpc "I plunderd this coastline for years and amassed a fortune. "
       "Then, on the eve of my retirement, my crew betrayed me. "
       "They were like sons to me, the wretches!"))

(define (ghertie-betr knpc kpc)
  (say knpc "The cowards slew me in my sleep and stole my ship."))

(define (ghertie-ship knpc kpc)
  (say knpc "The Merciful Death was fast, limber and mean! "
       "I could not wish for a finer ship. My treasure was nothing, "
       "my life was doomed, but for stealing my ship I can never "
       "forgive my crew!"))

(define (ghertie-haun knpc kpc)
  (say knpc "I am bound to this spot. My first mate knew I would not rest "
       "in my grave, and with a curse he fixed me here so I could not "
       "pursue them with my vengeance! Ha! He did not know "
       "about the curse upon my ship!"))

(define (ghertie-curs knpc kpc)
  (say knpc "I had a curse put upon my ship. Should it ever be stolen it "
       "would steer itself to the Misty Isles and sink! Only I know the place "
       "of its watery grave!"))

(define (ghertie-grav knpc kpc)
  (say knpc "Why should I tell you?"))

(define (ghertie-reve knpc kpc)
  (say knpc "[She fixes you with an icy glare] You speak the word dearest to "
       "my dead heart. Are you offering to avenge me upon my crew?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (say knpc "Slay any survivors, and I will tell you where my ship "
             "lies. Do we have an oath?")
        (if (kern-conv-get-yes-no? kpc)
            (say knpc "Agreed. I have met the spirits of those who died when "
                 "the ship sank, but I have not seen the spirits of those who "
                 "live. They are Ungfried, Jovis and Shearn. Slay them, "
                 "and I will tell you where my ship lies.")
            (say knpc "It is wise you do not take such an oath lightly, for I "
                 "await oath-breakers on this side of the divide.")))
      (begin
        (say knpc "Then do not toy with me, fool!")
        (kern-conv-end))))

(define (ghertie-fort knpc kpc)
  (say knpc "Gold, gems, magical items, weapons armor and runes."))

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
                 30 0 0 6            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'ghertie-conv         ; conv
                 sch_ghertie           ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (ghertie-mk)))
