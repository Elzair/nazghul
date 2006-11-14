;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define anaxes-lvl 5)
(define anaxes-species sp_ghast)
(define anaxes-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (anaxes-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (anaxes-hail knpc kpc)
  (meet "[You meet the proud shade of a long-dead wizard]")
  (say knpc "Art thou the agent of Luximene?")
  (if (yes? kpc)
      (say knpc "But who else? [He chuckles grimly] "
           "Thou hast broken through the defenses, "
           "but thy foul master will not find me toothless!")
      (say knpc "Deny it not, I know he seeks to slay me for my rebellion!"))
  (aside kpc 'ch_nate 
         "[Whispering] Milord, Luximene has been dead for ages!")
  )


(define (anaxes-default knpc kpc)
  (say knpc "[He silently regards you]"))

(define (anaxes-name knpc kpc)
  (say knpc "I am Anaxes, formerly of the Twelve."))

(define (anaxes-luxi knpc kpc)
  (say knpc "Luximene is not a man, but a demon in a mask! "
       "He hath sown dissent and conquered in its wake, "
       "and now seeks to dethrone the gods.")
  (prompt-for-key)
  (say knpc "He hath ordered his own image to be erected in the shrines. "
       "This is too much! Now his perfidy is made plain! ")
  (prompt-for-key)
  (say knpc "So long as I draw breath, the Shrine of Brune will not be defiled!")
  (aside kpc 'ch_nate "Ah! The name of the god at last! "
         "I feared it was forgotten forever!")
  )

(define (anaxes-gods knpc kpc)
  (say knpc "The gods will take a terrible vengeance on the Shard for its "
       "faithlessness! Luximene and his followers will be consumed by fire and "
       "drowned in blood at the last battle! "
       "Vigilance to the faithful!"))

(define (anaxes-vigi knpc kpc)
  (say knpc "This is the Shrine of Brune, god of vigilance. "
       "I rebelled against Luximene's order to disgrace it with his image. "
       "I ordered the entrace to Brune-Guard sealed, and awaited the siege. ")
  )

(define (anaxes-twel knpc kpc)
  (say knpc "I was among the Twelve Counselors of Luximene. "
       "To my shame I helped him achieve his empire. "
       "I thought a strong ruler would bring order to the Shard, "
       "and reign in the heresies of the Accursed. "
       "Alas! We were all deceived!"))

(define (anaxes-accu knpc kpc)
  (say knpc "In the years that followed the Sundering, "
       "the priesthood became corrupted. "
       "All manner of wretched men claimed to speak on behalf of the gods, "
       "polluting the worship of the gods with their foul practices. ")
  (prompt-for-key)
  (say knpc "Under Luximene we waged war on the heretics. "
       "We burned the false priests at the stake and put their "
       "verminous followers to the sword. "))

(define (anaxes-bye knpc kpc)
  (say knpc "On thy guard, defiler of the holy! I shall resist you; "
       "and should I fall in battle, I will strike from beyond the grave!")
  (aside kpc 'ch_nate "[Muttering] Methinks he is ahead of schedule.")
  (kern-being-set-base-faction knpc faction-monster)
  )

(define (anaxes-job knpc kpc)
  (say knpc "I was one of Luximene's Twelve, trusted with the command of "
       "this fortress and its watch-tower. "
       "But Luximene has ordered the desecration of the shrines, "
       "and I have rebelled!")
  (aside kpc 'ch_nate 
         "[Whispering] This lich thinks himself a wizard living "
         "in the time of Luximene. Can it be that he once was?")
  )

(define (anaxes-fort knpc kpc)
  (say knpc "This fortress is Brune-Guard, so-called for this Shrine of Brune, "
       "and for the vigilance of the tower. It is virtually impregnable to siege."))

(define (anaxes-towe knpc kpc)
  (say knpc "The watch-tower of Brune-Guard looks far over the sea, the forest, "
       "the mountains, and deep into the sky. From time beyond memory, "
       "from before the Sundering, the vigilant have kept lookout here for "
       "danger below or signs from above."))

(define (anaxes-sund knpc kpc)
  (say knpc "The Sundering is the catclysm that nearly destroyed this world. "
       "Surely thou dost know this! "
       "Unless thou art a demon from beyond the void..."))

(define anaxes-conv
  (ifc basic-conv

       ;; basics
       (method 'accu anaxes-accu)
       (method 'assa anaxes-luxi)
       (method 'bye anaxes-bye)
       (method 'brun anaxes-vigi)
       (method 'default anaxes-default)
       (method 'defe anaxes-fort)
       (method 'fait anaxes-vigi)
       (method 'fort anaxes-fort)
       (method 'god  anaxes-gods)
       (method 'gods anaxes-gods)
       (method 'hail anaxes-hail)
       (method 'job  anaxes-job)
       (method 'luxi anaxes-luxi)
       (method 'mast anaxes-luxi)
       (method 'name anaxes-name)
       (method 'rebe anaxes-luxi)
       (method 'shri anaxes-vigi)
       (method 'sund anaxes-sund)
       (method 'towe anaxes-towe)
       (method 'twel anaxes-twel)
       (method 'vigi anaxes-vigi)

       ))

(define (mk-anaxes)
  (bind 
   (kern-mk-char 
    'ch_lux           ; tag
    "Anaxes"             ; name
    anaxes-species         ; species
    anaxes-occ              ; occ
    s_lich     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    anaxes-lvl
    #f               ; dead
    'anaxes-conv         ; conv
    nil           ; sched
    'lich-ai         ; special ai
    nil              ; container
    nil              ; readied
    )
   (anaxes-mk)
   ))