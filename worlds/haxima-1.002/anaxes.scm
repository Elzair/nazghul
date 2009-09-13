;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define anaxes-lvl 6)
(define anaxes-species sp_lich)
(define anaxes-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; No schedule (Cave Shrine within Brundegart).
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (anaxes-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Anaxes is the lich/shade of a long-dead wizard, who once served 
;; Luximene and later rebelled against him.
;; Anaxes abides in the Cave Shrine within the lost realm of Brundegart.
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
  (aside kpc 'ch_nate "Ah! The name of the god at last! I feared it was forgotten forever!")
  (cond ((has? kpc t_lich_skull 1)
         (say knpc "Wait! What have you there? [He points to Luximene's skull] What doth this mean? Is Luximene then dead?")
         (yes? kpc)
         (say knpc "[He ignores you. The light in his eyes begins to fade and his voice grows weak] It is over...")
         (prompt-for-key kpc)
         (say knpc "[He collapses in a heap] ...Isin! ...Isin...")
         (aside kpc 'ch_nate "Dibs on his boots.")
         (kern-conv-end)
         (kern-char-kill knpc))))

(define (anaxes-gods knpc kpc)
  (say knpc "The gods will take a terrible vengeance on the Shard for its "
       "faithlessness! Luximene and his followers will be consumed by fire and "
       "drowned in blood at the last battle! "
       "Vigilance to the faithful!"))

(define (anaxes-brun knpc kpc)
  (say knpc "This is the Shrine of Brune, god of vigilance. "
       "I rebelled against Luximene's order to disgrace it with his image. "
       "I ordered the entrace to Brune-Guard sealed, and awaited the siege. ")
  )

(define (anaxes-vigi knpc kpc)
  (say knpc "We have failed in our vigilance! "
       "Luximene has deceived us all, and now we must pay the price for failure."))

(define (anaxes-fail knpc kpc)
  (say knpc "We have failed the gods, it is no wonder they have forsaken us!"))

(define (anaxes-twel knpc kpc)
  (say knpc "I was among the Twelve Counselors of Luximene. "
       "To my shame I helped him achieve his empire. "
       "I thought a strong ruler would bring order to the Shard, "
       "and rein in the heresies of the Accursed. "
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
         "[Whispering] This lich thinks himself a wizard living in the time of Luximene. Can it be that he once was?")
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
  (say knpc "The Sundering is the cataclysm that nearly destroyed this world. "
       "Surely thou dost know this! "
       "Unless thou art a demon from beyond the void..."))

(define anaxes-conv
  (ifc nil

       ;; basics
       (method 'accu anaxes-accu)
       (method 'assa anaxes-luxi)
       (method 'bye anaxes-bye)
       (method 'brun anaxes-brun)
       (method 'default anaxes-default)
       (method 'defe anaxes-fort)
       (method 'fail anaxes-fail)
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
  (let ((kchar
         (bind 
          (kern-char-force-drop
           (kern-mk-char 
            'ch_lux          ; tag
            "Anaxes"         ; name
            anaxes-species   ; species
            anaxes-occ       ; occ
            s_lich           ; sprite
            faction-men      ; starting alignment
            0 0 0            ; str/int/dex
            0 0              ; hp mod/mult
            0 0              ; mp mod/mult
            max-health       ; hp
            -1               ; xp
            max-health       ; mp
            0
            anaxes-lvl       ; level
            #f               ; dead
            'anaxes-conv     ; conv
            nil              ; sched
            'lich-ai         ; special ai
            (mk-inventory
             ;; hack: as the kernel is currently written, he won't drop his
             ;; readied arms on death, and he won't ready arms from inventory
             ;; (its all messed up), but he will drop his inventory. So put
             ;; some decent arms in as loot.
             (list (list 1 t_armor_chain)
                   (list 1 t_chain_coif)
                   (list 1 t_morning_star)
                   (list 1 t_shield)
                   (list 3 mandrake)
                   (list 3 nightshade)
                   (list 8 sulphorous_ash)
                   (list 5 blood_moss)
                   (list 5 black_pearl)
                   (list 50 t_gold_coins)
                   (list 1 t_anaxes_letter)
                   (list 1 t_lichs_blood)
                   ))
            ;; readied
            (list
             t_armor_chain_4
             t_chain_coif_4
             t_morning_star_2
             t_shield_4
             )
            ) ; kern-mk-char
           #t) ; kern-char-force-drop
          (anaxes-mk)) ; bind
         ))
    (map (lambda (eff) (kern-obj-add-effect kchar eff nil))
         undead-effects)
    kchar))
