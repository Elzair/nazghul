;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_chant
               (list 0  0  trigrave-east-west-road   "drunk")
               (list 2  0  trigrave-chants-bed       "sleeping")
               (list 12 0  trigrave-tavern-hall      "working")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (chant-mk) (list 0))
(define (chant-get-gold knpc) (car (kobj-gob-data knpc)))
(define (chant-set-gold! knpc amount) (set-car! (kobj-gob-data knpc) amount))

;; ----------------------------------------------------------------------------
;; Chanticleer
;;
;; Chanticleer is a roaming bard who spends most of his time in Trigrave. He's
;; a confident, easy-going fellow who is also an unstoppable showman. He
;; possesses a great deal of knowledge about the region, loves rumours and
;; gossip, has friends in low and high places (uncluding the reclusive
;; Enchanter) and is probably a spy for *somebody* but it isn't clear who.
;;
;; ----------------------------------------------------------------------------
(define (chant-song knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "All together now! Row, row, row your boat! [Hee hee hee!]")
      (begin
        (say knpc "A thousand songs wait caged behind the bars of my dry "
             "throat! Perhaps a little coin to whet the pipes? [He looks at "
             "you expectantly. Do you give him some gold?]")
        (if (kern-conv-get-yes-no? kpc)
            ;; yes - give chant some gold
            (let ((amount (kern-conv-get-amount)))
              (display "amount=")(display amount)(newline)
              (cond ((= 0 amount) 
                     (say knpc "An imaginary song then, for your imaginary "
                          "gold! [He pretends to play his lute]"))
                    ((< amount 2)
                     (say knpc "Yes... I see. Here goes:\n"
                          "There was a Wanderer oh so cheap\n"
                          "He wanted all his gold to keep\n"
                          "And when he mocked poor Chanticleer\n"
                          "The sound of discord filled his ear!\n"
                          " [He strikes a loud, brash, chord and bows]"))
                    (else
                     (say knpc "That will fill my yearning mug! "
                          "Now, of what would you hear? Of land, legend, "
                          "or loot?")
                     (chant-set-gold! knpc amount))))
            ;; no -- don't give him some gold
            (say knpc "A sober bard doesn't do anybody any good!")))))

(define (chant-land knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "Drier than sea! [Hic]")
      (if (= (chant-get-gold knpc) 0)
          (chant-song knpc kpc)
          (say knpc 
               "Hmm... can't think of anything right now. Sorry."))))

(define (chant-legend knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "Yes, I am! [He belches]")
      (if (= (chant-get-gold knpc) 0)
          (chant-song knpc kpc)
          (say knpc 
               "Hmm... can't think of anything right now. Sorry."))))

(define (chant-loot knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "I drank all my money!")
      (if (= (chant-get-gold knpc) 0)
          (chant-song knpc kpc)
          (say knpc 
               "Hmm... can't think of anything right now. Sorry."))))

(define chant-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (if (isdrunk? knpc)
                     (say knpc "I's drunk... [hic]")
                     (say knpc "Surprisingly, I know nothing about that."))))
       (method 'hail 
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "[Saluting drunkenly] Hail to the cheese!")
                     (say knpc "Well met, my fine fellow!"))))
       (method 'bye 
               (lambda (knpc kpc) 
                 (if (isdrunk? knpc)
                 (say knpc "Buhhhh-bye [snicker]")
                 (say knpc "May the road kiss your feet!"))))
       (method 'job 
               (lambda (knpc kpc) 
                 (if (isdrunk? knpc)
                     (say knpc "[Overdramatically, he points to his eyes, "
                          "then his ears, then makes a zipping motion on his "
                          "mouth, nods sagely and winks]")
                     (say knpc "I am a very vessel of wit and song!"))))
       (method 'name 
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "Chantibard! [He tries to bow but falls down]")
                     (say knpc "Chanticleer the Bard, at your service "
                          "[He bows and flourishes]"))))
       (method 'join 
               (lambda (knpc kpc) 
                 (if (isdrunk? knpc)
                     (say knpc "Join you for a drink? "
                          "I thought you'd never azzk!")
                     (say knpc "Some bards adventure, others are wise. "
                          "I am among the latter."))))

       (method 'chan
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "[Hic!] Whooze that maudlin, drunken fool,\n"
                          "Who plaize the tung like beast made man...")
                     (say knpc
                          "\n"
                          " Who is that wise, enchanting bard,\n"
                          " Who plays the lute like song made flesh\n"
                          " And tells tales Fate herself would tell\n"
                          " If Fate could only lie so well?"))))
       (method 'ench
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "Enchy! Sits in his comfy tower, giving "
                          "orders, while Chant has to make the rounds!")
                     (say knpc 
                          "My travels sometimes take me to the "
                          "Enchanters Tower."))))
       (method 'gwen
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "[He leans in close and whispers loudly] "
                          "Ever seen a witch carry a sword?")
                     (say knpc
                          "Ah, our mysterious inkeeper...\n"
                          " When the gray dove is weeping\n"
                          " And the whole world is sleeping\n"
                          " When ghosts rise like mist from the sea\n"
                          " The owl in the moonlight\n"
                          " Inquires of the still night\n"
                          " The question we all ask of she!\n"))))
       (method 'land chant-land)
       (method 'lege chant-legend)
       (method 'loot chant-loot)
       (method 'jim
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "A knight turned inside out! [Burp!]")
                     (say knpc
                          "Ha! Here's a bit of gossip!\n"
                          "Full fell and grim\n"
                          " the righteous Jim,\n"
                          " His armour slicked in gore,\n"
                          " Slew his master on the field,\n"
                          " Scratched the emblem from his shield\n"
                          " And fled that cursed shore."
                          ))))
       (method 'roun
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "Azzright! I watches the roads, "
                          "watching for them...")
                     (say knpc "What a strange thing to inquire about!"))))
       (method 'song chant-song)
       (method 'them
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "[Hissing] THEM! The Enemy of the Wise! Shhh!")
                     (say knpc "[Looking at you with mock suspicion] "
                          "Are you sure you're not paranoid?"))))
       (method 'towe
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "Deprezzing place!")
                     (say knpc
                          "It watches the Fen to the north, "
                          "but if you're thinking of going there, "
                          "know that the Enchanter does not like visitors!"))))
       (method 'wit
               (lambda (knpc kpc)
                 (say knpc "Anything worth taking seriously is worth making "
                      "fun of.")))
       ))
