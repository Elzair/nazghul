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
               (list 23 0  trigrave-east-west-road   "drunk")               
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (chant-mk) (list 0))
(define (chant-get-gold knpc) (car (kobj-gob-data knpc)))
(define (chant-has-gold? knpc) (> (chant-get-gold knpc) 0))
(define (chant-set-gold! knpc amount) 
  (if (>= amount 0)
      (set-car! (kobj-gob-data knpc) 
                amount)))
(define (chant-dec-gold! knpc) (chant-set-gold! knpc 
                                                (- (chant-get-gold knpc)
                                                   1)))

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
                          "Now, of what would you hear? Of Fen, Forest "
                          "or Forgotten places?")
                     (chant-set-gold! knpc amount))))
            ;; no -- don't give him some gold
            (say knpc "A sober bard doesn't do anybody any good!")))))

(define (chant-fen knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "Nazzty place! [Hic]")
      (if (not (chant-has-gold? knpc))
          (chant-song knpc kpc)
          (begin
            (chant-dec-gold! knpc)
            (say knpc 
                 "THE FENS\n"
                 "\n"
                 "  If you like...\n"
                 "  biting flies, and farting bogs,\n"
                 "  dismal skies, man-sized frogs,\n"
                 "  evil altars, muck-filled boots,\n"
                 "  mislaid rangers (wary of strangers),\n"
                 "  trackless wastes,\n"
                 "  lichs, wraiths,\n"
                 "  and skeletal warriors, too...\n"
                 "  then, my friend,\n"
                 "  to the northern Fen!\n"
                 "  For that's the place for you.\n")))))

(define (chant-forest knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "Run, Forest, run! [Hee-hee]")
      (if (not (chant-has-gold? knpc))
          (chant-song knpc kpc)
          (begin
            (chant-dec-gold! knpc)
            (say knpc 
                 "THE EASTERN WOODS\n"
                 "\n"
                 "The woods are lovely, dark and deep\n"
                 "and always hungry, too!\n"
                 "They've eaten many travelers\n"
                 "and gulped a king or two.\n"
                 "\n"
                 "There goblins lurk and bandits hide\n"
                 "and giant spiders drool,\n"
                 "So if you go there don't forget\n"
                 "to bring a slower fool!\n"
                 )))))

(define (chant-forgotten knpc kpc)
  (if (isdrunk? knpc)
      (say knpc "I don't remember! [he weeps with laughter]")
      (if (not (chant-has-gold? knpc))
          (chant-song knpc kpc)
          (begin
            (chant-dec-gold! knpc)
            (say knpc 
                 "LOST HALLS\n"
                 "\n"
                 "Delving ever deeper,\n"
                 "They woke the ancient sleeper,\n"
                 "You know the tale\n"
                 "(The moral's stale)\n"
                 "So I won't bore you with detail!\n"
                 "\n"
                 "[stops playing] If you simply MUST go investigate, "
                 "search the southern coast.")
		 (if (null? (quest-data-getvalue 'questentry-rune-l 'know-hall))
			(quest-data-update-with 'questentry-rune-l 'approx-hall 1 (quest-notify nil))
		)
		 ))))

(define (chant-thie knpc kpc)
  (if (isdrunk? knpc)
      (say knpc 
           "Hey diddle riddle,\n"
           "You're in the middle!\n"
           "When Kalcifax jumped through moon\n"
           "The Necromage laughed to see such sport\n"
           "And the Thief ran away with the Rune!")
      (say knpc "So Enchy has you looking too? The thief must have avoided this "
           "town, but travelers may have crossed his path. Ask Gwen.")))

(define (chant-man knpc kpc)
  (if (isdrunk? knpc)
      (begin
        (say knpc "HEY! Can you keep a zecret?")
        (if (yes? knpc)
            (begin
              (say knpc "The MAN haz a zecret cave. You know where it iz?")
              (if (yes? kpc)
                  (say knpc "Me too! [Hee-hee!]")
                  (say knpc "The mountain at [" 
                       (loc-x the-mans-hideout-loc) " " 
                       (loc-y the-mans-hideout-loc) 
                       "] izn't really a mountain! Go zurprize him!")))
            (say knpc "Me neither! [He snorts]")))
      (say knpc "The MAN? I don't know anything about the MAN. Why are you asking me?")))

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
                     (say knpc "I am at a loss."))))
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
                     (say knpc "\n"
                          "\n"
                          " Who is that wise, enchanting bard,\n"
                          " Who plays the lute like song made flesh\n"
                          " And tells tales Fate herself would tell\n"
                          " If Fate could only lie so well?"))))
       (method 'earl
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "e's furgotten mor' 'an I'll ever know!")
                     (say knpc 
                          "Our distinguished shopkeeper...\n"
                          "\n"
                          " There once was a master of flame\n"
                          " As a warmage he gathered great fame\n"
                          " But as you can tell\n"
                          " He tried a bad spell\n"
                          " And now can't remember his name!\n"
                          ))))

       (method 'ench
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "Enchy! Sits in his comfy tower, giving "
                          "orders, while Chant has to make the rounds!")
                     (say knpc 
                          "My travels sometimes take me to the "
                          "Enchanter's Tower."))))
       (method 'gwen
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "[He leans in close and whispers loudly] "
                          "Ever seen a witch carry a sword?")
                     (say knpc
                          "Ah, our mysterious innkeeper...\n"
                          "\n"
                          " When the gray dove is weeping\n"
                          " And the whole world is sleeping\n"
                          " When ghosts rise like mist from the sea\n"
                          " The owl in the moonlight\n"
                          " Inquires of the still night\n"
                          " The question we all ask of she!\n"))))
       (method 'fen chant-fen)
       (method 'fore chant-forest)
       (method 'forg chant-forgotten)
       (method 'jim
               (lambda (knpc kpc)
                 (if (isdrunk? knpc)
                     (say knpc "A knight turned inside out! [Burp!]")
                     (say knpc
                          "Ha! Here's a bit of gossip!\n"
                          "\n"
                          " Full fell and grim\n"
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
       (method 'thie chant-thie)
       
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
                 (say knpc "Well, gossip really. Give me a name, I'll give "
                      "you the dirt.")))
       (method 'lost
               (lambda (knpc kpc)
                 (say knpc "Well, I wouldn't be a bard if I didn't know a song about the fabled Lost Halls!")))
       (method 'man chant-man)
       (method 'wrog chant-man)
       ))
