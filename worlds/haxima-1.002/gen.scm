;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Green Tower.
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_green_tower x y w h))
(kern-mk-sched 'sch_gen
               (list 0  0  (mk-zone 2  13 1  1)  "sleeping")
               (list 4  0  (mk-zone 3  12 3  3)  "eating")
               (list 5  0  gt-woods  "idle")
               (list 10 0  (mk-zone 26 27 2  12) "idle")
               (list 12 0  (mk-zone 49 54 1  1)  "eating")
               (list 13 0  (mk-zone 49 3  7  2)  "idle")
               (list 14 0  (mk-zone 7  20 5  5)  "idle")
               (list 18 0  (mk-zone 49 54 1  1)  "eating")
               (list 19 0  (mk-zone 3  12 3  3)  "idle")
               (list 0  0  (mk-zone 2  13 1  1)  "sleeping")
               )

;; ----------------------------------------------------------------------------
;; Gen's Goblin Lexicon
;; ----------------------------------------------------------------------------
(mk-reusable-item 
 't_goblin_lexicon "Goblin Lexicon" s_lexicon norm
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "Goblin Lexicon"
   "I compiled these notes to help others learn the goblin language. "
   "I hope they are useful."
   "--Gen"
   ""
   "Bo.....My, Myself"
   "Cho....Mankind"
   "Da.....Abode, World"
   "Eh.....'What?'"
   "Gu.....Spirit, Ancestor"
   "Ha.....Good, Yes, Skillful"
   "Hi.....Magic"
   "Iki....Go, At"
   "Jo.....Join"
   "Ka.....Kill, Destroy, End"
   "Ki.....Health, Life-Force, Power"
   "Lu.....Change, Metamorphosis, Transformation"
   "Ma.....Forest, Hidden Ways"
   "Me.....Duty, Job, Destiny"
   "Na.....Yours, Yourself"
   "Nu.....Give Birth, Create, Begin"
   "No.....Name"
   "Nin....Stealth"
   "Ru.....Ancient, Primordal, Deep, Cave"
   "To.....Individual"
   "Tu.....No, Bad"
   "Zu.....Watch, Seek"
   )))

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (gen-mk will-join? gave-notes?) (list will-join? gave-notes?))
(define (gen-will-join? gen) (car gen))
(define (gen-gave-notes? gen) (cadr gen))
(define (gen-set-will-join! gen val) (set-car! gen val))
(define (gen-set-gave-notes! gen val) (set-car! (cdr gen) val))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Gen is a Ranger who dwells in Green Tower.
;; He has much knowledge of Goblin kind, and is a friend of Kama.
;; Gen is a potential party member.
;;----------------------------------------------------------------------------
(define (gen-hail     gen player) (say gen "Hail, Wanderer"))
(define (gen-bye      gen player) (say gen "Farewell"))
(define (gen-default  gen player) (say gen "I can't help you with that"))
(define (gen-name     gen player) (say gen "I am Gen." ))
(define (gen-woodsman gen player) (say gen "Yes, some call me the Woodsman." ))
(define (gen-job      gen player) (say gen "Once I was a Ranger, but my duty now is done. I wander 'midst the woods for my own reasons." ))
(define (gen-reasons  gen player) (say gen "My reasons are my own." ))

(define (gen-captain gen player) 
  (say gen "Captain Deric commands the Rangers of Green Tower. Have you met him?")
  (if (kern-conv-get-yes-no? player)
      (say gen "A decent man, if somewhat ambitious.")
      (say gen "You can find him in the Tower. His office is on the second floor.")))

(define (gen-ambitious   gen player) (say gen "In peace there's nothing so becomes a man as modest stillness and humility." ))
(define (gen-culture     gen player) 
  (say gen "Though a culture their own, truly a culture they have, (unlike the trolls and headless)."
       "The truths of culture can be seen in Shakespeare, truths which hold across the gulfs between different folk."))
(define (gen-shakespeare gen player)
  (say gen "You know of him! Very good! Some interesting conversation at last.")
  (if (in-player-party? 'ch_kama)
      (say gen player "[He points at Kama] Another who knows of the Bard! You should hear his telling of Hamlet!")
      ))

(define (gen-ranger gen player) (say gen "Rangers fought in these woods during the Goblin Wars. Now they maintain a token presence."))
(define (gen-wars   gen player) (say gen "Yes, I fought as a Ranger in the goblin wars. That was a generation ago, and people forget. "
				     "They see the goblins as lesser beings, defeated and worthy of slow extinction."))
(define (gen-goblin gen player) (say gen "An interesting species. Although they have their own language they have no writing. "
				     "They are much like men, but more savage, more primal. "
				     "Their warriors are beserkers, their shamans are ecstatic mystics."))
(define (gen-primal gen player) (say gen "You can tell I admire them? But in the wars I fought them, not understanding what they were. "
				     "I have friends among the wild forest goblins, now. The cave goblins, though, they are another story..." ))
(define (gen-cave   gen player) (say gen "The cave goblins, who are larger and stronger than their forest cousins, prefer to live in the deeps of the world. "
				     "Their dark god demands living sacrifices. Beware them if you explore the caves, they burn with hatred for humankind." ))

(define (gen-language kgen player)
  (let ((gen (kobj-gob-data kgen)))
    (say kgen "Yes, I can speak a few words of goblin. Would you like to learn?")
    (if (kern-conv-get-yes-no? player)
        (if (gen-gave-notes? gen)
            (say kgen "Study the notes I gave you, and then practice on me.")
            (begin
              (say kgen "Here are some notes I have made on their language. You may keep it. Feel free to practice with me.")
              (kern-obj-add-to-inventory player t_goblin_lexicon 1)
              (gen-set-gave-notes! gen #t)))
        (say kgen "Perhaps another time."))))


(define (gen-practice gen player) (say gen "If you want to practice speaking goblin, just ask me something in goblin!" ))

(define (gen-join gen player)
  (if (gen-will-join? (kobj-gob-data gen))
      (begin
        (say gen "Yes, I will join you. "
             "I'll need my equipment from my chest, "
             "so let's go to my hut in the west part of town. "
             "Once more unto the breach, dear friends!")
             (join-player gen))
      (say gen "No, for the woods call my name.")))

;; SAM: Added a few words from the Lexicon which were not defined as responses.
;;      These were (Iki, Lu, Nin)
;;      Also enhanced a few responses such as for (Eh).
;; Added responses having to do with the concepts of Wanderer, Warrior, Wizard, Rogue, Wright.
;; A bit of organization/tidying may still be wanted, to make sure there are no loose ends .

(define (gen-da  gen player) (say gen "Ha! Da-Ma-To means forest goblin." ))
(define (gen-gu  gen player) (say gen "Ha! Da-Gu means world." ))
(define (gen-ru  gen player) (say gen "Ha! Da-Ru-To means cave goblin." ))
(define (gen-no  gen player) (say gen "Bo-No-Gen. But the goblins call me Ma-Zu-To." ))
(define (gen-ki  gen player) (say gen "Bo-Ha-Ki! I am healthy." ))
(define (gen-jo  gen player) (say gen "Very good! If you befriend a goblin he may join you in your adventures."))
(define (gen-cho gen player) (say gen "Ha! Cho-To means 'a man'." ))
(define (gen-nu  gen player) (say gen "Ha! Nu-Ki is the goblin word for 'food'" ))
(define (gen-ha  gen player) (say gen "Yes, Ha is a general affirmative term." ))
(define (gen-tu  gen player) (say gen "Right, Tu is a general negative term." ))
(define (gen-bo  gen player) (say gen "Yes, Bo-Gu means your spirit self, which is your altar ego in the spirit world." ))
(define (gen-na  gen player) (say gen "Yes, Bo-Na means 'us', or 'tribe'. Bo-Na-Ma refers to forest goblins in general." ))
(define (gen-to  gen player) (say gen "Right, To is a general suffix meaning person." ))
(define (gen-ma  gen player) (say gen "Yes, and Ka-Ma-To is their term for lumberjack." ))
(define (gen-eh  gen player) (say gen "Eh?  Ah yes, Eh-Na-Me means what is your job, or duty." ))
(define (gen-iki gen player) (say gen "Ha! Bo-Iki-Da means 'I go home'."))

(define (gen-me  gen player) (say gen "Bo-Ma-Zu. I watch the forest, or I seek the hidden ways.  Me-Lu-Ki is to grow, change, learn, explore.  Such is the path of the Wanderer!"))
(define (gen-ka  gen player) (say gen "Ha! Ka-Ha-To means warrior.  And Me-Ka-Ha is the path of the Warrior!"))
(define (gen-hi  gen player) (say gen "Ha! Hi-Ma-To is the word for 'shaman'.  And Me-Ha-Zu-Ru is the path of the Wizard!"))
(define (gen-nin gen player) (say gen "Ha! Nin-Ma-To is a Forest Stalker.  And Me-Ha-Nin-Zu is the path of the Rogue!"))
(define (gen-lu  gen player) (say gen "Ha! Lu-Da-To is one who makes.  And Me-Ha-Lu-Da is the path of the Wright!"))

(define (gen-zu       gen player) (say gen "Good! And Zu-To means seeker, or Wanderer.  [He looks at you percingly.]  Iki Meluki?"))
(define (gen-meluki   gen player) (say gen "So, you are a seeker indeed.  I would join you, if you can master Gunodama"))
(define (gen-gunodama gen player) (say gen "The names given by the ancestor spirits, to those who abide in the forest.  In other words, the language of the Forest Goblins."))

(define (gen-nuki knpc kpc)
  (say knpc "That's goblinese for 'food'."))

(define (gen-bonaha gen player) 
  (say gen "Excellent! That is the goblin word for friend. You have come far in mastering their language.")
  (gen-set-will-join! (kobj-gob-data gen) #t))

(define (gen-shroom gen player) (say gen "She is an old friend. Can you believe she was a war-maiden in the Goblin Wars?"))
(define (gen-maiden gen player) (say gen "It's true! I can still remember her hand-axe flashing in the moonlight "
                                   "as she hacked her way through goblin war parties, chanting a battle-ward "
                                   "with ragged breath! She was a sight to see."))

(define (gen-thie knpc kpc)
  (say knpc "I've seen no one odd around here, but the goblins who live in "
       "the north wood recently saw a lone man traveling northeast toward "
       "Bole.")
       (quest-data-update 'questentry-thiefrune 'tower 1)
       (quest-data-update-with 'questentry-thiefrune 'bole 1 (quest-notify (grant-party-xp-fn 10)))
       )

(define (gen-kama knpc kpc)
  (if (is-player-party-member? ch_kama)
      (begin
        (say knpc "I see Kama has joined you. Bonaha Kama!")
        (say ch_kama "Unh. Bonaha Mazuto."))
      (begin
        (say knpc "Kama is a forest goblin hunter. He was supposed to meet me at the edge of town a few nights back but he never showed up. Have you seen him?")
        (if (yes? kpc)
            (begin
              (say knpc "Is he in trouble?")
              (if (yes? kpc)
                  (say knpc "We must help him if we can!")
                  (say knpc "That is a relief!")))
            (say knpc "If you do, let me know. I'm a bit worried.")))))
            
(define (gen-ruka knpc kpc)
  (say knpc "Ruka is the goblin's name for Angriss, their god of death. "
       "The priests of Angriss were agitators during the goblin wars. "
       "Her cult has all but died out since their defeat. ")
  (prompt-for-key)
  (say knpc "Now, she is only a myth to keep children from straying into the woods, "
       "where more practical dangers still exist."))

(define (gen-clov knpc kpc)
  (say knpc "King Clovis led the paladins in the Goblin War. "
       "If my friend Kama would ever show up we could ask him "
       "if the goblins ever recovered his body.")
       (quest-data-update-with 'questentry-rune-f 'kama 1 (quest-notify nil))
       )

(define (gen-band knpc kpc)
  (say knpc "The goblins report a bandit camp somewhere to the south and west. "
       "Perhaps Deric knows more."))

(define gen-conv
  (ifc basic-conv
       ;;;; Goblin root words:
       (method 'bo  gen-bo)  ; My, Myself
       (method 'cho gen-cho) ; Mankind
       (method 'da  gen-da)  ; Abode, World
       (method 'eh  gen-eh)  ; What?
       (method 'gu  gen-gu)  ; Spirit, Ancestor
       (method 'ha  gen-ha)  ; Good, yes, skillful
       (method 'hi  gen-hi)  ; Magic
       (method 'iki gen-iki) ; Go
       (method 'jo  gen-jo)  ; Join
       (method 'ka  gen-ka)  ; Kill
       (method 'ki  gen-ki)  ; Health
       (method 'lu  gen-lu)  ; Change
       (method 'me  gen-me)  ; Forest
       (method 'ma  gen-ma)  ; Duty, Job, Destiny
       (method 'na  gen-na)  ; Your, yourself
       (method 'nin gen-nin) ; Stealth
       (method 'no  gen-no)  ; Name
       (method 'nu  gen-nu)  ; Give birth, Create, Begin
       (method 'ru  gen-ru)  ; Ancient, Primordial, Deep, Cave
       (method 'to  gen-to)  ; Individual
       (method 'tu  gen-tu)  ; No, Bad
       (method 'zu  gen-zu)  ; Watch, Seek

       ;;;; Goblin composite words / phrases:
       (method 'bona gen-bonaha)   ; Friend
       (method 'kama gen-kama)     ; Kama, the goblin friend of Gen
       (method 'nuki gen-nuki)     ; Food
       (method 'ruka gen-ruka)     ; Angriss, the Spider Queen
       (method 'melu gen-meluki)   ; Seeker, Wanderer
       (method 'guno gen-gunodama) ; the language of the Forest Goblins

       ;;;; Responses in human speech:
       ;; Standard responses:
       (method 'default gen-default)
       (method 'hail gen-hail)
       (method 'name gen-name)
       (method 'job  gen-job)
       (method 'join gen-join)
       (method 'bye  gen-bye)

       ;; Having to do with the goblin language:
       (method 'gobl gen-goblin)
       (method 'lang gen-language)
       (method 'prac gen-practice)

       ;; Other responses:
       (method 'admi gen-primal)
       (method 'ambi gen-ambitious)
       (method 'band gen-band)
       (method 'capt gen-captain)
       (method 'cave gen-cave)
       (method 'fore gen-job)
       (method 'maid gen-maiden)
       (method 'prim gen-primal)
       (method 'rang gen-ranger)
       (method 'reas gen-reasons)
       (method 'sava gen-primal)

       (method 'cult gen-culture)
       (method 'shak gen-shakespeare)
       (method 'bard gen-shakespeare)  ;; synonyn
       (method 'haml gen-shakespeare)  ;; synonyn

       (method 'shro gen-shroom)
       (method 'thie gen-thie)
       (method 'wars gen-wars)
       (method 'wood gen-woodsman)
       (method 'clov gen-clov)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-gen tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Gen"               ; name
                 sp_human            ; species
                 oc_ranger           ; occ
                 s_old_ranger  ; sprite
                 faction-men         ; starting alignment
                 4 2 4           	 ; str/int/dex
                 pc-hp-off  ; hp bonus
                 pc-hp-gain ; hp per-level bonus
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'gen-conv           ; conv
                 sch_gen             ; sched
                 'townsman-ai        ; special ai
                 (mk-inventory (list (list 1 t_dagger) 
				     (list 1 t_playbook_hamlet)
				     ))  ; container
                 (list t_armor_leather)                ; readied
                 )
   (gen-mk #f #f)))
