;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_green_tower x y w h))
(kern-mk-sched 'sch_gen
               (list 0  0  (mk-zone 2  13 1  1)  "sleeping")
               (list 4  0  (mk-zone 3  12 3  3)  "eating")
               (list 5  0  (mk-zone 7  20 5  5)  "idle")
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
 't_goblin_lexicon "Goblin Lexicon" s_lexicon 1
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
   "Ka.....Kill, Destroy, End"
   "Ki.....Health, Life-Force, Power"
   "Ma.....Forest, Hidden Ways"
   "Me.....Duty, Job, Destiny"
   "Na.....Yours, Yourself"
   "Nu.....Give Birth, Create, Begin"
   "No.....Name"
   "Ru.....Ancient, Primordal, Deep, Cave"
   "To.....Individual"
   "Tu.....Bad, No, Useless"
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
;;----------------------------------------------------------------------------
(define (gen-hail gen player) (say gen "Hail, Wanderer"))
(define (gen-bye gen player) (say gen "Farewell"))
(define (gen-default gen player) (say gen "I can't help you with that"))
(define (gen-name gen player) (say gen "I am Gen." ))
(define (gen-woodsman gen player) (say gen "Yes, some call me the Woodsman." ))
(define (gen-job gen player) (say gen "Once I was a Ranger, but my duty now is done. I wander 'midst the woods for my own reasons." ))
(define (gen-reasons gen player) (say gen "My reasons are my own." ))

(define (gen-captain gen player) 
  (say gen "Captain Deric commands the Rangers of Green Tower. Have you met him?")
  (if (kern-conv-get-yes-no? player)
      (say gen "A decent man, if somewhat ambitious.")
      (say gen "You can find him in the Tower. His office is on the second floor.")))

(define (gen-ambitious gen player) (say gen "In peace there's nothing so becomes a man as modest stillness and humility." ))
(define (gen-shakespeare gen player) (say gen "Very good! Some interesting conversation at last."))
(define (gen-ranger gen player) (say gen "Rangers fought in these woods during the Goblin Wars. Now they maintain a token presence."))
(define (gen-wars gen player) (say gen "Yes, I fought as a Ranger in the goblin wars. That was a generation ago, and people forget. "
                                 "They see the goblins as lesser beings, defeated and worthy of slow extinction."))
(define (gen-goblin gen player) (say gen "An interesting species. They have their own language, but no writing. "
                                   "They are much like men, but more savage, more primal. "
                                   "Their warriors are beserkers, their shamans are ecstatic mystics."))
(define (gen-primal gen player) (say gen "You can tell I admire them? But in the wars I fought them, not understanding what they were. "
                                   "I have friends among the wild forest goblins, now. The cave goblins, though, they are another story..." ))
(define (gen-cave gen player) (say gen "The cave goblins, who are larger and stronger than their forest cousins, prefer to live in the deeps of the world. "
                                 "Their dark god demands living sacrifices. Beware them if you explore the caves, they burn with hatred for humankind." ))

(define (gen-language kgen player)
  (let ((gen (kobj-gob-data kgen)))
    (say kgen "Yes, I can speak a few words of goblin. Would you like to learn?")
    (if (kern-conv-get-yes-no? player)
        (if (gen-gave-notes? gen)
            (say kgen "Study the notes I gave you, and then practice on me.")
            (begin
              (say kgen "Here are some notes I have made on their language. You may keep it.")
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

(define (gen-da gen player) (say gen "Ha! Da-Ma-To means forest goblin." ))
(define (gen-gu gen player) (say gen "Ha! Da-Gu means world." ))
(define (gen-ru gen player) (say gen "Ha! Da-Ru-To means cave goblin." ))
(define (gen-no gen player) (say gen "Bo-No-Gen. But the goblins call me Ma-Zu-To." ))
(define (gen-me gen player) (say gen "Bo-Ma-Zu. I watch the forest, or I seek the hidden ways." ))
(define (gen-ki gen player) (say gen "Bo-Ha-Ki! I am healthy." ))
(define (gen-hi gen player) (say gen "Ha! Hi-Ma-To is the word for 'shaman'." ))
(define (gen-cho gen player) (say gen "Ha! Cho-To means 'a man'." ))
(define (gen-nu gen player) (say gen "Ha! Nu-Ki is the goblin word for 'food'" ))
(define (gen-ka gen player) (say gen "Ha! Ka-Ha-To means warrior." ))
(define (gen-ha gen player) (say gen "Yes, Ha is a general affirmative term." ))
(define (gen-tu gen player) (say gen "Right, Tu is a general negative term." ))
(define (gen-bo gen player) (say gen "Yes, Bo-Gu means your spirit self, which is your altar ego in the spirit world." ))
(define (gen-na gen player) (say gen "Yes, Bo-Na means 'us', or 'tribe'. Bo-Na-Ma refers to forest goblins in general." ))
(define (gen-to gen player) (say gen "Right, To is a general suffix meaning person." ))
(define (gen-ma gen player) (say gen "Yes, and Ka-Ma-To is their term for lumberjack." ))
(define (gen-zu gen player) (say gen "Good! And Zu-To means seeker." ))
(define (gen-eh gen player) (say gen "Eh?" ))
(define (gen-bonaha gen player) 
  (say gen "Excellent! That is the goblin word for friend. You have come far in mastering their language.")
  (gen-set-will-join! (kobj-gob-data gen) #t))

(define (gen-shroom gen player) (say gen "She is an old friend. Can you believe she was a war-maiden in the Goblin Wars?"))
(define (gen-maiden gen player) (say gen "It's true! I can still remember her hand-axe flashing in the moonlight "
                                   "as she hacked her way through goblin war parties, chanting a battle-ward "
                                   "with ragged breath! She was a sight to see."))

(define gen-conv
  (ifc basic-conv
       (method 'bo gen-bo)
       (method 'cho gen-cho)
       (method 'da gen-da)
       (method 'eh gen-eh)
       (method 'gu gen-gu)
       (method 'ha gen-ha)
       (method 'hi gen-hi)
       (method 'ka gen-ka)
       (method 'ki gen-ki)
       (method 'me gen-me)
       (method 'ma gen-ma)
       (method 'na gen-na)
       (method 'no gen-no)
       (method 'nu gen-nu)
       (method 'ru gen-ru)
       (method 'to gen-to)
       (method 'tu gen-tu)
       (method 'zu gen-zu)
       (method 'admi gen-primal)
       (method 'ambi gen-ambitious)
       (method 'bona gen-bonaha)
       (method 'bye  gen-bye)
       (method 'capt gen-captain)
       (method 'default gen-default)
       (method 'cave gen-cave)
       (method 'fore gen-job)
       (method 'gobl gen-goblin)
       (method 'hail gen-hail)
       (method 'job gen-job)
       (method 'join gen-join)
       (method 'lang gen-language)
       (method 'maid gen-maiden)
       (method 'name gen-name)
       (method 'prim gen-primal)
       (method 'prac gen-practice)
       (method 'rang gen-ranger)
       (method 'reas gen-reasons)
       (method 'sava gen-primal)
       (method 'shak gen-shakespeare)
       (method 'shro gen-shroom)
       (method 'wars gen-wars)
       (method 'wood gen-woodsman)
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
                 s_companion_ranger  ; sprite
                 faction-men         ; starting alignment
                 12 10 12            ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 90 0 9 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'gen-conv           ; conv
                 sch_gen             ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (gen-mk #f #f)))
