;; shroom.scm - an old hag with an interesting history who lives in the
;; northeast corner of green tower.

(kern-mk-sched 'sch_shroom
               (list 0  0  51 9  1  1  "sleeping")
               (list 8  0  40 11 3  3  "idle")
               (list 9  0  49 6  7  1  "working")
               (list 12 0  50 9  1  1  "eating")
               (list 13 0  49 6  7  1  "working")
               (list 18 0  56 54 1  1  "eating")
               (list 19 0  53 50 4  7  "idle")
               (list 21 0  51 9  1  1  "sleeping"))

(define (shroom-mk gave-quest? finished-quest?) (list gave-quest? 
                                                      finished-quest?))
(define (shroom-gave-quest? shroom) (car shroom))
(define (shroom-quest-done? shroom) (cadr shroom))
(define (shroom-give-quest shroom) (set-car! shroom #t))

;; Shroom's merchant procedure
(define (shroom-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Yes, I trade in mushrooms and the like. "
           "Come by my shop in the northeast corner of time when I'm open.")
      (begin
        (say knpc "Are ye interested in mushrooms or other reagents?")
        (if (not (kern-conv-get-yes-no? kpc))
            (say knpc "Don't try to pick your own. "
                 "Kill you, the bad ones will!")
            (begin
              ;; Trading!
              (kern-conv-trade knpc kpc
                               (list sulphorous_ash 2)
                               (list garlic         2)
                               (list ginseng        2)
                               (list blood_moss     8)
                               (list black_pearl    8)
                               (list spider_silk    8)
                               (list mandrake       16)
                               (list nightshade     16)
                               (list t_sleep_potion 10))
              (say knpc "Be careful with those."))))))

;; Shroom's mushroom quest
(define (shroom-wards knpc kpc)
  (let ((shroom (kobj-gob-data knpc)))
    (display "shroom-wards")(newline)
    (if (shroom-gave-quest? shroom)
        (if (shroom-quest-done? shroom)
            (say knpc "I've forgotten all the others. But long ago the "
                 "librarian from Glasdrin had me teach him and he wrote them "
                 "all down. Check with him.")
            (begin
              (say knpc "Bring me the mushrooms and I will teach ye the ward "
                   "of panic. "
                   "Do ye remember where they are?")
              (if (kern-conv-get-yes-no? kpc)
                  (say knpc "Well...")
                  (say knpc "[sigh] Perhaps ye should write this down. "
                       "Leave town and take the path north to the open plain "
                       "and travel west until ye reach the mountains. "
                       "Nearby will be the cave entrance. "
                       "In the cave ye will find the mushrooms."))))
        (begin
          (say knpc "In my time I knew many battle wards. "
               "Be wanting me to teach ye, now, won't ye?")
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (say knpc "I know a battle ward that will throw your enemies "
                     "into a panic. But perhaps ye can do me a favor first, "
                     "yes?")
                (if (kern-conv-get-yes-no? kpc)
                    (begin
                      (say knpc "In a cave to the west in the mountains grows "
                           "a purple mushroom. Bring me four. Agreed?")
                      (if (kern-conv-get-yes-no? kpc)
                          (begin
                            (say knpc "Good. A colony of slimes infests that "
                                 "cave, so take plenty of flaming oil!")
                            (shroom-give-quest shroom))
                          (say knpc "Yes, perhaps ye are afraid.")))
                    (say knpc "Naught for naught, youngling!")))
              (say knpc "Of course, a skillfull warrior such as you has "
                   "nothing to learn from an old witch like me."))))))
                               

(define shroom-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Long ago I might have "
                                                "known about that.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Hello, dearie")))
       (method 'bye (lambda (knpc kpc) (say knpc "Toodaloo!")))
       (method 'job (lambda (knpc kpc) (say knpc "I sell potions, reagents "
                                            "and the like.")))
       (method 'name (lambda (knpc kpc) (say knpc "I'm known as Shroom. At "
                                                "your service.")))
       (method 'shro (lambda (knpc kpc) (say knpc "Mushrooms are my "
                                                "specialty. That's why they "
                                                "call me Shroom.")))
       (method 'mush shroom-trade)
       (method 'trad shroom-trade)
       (method 'sell shroom-trade)
       (method 'join (lambda (knpc) (say knpc "You're too young for me, "
                                         "sweetie!")))
       (method 'gen (lambda (knpc) (say knpc "Aye, a handsome young man he "
                                        "was, once. He could stay up all night"
                                        " in bed! But gone a bit strange, he "
                                        "has, befriending the goblins and all "
                                        "that.")))
       (method 'stra (lambda (knpc) (say knpc "He meets them in the forest "
                                            "and runs with their hunts. "
                                            "Half-goblin he nearly is; "
                                            "learned their ways he has. But "
                                            "ye could never tell him what to "
                                            "do, the silly man.")))
       (method 'gobl (lambda (knpc) (say knpc "I trade with them now and "
                                            "then. Their shamans know well "
                                            "the plants in these woods. I "
                                            "even speak a little. Know some "
                                            "of their magic. But I never "
                                            "trust them.")))
       (method 'trus (lambda (knpc) (say knpc "The goblins will turn on us "
                                          "when their opportunity comes. I "
                                          "would do the same in their "
                                          "place!")))
       (method 'wars (lambda (knpc) (say knpc "Ha! Yes, I fought the goblins. "
                                         "Long ago that was. People forget.")))
       (method 'maid (lambda (knpc) (say knpc "[she grins with crooked "
                                           "teeth] Is it so hard to believe I "
                                           "was once a fair war-maiden? [she "
                                           "cackles obscenely]")))
       (method 'ward shroom-wards)
       ))
