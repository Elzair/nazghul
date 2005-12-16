;;----------------------------------------------------------------------------
;; Sched
;;----------------------------------------------------------------------------
(define (mk-zone x y w h) (list 'p_green_tower x y w h))
(kern-mk-sched 'sch_deric
               (list 0  0 (mk-zone 17  4  1   1)  "sleeping")
               (list 6  0 (mk-zone 30 30  5   5)  "working")
               (list 12  0 (mk-zone 52 54  1   1)  "eating")
               (list 13  0 (mk-zone 30 30  5   5)  "working")
               (list 18  0 (mk-zone 52 54  1   1)  "eating")
               (list 19  0 (mk-zone 30 30  5   5)  "working")
               (list 21  0 (mk-zone 13  2  4   4)  "idle")
               (list 22  0 (mk-zone 17  4  1   1)  "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (deric-mk tell-secret?) (list tell-secret?))
(define (deric-tell-secret? deric) (car deric))
(define (deric-set-tell-secret! deric) (set-car! deric #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

(define (deric-name knpc kpc)
  (say knpc "I am Captain Deric, commander of the Rangers, at your service."))

(define (deric-job knpc kpc)
  (say knpc "I command the Rangers, though I aspire to greater things. I "
       "think I have proved myself quite well as a leader, but around here "
       "you have to wait for someone to die before you get promoted!"))

(define (deric-health knpc kpc)
  (say knpc "I am the very definition of good health!"))

(define (deric-rangers knpc kpc)
  (say knpc "Right then! The Rangers are charged with keeping the peace in "
       "the Great Wood. We enforce the law, keep an eye on the goblins, that "
       "sort of thing. We're also charged with maintenance of the forest and "
       "keeping the roads in and out clear. If I say so myself, we have done "
       "an amazing job under my leadership. Thanks to the hard work of my "
       "people, of course. Ahem."))

(define (deric-two knpc kpc)
  (say knpc "The upper level of the tower is mostly fortifications. The lower "
       "level is where the council chamber, barracks, kitchen and other "
       "facilities are. We also have a prison down there. Some think the "
       "lower level is haunted."))

(define (deric-haunted knpc kpc)
  (say knpc "They say the lower level is haunted, and I for one have heard "
       "some strange noises. They sound like they're coming from deep within "
       "the walls - chanting, or shrieking. But I've never seen any ghosts. "
       "And of course I am not afraid of such things!"))

(define (deric-gen knpc kpc)
  (say knpc "A splendid old chap, if a bit daft. He's a legend among the "
       "Rangers for his efforts during the Goblin War, and one of the last "
       "surviving veterans. Still looks to be in good shape physically, but "
       "he's gone a bit native, if you know what I mean."))

(define (deric-native knpc kpc)
  (say knpc "Yes, he's adopted some of the habits of the Wood Folk - the "
       "forest goblins. But I don't suspect him of conspiring against us - if "
       "I did I would turn him in and perhaps get a promotion! But alas, 'tis "
       "not the case."))

(define (deric-shroom knpc kpc)
  (say knpc "A nice old hag. Bit of a witch, but not any trouble. Keeps a "
       "reagent shop in the northeast corner of town."))

(define (deric-abe knpc kpc)
  (say knpc "A queer young man. Queer as in strange, you know, not that "
       "other... well, maybe... it's hard to say. Anyway, he studies the "
       "ruins in the southwest corner. Working for the Royal Library or some "
       "such."))


(define (deric-tower knpc kpc)
  (say knpc "Yes, the Tower itself which gives Green Tower its name is "
       "located in the center of town. It is my command post and headquarters "
       "for the Rangers. A fascinating building! Did you know it is built on "
       "the ruins of an older tower?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Interesting, eh? Not that I'm an expert on such things. "
           "You'd have to talk with old Abe, or perhaps Shroom, to learn "
           "more.")
      (say knpc "'Tis true! What is now the Tower was once only the central "
           "spire of a very great structure that reached much higher! And "
           "much lower as well, if young Abe is to be believed. Currently "
           "there are only two stories.")))

(define (deric-ambition knpc kpc)
  (say knpc "Yes, I am quite ambitious and not afraid to admit it! No point "
       "sneaking about when one is ambitious, it just makes people nervous. "
       "I wouldn't do anything immoral to advance my position, of course, "
       "I believe too firmly in the principles of our great society. Do you "
       "have any ambitions?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (say knpc "Well, enough about you! I hope someday to be a Lord. "
             "Do you think I have a chance?")
        (if (kern-conv-get-yes-no? kpc)
            (begin
              (say knpc "I knew it! You know, I wasn't sure if I could trust "
                   "you, but you're obviously a person of insight, so I'll "
                   "let you in on a secret.")
              (deric-set-tell-secret! (kobj-gob-data knpc)))
            (say knpc "[laughing] I'm sorry if I must disagree! People of "
                 "destiny are never appreciated by their fellows.")))
      (say knpc "[sigh] So many people lead worthless, wasted lives. I'm glad "
           "I'm not one of them!")))

(define (deric-secret knpc kpc)
  (if (deric-tell-secret? (kobj-gob-data knpc))
      (say knpc "In the lower level of the Tower there is a hidden passage. "
           "Go down the ladder and enter the southeast supply room. "
           "On the east wall is a secret door! "
           "I have no idea what it was for.")
      (say knpc "Nothing I want to tell a stranger like you!")))

(define (deric-afraid knpc kpc)
  (say knpc "Of course not! Fear is the hobgoblin of little minds. "
       "Or is it hobgoblins are the fear of little minds? "
       "Damn, I can never remember that silly saying."))

(define (deric-prison knpc kpc)
  (say knpc "Yes. Very secure. We currently only have one prisoner, "
       "a forest goblin we caught skulking around here. "
       "A quite vicious-looking brute, I must say. "
       "Not that I am afraid of him. Ahem."))

(define (deric-gobl knpc kpc)
  (say knpc "Sneaky fellows! Why, just the other day I caught one here in town and had him thrown into prison."))

(define (deric-brute knpc kpc)
  (say knpc "Very suspicious. He obviously did not come to trade, "
       "for he carried no merchandise. And he speaks not a lick of Common. "
       "In fact, he refuses to say anything! Well, some time spent "
       "underground should loosen his tongue. I'll find out what he's "
       "scheming."))

(define (deric-default knpc kpc)
  (say knpc "You must ask another of that!"))

(define (deric-hail knpc kpc)
  (say knpc "Well met, indeed!"))

(define (deric-bye knpc kpc)
  (say knpc "Until next time"))

(define (deric-thie knpc kpc)
  (say knpc "Hm. The goblin brute we have locked up below may be a thief. But "
       "he's been in our prison for some time. Now, our rangers did report "
       "a lone man traveling north through the forest, but we know not "
       "whence. The only thing to the north is Bole."))

(define (deric-accu knpc kpc)
  (say knpc "I assure you, my good man, there are no Accursed around here."))

(define deric-conv
  (ifc green-tower-conv
       (method 'abe        deric-abe)
       (method 'afra       deric-afraid)
       (method 'ambi       deric-ambition)
       (method 'aspi       deric-ambition)
       (method 'brut       deric-brute)
       (method 'bye        deric-bye)
       (method 'comm       deric-rangers)
       (method 'default    deric-default)
       (method 'die        deric-ambition)
       (method 'gen        deric-gen)
       (method 'hail       deric-hail)
       (method 'haun       deric-haunted)
       (method 'heal       deric-health)
       (method 'job        deric-job)
       (method 'name       deric-name)
       (method 'nati       deric-native)
       (method 'pris       deric-prison)
       (method 'prom       deric-ambition)
       (method 'rang       deric-rangers)
       (method 'secr       deric-secret)
       (method 'shro       deric-shroom)
       (method 'skul       deric-brute)
       (method 'stor       deric-two)
       (method 'thie       deric-thie)
       (method 'towe       deric-tower)
       (method 'two        deric-two)
       (method 'gobl       deric-gobl)
))                

(define (mk-deric tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Deric"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_companion_ranger   ; sprite
                 faction-men         ; starting alignment
                 0 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 120 0 9 4            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'deric-conv        ; conv
                 sch_deric          ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (deric-mk #f)))
