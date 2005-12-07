;;----------------------------------------------------------------------------
;; Generic conversation
;;----------------------------------------------------------------------------

;; fundamentals
(define (generic-hail knpc kpc)
  (say knpc "Well met"))

(define (generic-unknown knpc kpc)
  (say knpc "I can't help you with that"))

(define (generic-bye knpc kpc)
  (say knpc "Farewell")
  (kern-conv-end))

(define (generic-join knpc kpc)
  (kern-conv-say knpc "I cannot join you."))

;; wise
(define (basic-ench knpc kpc)
  (say knpc "The Enchanter is the Wise Wizard. "
       "He lives in a tower by the Fens, do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the ladder down. You'll come out in Eastpass. "
                    "Lord Froederick's men can help you from there."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the road west to Trigrave and ask around there."))
              ((equal? kplace p_trigrave)
               (say knpc "Take the road north to Southpass and ask the guards there."))
              (else 
               (say knpc "I'm not sure how to get there from here."))
        ))))

;; towns
(define (basic-trig knpc kpc)
  (say knpc "Trigrave is a small town in the west, "
       "settled where two rivers meet."))

(define (basic-gree knpc kpc)
  (say knpc "Green Tower, home of the Rangers, lies deep in the Great Forest. "
       "Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the road east into the forest. "
                    "Eventually it turns into a trail, follow it as best you can."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the ladder down to Westpass and ask the Rangers there."))
              ((equal? kplace p_trigrave)
               (say knpc "Take the road east to the mountains and go through Eastpass. "
                    "After that, you'll have to ask around."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              (else 
               (say knpc "I'm not sure how to get there from here."))
              ))))

(define (basic-bole knpc kpc)
  (say knpc "The hamlet of Bole sits in a canyon in the mountains north of "
       "the Great Wood. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "It's northeast of here. Follow the mountains."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the ladder down to Westpass and ask the Rangers there."))
              ((equal? kplace p_trigrave)
               (say knpc "Take the road east to the mountains and go through Eastpass. "
                    "After that, you'll have to ask around."))
              ((equal? kplace p_green_tower)
               (say knpc "Go north through the forest until you hit the mountains, "
                    "then follow them east a short while."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              (else 
               (say knpc "I'm not sure how to get there from here."))
              ))))
              
(define (basic-absa knpc kpc)
  (say knpc "Absalot, a great and wicked city, was destroyed for its sins."))

(define (basic-opar knpc kpc)
  (say knpc "The city of Oparine can be found in the southwest by a "
       "deep harbor. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the ladder down to Eastpass and follow the road west."))
              ((equal? kplace p_eastpass)
               (say knpc "Follow the road west."))
              ((equal? kplace p_trigrave)
               (say knpc "Follow the road west and south all the way to the sea."))
              ((equal? kplace p_green_tower)
               (say knpc "Follow the trail south and west to Westpass and ask the rangers when you get there."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              (else 
               (say knpc "I'm not sure how to get there from here."))
              ))))

(define (basic-glas knpc kpc)
  (say knpc "Glasdrin is the fortified city of the Paladins. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "The best way is to take the ladder down to Eastpass and go to Trigrave first."))
              ((equal? kplace p_eastpass)
               (say knpc "Go west to Trigrave then up to Northpass."))
              ((equal? kplace p_trigrave)
               (say knpc "Go up to Northpass and ask there."))
              ((equal? kplace p_green_tower)
               (say knpc "Follow the trail south and west to Westpass and ask the rangers there."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Head east of the Fens, you'll find it by the sea."))
              ((equal? kplace p_oparine)
               (say knpc "Follow the road north to Trigave and ask there."))
              (else 
               (say knpc "I'm not sure how to get there from here."))
              ))))

(define (basic-fens knpc kpc)
  (say knpc "The Fens are a swampy area in the northwest."))

(define (basic-kurp knpc kpc)
  (say knpc "Kurpolis is an ancient underground ruin. "
       "You'll find the entrance somewhere near Glasdrin."))

;; establishments
(define (basic-whit knpc kpc)
  (say knpc "The White Stag is in Green Tower."))

;; quests
(define (basic-thie knpc kpc)
  (say knpc "No, I don't know anything about a thief."))

(define (basic-rune knpc kpc)
  (say knpc "I don't know much about runes."))

(define (basic-wise knpc kpc)
  (say knpc "The Wise influence affairs in the Shard."))

(define (basic-shar knpc kpc)
  (say knpc "The Shard is what we call our world."))

(define (basic-warr knpc kpc)
  (say knpc "The Warritris is the Wise Warrior. If you're looking for her try Glasdrin."))

(define basic-conv
  (ifc '()
       ;; fundamentals
       (method 'hail generic-hail)
       (method 'default generic-unknown)
       (method 'bye generic-bye)
       (method 'join generic-join)
       
       ;; wise
       (method 'ench basic-ench)
       (method 'wise basic-wise)
       (method 'warr basic-warr)

       ;; towns & regions
       (method 'absa basic-absa)
       (method 'bole basic-bole)
       (method 'gree basic-gree)
       (method 'trig basic-trig)
       (method 'opar basic-opar)
       (method 'fens basic-fens)
       (method 'shar basic-shar)
       (method 'kurp basic-kurp)
       (method 'glas basic-glas)

       ;; establishments
       (method 'whit basic-whit)

       ;; quests
       (method 'thie basic-thie)
       (method 'rune basic-rune)

       ;; monsters

       ))

;; Helper(s)
(define (say knpc . msg) (kern-conv-say knpc msg))
(define (yes? kpc) (kern-conv-get-yes-no? kpc))
(define (no? kpc) (not (kern-conv-get-yes-no? kpc)))
(define (prompt-for-key)
  (kern-log-msg "<Hit any key to continue>")
  (kern-ui-waitkey))

(define (working? knpc)
  (string=? "working" (kern-obj-get-activity knpc)))

;;----------------------------------------------------------------------------
;; Quests
;;----------------------------------------------------------------------------
(define (mk-quest) (list #f #f #f))
(define (quest-offered? qst) (car qst))
(define (quest-accepted? qst) (cadr qst))
(define (quest-done? qst) (caddr qst))
(define (quest-offered! qst val) (set-car! qst val))
(define (quest-accepted! qst val) (set-car! (cdr qst) val))
(define (quest-done! qst val) (set-car! (cddr qst) val))


;;----------------------------------------------------------------------------
;; Ranger Conversation
;;----------------------------------------------------------------------------
(define (ranger-ranger knpc kpc)
  (say knpc "Rangers guard the borders between wilderness and "
       "civilization. We patrol the frontier and give aid where we can to the "
       "Wise."))

(define (ranger-wise knpc kpc)
  (say knpc "Rangers have an informal alliance with the Wise. They give us "
       "aid and hospitality. In return we give them news. Sometimes we serve "
       "them as messengers and scouts."))


(define ranger-conv
  (ifc basic-conv
       (method 'rang ranger-ranger)
       (method 'wise ranger-wise)
       ))


;; Knight conversation -- used by Lord Froederick's troops
(define knight-conv basic-conv)

;; Glasdrin
(define (glasdrin-warr knpc kpc)
  (say knpc "The Warritrix is the most cunning warrior of the age. I'm not sure where she is right now, ask the Steward or Commander Jeffries."))

(define (glasdrin-stew knpc kpc)
  (say knpc "The Steward is the keeper of the city and realms of Glasdrin. You can usually find her in the Citadel."))

(define (glasdrin-jeff knpc kpc)
  (say knpc "Jeffries is the commander of the Glasdrin militia. He's usually at work in the Citadel."))

(define (glasdrin-kurp knpc kpc)
         (say knpc "Take the bridge north across the river then follow the "
              "mountains east and north into a canyon."))

(define (glasdrin-cita knpc kpc)
  (say knpc "The Citadel is the civil and military headquarters of Glasdrin. It's the big keep in the southeast corner of town."))

(define glasdrin-conv
  (ifc basic-conv
       (method 'warr glasdrin-warr)
       (method 'stew glasdrin-stew)
       (method 'jeff glasdrin-jeff)
       (method 'kurp glasdrin-kurp)
       (method 'cita glasdrin-cita)
       ))
