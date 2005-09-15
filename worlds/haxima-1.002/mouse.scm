;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define mouse-start-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (mouse-mk) (list #t))
(define (mouse-first-meeting? mouse) (car mouse))
(define (mouse-set-first-meeting! mouse val) (set-car! mouse val))

(define (mouse-meet-first-time knpc kpc)
  (mouse-set-first-meeting! (kobj-gob-data knpc) #f)

  (define (mouse-disappear)
    (say knpc "Oh, bother. Not again!")
    (kern-obj-add-effect knpc ef_invisibility nil)
    (kern-conv-end kpc))
    
  (define (betray-player kchar)
    (kern-char-leave-player kchar)
    (kern-being-set-base-faction kchar faction-monster))

  (define (kathryn-in-party)
    (betray-player ch_kathryn)
    (if (and (defined? 'ch_thud)
             (is-alive? ch_thud))
        (begin
          (say ch_kathryn "The fool has led us right to the Thief! "
               "Quickly, Thud! Kill them all!")
          (betray-player ch_thud))
        (say ch_kathryn "Fool! Now the Thief is mine!"))
    (mouse-disappear))

  (define (open-moongate loc)
    (let ((kgate (mk-moongate nil)))
      (kern-obj-relocate kgate loc nil)
      (moongate-animate kgate moongate-stages)
      kgate))

  (define (close-moongate kgate)
    (moongate-animate kgate (reverse moongate-stages))
    (moongate-destroy kgate))
    
  (define (warp-in kchar loc dir)
    (display "warp-in")(newline)
    (kern-obj-inc-ref kchar)
    (kern-obj-remove kchar)
    (kern-obj-relocate kchar loc nil)
    (kern-obj-dec-ref kchar)
    (kern-map-repaint)
    (kern-sleep 250)
    (kern-obj-relocate kchar (loc-offset loc dir) nil)
    (kern-being-set-base-faction kchar faction-monster)
    (kern-map-repaint))

  (define (kathryn-alive-but-not-in-party)
    (let* ((gate-loc (mk-loc (loc-place (kern-obj-get-location knpc)) 7 2))
           (kgate (open-moongate gate-loc)))
      (warp-in ch_kathryn gate-loc south)
      (say ch_kathryn "I knew this fool would lead me right to you, "
           "Thief!")
      (if (and (defined? 'ch_thud)
               (is-alive? ch_thud))
          (begin
            (warp-in ch_thud gate-loc west)
            (say ch_kathryn "Thud, do what you do best!")
            (say ch_thud "KILL! KILL! KILL! KILL")))
      (close-moongate kgate))
    (mouse-disappear))

  (define (thud-alive-in-party)
    (say ch_thud "Thief here, but Red Lady dead! Thud kill all!")
    (mouse-disappear))

  (define (both-dead-in-party)
    (betray-player ch_thud)
    (say knpc "It appears I owe you a debt of gratitude. That sorceress and "
         "her brute were getting to be a nuisance!"))

  (define (kathryn-dead-in-party)
    (betray-player ch_kathryn)
    (if (and (defined? 'ch_thud)
             (is-player-party-member? ch_thud))
        (if (is-alive? ch_thud)
            (thud-alive-in-party)
            (both-dead-in-party))))

  (if (defined? 'ch_kathryn)
      (if (is-player-party-member? ch_kathryn)              
          (if (is-alive? ch_kathryn)
              (kathryn-alive-in-party)
              (kathryn-dead-in-party))
          (if (is-alive? ch_kathryn)
              (kathryn-alive-but-not-in-party)
              (if (and (defined? 'ch_thud)
                       (is-player-party-member? ch_thud))
                  (if (is-alive? ch_thud)
                      (thud-alive-in-party)
                      (both-dead-in-party)))))))
  

;;----------------------------------------------------------------------------
;; Conv
;;
;; Main Points
;;
;; 1. Must cast doubt on Enchanter's Gate Theory
;; 2. Must return the Enchanter's Rune
;; 3. Will give the Rune if player agrees to seek the other Runes and unlock
;;    the Demon Gate
;;
;; The Thief is one of a cadre of the Mighty who want to re-open the Gate. If
;; the player agrees to help their cause they will give him their Runes. If he
;; disagrees they will not. The Enchanter, when the player returns, will ask
;; him to seek out all the Runes from these dissenters so they can be kept
;; under lock and key. There are three ways to get the Thief's Rune:
;; 1. Agree to open the Gate
;; 2. Kill him (should be extremely tough to do so)
;; 3. Pickpocket it from him (should also be tough)
;;----------------------------------------------------------------------------
(define (mouse-hail knpc kpc)
  (let ((mouse (kobj-gob-data knpc)))
    (if (mouse-first-meeting? mouse)
        (mouse-meet-first-time knpc kpc)
        (say knpc "Hello again, Wanderer."))))

(define (mouse-default knpc kpc)
  (say knpc "Got me there."))

(define (mouse-name knpc kpc)
  (say knpc "I'm The Mouse."))

(define (mouse-join knpc kpc)
  (say knpc "I prefer to work alone."))

(define (mouse-job knpc kpc)
  (say knpc "I... collect things."))


(define (mouse-coll knpc kpc)
  (say knpc "Some would even call me a thief."))

(define (mouse-thie knpc kpc)
  (say knpc "Well, you caught me. Let me guess, the Enchanter sent you, right?")
  (if (kern-conv-get-yes-no? kpc)
      ;; yes, enchanter sent player
      (begin
        (say knpc "[Chuckling] Leave it to Enchy to get the first Wanderer in "
             "500 years to be his errand boy! Let me guess again, he didn't "
             "tell you what's missing, right? He probably referred to it as an "
             "item?")
        (if (kern-conv-get-yes-no? kpc)
            ;; yes, enchanter didn't tell about item
            (begin
              (say knpc "I knew it! It's just like the old fart. The Enchanter "
                   "never gives you the whole story. Well, my friend, this item "
                   "you're looking for is a rune stone. In fact, its one of eight "
                   "very special rune stones. Do you know which eight I'm "
                   "referring to?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes, knows about the eight runes
                  (say knpc "Good! Then you know that as long as the Enchanter "
                       "and some of the other Mighty have control of "
                       "their own rune stones we'll never be able to open the "
                       "Demon Gate again.")
                  ;; no, doesn't know about the eight runes
                  (begin
                    (say knpc "The eight keys to the Demon Gate! Do you know "
                         "what that is?")
                    (if (kern-conv-get-yes-no? kpc)
                        ;; yes, knows what demon gate is
                        (say knpc "Whew! I was starting to think I'd been caught "
                             "by a fool! That would be bad for my self-esteem.")
                        ;; no, doesn't know what demon gate is
                        (say knpc "You need to get around more. The Demon Gate "
                             "is a door to other worlds. Long ago the Mighty "
                             "of that age locked it shut. Opinions differ as to "
                             "why, but it has remained locked ever since.")
                        ))))
            ;; no, enchanter told about item
            (say knpc "That's not like him. He must be getting lax in his old "
                 "age. So, you know that he was careless enough to allow one "
                 "of his casual enemies to take one of the eight locks to the "
                 "Demon Gate right out from under his nose. Frankly, I don't "
                 "think he's responsible enough to take care of a rune stone.")))
      ;; no, enchanter did not send player
      (say knpc "Oh. Well, welcome to my humble abode! Bit of a mess, sorry. "
           "Have some food and wine, take a rest. It's a long climb back up, "
           "and I'm sure a busy Wanderer like yourself has places to go and "
           "evil to fight!")))
                  
(define (mouse-migh knpc kpc)
  (say knpc "The Mighty are the so-called greatest Warriors, Wizards, Wrights "
       "and Wrogues in the land. We get together for barbecues, play golf, "
       "that sort of thing."))

(define (mouse-demo knpc kpc)
  (say knpc "The Mighty are divided. Some, like the Enchanter, believe that "
       "the Demon Gate must remain locked forever. Others, like myself, "
       "believe it should be re-opened. Will you help me re-open it?")
  (if (kern-conv-get-yes-no? kpc)
      ;; yes -- he will help
      (begin
        (say knpc "Excellent! We will need to gather all the Runes from the "
             "other Mighty. Seek the Alchemist.")
        (if (in-inventory? knpc t_rune_f)
            (begin
              ;; thief still has his rune, give to player
              (say knpc "You will be the keeper of the runes, and as a "
                   "sign of trust I will give you mine. Don't betray me on "
                   "this, I don't often give things away. By this you know I "
                   "am serious.")
              (kern-obj-remove-from-inventory knpc t_rune_f 1)
              (kern-obj-add-to-inventory kpc t_rune_f 1))))
      ;; no -- he will not help
      (say knpc "A pity. Perhaps you will reconsider after you have traveled "
           "more widely.")))
      
(define (mouse-evil knpc kpc)
  (say knpc "I'm not an expert. I try to stay out of philosophical debates. "
       "Ask the Enchanter or the Necromancer about that, they'll talk your "
       "ear off."))

(define (mouse-ench knpc kpc)
  (say knpc "Crusty old fart. And a bit preachy, too. He's perfectly situated "
       "in a tower by a gassy old bog."))

(define (mouse-necr knpc kpc)
  (say knpc "Creepy fellow, what with all the dead things hanging about, but "
       "nice enough. I don't know why he sticks around in Absalot, though."))

(define (mouse-rune knpc kpc)
  (if (in-inventory? knpc t_rune_k)
      (begin
        (say knpc "Hm. Yes. I suppose you'll be wanting to take the Enchanter's Rune "
             "back to him. Well, I guess I can always steal it back later.")
        (kern-obj-remove-from-inventory knpc t_rune_k 1)
        (kern-obj-add-to-inventory kpc t_rune_k 1))
      (say knpc "I already gave it to you! If you lost it don't blame me!")))

(define (mouse-absa knpc kpc)
  (say knpc "Absalot was a fun town. 'Til they destroyed it."))

(define (mouse-dest knpc kpc)
  (say knpc "A coalition led by the city of Glasdrin destroyed Absalot and "
       "sealed off the passage to it. You'll hear that it was destroyed for "
       "its sins, or whatever, but it was destroyed by men, plain and "
       "simple."))

(define (mouse-sins knpc kpc)
  (say knpc "Absalot was indeed a sinful city. People did whatever they "
       "wanted, and had no rulers to speak of. Talk about depraved!"))

(define (mouse-pers knpc kpc)
  (say knpc "You're the first to make it down here."))

(define (mouse-mous knpc kpc)
  (say knpc "Since I'm technically one of the Mighty, I thought it would "
       "be fun to call myself the Mighty Mouse. You don't find that amusing, "
       "either? Nobody does. I don't understand why."))

(define (mouse-glas knpc kpc)
  (say knpc "Self-righteous city of paladins. Say, do you know what a paladin is?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "I guess you've heard that one.")
      (say knpc "A paladin is someone who wakes up in the middle of the night afraid "
           "that somebody, somewhere, is having a good time.")
      ))

(define (mouse-know knpc kpc)
  (say knpc "The Rune of Knowledge? Better ask the Enchanter what its good "
       "for, other than keeping the Demon Gate shut."))

(define (mouse-free knpc kpc)
  (say knpc "The Rune of Freedom? I suppose you'll want to hear the sermon?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "[He recites in a bored voice]:\n"
           "Freedom is the state of a man who is able and willing to choose. "
           "This condition cannot be given to a man, nor can it be taken away "
           "from him. The only way to control a free man is to take away his "
           "choices, or to prevent him from seeing that they are there for the "
           "making.")
      (say knpc "Good. My predecessor made me memorize that before he gave me "
           "the rune. Each rune is useless unless the one who holds it can "
           "recite the corresponding nugget of wisdom. I think a dirty "
           "limmerick would have worked just as well, but apparently the "
           "Mighty of Old lacked a sense of humor.")))

(define (mouse-bye knpc kpc)
  (say knpc "Say high to the Enchanter for me!"))

(define mouse-conv
  (ifc nil
       (method 'default mouse-default)
       (method 'hail mouse-hail)
       (method 'bye mouse-bye)
       (method 'job mouse-job)
       (method 'name mouse-name)
       (method 'join mouse-join)

       (method 'absa mouse-absa)
       (method 'coll mouse-coll)
       (method 'demo mouse-demo)
       (method 'dest mouse-dest)
       (method 'ench mouse-ench)
       (method 'evil mouse-evil)
       (method 'free mouse-free)
       (method 'glas mouse-glas)
       (method 'know mouse-know)
       (method 'migh mouse-migh)
       (method 'mous mouse-mous)
       (method 'necr mouse-necr)
       (method 'pers mouse-pers)
       (method 'prid mouse-sins)
       (method 'rune mouse-rune)
       (method 'sins mouse-sins)
       (method 'they mouse-dest)
       (method 'thie mouse-thie)
       (method 'thin mouse-coll)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-mouse)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_mouse ;;..tag
     "Mouse" ;;....name
     sp_human ;;.....species
     nil ;;..........occupation
     s_brigand ;;.....sprite
     faction-men ;;..faction
     0 ;;............custom strength modifier
     0 ;;............custom intelligence modifier
     10 ;;............custom dexterity modifier
     2 ;;............custom base hp modifier
     2 ;;............custom hp multiplier (per-level)
     1 ;;............custom base mp modifier
     1 ;;............custom mp multiplier (per-level)
     (max-hp sp_human nil mouse-start-lvl 0 0) ;;..current hit points
     10000 ;;...........current experience points
     (max-mp sp_human nil mouse-start-lvl 0 0) ;;..current magic points
     mouse-start-lvl  ;;..current level
     #f ;;...........dead?
     'mouse-conv ;;...conversation (optional)
     nil ;;..........schedule (optional)
     nil ;;..........custom ai (optional)

     ;;..........container (and contents)
     (mk-chest nil
               (mk-contents (add-content 1 t_rune_f)
                            (add-content 1 t_rune_k)
                            (add-content 1 t_armor_leather)
                            (add-content 1 t_leather_helm)
                            (add-content 1 t_sword)
                            (add-content 1 t_bow)
                            (add-content 50 t_arrow)))

     nil ;;..........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (mouse-mk)))
