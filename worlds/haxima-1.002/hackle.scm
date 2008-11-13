;;----------------------------------------------------------------------------
;; Schedule
;;
;; In Bole.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_hackle
               (list 0  0  bole-hackles-hut "idle")
               (list 2  0  bole-bed-hackle "sleeping")
               (list 10 0  bole-hackles-hut "idle")
               (list 20 0  bole-hackles-yard "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (hackle-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Hackle is a female hedge-witch dwelling in Bole.
;; Her mind was shattered by some misadventure with Gazer(s),
;; though she is still capable of serving as a healer.
;;----------------------------------------------------------------------------
(define (hackle-trade knpc kpc)
  (say knpc "She can heal it, or one of its friends. She requires a piece of "
       "gold for each piece of life! Does it agree?")
  (define (hackle-heal)
    (say knpc "Which shall she heal?")
    (let ((kchar (kern-ui-select-party-member)))
      (if (null? kchar)
          (say knpc "How else can she help it?")
          (let* ((gold (kern-player-get-gold))
                 (pts (- (kern-char-get-max-hp kchar)
                         (kern-char-get-hp kchar))))
            (if (= 0 gold)
                (say knpc "No gold, no life! It is the law of the world!")
                (begin
                  (if (= 0 pts)
                      (say knpc "It is well! She can do nothing for it!")
                      (let ((n (min gold pts)))
                        (say knpc "She heals it!")
                        (kern-obj-heal kchar n)
                        (kern-player-set-gold (- gold n))))
                  (say knpc "Will it heal another?")
                  (if (kern-conv-get-yes-no? kpc)
                      (hackle-heal)
                      (say knpc "Then something else it wants.")
                      )))))))
  (if (kern-conv-get-yes-no? kpc)
      (hackle-heal)
      (say knpc "Have it its way!")))
                   
              
        

;; basics...
(define (hackle-default knpc kpc)
  (say knpc "She cannot help it with that."))

(define (hackle-hail knpc kpc)
  (if (in-player-party? 'ch_mesmeme)
      (begin
        (say knpc "[She shrinks back in horror at the sight of Mesmeme] AHHH! It frightens her!")
        (aside kpc 'ch_mesmeme "No frighten.")
        )
      (say knpc "[You meet a disheveled, middle-aged woman] It comes to her! "
           "It wants something!")
  ))

(define (hackle-name knpc kpc)
  (say knpc "She is Hackle."))

(define (hackle-job knpc kpc)
  (say knpc "She is mad! But she can heal it!"))

(define (hackle-join knpc kpc)
  (say knpc "She cannot join it! She must mind her sheep!"))

(define (hackle-bye knpc kpc)
  (say knpc "She bids it farewell, but knows it will return!"))


;; other characters & town...
(define (hackle-may knpc kpc)
  (say knpc "It is a hard woman, hard but kind to Hackle!"))

(define (hackle-kath knpc kpc)
  (say knpc "A woman clothed in red! A fiend clothed in woman!"))

(define (hackle-bill knpc kpc)
  (say knpc "It is careless. Yes, the wood gods will have it for supper one "
       "day."))

(define (hackle-thud knpc kpc)
  (say knpc "[She laughs, a surprising, golden sound] That is no toy! They "
       "called for a servant, but summoned a demon!"))

(define (hackle-melv knpc kpc)
  (say knpc "It is a good soul."))

(define (hackle-bole knpc kpc)
  (say knpc "Not Bole! Hole! The keyhole to the deep! She knows it!"))

;; misc...
(define (hackle-mesm knpc kpc)
  (say knpc "It is only a child. Deaf and dumb it is, to its own kind. "
       "But the pet will make a pet of its master.")
  (aside kpc 'ch_mesmeme "No pet")
  )

(define (hackle-shee knpc kpc)
  (say knpc "The sheep hides in wolf's clothing! How it howls when it "
       "bleats!"))

(define (hackle-wood knpc kpc)
  (say knpc "She has seen them sleeping in the old oaks. She wakes them "
       "not! They do not like us, they do not like anyone! The goblins "
       "appease them but she does not know the words."))

(define (hackle-mad knpc kpc)
  (say knpc "The gazers caught her as a girl! She escaped, but her mind did "
       "not!"))

(define (hackle-gaze knpc kpc)
  (say knpc "If it meets one, it should kill one without question! They know "
       "the answer to any question, and in the answer lies bondage!")
  (aside kpc 'ch_mesmeme "[Blinks nervously]")
)

;; thief quest...
(define (hackle-thie knpc kpc)
  (say knpc "It is a mighty wrogue indeed that robs a mighty wizard!"))

(define (hackle-robs knpc kpc)
  (say knpc "It robs and runs, down its little mouse-hole!"))

(define (hackle-hole knpc kpc)
  (say knpc "Does it like riddles?\n"
       "  An 'o' has a hole!\n"
       "  And hole has an 'o'!\n"
       "  And a mouse has both!\n"
       "  In the middle of the night\n"
       "  Down it will go!\n"
       "Does it know what ^c+mreveals^c-?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Then let it ^c+mreveal^c- and understand my riddle!")
      (begin
        (say knpc "Wis Quas! The Red Bitch has a scroll, but has not the "
             "wisdom.")))
  )

(define (hackle-reve knpc kpc)
  (say knpc "The Bill-boy knows where the mouse disappeared! "
       "Let it ^c+mreveal^c- there!"))

(define (hackle-midd knpc kpc)
  (say knpc "Yes! The middle of the night!"))

(define hackle-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default hackle-default)
       (method 'hail hackle-hail)
       (method 'bye  hackle-bye)
       (method 'job  hackle-job)       
       (method 'name hackle-name)
       (method 'join hackle-join)

       (method 'trad hackle-trade)
       (method 'buy hackle-trade)
       (method 'sell hackle-trade)
       (method 'heal hackle-trade)

       (method 'bill hackle-bill)
       (method 'kath hackle-kath)
       (method 'red  hackle-kath)
       (method 'bitc hackle-kath)
       (method 'may  hackle-may)
       (method 'melv hackle-melv)
       (method 'thud hackle-thud)
       
       (method 'bole hackle-bole)
       (method 'gaze hackle-gaze)
       (method 'god  hackle-wood)
       (method 'gods hackle-wood)
       (method 'hole hackle-hole)
       (method 'mad  hackle-mad)
       (method 'mesm hackle-mesm)
       (method 'migh hackle-robs)
       (method 'mous hackle-hole)
       (method 'reve hackle-reve)
       (method 'rob  hackle-robs)
       (method 'robs hackle-robs)
       (method 'wrog hackle-robs)
       (method 'wiza hackle-robs)
       (method 'shee hackle-shee)
       (method 'thie hackle-thie)
       (method 'wood hackle-wood)
       (method 'midd hackle-midd)
       (method 'nigh hackle-midd)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-hackle)
  (bind 
   (kern-mk-char 'ch_hackle          ; tag
                 "Hackle"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_beggar             ; sprite
                 faction-men         ; starting alignment
                 0 0 1            ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 6  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'hackle-conv        ; conv
                 sch_hackle          ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (hackle-mk)))
