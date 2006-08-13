;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_melvin
               (list 0  0  bole-bed-melvin      "sleeping")
               (list 7  0  bole-kitchen "working")
               (list 21 0  bole-bedroom-may      "idle")
               (list 22 0  bole-bed-melvin "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (melvin-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Melvin is the tavern-keeper.
;;----------------------------------------------------------------------------
(define (melvin-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by the tavern when I'm open. I open at 7:00AM and close "
           "at midnight.")
      (begin
        (kern-conv-trade knpc kpc
                         (list t_food 3)
                         )
        )))

;; basics...
(define (melvin-default knpc kpc)
  (say knpc "Ask May about that, I don't know."))

(define (melvin-hail knpc kpc)
  (say knpc "[You meet a hungover chef] Hello, partner."))

(define (melvin-name knpc kpc)
  (say knpc "I'm Melvin, the cook."))

(define (melvin-job knpc kpc)
  (say knpc "I run the Bole Inn & Tavern with my wife, May. I cook, she "
       "serves."))

(define (melvin-join knpc kpc)
  (say knpc "I wouldn't be much good to you, partner. Better leave me here "
       "to my kitchen where at least I can cook you some grub."))

(define (melvin-bye knpc kpc)
  (say knpc "Farewell, partner. Come back any time you're hungry."))

;; other characters & town...
(define (melvin-may knpc kpc)
  (say knpc "My wife May is an ornery woman, but sharp as a tack."))

(define (melvin-kath knpc kpc)
  (say knpc "That red lady is  a wicked beauty! But I'm neither drunk nor "
       "fool enough to mingle with wizards business. Take my advice, stay "
       "far away from her and that thing she travels with!"))

(define (melvin-bill knpc kpc)
  (say knpc "He's missing a screw or too, but he's a good lad."))

(define (melvin-thud knpc kpc)
  (say knpc "That's no man traveling with the red lady, but I've never seen a "
       "troll that could talk so well. I'm not sure what it is, but magic had "
       "a hand in making it."))

(define (melvin-bole knpc kpc)
  (say knpc "It's a nice enough place."))

(define (melvin-hack knpc kpc)
  (say knpc "Hackle lives in the northwest corner of town across the bridge. "
       "She's a crazy old hedge-witch but harmless."))


;; thief quest...
(define (melvin-thie knpc kpc)
  (say knpc "There was a strange rogue who came in here not long ago, "
       "looking like he had demons hounding his trail. He left shortly after "
       "the red lady arrived. Spoke with Hackle before he left, I think. "
       "Most odd."))

;; misc...
(define (melvin-wiza knpc kpc)
  (say knpc "There's foul business afoot! There's something going on between "
       "that red sorceress and that mysterious rogue, I know it."))

(define (melvin-inn knpc kpc)
  (say knpc "If you need a room for the night or something cold to wash the "
       "dust from your throat, talk to May. But if you're hungry just say so "
       "and I'll fix you right up!"))

(define (melvin-hung knpc kpc)
  (say knpc "Are you hungry?")
  (if (kern-conv-get-yes-no? kpc)
      (melvin-trade knpc kpc)
      (say knpc "Well if you get hungry just say so!")))

(define melvin-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default melvin-default)
       (method 'hail melvin-hail)
       (method 'bye  melvin-bye)
       (method 'job  melvin-job)       
       (method 'name melvin-name)
       (method 'join melvin-join)

       (method 'food melvin-trade)
       (method 'trad melvin-trade)
       (method 'buy  melvin-trade)
       (method 'sell melvin-trade)

       (method 'bill melvin-bill)
       (method 'cook melvin-inn)
       (method 'inn  melvin-inn)
       (method 'kath melvin-kath)
       (method 'red  melvin-kath)
       (method 'lady melvin-kath)
       (method 'sorc melvin-kath)
       (method 'may  melvin-may)
       (method 'hack melvin-hack)
       (method 'hung melvin-hung)
       (method 'tave melvin-inn)
       (method 'thud melvin-thud)
       (method 'thin melvin-thud)
       (method 'pet  melvin-thud)

       (method 'thie melvin-thie)
       (method 'rogu melvin-thie)
       (method 'char melvin-thie)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-melvin)
  (bind 
   (kern-mk-char 'ch_melvin          ; tag
                 "Melvin"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_townsman          ; sprite
                 faction-men         ; starting alignment
                 2 0 1             ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'melvin-conv        ; conv
                 sch_melvin          ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (melvin-mk)))
