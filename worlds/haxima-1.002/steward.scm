;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Glasdrin
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_stew
               (list 0  0  gv-bed       "sleeping")
               (list 7  0  ghg-s4       "eating")
               (list 8  0  gc-hall "idle")
               (list 12 0  ghg-s1       "eating")
               (list 13 0  gc-hall "idle")
               (list 18 0  ghg-s1       "eating")
               (list 19 0  gc-hall "idle")
               (list 20 0  gv-bed       "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (stew-mk) (list 'townsman #f))
(define (stew-met? stew) (cadr stew))
(define (stew-met! stew) (set-car! (cdr stew) #t))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Victoria is the temporal ruler of Glasdrin, 
;; bearing the title of Stewardess.
;;----------------------------------------------------------------------------

;; Basics...
(define (stew-hail knpc kpc)
  (if (not (stew-met? (kobj-gob-data knpc)))
      (begin
        (say knpc "[You meet a stately woman with an air of authority] "
             "Hail, Wanderer. I have heard of your coming.")
        (stew-met! (kobj-gob-data knpc)))
      (say knpc "[You meet a stately woman with an air of authority] "
           "Hail again, Wanderer.")))

(define (stew-default knpc kpc)
  (say knpc "I cannot help you with that."))

(define (stew-name knpc kpc)
  (say knpc "I am Victoria, Stewardess of Glasdrin."))

(define (stew-join knpc kpc)
  (say knpc "How bold!"))

(define (stew-job knpc kpc)
  (say knpc "I am the Stewardess of Glasdrin."))

(define (stew-bye knpc kpc)
  (say knpc "Keep to the path, Wanderer."))

;; Warritrix...
(define (stew-warr knpc kpc)
  (cond ((player-found-warritrix?)
      (if (ask? knpc kpc "I heard of her passing. Surely you do not think I had anything to do with it?")
          (begin
            (say knpc "If you dare to accuse me, strike the statue in the courtyard. "
                 "But I warn you: you are a homeless vagrant, and I am ruler of the most powerful city in the land. "
                 "It will be my word against yours, and we deal harshly with false accusers.")
            (aside kpc 'ch_ini "Doubt not, you treacherous witch, that we will accuse you, "
                   "and with more than just words.")
            )
          (say knpc "Of course not. Ignore the rumours. They are spread by jealous political rivals.")
          ))
      ((quest-data-assigned? 'questentry-wise)
		(say knpc "I have not seen her in some time. I believe she has been called away on some errand.")
		(quest-data-update 'questentry-warritrix 'assignment 1)
		)
	(else
		(say knpc "One of our finest warriors. I believe she is out on patrol right now.")
		 (quest-data-update 'questentry-warritrix 'general-loc 1)
		 )
      ))

(define (stew-erra knpc kpc)
  (say knpc "Speak with Commander Jeffries, he may know the details."))

;; Steward...
(define (stew-stew knpc kpc)
  (say knpc "Glasdrin is ruled by an elected Steward, "
       "who is charged with the safekeeping of the City and the Realm. "
       ))

(define (stew-real knpc kpc)
  (say knpc "The Realm of Glasdrin extends west to the Fens and southeast to "
       "the Great Wood. We also maintain a presence between the surface and "
       "the Underworld."))

;; Rune...
(define (stew-rune knpc kpc)
	(if (quest-data-assigned? 'questentry-wise)
		(say knpc "I know the Warritrix wore... wears a Rune about her neck, "
			"but I know not what it signifies.")
		(say knpc "I know the Warritrix wears a Rune about her neck, "
			"but I know not what it signifies.")
		)
       (quest-data-assign-once 'questentry-rune-l)
       )

(define (stew-wore knpc kpc)
	(if (quest-data-assigned? 'questentry-wise)
		(say knpc "What are you implying? It was merely a slip of the tongue.")
		(stew-default knpc kpc)
	))

;; Absalot...
(define (stew-absa knpc kpc)
  (say knpc "Absalot was a cancer on the land. It had to be removed "
       "completely before its evil spread. Surely you see the wisdom "
       "of this?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "The loss of innocent lives is always a tragedy, but it is "
           "impossible to fight wars without them.")
      (say knpc "What do you know of affairs of state? You are little more "
           "than a common rogue.")))

(define (stew-inno knpc kpc)
  (say knpc "Not all the citizens of Absalot were wicked, "
       "but they tolerated wickedness in their midst."))

(define (stew-wick knpc kpc)
  (say knpc "The people of Absalot engaged in human sacrifice and "
       "demon-worship. They began to have converts in other cities, even "
       "here."))

(define (stew-conv knpc kpc)
  (say knpc "When we learned of them, we burned them at the stake. "
       "I met with the Wise and the leaders of the other cities to decide "
       "what to do.")
  (prompt-for-key)
  (say knpc "Some of the Wise were squeamish, but in the end "
       "we overruled their dissent and formed a coalition army. "
       "It marched on Absalot, razed it and sealed "
       "the passage so that none may build on that site ever again."))

(define (stew-wise knpc kpc)
  (say knpc "The Wise are an aid to the rulers of the cities, but alone they "
       "have little power. Even the Enchanter could not stand against the "
       "armed might of Glasdrin should he ever stand in our way.")
       (quest-wise-subinit 'questentry-enchanter)
       )

(define (stew-rogu knpc kpc)
  (say knpc "Where do you come from? What is your purpose in our land? "
       "For all we know you are a spy, a scout at the vanguard of an alien "
       "army. Are you?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Hmm. If I believed you I would throw you to the inquisitors "
           "and torture the truth out of you. But I think you are a poser "
           "and a self-inflated fool. You will vanish into obscurity like "
           "the Wanderers that preceded you.")
      (say knpc "No, I think you are merely a vagabond, here by "
           "accident. All the same, I will be watching you carefully, "
           "Wanderer. If you betray my city you will know my wrath at the "
           "hands of our torturers."))
  (kern-conv-end))

;; Townspeople...
(define (stew-glas knpc kpc)
  (say knpc "Glasdrin is a beacon of light in these dark times. "
       "Time and again her paladins have sacrificed for the good of "
       "the realm, and turned back the tide of darkness. The Peninsula "
       "owes much to this city."))

(define (stew-unde knpc kpc)
  (say knpc "The Underworld is a sunless realm where monsters breed. "
       "I am committed to purging it of these evil creatures, "
       "for they are a menace to all."))

(define stew-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default stew-default)
       (method 'hail stew-hail)
       (method 'bye  stew-bye)
       (method 'job  stew-job)
       (method 'name stew-name)
       (method 'join stew-join)

       (method 'city stew-glas)
       (method 'glas stew-glas)
       (method 'warr stew-warr)
       (method 'erra stew-erra)
       (method 'stew stew-stew)
       (method 'real stew-real)
       (method 'absa stew-absa)
       (method 'wore stew-wore)
       (method 'rune stew-rune)
       (method 'inno stew-inno)
       (method 'wick stew-wick)
       (method 'conv stew-conv)
       (method 'wise stew-wise)
       (method 'rogu stew-rogu)
       (method 'unde stew-unde)
       ))

(define (mk-steward)
  (bind 
   (kern-char-force-drop
    (kern-mk-char 'ch_steward         ; tag
                  "Victoria"          ; name
                  sp_human            ; species
                  nil                 ; occ
                  s_lady              ; sprite
                  faction-glasdrin         ; starting alignment
                  1 3 0               ; str/int/dex
                  0 0                 ; hp mod/mult
                  0 0                 ; mp mod/mult
                  max-health -1 max-health 0 6  ; hp/xp/mp/AP_per_turn/lvl
                  #f                  ; dead
                  'stew-conv          ; conv
                  sch_stew            ; sched
                  'townsman-ai                 ; special ai
                  (mk-inventory (list (list 1 t_sword_4)
                                      (list 1 t_stewardess_chest_key)
                                      ))    ; container
                  nil                 ; readied
                  )
    #t)
   (stew-mk)))
