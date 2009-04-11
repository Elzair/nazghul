;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define the-man-start-lvl 9)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In The MAN's hideout, a secure undisclosed location.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_man
               (list 0  0 mans-bed    "sleeping")
               (list 7  0 mans-supper "eating")
               (list 8  0 mans-hall   "idle")
               (list 12 0 mans-supper "eating")
               (list 13 0 mans-tools  "idle")
               (list 15 0 mans-hall   "idle")
               (list 18 0 mans-supper "eating")
               (list 19 0 mans-dock   "idle")
               (list 22 0 mans-bed    "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (man-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Yvonne, known as "The MAN", is a female rogue of pre-eminent skill,
;; living in a secure undisclosed location (The MAN's hideout).
;; 
;;----------------------------------------------------------------------------

;; Basics...
(define (man-hail knpc kpc)
  (say knpc "[You meet an attractive, middle-aged woman with a catlike poise "
       "and easy bearing] Hello, Good-looking."))

(define (man-default knpc kpc)
  (say knpc "You've got me there, hotshot."))

(define (man-name knpc kpc)
  (say knpc "I'm Yvonne. But everybody calls me The MAN."))

(define (man-join knpc kpc)
  (say knpc "Can't say I'm not tempted, handsome, but when it comes to "
       "business I like to work alone."))

(define (man-job knpc kpc)
  (say knpc "Hm. How shall I put this? I suppose the truth is easiest: "
       "I'm a Wrogue."))

(define (man-bye knpc kpc)
  (say knpc "Bye bye..."))


;; Misc
(define (man-man knpc kpc)
  (say knpc "[She laughs easily] Were you expecting someone... manlier?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "The MAN is an acronym for the Mistress of Acquisitive "
       "Nature. Do you like it?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "I think it was the Engineer who originally coined it. "
       "You know those engineering types, suckers for an acronym."))

(define (man-wrog knpc kpc)
  (say knpc "A Wrogue specializes in breaking rules. Tell me, stranger, "
       "do you like to break the rules?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "[She feigns a shocked look. It really is quite fetching.] "
           "Naughty boy! I may have to edify your character with a "
           "spanking before you leave.")
      (say knpc "Oh, so you're a square? I do like squares. Perhaps you'll "
           "rub off on me. [She gives you a cutely innocent look]")))

(define (man-rule knpc kpc)
  (say knpc "A rule, a lock, a secret, a riddle. I love to break them all "
       "and see what lies behind. It's curiousity mainly, and the challenge "
       "of the crack. Look around, do you see much wealth here in my home?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Sigh. Either you come from a poor childhood or you haven't "
           "bothered to look.")
      (say knpc "No, despite my moniker I'm not that acquisitive. There's no "
           "pleasure for me in HAVING, only in DOING. Once a thing is done "
           "I've no further interest in it. The most interesting and valuable "
           "things I've acquired are secrets.")))

(define (man-secr knpc kpc)
  (say knpc "Just try me."))

(define (man-enem knpc kpc)
  (say knpc "The Warritrix has a mighty enemy in the person of the "
       "Stewardess of Glasdrin."))

(define (man-stew knpc kpc)
  (say knpc "I read her diary once. She really ought to keep it locked up "
       "better. She hates the Warritrix for defying her at Absalot."))

(define (man-hate knpc kpc)
  (say knpc "You can read the Stewardess's diary for yourself. "
       "Would you like to know how?")
  (if (yes? kpc)
      (say knpc "Just remember two little words: Wis Quas. "
           "I'm sure a sharp guy like you can figure out the rest.")
      (say knpc "Oh, but you're missing out! "
           "It's simply dripping with political intrigue.")))


;; Wise Queries
(define (man-wiza knpc kpc)
  (say knpc "Yes, they're powerful. But I've pilfered their secrets."))

(define (man-wrig knpc kpc)
  (say knpc "A clever bunch, but they've yet to design a lock I can't crack."))

(define (man-warr knpc kpc)
  (say knpc "The Warritrix is legendary for her noble demeanor. I do not know "
       "her well, but I do know her enemies, perhaps better than she."))

(define (man-necr knpc kpc)
  (say knpc "A useful fellow, the dead know many things forgotten by the "
       "living, and he knows how to charm them."))

(define (man-alch knpc kpc)
  (say knpc "I understand he's a clever little worm, but I don't have much "
       "to do with him."))

(define (man-engi knpc kpc)
  (say knpc "I'd love to challenge myself with a lock of his design, but "
       "alas, he keeps no secrets or treasure to lock up!"))

(define (man-ench knpc kpc)
  (say knpc "My most favorite wizard. I do love to hear him go on and on in "
       "that virtuous vein of his! But he really should keep his stuff more "
       "secure."))

;; Accursed Queries
(define (man-accu knpc kpc)
  (say knpc "They speak of freedom, by which they mean the freedom to enslave others. "
       "Some who would break the rules join them, only to find themselves enchained. "
       "A nasty bunch."))


;; Rune
(define (man-rune knpc kpc)
  (say knpc "Runes... I once knew a pirate that carried a Rune. Have you "
       "heard the tale of Ghertie and the Merciful Death?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "You are already well-traveled. I like an experienced man.")
      (say knpc "Ask around Oparine about Ghertie."))
  (say knpc "If you can find where the Merciful Death lies you can raise it "
       "with a spell. Do you know the spell?")
  (if (kern-conv-get-yes-no? kpc)
      (say knpc "Well, aren't we the accomplished magician? I hope you don't "
           "know any love spells, Wizard!")
      (begin
			(say knpc "Mix mandrake, blood moss and spider silk, then chant Vas "
			"Uus Ylem next to the spot where the ship has sunk.")
			(quest-data-update 'questentry-rune-c 'shipraise 1)
		)
	)
  (say knpc "Ghertie will not give up the location of her ship freely. But "
       "even the dead have desires, indeed that is the worst part of "
       "death! And Ghertie desires nothing more than ^c+mrevenge^c-. Mark that word "
       "well, and remember it when you meet her ghost.")
	(quest-data-update 'questentry-rune-c 'info 1)
	(quest-data-assign-once 'questentry-rune-c)
	(quest-data-update 'questentry-ghertie 'ghertieid 1)
	(quest-data-update-with 'questentry-ghertie 'revenge 1 (quest-notify nil))
	(quest-data-assign-once 'questentry-ghertie)
	)

(define (man-chan knpc kpc)
  (say knpc "So you've met Chanticleer? No doubt HE was the one who told you where to find me! "
       "[Sigh] Well, I hope the irresponsible wrogue is well."))

(define man-conv
  (ifc basic-conv

       ;; basics
       (method 'default man-default)
       (method 'hail man-hail)
       (method 'bye  man-bye)
       (method 'job  man-job)
       (method 'name man-name)
       (method 'join man-join)
       
       ;; special
       (method 'man man-man)
       (method 'wrog man-wrog)
       (method 'rogu man-wrog)  ;; a synonym
       (method 'rule man-rule)
       (method 'secr man-secr)
       (method 'enem man-enem)
       (method 'stew man-stew)
       (method 'diar man-hate)
       (method 'evid man-hate)
       (method 'hate man-hate)
       (method 'wiza man-wiza)
       (method 'wrig man-wrig)
       (method 'warr man-warr)
       (method 'necr man-necr)
       (method 'alch man-alch)
       (method 'engi man-engi)
       (method 'ench man-ench)
       (method 'accu man-accu)
       (method 'rune man-rune)
       (method 'chan man-chan)
       ))

(define (mk-the-man)
  (bind 
   (kern-mk-char 'ch_man           ; tag
                 "The MAN"             ; name
                 sp_human            ; species
                 oc_wrogue           ; occ
                 s_brigandess        ; sprite
                 faction-men         ; starting alignment
                 0 3 10               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health ; hp
                 -1                   ; xp
                 max-health ; mp
                 0
                 the-man-start-lvl
                 #f                  ; dead
                 'man-conv         ; conv
                 sch_man           ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_armor_leather_4
                 		t_leather_helm_2
                 		t_magic_axe)                 ; readied
                 )
   (man-mk)))
