;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define abe-lvl 3)
(define abe-species sp_human)
(define abe-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Green Tower
;;----------------------------------------------------------------------------
(define abe-bed gt-abe-bed)
(define abe-mealplace gt-ws-tbl2)
(define abe-workplace gt-ruins)
(define abe-leisureplace gt-ws-hall)
(kern-mk-sched 'sch_abe
               (list 0  0 abe-bed          "sleeping")
               (list 7  0 abe-mealplace    "eating")
               (list 8  0 abe-workplace    "working")
               (list 12 0 abe-mealplace    "eating")
               (list 13 0 abe-workplace    "working")
               (list 18 0 abe-mealplace    "eating")
               (list 19 0 abe-leisureplace "idle")
               (list 22 0 abe-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (abe-mk) (list #f))
(define (abe-met? gob) (car gob))
(define (abe-met! gob) (set-car! gob #t))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Abe is a scholar who knows much of the runes.
;; He fled from Absalot with the Alchemist, and now lives in Green Tower.
;;----------------------------------------------------------------------------

;; Basics...
(define (abe-hail knpc kpc)
  (kern-print "You meet a young, bookish-looking fellow.\n")
  (if (abe-met? (gob knpc))
      (say knpc "Hello again.")
      (begin
        (abe-met! (gob knpc))
        (say knpc "Hello. Say, aren't you a Wanderer?")
        (if (yes? kpc)
            (say knpc "I am most honored! I can't believe my good fortune. "
                 "I have so many questions for you. When you get the time. "
                 "If you don't mind.")
            (say knpc "Oh, of course not. Sorry. I just thought... never mind.")))))

(define (abe-default knpc kpc)
  (say knpc "I'll look that up in the archives when I get a chance."))

(define (abe-name knpc kpc)
  (say knpc "Oh. Yes. I'm Abe."))

(define (abe-join knpc kpc)
  (say knpc "Oh, no, I couldn't possibly... I'm not really that sort of person."))

(define (abe-job knpc kpc)
  (say knpc "I'm a scholar. I'm studying the ruins here in Green Tower. Have you examined them?")
  (if (no? kpc)
      (say knpc "They're in the southwest corner of town. Fascinating.")
      (begin
        (say knpc "Did you know there are more below the surface?")
        (yes? kpc)
        (say knpc "Yes, just like Absalot!"))))

(define (abe-absa knpc kpc)
  (say knpc "Not many know that beneath Absalot is an older city. "
       "The ruins beneath Absalot are very similar to the ruins here in Green Tower. "
       "I am certain they were built by the same civilization!"))

(define (abe-rune knpc kpc)
  (if (any-in-inventory? kpc rune-types)
      ( (say knpc "\n[He whistles softly]\n"
	     "You have some of the Eight Keys to the Demon Gate?\n"
	     "I shall examine them for you!")
	(if (any-in-inventory? kpc t_rune_k)
	    (say kpnc "[He examines a rune] This is the Rune of KNOWLEDGE!")
	    )
	(if (any-in-inventory? kpc t_rune_p)
	    (say kpnc "[He examines a rune] This is the Rune of POWER!")
	    )
	(if (any-in-inventory? kpc t_rune_s)
	    (say kpnc "[He examines a rune] This is the Rune of SKILL!")
	    )
	(if (any-in-inventory? kpc t_rune_c)
	    (say kpnc "[He examines a rune] This is the Rune of CURIOUSITY!")
	    )
	(if (any-in-inventory? kpc t_rune_f)
	    (say kpnc "[He examines a rune] This is the Rune of FREEDOM!")
	    )
	(if (any-in-inventory? kpc t_rune_w)
	    (say kpnc "[He examines a rune] This is the Rune of WISDOM!")
	    )
	(if (any-in-inventory? kpc t_rune_d)
	    (say kpnc "[He examines a rune] This is the Rune of DISCRETION!")
	    )
	(if (any-in-inventory? kpc t_rune_l)
	    (say kpnc "[He examines a rune] This is the Rune of LEADERSHIP!")
	    )
	(if (has-all-runes? kpc) 
	    (say knpc "This is incredible!\n"
		 "You have all of the Eight Keys to the Demon Gate!!!\n"
		 "What do you intend to do with them?")
	    (say knpc "Legend says that there are eight runes in all, are you seeking the others?")
	    )
	)
      (say knpc "There are many runes. Perhaps if you brought me an example...?")))

(define (abe-demo knpc kpc)
  (say knpc "The Demon Gate was sealed shut by the Wise long ago. "
       "Its location was blotted from all records, but legend puts it somewhere to the north. "
       "Are you looking for the other Runes?")
  (if (yes? kpc)
      (say knpc "I know a legend which tells of one kept in a temple in the void.")
      (say knpc "Oh.")
       ))

(define (abe-void knpc kpc)
  (say knpc "The Shard, the moons and the stars all habitate a great void. "
       "The ancients could sail across the void in ships, the way we sail across a sea!"))

(define (abe-ship knpc kpc)
  (say knpc "I know of the void ships, but not how they worked. "
       "Even the Master Wrights have lost the technique for making them."
       ))

(define (abe-wrig knpc kpc)
  (say knpc "Wrights specialize in the making of things. "
       "The Engineer is the greatest living Wright."))

(define (abe-keys knpc kpc)
  (say knpc "Yes, the Demon Gate was locked with eight locks and the keys separated. "
       "Each takes the form of a powerful rune. "
       "They've been lost or hidden since then. Are you looking for them?")
  (if (yes? kpc)
      (say knpc "There's a legend that King Clovis carried one as a charm. "
           "He fell in battle during the goblin wars, but it was never found on his body. "
           "Perhaps someone (perhaps a GOBLIN!) looted his corpse and took it.")
      (say knpc "Just idle curiosity? Believe me, I understand.")))

(define (abe-quee knpc kpc)
  (say knpc "I'm not sure what you're insinuating."))

(define (abe-civi knpc kpc)
  (say knpc "I don't know much about the civilization that built the ruins, but clues indicate it was quite wicked by our standards! Do you know what I mean?")
  (if (yes? kpc)
      (say knpc "Then I won't mention it!")
      (say knpc "Human sacrifice, cannibalism, demon worship. Accursed practices!")))

(define (abe-accu knpc kpc)
  (say knpc "Yes, the Accursed have a long history. They may be a convenient political bogeyman, "
       "but there is enough evidence to suggest they do, or did, exist."))

(define (abe-bye knpc kpc)
  (say knpc "Let me know if you find any more ruins!"))

(define (abe-alch knpc kpc)
  (say knpc "Ah, how is the secretive old rascal? I haven't seen him since we were neighbors."))

(define (abe-neig knpc kpc)
  (say knpc "In Absalot. Before we had to flee."))

(define (abe-flee knpc kpc)
  (say knpc "[He sighs] It's a long story. Ask around. It doesn't matter anymore."))

(define (abe-gobl knpc kpc)
  (say knpc "Ask Deric or Gen about goblins, they have a lot of experience with them."))

(define abe-conv
  (ifc green-tower-conv

       ;; basics
       (method 'default abe-default)
       (method 'hail abe-hail)
       (method 'bye abe-bye)
       (method 'job abe-job)
       (method 'name abe-name)
       (method 'join abe-join)
       

       (method 'absa abe-absa)
       (method 'rune abe-rune)
       (method 'demo abe-demo)
       (method 'keys abe-keys)
       (method 'key  abe-keys)
       (method 'demo abe-keys)
       (method 'gate abe-keys)
       (method 'quee abe-quee)
       (method 'civi abe-civi)
       (method 'accu abe-accu)
       (method 'bye  abe-bye)
       (method 'ruin abe-job)

       (method 'void abe-void)
       (method 'ship abe-ship)
       (method 'sail abe-ship)
       (method 'wrig abe-wrig)
       (method 'alch abe-alch)
       (method 'neig abe-neig)
       (method 'flee abe-flee)
       (method 'gobl abe-gobl)
       ))

(define (mk-abe)
  (bind 
   (kern-mk-char 
    'ch_abe           ; tag
    "Abe"             ; name
    abe-species         ; species
    abe-occ              ; occ
    s_companion_wizard ; sprite
    faction-men      ; starting alignment
    2 1 1            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    abe-lvl
    #f               ; dead
    'abe-conv         ; conv
    sch_abe           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_staff
					         t_armor_leather
					         )              ; readied
    )
   (abe-mk)))
