;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Glasdrin.
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ini
               (list 0  0  gi-bed      "sleeping")
               (list 5  0  gs-altar    "idle")
               (list 6  0  gc-train    "working")
               (list 12 0  ghg-s2      "eating")
               (list 13 0  gc-hall     "working")
               (list 18 0  ghg-s2      "eating")
               (list 19 0  ghg-hall    "idle")
               (list 21 0  gi-bed      "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ini-mk) (list 'townsman #f))
(define (ini-will-join? ini) (cadr ini))
(define (ini-will-join! ini) (set-car! (cdr ini) #t))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Ini is a dispirited paladin, dwelling in Glasdrin.
;; He is loyal to the ideals lived by the Warritrix,
;; and offended by the corruption which produced her
;; assassination.
;; Ini is a potential party member.
;;----------------------------------------------------------------------------

;; Basics...
(define (ini-hail knpc kpc)
  (say knpc "[You meet a morose paladin] Hi."))

(define (ini-default knpc kpc)
  (say knpc "I can't help you with that."))

(define (ini-notyet knpc kpc)
  (say knpc "I probably shouldn't talk to civilians about that."))

(define (ini-name knpc kpc)
  (say knpc "I am Inago, but everyone calls me Ini."))

(define (ini-join knpc kpc)
  (if (is-player-party-member? knpc)
      (say knpc "I already joined you. Now let's go!")
      (let ((ini (kobj-gob-data knpc)))
        (if (ini-will-join? ini)
            (begin
              (say knpc "I thank you! Let's not waste any time finding "
                   "the Warritrix!")
              (kern-conv-end)
              (join-player knpc))
            (say knpc "[Sigh] My duty is with the paladins.")
            ))))
        
(define (ini-lost knpc kpc)
   (let ((ini (kobj-gob-data knpc)))
     (if (ini-will-join? ini)
			(begin
  			(say knpc "The entrance to the Lost Halls is a cavern far to the southwest. "
  				"Find a ship and sail to ["
           (loc-x lost-halls-loc) ","
           (loc-y lost-halls-loc) "].")
			(quest-data-update-with 'questentry-rune-l 'know-hall 1 (quest-notify nil))
			(quest-data-update 'questentry-warritrix 'lost-hall-loc 1)
			)
  			(say knpc "The Lost Halls are very dangerous. I'd advise you to stay clear!"))))
     		
(define (ini-cave knpc kpc)
   (let ((ini (kobj-gob-data knpc)))
     (if (ini-will-join? ini)
     		(begin
     			(say knpc "The Lost Halls themselves are deep within the caverns.")
     			(say knpc (if (is-player-party-member? knpc) "We" "You") " will need to look for the great stairs, in the northern part of the caves.")
     			(say knpc "Beware of the dungeon's inhabitants!")
     			)
     		(ini-notyet knpc kpc))))
     		
(define (ini-inha knpc kpc)
   (let ((ini (kobj-gob-data knpc)))
     (if (ini-will-join? ini)
     		(begin
     			(say knpc "Everytime we try to clear the place out, another band of gints or trolls decides to move in.")
     			(say knpc (if (is-player-party-member? knpc) "We" "You") "'d best be prepared for a long, hard battle.")
     			)
     		(ini-notyet knpc kpc))))

(define (ini-stair knpc kpc)
   (let ((ini (kobj-gob-data knpc)))
     (if (ini-will-join? ini)
     		(begin
     			(say knpc "I know the stairs are somewhere in the north, but I'm afraid I've never been that far, so I don't know their precise location.")
     			)
     		(ini-notyet knpc kpc))))
     			
(define (ini-job knpc kpc)
  (say knpc "I'm a paladin. But I don't like it very much."))

(define (ini-bye knpc kpc)
  (say knpc "So long."))

(define (ini-warr knpc kpc)
  (cond ((player-stewardess-trial-done?)
                (say knpc "Justice has been served, and now I can grieve her loss." ))
	((player-found-warritrix?)
                (if (ask? knpc kpc "The Stewardess is powerful, but there is a way to see that justice is done. An ancient way. A dangerous way. Will you hear me?")
                    (say knpc "There is a statue in the center of Glasdrin. "
                         "If you strike it with your sword, you will invoke the oldest law of the city, and a trial will be held. "
                         "But we dare not strike it until we have convincing evidence against the Stewardess, because if our evidence is lacking then judgment will be passed on us instead!")
                    (say knpc "We can't let the Stewardess get away with this. Someone must call her to account!")))
	((quest-data-assigned? 'questentry-wise)
		 (say knpc "[He straightens up a bit] Something is amiss! The Warritrix has "
		      "been gone too long with no word. The Commander should have sent out "
		      "search parties by now. Instead he sits idly by, pretending to be "
		      "distracted by other problems. I sense foul play. Will you search for "
		      "her?")
		 (if (kern-conv-get-yes-no? kpc)
		     (begin
		       (say knpc "I would join you! I know the deeps well, and though my "
			    "duty is here, I won't obey an order that leaves her to die.")
		       (ini-will-join! (kobj-gob-data knpc)))
		     (say knpc "Someone must do something! The realm owes her a great "
			  "debt.")))
	(else
		(say knpc "Are you looking for the Warritrix?")
		(if (kern-conv-get-yes-no? kpc)
			(begin
				(say knpc "She can be hard to find. I believe she is out on patrol right now. ")
				(say knpc "If you want come back later, I'll keep an eye out and let you know if she's back")
			)
		))
	))

;; Paladin...
(define (ini-pala knpc kpc)
  (say knpc "I've been a paladin my whole life. I'm not very good at it; "
       "I get sick before and after every battle. I'm surprised they "
       "let me stay in, but I guess they need the warm bodies. "
       "I would have quit long ago but I don't know what else to do."
       ))

(define (ini-quit knpc kpc)
  (say knpc "I've managed to save some pay. I'll retire soon, buy a farm "
       "near Trigrave, get away from this place. Just think: no more long "
       "marches, no more sleeping on stony ground in the lightless deep, "
       "no more waking up to monsters eating your squad for breakfast."
       ))

;; Townspeople...
(define (ini-glas knpc kpc)
  (say knpc "Kind of a dreary place, don't you think?")
  (kern-conv-get-yes-no? kpc)
  (say knpc "I've always wanted to visit Green Tower, see the trees."))

(define (ini-ange knpc kpc)
  (say knpc "A modest lady. I once saw her spit a cave goblin with a dagger."))

(define (ini-spit knpc kpc)
  (say knpc "She was assigned to my squad on a standard patrol in the "
       "hills. We'd just barely survived an encounter with gints when some "
       "cave goblins decided we looked like easy pickings. "
       "It was dicey there for a while."))

(define (ini-patc knpc kpc)
  (say knpc "I owe him my life."))

(define (ini-life knpc kpc)
  (say knpc "I was killed once. We were in the deeps on patrol, dead tired "
       "after fleeing a party of death knights that killed our medik, "
       "and we camped right in the middle of a party of sleeping trolls. ")
  (prompt-for-key)
  (say knpc
       "They woke up first. One minute I was fighting for my life, "
       "and the next I was in the hospital looking up at Doc Patch. "
       "Nobody else survived from my squad."
       ))

(define ini-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'default ini-default)
       (method 'hail ini-hail)
       (method 'bye  ini-bye)
       (method 'job  ini-job)
       (method 'name ini-name)
       (method 'join ini-join)

       (method 'warr ini-warr)
       (method 'pala ini-pala)
       (method 'quit ini-quit)
       (method 'glas ini-glas)
       (method 'ange ini-ange)
       (method 'spit ini-spit)
       (method 'dagg ini-spit)
       (method 'patc ini-patc)
       (method 'life ini-life)
       
       (method 'lost ini-lost)
       (method 'hall ini-lost)
       (method 'cave ini-cave)
       (method 'entr ini-cave)
       (method 'stai ini-stair)
       (method 'nort ini-stair)
       (method 'deep ini-stair)
       (method 'grea ini-stair)
       (method 'inha ini-inha)
       (method 'bewa ini-inha)
       (method 'dung ini-inha)
       
       ))
       
(define (mk-ini)
  (bind 
   (kern-mk-char 'ch_ini           ; tag
                 "Ini"             ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_companion_paladin ; sprite
                 faction-glasdrin         ; starting alignment
                 5 0 5               ; str/int/dex
                  pc-hp-off  ; hp bonus
                  pc-hp-gain ; hp per-level bonus
                  0 ; mp off
                  0 ; mp gain
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'ini-conv         ; conv
                 sch_ini           ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_halberd
                       ))         ; readied
   (ini-mk)))
