;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define nossifer-x 16)
(define nossifer-y 6)
(define demon-gate-x 16)
(define demon-gate-y 6)
(define noss-lvl 9)
(define noss-species sp_balron)
(define noss-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_noss
               (list 0  0 (list 'p_ankh_shrine 10 7 11 6) "idle"))

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (noss-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (noss-hail knpc kpc)
  (say knpc "[You confront an enormous being of blue flame with a voice like "
       "an earthquake] Greetings, little Man. I have been waiting for you for "
       "a long time now."))

(define (noss-default knpc kpc)
  (say knpc "[He is silent, save for the rumbling of his breath]"))

(define (noss-name knpc kpc)
  (say knpc "I am Nossifer, the Sleeper."))

(define (begin-last-battle knpc kpc)
  (say knpc "Prepare what magic and weapons you have, little Man, "
       "I prefer to face a challenge before I send your soul into "
       "the void!")
  (kern-being-set-base-faction knpc faction-demon)
  (kern-conv-end))

(define (open-demon-gate knpc kpc)
  (say knpc "At last! Today my exile will end, "
       "and I will become a legend among my people!")
  (let* ((loc (mk-loc (loc-place (kern-obj-get-location knpc))
                      demon-gate-x
                      demon-gate-y))
         (gate (mk-moongate nil))
         (stages (list (list '()                       0)
                       (list s_blackgate_quarter        32)
                       (list s_blackgate_half           64)
                       (list s_blackgate_three_quarters 96)
                       (list s_blackgate_full           128))))

    (kern-obj-put-at gate loc)
    (moongate-animate gate stages)
    (say knpc "Come forth, legions!")
    (kern-sleep 5000)
    (say knpc "Forward, to battle!")
    (kern-sleep 5000)
    (say knpc "Legions?")
    (kern-sleep 5000)
    (say knpc "Anybody in there?")
    (kern-sleep 5000)
    (say knpc "Hm. That's funny. Where the hell is everybody?")
    (kern-sleep 5000)
    (say knpc "Well. Damn, I wish I'd known that sooner. "
         "I guess I'll just have to conquer this world by myself.")
    (begin-last-battle knpc kpc)))

(define (noss-job knpc kpc)
  (say knpc "You've heard no doubt how Wizard's summon Demons to do their bidding?")
  (if (no? kpc)
      (say knpc "It was a rhetorical question. Anyway..."))
  (say knpc "I summon Men to do mine. Specifically, I summon Wanderers. "
       "I open the Shrine Gate, I call them through. I have summoned them "
       "all. And that means, of course, that I summoned you.")
  (prompt-for-key)
  (say knpc "Do you know what this means, little Man?")
  (if (no? kpc)
      (say knpc "I wonder how one so stupid could have made it so far. "
           "It means, fool, that you are my servant, here to do my bidding.")
      (say knpc "That is correct. You serve me now. In fact, you always have."))
  (prompt-for-key)
  (say knpc "A thousand years ago I passed through the Demon Gate to scout "
       "the strength of this world. The cursed Wizards of that age, "
       "not knowing I was here, locked the Gate before I could return. ")
  (prompt-for-key)
  (say knpc "I have waited all this time, hiding in the deeps, "
       "calling forth Wanderers time and again, "
       "waiting for the one who could reassemble the Runes. "
       "They all failed, or became Wise to my presence. "
       "You alone were able enough, and foolish enough, to succeed.")
  (prompt-for-key)
  (say knpc "Now, you will give me the Runes. "
           "And then, my servant, you will watch as the armies "
           "of my world march through and conquer the Shard. And in time, "
           "when Demon Lords sit on the thrones of the Shard, "
           "and the people are properly subdued, we will discuss the "
           "conquest of your world.")
  (prompt-for-key)
  (say knpc "[Do you give him the runes?]")
  (if (yes? kpc)
      (open-demon-gate knpc kpc)
      (begin-last-battle knpc kpc)))
(define (noss-bye knpc kpc)
  (say knpc "Not yet. We have unfinished business to discuss.")
  (prompt-for-key)
  (noss-job knpc kpc))



(define noss-conv
  (ifc basic-conv

       ;; basics
       (method 'default noss-default)
       (method 'hail noss-hail)
       (method 'bye noss-bye)
       (method 'job noss-job)
       (method 'name noss-name)

       ))

(define (mk-nossifer)
  (bind 
   (kern-mk-char 
    'ch_nossifer           ; tag
    "Nossifer"             ; name
    noss-species         ; species
    noss-occ              ; occ
    s_balron          ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp noss-species noss-occ noss-lvl 0 0) ; hp
    0                   ; xp
    (max-mp noss-species noss-occ noss-lvl 0 0) ; mp
    noss-lvl
    #f               ; dead
    'noss-conv       ; conv
    sch_noss           ; sched
    'spell-sword-ai  ; special ai
    nil              ; container
    nil              ; readied
    )
   (noss-mk)))
