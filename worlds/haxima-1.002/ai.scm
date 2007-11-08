;;----------------------------------------------------------------------------
;; procedures for using items or spells on self or others

;; use-potion? -- use potion on self if desired and available
(define (use-potion? kchar)
  (or (and (wants-healing? kchar)
           (has-heal-potion? kchar)
           (drink-heal-potion kchar))
      (and (wants-mana? kchar)
           (has-mana-potion? kchar)
           (drink-mana-potion kchar))))

(define (use-torch? kchar)
  (if (not (is-in-darkness? kchar))
      #f
      (begin
        (kern-obj-add-effect kchar ef_weaklight nil)
        #t)))

(define (use-heal-spell-on-self? kchar)
  ;;;;(display "use-heal-spell-on-self?")(newline)
  (and (wants-healing? kchar)
       (can-use-ability? heal-ability kchar)
       (use-ability heal-ability kchar kchar)))

(define (use-great-heal-spell-on-self? kchar)
  ;;;;(display "use-great-heal-spell-on-self?")(newline)
  (and (wants-great-healing? kchar)
       (can-use-ability? great-heal-ability kchar)
       (use-ability great-heal-ability kchar kchar)))

(define (use-spell-on-self? kchar)
  ;;;;(display "use-spell-on-self?")(newline)
  (or (use-great-heal-spell-on-self? kchar)
      (use-heal-spell-on-self? kchar)))

;; use-melee-spell-on-foe? -- randomly select from a list of melee spells and
;; return #t iff the spell is used
(define (use-melee-spell-on-foe? kchar ktarg spell-list)
  (let ((spell (random-select (filter (lambda (spell)
                                        (can-use-ability? spell kchar))
                                      spell-list))))
    (if (null? spell)
        #f
        (use-ability spell kchar ktarg))))

(define (use-melee-spell-on-foes? kchar spell-list)
  (foldr (lambda (val ktarg)
           (or val
               (use-melee-spell-on-foe? kchar ktarg spell-list)))
         #f 
         (get-hostiles-in-range kchar 1)))


(define (use-ranged-spell-on-foe? kchar ktarg spell-list)
  (if (> (kern-dice-roll "1d20") 10)
      (let ((abil (random-select (filter (lambda (abil)
                                           (and (can-use-ability? abil kchar)
                                                (can-hit? kchar 
                                                          ktarg 
                                                          (ability-range abil))))
                                         spell-list))))
        (if (null? abil)
            #f
            (use-ability abil kchar ktarg)))
      #f))

(define (use-ranged-spell-on-foes? kchar spell-list)
  (foldr (lambda (val ktarg)
           (or val
               (use-ranged-spell-on-foe? kchar ktarg spell-list)))
         #f 
         (all-visible-hostiles kchar)))

(define (use-heal-spell-on? kchar ktarg)
  ;;(println "use-heal-spell-on?")
  (or (and (wants-great-healing? ktarg)
           (can-use-ability? great-heal-ability kchar)
           (use-ability great-heal-ability kchar ktarg)
           )
      (and (wants-healing? ktarg)
           (can-use-ability? heal-ability kchar)
           (use-ability heal-ability kchar ktarg)
           )))

(define (use-heal-spell-on-ally? kchar)
  ;;(println "use-heal-spell-on-ally?")
  (and (or (can-use-ability? heal-ability kchar)
           (can-use-ability? great-heal-ability kchar))
       (foldr (lambda (val ktarg)
                (or val
                    (use-heal-spell-on? kchar ktarg)))
              #f 
              (all-in-range (kern-obj-get-location kchar)
                            2
                            (all-visible-allies kchar)))))

;;----------------------------------------------------------------------------
;; procedures for searching for nearby things

(define (get-nearest-patient kchar)
  (let ((kloc (kern-obj-get-location kchar)))
    (foldr (lambda (kpatient ktarg)
             ;;(display "  checking ")(dump-char ktarg)
             (if (and (wants-healing? ktarg)
                      (or (null? kpatient)                      
                          (< (kern-get-distance kloc 
                                                (kern-obj-get-location ktarg))
                             (kern-get-distance kloc 
                                                (kern-obj-get-location kpatient)))))
                 ktarg
                 kpatient))
           nil
           (all-visible-allies kchar))))


;;----------------------------------------------------------------------------
;; procedures for pursuing or avoiding

(define (avoid-melee? kchar)
  ;;;;(display "avoid-melee? kchar")(newline)
  (let ((nearby-foes (get-hostiles-in-range kchar 1)))
    (if (null? nearby-foes)
        #f
        (evade kchar nearby-foes))))

;; This is for medics. A patient is an ally that needs healing. If a patient is
;; less than 2 tiles away then do nothing. If a patient is more than 2 tiles
;; away then pathfind toward it.
(define (move-toward-patient? kchar)
  (let ((patient (get-nearest-patient kchar)))
    (if (null? patient)
        #f
        (begin
          ;;(display "selected ")(dump-char patient)
          (if (in-range? (kern-obj-get-location kchar)
                         2
                         patient)
              #f
              (pathfind kchar (kern-obj-get-location patient)))))))


(define (move-away-from-foes? kchar)
  ;;(println "move-away-from-foes?")
  (evade kchar (all-visible-hostiles kchar)))

(define (in-melee-range-of-foes? kchar)
  (> (length (get-hostiles-in-range kchar 1))
     0))

;; stuck? -- #t iff kchar cannot safely move to a neighboring tile
(define (stuck? kchar)
  ;;(println " stuck?")
  (let* ((cloc (kern-obj-get-location kchar))
         (kplace (loc-place cloc))
         (x (loc-x cloc))
         (y (loc-y cloc)))
    (foldr (lambda (x loc)
             (and x 
                  (not (is-good-loc? kchar loc))))
           #t
           (list (mk-loc kplace (- x 1) y)
                 (mk-loc kplace (+ x 1) y)
                 (mk-loc kplace x (- y 1))
                 (mk-loc kplace x (+ y 1))))))
  

(define (blink-offset kchar)
  (let ((origin (kern-obj-get-location kchar)))
    (loc-add origin
             (loc-norm (apply loc-add 
                              (map (lambda (kfoe)
                                     (loc-diff origin
                                               (kern-obj-get-location kfoe)))
                                   (all-visible-hostiles kchar)))))))

(define (choose-blink-loc kchar)
  (let ((loc (blink-offset kchar)))
    ;;(println "choose-blink-loc " loc)
    (if (and (not (null? loc))
             (not (loc-equal? loc
                              (kern-obj-get-location kchar)))
             (passable? loc kchar)
             (not (is-bad-terrain-at? loc))
             (not (any-object-types-at? loc all-field-types))
             (not (occupied? loc)))
        loc
        nil)))

(define (choose-random-blink-loc kchar)
  ;;(println "   choose-random-blink-loc")
  (random-loc-place-iter (loc-place (kern-obj-get-location kchar))
                         (lambda (loc)
                           ;;(println "    check loc" loc)
                           (and (not (loc-equal? loc
                                                 (kern-obj-get-location kchar)))
                                (passable? loc kchar)
                                (not (is-bad-terrain-at? loc))
                                (not (any-object-types-at? loc all-field-types))
                                (not (occupied? loc))
                                (null? (get-hostiles-in-range-of-loc kchar 3 loc))
                                ))
                         3))

(define (blink-away-from-foes kchar)
  ;;(println " blink-away-from-foes")
  (if (not (can-use-ability? teleport kchar))
      #f
      (let ((loc (choose-random-blink-loc kchar)))
        ;;(println " blink-away-from-foes:" loc)
        (if (null? loc)
            #f
            (use-ability teleport kchar loc)))))

;; Bandit AI --------------------------------------------------

(define bandit-taunts 
  (list 
   "Yer money or yer life!"
   "Have at 'cher!"
   "Yer a dead man, ye are!"
   "Oy!  You!  Gerrout!"
   "'Ave at 'im, boys!"
   "Circle round, we've got a dead one!"
   "Dibs on 'is boots!"
   "Stranger, meetcha couple my friends..."
   ))

(define (bandit-taunt kbandit ktarg)
  (taunt kbandit ktarg bandit-taunts)
  (npcg-set-taunted! (gob kbandit) #t))

(define (bandit-ai kchar)
  (let ((ktarg (ai-select-target kchar)))
    (if (null? ktarg)
        (ai-wander kchar)
        (begin
          (or (npcg-taunted? (gob kchar))
              (bandit-taunt kchar ktarg))
          (or (ai-attack-target kchar ktarg)
              (ai-pathfind-to-target kchar ktarg))))))

;; --------------------------------------------------
;; sound effects            
              
(kern-mk-sound 'npc-sound-bark          "dog_9.wav")

(define (npc-sound ksound prob)
	(lambda (kobject)
		(if (< (kern-dice-roll "1d100") prob)
			(kern-sound-play-at ksound (kern-obj-get-location kobject))
		)
		#f
	))

(define npc-dosound-bark (npc-sound npc-sound-bark 8))

;; --------------------------------------------------

(define (animal-ai kchar)
  (get-off-bad-tile? kchar))

(define (wolf-ai kchar)
	(or (npc-dosound-bark kchar)
		(animal-ai kchar)))
  
(define (nolight-ai kchar)
  (or 
   (get-off-bad-tile? kchar)
   (use-potion? kchar)
   ))

(define (std-ai kchar)
  (or 
   (get-off-bad-tile? kchar)
   (use-potion? kchar)
   (use-torch? kchar)
   ))

(define (nixie-ai kchar)
  (nolight-ai kchar))

;; Invoke a summoning ability if allies are outnumbered by a certain amount
(define (ai-summon kchar ability)
  (and (can-use-ability? ability kchar)
       (< (num-visible-allies kchar) (* 2 (num-visible-hostiles kchar)))
       (use-ability ability kchar)))

(define (use-enslave? kchar)
  (if (or (not (can-use-ability? enslave kchar))
          (<= (kern-dice-roll "1d20") 16))
      #f
      (let ((hostiles (filter
                       not-disabled?
                       (all-visible-hostiles kchar))))
        (if (null? hostiles)
            #f
            (let ((ktarg (nearest-obj kchar hostiles)))
              (if (not (can-hit? kchar ktarg (ability-range enslave)))
                  #f
                  (use-ability enslave kchar ktarg)
              ))))))
          
(define (use-narcotize? kchar)
  (if (or (not (can-use-ability? narcotize kchar))
          (<= (kern-dice-roll "1d20") 17))
      #f
      (let ((hostiles (filter
                       not-disabled?
                       (all-visible-hostiles kchar))))
        ;;(println " hostiles=" hostiles)
        (if (null? hostiles)
            #f
            (use-ability narcotize kchar)
            ))))
          
(define (goto-post kchar)
  ;;;;(println "goto-post")
  (let ((guard (gob kchar)))
    (if (npcg-has-post? guard)
        (let ((post (cons (loc-place (kern-obj-get-location kchar))
                          (npcg-get-post guard))))
          ;;;;(println "post:" post)
          (pathfind kchar post)))))

(define (summon-demon? kchar)
  (if (and (> (kern-dice-roll "1d20") 18)
           (can-use-ability? summon-demon kchar))
      (use-ability summon-demon kchar)
      #f))

(define (summon-wolves? kchar)
  (if (and (> (kern-dice-roll "1d20") 18)
           (can-use-ability? summon-wolves kchar))
      (use-ability summon-wolves kchar)
      #f))

(define (summon-ratlings? kchar)
  (cond ((not (can-use-ability? summon-ratlings kchar)) #f)
        ;;((null? (get-hostiles-in-range kchar 4)) #f)
        (else
         (use-ability summon-ratlings kchar))))

(define (turn-invisible? kchar)
  (and (> (kern-dice-roll "1d20") 14)
       (not (is-invisible? kchar))
       (can-use-ability? turn-invisible kchar)
       (use-ability turn-invisible kchar)))

;;----------------------------------------------------------------------------
;; spell-sword-ai -- aggressive, selfish fighter that uses magic for combat.
(define (spell-sword-ai kchar)
  ;;(display "spell-sword-ai ")(dump-char kchar)
  (or (std-ai kchar)
      (use-spell-on-self? kchar)
      (use-melee-spell-on-foes? kchar melee-spells)
      (use-ranged-spell-on-foes? kchar all-ranged-spells)))

(define (warlock-ai kchar)
  ;;(display "warlock-ai ")(dump-char kchar)
  (or (std-ai kchar)
      (use-spell-on-self? kchar)
      (summon-demon? kchar)
      (use-ranged-spell-on-foes? kchar all-ranged-spells)
      ))

(define (dryad-ai kchar)
  (or 
   (summon-wolves? kchar)
   (use-narcotize? kchar)
   (use-ranged-spell-on-foes? kchar all-field-spells)
      ))

(define (demon-ai kchar)
  (display "demon-ai ")(dump-char kchar)
  (or (std-ai kchar)
      (turn-invisible? kchar)
      ))

(define (gazer-ai kchar)
  ;;(display "gazer-ai")(dump-char kchar)
  (or (std-ai kchar)
      (use-narcotize? kchar)
      (use-enslave? kchar)))

(define (shaman-ai kchar)
  (or (std-ai kchar)
      (use-heal-spell-on-ally? kchar)
      (move-toward-patient? kchar)
      (spell-sword-ai kchar)
      (move-away-from-foes? kchar)))

(define (priest-ai kchar)
  (or (std-ai kchar)
      (and (in-melee-range-of-foes? kchar)
           (blink-away-from-foes kchar))
      (spell-sword-ai kchar)))

(define (wisp-ai kchar)
  (display "wisp-ai ")(dump-char kchar)
  (or (animal-ai kchar)
	  (use-torch? kchar)
      (and (wants-healing? kchar)
           (in-melee-range-of-foes? kchar)
           (blink-away-from-foes kchar))
      (use-ranged-spell-on-foes? kchar all-field-spells)
      ))

(define (generic-ai kchar)
  (std-ai kchar))

;; Death knights can use Vampiric Touch at L3 and Disease at L6
(define (death-knight-ai kchar)
  (or (nolight-ai kchar)
      (let ((vt (can-use-ability? vampiric-touch kchar))
            (dis (can-use-ability? disease-touch kchar)))
        (if (not (or vt dis))
            #f
            (let ((victims (get-hostiles-in-range kchar 1)))
              (if (null? victims)
                  #f
                  (if (wants-healing? kchar)
                      (use-ability vampiric-touch kchar (car victims))
                      (if (and dis
                               (>= (kern-dice-roll "1d20") 16))
                          (use-ability disease-touch kchar (car victims))
                          #f))))))))

(define (rat-ai kchar)
  (or (animal-ai kchar)
      (use-ranged-spell-on-foes? kchar 
                                 (list disease-touch))))

(define (craven-archer-ai kchar)
  ;;(println "craven-archer-ai")
  (or (nolight-ai kchar)
      (and (stuck? kchar)
           (in-melee-range-of-foes? kchar)
           (blink-away-from-foes kchar))))

(define (medik-ai kchar)
  ;;(println "medik-ai")
  (or (std-ai kchar)
      (use-heal-spell-on-ally? kchar)
      (move-toward-patient? kchar)
      (move-away-from-foes? kchar)))

;; guard-ai
(define (try-to-use-disarm kchar)
    ;;(display "try-to-use-ability")(newline)
    (if (can-use-ability? disarm kchar)
        (let ((victims (get-hostiles-in-range kchar 1)))
          (and (not (null? victims))
               (>= (kern-dice-roll "1d20") 16)
               (or (use-ability disarm kchar (car victims))
                   #t)))
        #f))
(define (guard-ai kchar)
  (or (std-ai kchar)
      (if (any-visible-hostiles? kchar)
          (try-to-use-disarm kchar)
          (goto-post kchar))))


;; ranger-ai -- nothing special, but can act like a guard
(define (ranger-ai kchar)
  (or (std-ai kchar)
      (if (any-visible-hostiles? kchar)
          #f
          (goto-post kchar)
          )))

;; A lich will summon undead minions
(define (lich-ai kchar)
  (display "lich-ai:") (dump-char kchar)
  (or (nolight-ai kchar)
      (ai-summon kchar summon-skeleton)
      (spell-sword-ai kchar)))

(define (flee kchar)
  (println (kern-obj-get-name kchar) " flees")
  (kern-char-set-fleeing kchar #t)
  )

;; A kraken will chomp through planking to get at its foes. If foes exist, and
;; they are not in melee range, and the kraken cannot pathfind to them, it will
;; attempt a directional move toward them. If the move is blocked by deck, the
;; kraken will destroy the deck, using the given ability. The ability varies by
;; kraken type to match the expected terrain that the deck should be converted
;; into.
(define (generic-kraken-ai kchar ability)
  (let ((foes (all-visible-hostiles kchar)))
    (if (null? foes)
        (flee kchar)
        (let* ((kfoe (nearest-obj kchar foes))
               (dest (kern-obj-get-location kfoe)))
          (if (pathfind kchar dest)
              #f
              (if (not (null? (get-hostiles-in-range kchar 1)))
                  #f
                  (let* ((cloc (kern-obj-get-location kchar))
                         (vect (loc-to-delta (loc-diff cloc dest)))
                         (dest (loc-add cloc vect)))
                    (if (passable? dest kchar)
                        (kern-obj-move kchar (loc-x vect) (loc-y vect))
                        (or (and (is-deck? (kern-place-get-terrain dest))
                                 (can-use-ability? ability kchar)
                                 (use-ability ability kchar dest))
                            (flee kchar)
                        )))))))))

(define (submerge kchar) 
  ;;(println "SUBMERGE")
  (kern-obj-set-submerged kchar #t)
  )

(define (surface kchar) 
  ;;(println "SURFACE") 
  (kern-obj-set-submerged kchar #f)
  )

(define (generic-kraken-ai kchar ability)
  (let ((foes (all-visible-hostiles kchar)))
    (if (null? foes)
        (begin
          ;;(println "no foes, flee.")
          (flee kchar)
          )
        (if (not (null? (get-hostiles-in-range kchar 1)))
            (begin
              ;;(println "foes in range 1, attack.")
              (surface kchar)
              #f)
            (let* ((kfoe (nearest-obj kchar foes))
                   (dest (kern-obj-get-location kfoe)))
              ;;(println "no foes in range, submerging and selecting " (kern-obj-get-name kfoe) " as target...")
              (submerge kchar)
              (if (pathfind kchar dest)
                  (begin
                    ;;(println " pathfinding...")
                    #f
                    )
                  (let* ((cloc (kern-obj-get-location kchar))
                         (vect (loc-to-delta (loc-diff cloc dest)))
                         (dest (loc-add cloc vect)))
                    ;;(println " can't pathfind...")
                    (if (passable? dest kchar)
                        (begin
                          ;;(println "   moving directly.")
                          (kern-obj-move kchar (loc-x vect) (loc-y vect))
                          )
                        (begin
                          ;;(println "   trying to chomp...")
                          (if (and (is-deck? (kern-place-get-terrain dest))
                                   (can-use-ability? ability kchar)
                                   (use-ability ability kchar dest))
                              (begin
                                ;;(println "    success!")
                                (surface kchar)
                                #t)
                              (begin
                                ;;(println "    can't chomp, so flee.")
                                (flee kchar)
                                )
                              ))))))))))

;; A kraken will chomp through planking to get at its foes. If foes exist, and
;; they are not in melee range, and the kraken cannot pathfind to them, it will
;; attempt a directional move toward them. If the move is blocked by deck, the
;; kraken will destroy the deck.
(define (kraken-ai kchar)
  (generic-kraken-ai kchar chomp-deck))

;; The sludge kraken spawns tentacles.
(define (sludge-kraken-ai kchar)
  (define (spawn ktarg)
    (let* ((lvl (kern-char-get-level kchar))
           (knpc (spawn-npc 'sludge-tentacle lvl))
           (loc (pick-loc (kern-obj-get-location ktarg) knpc))
           )
      (cond ((null? loc) 
             (kern-obj-dec-ref knpc)
             #f)
            (else
             (kern-being-set-base-faction knpc 
                                          (kern-being-get-base-faction kchar))
             (kern-obj-put-at knpc loc)
             #t))))
  (define (spawn-tentacle?)
    (let ((ktarg (random-select (all-visible-hostiles kchar)))
          (tentacles (filter is-sludge-tentacle? 
                             (kern-place-get-beings (loc-place (kern-obj-get-location kchar)))))
          )
      ;;(println ktarg tentacles)
      (cond ((null? ktarg) #f)
            ((< (length tentacles) 
                (* 2 (kern-char-get-level kchar)))
             (spawn ktarg))
            (else
             #f))))
  (or (animal-ai kchar)
      (spawn-tentacle?)))

;; Sludge kraken tentacles work like sea krakens, but convert deck to sludge
;; instead of shoals.
(define (sludge-tentacle-ai kchar)
  (generic-kraken-ai kchar deck-to-sludge))

;; sea-serpent-ai -- spit fireballs every once in a while
(define (sea-serpent-ai kchar)
  (let ((ktarg (nearest-visible-hostile kchar)))
    (cond ((null? ktarg) #f)
          (else
           (and (> (kern-dice-roll "1d20") 14)
                (use-ranged-spell-on-foes? kchar (list fireball-spell)))))))

;; subtle: like dryads, don't let kernal AI run because hydra's don't move, but
;; if I give them mmode-none they won't get placed in wilderness combat
(define (hydra-ai kchar)
  ;;(display "hydra-ai")(dump-char kchar)
  (or (ai-summon kchar summon-slimes)
      (use-ranged-spell-on-foes? kchar (list poison-missile-spell
                                             acid-missile-spell
                                             ))
      ))

(define (dragon-ai kchar)
  (or (std-ai kchar)
      (and (> (kern-dice-roll "1d20") 14)
           (use-ranged-spell-on-foes? kchar 
                                      (list fireball-spell 
                                            cast-fire-wind
                                            )))))

;; townsman-ai -- may be extended in the future to do things like flee from
;; invaders, for now just do the basics and light up a torch if it gets dark.
(define (townsman-ai kchar)
  (std-ai kchar))

;; ratlings fear snakes
(define (ratling-ai kchar)
  (define (evade-snakes?)
    (let ((snakes (filter is-snake? (get-hostiles-in-range kchar 4))))
      (cond ((null? snakes) #f)
            (else (evade kchar snakes)))))
  (or (std-ai kchar)
      (evade-snakes?)))

(define (ratling-sorcerer-ai kchar)
  (or (ratling-ai kchar)
      (summon-ratlings? kchar)
      (move-away-from-foes? kchar)))

;; snakes eat rats and ratlings, recovering hp
(define (snake-ai kchar)
  (define (eat-ratling?)
    (let ((ratlings (filter is-rat? (get-hostiles-in-range kchar 1))))
      (cond ((null? ratlings) #f)
            (else
             (kern-log-msg (kern-obj-get-name kchar)
                           " eats "
                           (kern-obj-get-name (car ratlings)))
             (kern-obj-remove (car ratlings))
             (kern-obj-heal kchar 2)
             #t
             ))))
  (or (std-ai kchar)
      (eat-ratling?)))

;; carabid beetles tunnel through stone
(define (carabid-ai kchar)
  ;;(println "carabid-ai")
  (define (tunnel loc kter)
    ;;(println "tunnel")
    (kern-place-set-terrain (kern-obj-get-location kchar) kter)
    (kern-place-set-terrain loc t_gravel)
    (kern-obj-relocate kchar loc nil)
    #t)
  (define (tunnel?)
    ;;(println "tunnel?")
    (let* ((loc (random-neighbor-loc kchar))
           (kter (kern-place-get-terrain loc)))
      ;;(println "loc=" loc)
      (cond ((or (eqv? kter t_wall)
                 (eqv? kter t_wall_rock))
             (tunnel loc t_boulder))
            ((eqv? kter t_boulder) (tunnel loc t_gravel))
            (else #f))))
  (or (animal-ai kchar)
      (tunnel? kchar)))

(define (eat-corpse? kchar)
  (cond ((wants-healing? kchar)
         (let ((kcorpse (find-nearest kchar t_corpse)))
           (cond ((null? kcorpse) #f)
                 (else
                  (cond ((loc-equal? (kern-obj-get-location kchar)
                                     (kern-obj-get-location kcorpse))
                         (kern-log-msg (kern-obj-get-name kchar) " eats " (kern-obj-get-name kcorpse))
                         (kern-obj-remove kcorpse)
                         (kern-obj-heal kchar (kern-dice-roll "1d10+2"))
                         #t)
                        (else
                         (pathfind kchar (kern-obj-get-location kcorpse))
                         ))))))
        (else
         #f)))

;; griffin's recover hp by eating corpses
(define (griffin-ai kchar)
  (or (get-off-bad-tile? kchar)
      (eat-corpse? kchar)))

;; FIXME: need to do something here. Geomancers should be able to do things
;; like transform terrain, convert boulders into trolls, convert rock walls
;; into boulders, etc. (Note: the old troll AI is broken, too).
(define (geomancer-ai kchar)
  (std-ai kchar))