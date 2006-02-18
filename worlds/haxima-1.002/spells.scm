;; ----------------------------------------------------------------------------
;; Set the list of magic syllables we'll use in our game. The kernel sets a max
;; limit of 26 (one for each letter of the alphabet) largely for reasons I am
;; not willing to address at this point.
;; ----------------------------------------------------------------------------

(kern-set-spell-words "An"
                      "Bet"
                      "Corp"
                      "Des"
                      "Ex"
                      "Flam"
                      "Grav"                       
                      "Hur"
                      "In"
                      "Jux"
                      "Kal"
                      "Lor"
                      "Mani"
                      "Nox"
                      "Ort"
                      "Por"
                      "Quas"
                      "Rel"
                      "Sanct"
                      "Tym"
                      "Uus"
                      "Vas"
                      "Wis"
                      "Xen"
                      "Ylem"
                      "Zu")

;; ----------------------------------------------------------------------------
;; The only purpose of this list is to prevent the scheme gc from harvesting
;; the spell interfaces which are created on-the-fly in mk-spell. Without this
;; I'd have to explicitly assign a variable to each ifc, which is needlessly
;; verbose.
;; ----------------------------------------------------------------------------

(define spell-ifcs '())

;; ----------------------------------------------------------------------------
;; mk-spell creates a spell interface on the fly, puts it on the spell-ifcs to
;; prevent the gc from getting it, registers a new object type for the spell
;; with the kernel, and then adds it to the list of spells known to the kernel.
;; ----------------------------------------------------------------------------
(define (mk-spell tag name cast-handler magic-words level context 
                  reagents)
  (let ((spell-ifc (ifc '() (method 'cast cast-handler))))
    (set! spell-ifcs (cons spell-ifc spell-ifcs))
    (kern-add-spell (mk-obj-type tag name nil layer-none spell-ifc)
                    magic-words 
                    level  ;; level
                    level  ;; mana cost
                    context 
                    0      ;; flags (unused)
                    0      ;; range (unused)
                    (+ (/ level 2) 1) ;; action point cost
                    reagents)))

;; ui-target-char -- return the first available object for which 'pred' returns
;; true at a user-specified location on the map
(define (ui-target origin range pred)
  (define (select-from seq)
    (cond ((null? seq) 
           nil)
          (else (car seq))))
  (let ((coords (kern-ui-target origin range)))
    (cond ((null? coords) nil)
          (else (select-from (filter pred (kern-get-objects-at coords)))))))

(define (ui-get-direction)
  (kern-ui-direction))

(define (ui-get-adjacent origin pred)
  (define (select-from seq)
    (cond ((null? seq) 
           (kern-print "Nothing!\n") 
           nil)
          (else
           (car seq))))
  (let ((dir (ui-get-direction)))
    (if (null? dir) nil
        (select-from (filter pred (kern-get-objects-at (loc-offset origin dir)))))))

(define (ui-waitkey)
  (kern-ui-waitkey))

(define (mk-ifc-query signal)
  (lambda (kobj) (kobj-can? kobj signal)))

(define (neighbors loc)
  (let ((place (car loc))
        (x (cadr loc))
        (y (caddr loc)))
    (list (list place x (- y 1))
          (list place x (+ y 1))
          (list place (- x 1) y)
          (list place (+ x 1) y))))

(define (get-target-kchar caster range)
  (let ((loc (get-target-loc caster range)))
    (if (null? loc)
        nil
        (get-being-at loc))))

(define (user-cast-ranged-targeted-spell kchar range proc)
  (let ((ktarg (get-target-kchar kchar range)))
    (if (null? ktarg)
        result-no-target
        (begin
          (proc kchar ktarg)
          result-ok))))

(define (cast-field-spell caster field-type)
  (let ((coords (kern-ui-target (kern-obj-get-location caster) 1)))
    (cond ((null? coords) nil)
          (else
           (kern-obj-put-at (kern-mk-obj field-type 1) coords)))))

(define (cast-teleport-spell caster dir)
  (let ((coords (loc-offset (kern-obj-get-location caster) dir)))
    (cond ((null? coords) (kern-print "You sense nothing there!\n"))
          ((not (passable? coords caster)) (kern-print "You sense it is impassable!\n"))
          (else (kern-obj-relocate caster coords nil)))))

(define (cast-signal-spell caster signal target)
  (cond ((null? target) result-no-target)
        (else 
         ((kobj-ifc target) signal target caster)
         result-ok
         )))

(define (cast-bimodal caster proc)
  (define (cast-it target)
    (cond ((null? target) nil)
          (else (proc target))))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
  
(define (user-cast-spell-on-party-member caster proc)
  (define (cast-it target)
    (cond ((null? target) result-no-target)
          (else (proc caster target)
                result-ok)))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 obj-is-char?)))))
  
;; ============================================================================
;; Wind spell support
;; ============================================================================

;; ----------------------------------------------------------------------------
;; terrain-ok-for-field? -- check if the terrain at a given location will allow
;; a field to be dropped on it. Terrains with passability class equivalent to
;; Grass, trees and forest are ok, everything else is not.
;; ----------------------------------------------------------------------------
(define (terrain-ok-for-field? loc)
  (let ((pclass (kern-terrain-get-pclass (kern-place-get-terrain loc))))
    (foldr (lambda (a b) (or a (= pclass b)))
           #f
           (list pclass-grass pclass-trees pclass-forest))))

(define (get-line origin dir n)
  ;;(println "   get-line:" origin "," dir "," n)
  (cond ((= n 0) 
         ;;(println "    nil") 
         nil)
        (else
         (cons origin
               (get-line (loc-offset origin dir) dir (- n 1))))))

(define (get-cone-vert origin depth dy)
  ;;(println " get-cone-vert:" origin "," depth "," dy)
  (let ((place (loc-place origin)))
    (define (get-lines x y n h)
      ;;(println "  get-lines:" x "," y "," n "," h)
      (if (< h 0) nil
          (let ((line (filter (lambda (a) (and (kern-in-los? origin a)
                                               (kern-is-valid-location? a)
                                               (terrain-ok-for-field? a)))
                              (get-line (mk-loc place x y) east n))))
            ;;(println "   line:" line)
            (cons line
                  (get-lines (if (= x 0) 0 (- x 1))
                             (+ y dy) 
                             (+ n (if (= x 0) 1 2))
                             (- h 1))))))
    (get-lines (loc-x origin)
               (loc-y origin)
               1 
               depth)))

(define (get-cone-horz origin depth dx)
  (let ((place (loc-place origin)))
    (define (get-lines x y n h)
      (if (< h 0) nil
          (cons (filter (lambda (a) (and (kern-in-los? origin a)
                                         (kern-is-valid-location? a)
                                         (terrain-ok-for-field? a)))
                        (get-line (mk-loc place x y) south n))
                (get-lines (+ x dx)
                           (if (= y 0) 0 (- y 1))
                           (+ n (if (= y 0) 1 2))
                           (- h 1)))))
    (get-lines (loc-x origin)
               (loc-y origin)
               1 
               depth)))

(define (get-cone origin depth dir)
  ;;(println "get-cone:" origin "," depth "," dir)
  (cond ((= dir north) (get-cone-vert origin 
                                      (min depth (loc-y origin)) 
                                      -1))
        ((= dir east) (get-cone-horz origin 
                                     (min depth
                                          (- (kern-place-get-width (loc-place origin))
                                             (loc-x origin)))
                                     1))
        ((= dir south) (get-cone-vert origin 
                                      (min depth 
                                           (- (kern-place-get-height (loc-place origin))
                                              (loc-y origin)))
                                      1))
        ((= dir west) (get-cone-horz origin
                                     (min depth (loc-x origin))
                                     -1))
        (else nil)))

(define (cast-wind-spell origin proc field-type)
  (let ((dir (ui-get-direction)))
    (if (null? dir) nil
        (begin
          (define (dropfield loc)
            (if (kern-is-valid-location? loc)
                (kern-obj-put-at (kern-mk-obj field-type 1) loc)))
          (define (is-my-field? kobj) (eqv? field-type (kern-obj-get-type kobj)))
          (define (rmfield loc)
            (if (> (kern-dice-roll "2d20") 16)
                (let ((fields (filter is-my-field? (kern-get-objects-at loc))))
                  (cond ((null? fields) nil)
                        (else
                         (kern-obj-remove (car fields)))))))
          (define (doline line)
            (map (lambda (loc)
                   (map proc (kern-get-objects-at loc)))
                 line)
            (map dropfield line)
            (kern-map-repaint)
            (map rmfield line)
            )
          (let ((lines (get-cone origin 10 dir)))
            (cond ((null? lines) nil)
                  (else
                   (map doline (cdr lines))
                   (kern-map-repaint))))))))

;; This version:
;;   o has caller-limited depth
;;   o has caller-specified direction
;;   o applies caller-specified proc to each location
;; (Note: currently used for the spider's web-spew "spell")
(define (cast-wind-spell2 origin proc dir depth)
  ;;(println "cast-wind-spell2:" origin "," proc "," dir "," depth)
  (define (dropfield loc)
    (if (kern-is-valid-location? loc)
        (proc loc)))
  (define (doline line)
    (map dropfield line)
    (kern-map-repaint))
  (let ((lines (get-cone origin depth dir)))
    (cond ((null? lines) nil)
          (else
           ;;(println " doing lines")
           (map doline (cdr lines))
           (kern-map-repaint)))))

;;----------------------------------------------------------------------------
;; Core actions behind spells, special abilities, etc. No UI prompting, no mana
;; or level checking, no mana decrementing -- that all needs to be handled by
;; the callers. All of these calls must return #t on success or #f on
;; failure. No further details as to cause of failure are required.
;;----------------------------------------------------------------------------
(define (cure-poison caster ktarg)
  (kern-obj-remove-effect ktarg ef_poison))

(define (awaken caster ktarg)
  (and (kern-obj-remove-effect ktarg ef_sleep)
       (or (kern-char-set-sleep ktarg #f)
           #t)))

(define (resurrect kchar)
  (kern-char-resurrect kchar)
  #t)

;; ----------------------------------------------------------------------------
;; All the spell cast handlers are listed here. These are the procedures that
;; get called whenever a spell is cast.
;; ----------------------------------------------------------------------------

(define (cast-on-party-member spell)
  (let ((ktarg (kern-ui-select-party-member)))
    (if (null? ktarg)
        result-no-target
        (if (spell ktarg)
            result-ok
            result-no-effect))))

;;----------------------------------------------------------------------------
;; First Circle
;;----------------------------------------------------------------------------
(define (an-nox  caster)
  (user-cast-spell-on-party-member caster cure-poison))

(define (an-zu  caster)
  (user-cast-spell-on-party-member caster awaken))

(define (grav-por caster)
  (user-cast-ranged-targeted-spell caster 8 cast-magic-missile-proc))

(define (in-lor  caster)
  (kern-obj-add-effect caster ef_light nil)
  result-ok)

(define (an-xen-bet  caster)
  (kern-obj-add-effect caster ef_spider_calm nil)
  result-ok)

(define (mani caster)
  (user-cast-spell-on-party-member caster heal-proc))


;;----------------------------------------------------------------------------
;; Second Circle
;;----------------------------------------------------------------------------
(define (an-sanct  caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'unlock (ui-target loc 1 (mk-ifc-query 'unlock)))))
  
(define (sanct-nox  caster)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (kern-obj-add-effect target ef_temporary_poison_immunity nil))))
  
(define (sanct caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'lock (ui-target loc 1 (mk-ifc-query 'lock)))))

(define (an-xen-corp  caster)
  (define (is-undead-char? kobj)
    (and (obj-is-char? kobj)
         (species-is-undead? (kern-char-get-species kobj))))
  (define (repel kchar)
    (kern-char-set-fleeing kchar #t))
  (let ((all-kobjs (kern-place-get-objects (car (kern-obj-get-location caster)))))
    (cond ((null? all-kobjs) 
           (kern-print "Odd, Nobody here!\n")
           result-no-effect
           )
          (else (let ((all-undead-combatants (filter is-undead-char? all-kobjs)))
                  (cond ((null? all-undead-combatants) 
                         (kern-print "No undead here!\n")
                         result-no-effect
                         )
                        (else (map repel all-undead-combatants)
                              result-ok
                              )))))))

(define (in-wis  caster)
  (let ((loc (kern-obj-get-location caster)))
    (kern-log-msg "You are in " (kern-place-get-name (car loc)) " at [" (caddr loc) " " (cadr loc) "]")))

(define (kal-xen  caster)
  (summon (kern-obj-get-location caster)
          (lambda () (mk-animal " a snake"
                                sp_snake 
                                s_snake 
                                faction-player))
          (kern-being-get-current-faction caster)
          (kern-dice-roll "1d4")))

(define (rel-hur  caster)
  (let ((dir (ui-get-direction)))
    (cond ((null? dir)
           result-no-target)
          (else 
           (kern-set-wind dir (kern-dice-roll "20d6"))
           result-ok
           ))))

(define (in-nox-por  caster)
  (user-cast-ranged-targeted-spell caster 8 cast-poison-missile-proc))

;;----------------------------------------------------------------------------
;; Third Circle
;;----------------------------------------------------------------------------
(define (in-flam-grav  caster)
  (cast-field-spell caster F_fire))

(define (in-nox-grav  caster)
  (cast-field-spell caster F_poison))

(define (in-zu-grav  caster)
  (cast-field-spell caster F_sleep))

(define (vas-flam  caster)
  (user-cast-ranged-targeted-spell caster 8 cast-fireball-proc))

(define (vas-lor  caster)
  (kern-obj-add-effect caster ef_great_light nil))

(define (in-flam-sanct caster)
  (user-cast-spell-on-party-member caster 
                                   (lambda (caster ktarg)
                                     (kern-obj-add-effect ktarg ef_temporary_fire_immunity nil)
                                     )))

(define (in-nox-sanct caster)
  (user-cast-spell-on-party-member caster 
                                   (lambda (caster ktarg)
                                     (kern-obj-add-effect ktarg ef_temporary_poison_immunity nil)
                                     )))

;; wis-sanct -- detect traps on containers
(define (wis-sanct caster)
  (let ((ktarg (ui-target (kern-obj-get-location caster)
                          1
                          (lambda (kobj)
                            (and (kern-obj-is-container? kobj)
                                 (kern-obj-is-visible? kobj)))
                          )))
    (cond ((null? ktarg) result-no-target)
          ((kern-obj-is-trapped? ktarg)
           (kern-log-msg (kern-obj-get-name caster)
                         " detects a trap on "
                         (kern-obj-get-name ktarg)
                         "!")
           result-ok)
          (else
           (kern-log-msg (kern-obj-get-name caster)
                         " does not detect any traps")
           result-ok))))
                                     

(define (an-sanct-ylem caster)
  (let ((ktarg (ui-target (kern-obj-get-location caster)
                          1
                          (lambda (kobj)
                            (and (kern-obj-is-container? kobj)
                                 (kern-obj-is-visible? kobj)))
                          )))
    (cond ((null? ktarg) result-no-target)
          ((kern-obj-is-trapped? ktarg)
           (kern-log-msg (kern-obj-get-name caster)
                         " disarms a trap on "
                         (kern-obj-get-name ktarg)
                         "!")
           (kern-obj-remove-trap ktarg)
           result-ok)
          (else
           result-no-effect))))

;;----------------------------------------------------------------------------
;; Fourth Circle
;;----------------------------------------------------------------------------
(define (an-grav  caster)
  (let ((field (ui-get-adjacent (kern-obj-get-location caster) is-field?)))
    (cond ((null? field) nil)
          (else 
           (kern-print "Dispelled field!\n")
           (kern-obj-remove field)
           (kern-map-repaint)
           ))))

(define (uus-por  caster)
  (cast-teleport-spell caster up))

(define (des-por  caster)
  (cast-teleport-spell caster down))

(define (in-sanct-grav  caster)
  (cast-field-spell caster F_energy))

(define (in-sanct  caster)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) 
        (kern-obj-add-effect caster ef_protection nil)
        (kern-obj-add-effect party ef_protection nil)
        )
    result-ok))

(define (wis-quas  caster)
  (kern-add-reveal 50)
  result-ok
  )

;; bet-por -- single character blink
(define (bet-por kcaster)
  (let ((loc (kern-ui-target (kern-obj-get-location kcaster) (kern-char-get-level kcaster))))
    (if (null? loc)
        result-no-target
		(if (kern-place-is-passable loc kcaster)
			(begin
				(kern-obj-relocate kcaster loc nil)
				result-ok)
			(begin
				(kern-log-msg "Failed: Impassable terrain")
				result-no-effect)))))

;;----------------------------------------------------------------------------
;; Fifth Circle
;;----------------------------------------------------------------------------
(define (in-ex-por  caster)
  (let ((loc (kern-obj-get-location caster)))
    ;;(println "in-ex-por")
    (cast-signal-spell caster 'magic-unlock (ui-target loc 1 (mk-ifc-query 'magic-unlock)))))
  
(define (an-ex-por  caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'magic-lock (ui-target loc 1 (mk-ifc-query 'magic-lock)))))

(define (in-bet-xen  caster)
  (summon (kern-obj-get-location caster)
          (lambda () (mk-animal "an insect swarm"
                                sp_insect 
                                s_insects
                                faction-player))
          (kern-being-get-current-faction caster)
          (kern-dice-roll "1d6")))

(define (in-zu  caster)
  (let ((hostiles (all-hostiles caster)))
    (cond ((null? hostiles) 
           (kern-print "No hostiles here!\n")
           result-no-target
           )
          (else 
           (map apply-sleep hostiles)
           result-ok
           ))))

(define (vas-mani  caster)
  (user-cast-spell-on-party-member caster great-heal-proc))


(define (rel-tym  caster)
  (kern-add-quicken (kern-dice-roll "3d6"))
  result-ok
  )

;; ----------------------------------------------------------------------------
;; Sixth Circle
;; ----------------------------------------------------------------------------
(define (in-an  caster) 
  (kern-add-magic-negated (kern-dice-roll "3d6"))
  result-ok)

(define (in-rel-por caster)
  (let ((loc (get-target-loc caster 5)))
    (if (null? loc)
        result-no-target
        (if (handle-mech-at loc caster)
            result-ok
            result-no-effect))))

(define (wis-an-ylem caster) 
  (kern-add-xray-vision (kern-dice-roll "10d6"))
  result-ok
  )

(define (an-xen-exe  caster)
   (let ((target (ui-target (kern-obj-get-location caster) 
                            8 
                            (lambda (kobj) (obj-is-char? kobj)))))
     (if (null? target) 
         result-no-target
         (if (kern-obj-add-effect target 
                                  ef_charm 
                                  (charm-mk (kern-being-get-current-faction caster)))
             result-ok
             result-no-effect))))

(define (in-vas-por-ylem  caster)
  (define (tremor kchar)
    ;;(println "tremor")
    (cond ((kern-char-is-asleep? kchar) (kern-char-set-sleep kchar #f))
          ((> (kern-dice-roll "1d4") 1)
           (kern-map-set-jitter #t)
           (kern-map-repaint)
           (kern-char-set-sleep kchar #t)
           (kern-obj-apply-damage kchar "knocked down" (kern-dice-roll "1d10")))
          (else nil)))
  (define (loop n foes)
    ;;(println "loop:" n)
    ;;(println "foes:" foes)
    (if (not (= n 0))
        (begin
          (map tremor foes)
          (loop (- n 1) foes))))
  (define (wakeup kchar) (kern-char-set-sleep kchar #f))
  ;;(println "in-vas-por-ylem: entry")
  (let ((foes (all-hostiles caster)))
    ;;(println "in-vas-por-ylem:" foes)
    (kern-log-enable #f)
    (map kern-obj-inc-ref foes)
    (shake-map 20)
    (loop 3 foes)
    (kern-map-repaint)
    (map wakeup foes)
    (map kern-obj-dec-ref foes)
    (kern-log-enable #t)
    result-ok))

(define (quas-an-wis  caster)
  (define (confuse kchar)
    (if (> (kern-dice-roll "2d20") 16)
        (kern-being-set-base-faction kchar (random-faction))))
  (let ((foes (all-hostiles caster)))
    (cond ((null? foes) (kern-print "No hostiles here!\n"))
          (else
           (map confuse foes))))
  result-ok)

;; vas-uus-ylem -- special spell which raises a sunken ship
(define (vas-uus-ylem kcaster)
  (let ((loc (kern-ui-target (kern-obj-get-location kcaster) 1)))
    (if (null? loc)
        result-no-target
        (let ((kobjs (filter can-raise-vessel? 
                             (kern-get-objects-at loc))))
          (if (null? kobjs)
              result-no-effect
              (let ((kgen (car kobjs)))                
                (signal-kobj kgen 'raise kgen kcaster)
                result-ok))))))

;; vas-por -- whole party blink
(define (vas-por kcaster)
  (let ((loc (kern-ui-target (kern-obj-get-location kcaster) 
                             (kern-char-get-level kcaster))))
    (if (null? loc)
        result-no-target
        (if (kern-place-is-passable loc kcaster)
			(begin
				(kern-obj-relocate (kern-char-get-party kcaster) loc nil)
				result-ok)
			(begin
				(kern-log-msg "Failed: Impassable terrain")
				result-no-effect)))))

;; ----------------------------------------------------------------------------
;; Seventh Circle
;; ----------------------------------------------------------------------------

(define (in-nox-hur  caster)
  (define (poison-foe kobj)
    (if (is-hostile? caster kobj)
        (apply-poison kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   poison-foe
                   F_poison))

(define (in-zu-hur  caster)
  (define (lullaby-foe kobj)
    (if (is-hostile? caster kobj)
        (apply-sleep kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   lullaby-foe
                   F_sleep))

(define (in-quas-corp  caster)
  (define (repel kchar)
    (kern-log-msg (kern-obj-get-name kchar) " flees in terror!")
    (kern-char-set-fleeing kchar #t)
    )
  (define (try-repel kchar)
    (let ((roll (kern-dice-roll "1d20")))
      (println "roll=" roll)
      (if (> roll 6)
          (repel kchar)
          )))
  (let ((foes (all-hostiles caster)))
    (cond ((null? foes) 
           (kern-print "No hostiles here!\n")
           result-no-target
           )
          (else
           (map try-repel foes)
           result-ok
           ))))
  

(define (in-quas-wis  caster)
  (kern-map-set-peering #t)
  (kern-map-repaint)
  (kern-print "Hit a key when done gazing...\n")
  (ui-waitkey)
  (kern-map-set-peering #f)
  (kern-map-repaint)
  result-ok)

(define (sanct-lor  caster)
  (define (hide target)
    (kern-obj-add-effect target ef_invisibility nil))
  (cast-bimodal caster hide))

(define (in-quas-xen  caster)
  (let ((target (ui-target (kern-obj-get-location caster) 1 obj-is-char?)))
    (if (null? target) nil
        (let* ((clone (kern-obj-clone target))
               (loc (pick-loc (kern-obj-get-location target) clone)))
          (kern-being-set-base-faction clone (kern-being-get-current-faction caster))
          (kern-obj-put-at clone loc)
          ))))
          

;; ----------------------------------------------------------------------------
;; Eighth Circle
;; ----------------------------------------------------------------------------

(define (in-flam-hur caster)
  (define (flambe-foe kobj)
    (if (is-hostile? caster kobj)
        (burn kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   flambe-foe
                   F_fire))

(define (in-vas-grav-corp  caster)
  (define (energize-foe kobj)
    (if (is-hostile? caster kobj)
        (apply-lightning kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   energize-foe
                   F_energy))

(define (an-tym  caster)
  (kern-add-time-stop (kern-dice-roll "3d6"))
  result-ok
  )

(define (kal-xen-corp caster)
  (if (use-ability summon-skeleton caster)
      result-ok
      result-no-effect))

(define (xen-corp  caster)
  (user-cast-ranged-targeted-spell caster 6 cast-kill-proc))

(define (in-mani-corp  caster)
  (let ((target (kern-ui-select-party-member)))
    (if (null? target) nil
        (begin
          (resurrect target)
          (apply-sleep target)
          #t))))

(define (vas-rel-por  caster)
  (println "vas-rel-por")
  (define (rmgate kobj)
    (moongate-close gate)
    (kern-obj-remove gate))
  (let ((loc (kern-ui-target (kern-obj-get-location caster) 1)))
    (println " loc=" loc)
    (if (null? loc) 
        result-no-target
        (let ((gate (summon-moongate 'ord)))
          (println " gate=" gate)
          (kern-obj-put-at gate loc)
          (moongate-open gate)
          result-ok
          ))))

(define (kal-xen-nox caster)
  (if (use-ability summon-slime kchar)
      result-ok
      result-no-effect))

;;----------------------------------------------------------------------------
;; Spell accessors
;;----------------------------------------------------------------------------
(define (spell-name spell) (cadr spell))
(define (spell-handler spell) (caddr spell))
(define (spell-level spell) (list-ref spell 4))
(define (spell-cost spell) (spell-level spell))
(define (spell-ap spell) (spell-level spell))

;; ----------------------------------------------------------------------------
;; This is the table of spells.
;; ----------------------------------------------------------------------------

;; shorter alias
(define s_magic_door s_magically_locked_solid_wood_door_in_stone)

(define spells
  (list
   ;;    tag          name                handler      code L context      mixture
   ;;    ==========   ==============      =======      ==== = ===========  =====================================
   ;; First Circle
   (list 'an_nox      "An Nox spell"      an-nox      "AN"  1 context-any  (list garlic ginseng))
   (list 'an_zu       "An Zu spell"       an-zu       "AZ"  1 context-any  (list garlic ginseng))
   (list 'grav_por    "Grav Por spell"    grav-por    "GP"  1 context-town (list sulphorous_ash black_pearl))
   (list 'in_lor      "In Lor spell"      in-lor      "IL"  1 context-any  (list sulphorous_ash))
   (list 'mani        "Mani spell"        mani        "M"   1 context-any  (list ginseng spider_silk))
   (list 'wis_sanct   "Wis Sanct spell"   wis-sanct   "WS"  1 context-town (list sulphorous_ash))
   (list 'an_sanct_ylem "An Sanct Ylem spell" an-sanct-ylem "ASY" 1 context-town (list blood_moss))

   ;; Second Circle
   (list 'sanct_nox   "Sanct Nox spell"   sanct-nox   "SN"  2 context-any  (list nightshade garlic))
   (list 'an_sanct    "An Sanct spell"    an-sanct    "AS"  2 context-town (list sulphorous_ash blood_moss))
   (list 'sanct       "Sanct spell"       sanct       "S"   2 context-town (list sulphorous_ash spider_silk))
   (list 'an_xen_corp "An Xen Corp spell" an-xen-corp "AXC" 2 context-town (list garlic sulphorous_ash))
   (list 'in_wis      "In Wis spell"      in-wis      "IW"  2 context-any  (list nightshade))
   (list 'kal_xen     "Kal Xen spell"     kal-xen     "KX"  2 context-town (list spider_silk mandrake))
   (list 'rel_hur     "Rel Hur spell"     rel-hur     "RH"  2 context-any  (list sulphorous_ash blood_moss))
   (list 'in_nox_por  "In Nox Por spell"  in-nox-por  "INP" 2 context-town (list nightshade blood_moss black_pearl))
   (list 'an_xen_bet  "An Xen Bet spell"  an-xen-bet  "AXB" 2 context-town (list spider_silk garlic))

   ;; Third Circle
   (list 'in_flam_grav  "In Flam Grav spell"  in-flam-grav  "IFG" 3 context-town (list sulphorous_ash black_pearl spider_silk))
   (list 'in_nox_grav   "In Nox Grav spell"   in-nox-grav   "ING" 3 context-town (list nightshade black_pearl spider_silk))
   (list 'in_zu_grav    "In Zu Grav spell"    in-zu-grav    "IZG" 3 context-town (list ginseng black_pearl spider_silk))
   (list 'vas_flam      "Vas Flam"            vas-flam      "VF"  3 context-town (list sulphorous_ash black_pearl))
   (list 'vas_lor       "Vas Lor"             vas-lor       "VL"  3 context-any (list mandrake sulphorous_ash))
   (list 'in_flam_sanct "In Flam Sanct spell" in-flam-sanct "IFS" 3 context-any (list garlic sulphorous_ash t_royal_cape))
   (list 'in_nox_sanct  "In Nox Sanct spell"  in-nox-sanct  "INS" 3 context-any (list garlic nightshade t_royal_cape))

   ;; Fourth Circle
   (list 'an_grav       "An Grav spell"       an-grav       "AG"  4 context-any (list black_pearl sulphorous_ash))
   ;;(list 'uus_por       "Uus Por spell"       uus-por       "UP"  4 context-any (list blood_moss spider_silk))
   ;;(list 'des_por       "Des Por spell"       des-por       "DP"  4 context-any (list blood_moss spider_silk))
   (list 'in_sanct_grav "In Sanct Grav spell" in-sanct-grav "ISG" 4 context-town (list mandrake black_pearl spider_silk))
   (list 'in_sanct      "In Sanct spell"      in-sanct      "IS"  4 context-any (list sulphorous_ash ginseng garlic))
   (list 'wis_quas      "Wis Quas spell"      wis-quas      "WQ"  4 context-any (list nightshade spider_silk))
   (list 'bet_por       "Bet Por spell"       bet-por       "BP"  4 context-town  (list black_pearl blood_moss))

   ;; Fifth Circle
   (list 'in_ex_por   "In Ex Por spell"   in-ex-por   "IEP" 5 context-any  (list sulphorous_ash blood_moss))
   (list 'an_ex_por   "An Ex Por spell"   an-ex-por   "AEP" 5 context-any  (list sulphorous_ash blood_moss garlic))
   (list 'in_bet_xen  "In Bet Xen spell"  in-bet-xen  "IBX" 5 context-town (list spider_silk blood_moss sulphorous_ash))
   (list 'in_zu       "In Zu spell"       in-zu       "IZ"  5 context-town (list nightshade spider_silk black_pearl))
   (list 'vas_mani    "Vas Mani spell"    vas-mani    "VM"  5 context-any  (list mandrake spider_silk ginseng))
   (list 'rel_tym     "Rel Tym spell"     rel-tym     "RT"  5 context-any  (list sulphorous_ash blood_moss mandrake))

   ;; Sixth Circle
   (list 'in_an           "In An spell"           in-an           "IA"   6 context-any  (list garlic mandrake sulphorous_ash))
   (list 'wis_an_ylem     "Wis An Ylem spell"     wis-an-ylem     "WAY"  6 context-any  (list mandrake sulphorous_ash))
   (list 'an_xen_exe      "An Xen Exe spell"      an-xen-exe      "AXE"  6 context-town (list black_pearl nightshade spider_silk))
   (list 'in_vas_por_ylem "In Vas Por Ylem spell" in-vas-por-ylem "IVPY" 6 context-town (list mandrake blood_moss sulphorous_ash))
   (list 'quas_an_wis     "Quas An Wis spell"     quas-an-wis     "QAW"  6 context-town (list mandrake nightshade))
   (list 'vas_uus_ylem    "Vas Uus Ylem spell"    vas-uus-ylem    "VUY"  6 context-wilderness (list mandrake black_pearl spider_silk))
   (list 'in_rel_por      "In Rel Por spell"      in-rel-por      "IRP"  6 context-town (list black_pearl blood_moss spider_silk))
   (list 'vas_por         "Vas Por spell"         vas-por         "VP"   6 context-wilderness (list mandrake black_pearl blood_moss))

   ;; Seventh Circle
   (list 'in_nox_hur   "In Nox Hur spell"   in-nox-hur   "INH" 7 context-town (list nightshade sulphorous_ash blood_moss))
   (list 'in_zu_hur    "In Zu Hur spell"    in-zu-hur    "IZH" 7 context-town (list mandrake ginseng blood_moss))
   (list 'in_quas_corp "In Quas Corp spell" in-quas-corp "IQC" 7 context-town (list nightshade mandrake garlic))
   (list 'in_quas_wis  "In Quas Wis spell"  in-quas-wis  "IQW" 7 context-any  (list nightshade mandrake))
   (list 'sanct_lor    "Sanct Lor spell"    sanct-lor    "SL"  7 context-any  (list nightshade mandrake blood_moss))
   (list 'xen_corp     "Xen Corp spell"     xen-corp     "XC"  7 context-town (list nightshade black_pearl))
   (list 'in_quas_xen  "In Quas Xen spell"  in-quas-xen  "IQX" 7 context-town (list nightshade mandrake sulphorous_ash spider_silk
                                                                                                    blood_moss ginseng))
   (list 'kal_xen_nox      "Kal Xen Nox spell"      kal-xen-nox      "KXN"  8 context-town (list spider_silk mandrake nightshade))

   ;; Eighth Circle
   (list 'in_flam_hur      "In Flam Hur spell"      in-flam-hur      "IFH"  8 context-town (list mandrake sulphorous_ash blood_moss))
   (list 'in_vas_grav_corp "In Vas Grap Corp spell" in-vas-grav-corp "IVGC" 8 context-town (list mandrake sulphorous_ash nightshade))
   (list 'an_tym           "An Tym spell"           an-tym           "AT"   8 context-any (list mandrake garlic blood_moss))
   (list 'kal_xen_corp     "Kal Xen Corp spell"     kal-xen-corp     "KXC"  8 context-town (list spider_silk mandrake nightshade))
   (list 'in_mani_corp     "In Mani Corp spell"     in-mani-corp     "IMC"  8 context-any (list garlic ginseng spider_silk 
                                                                                                               sulphorous_ash blood_moss mandrake))
   (list 'vas_rel_por      "Vas Rel Por spell"      vas-rel-por     "VRP"  8 context-any (list sulphorous_ash mandrake black_pearl))

   ))

;; ----------------------------------------------------------------------------
;; Now rip through the list of spells, adding them to the kernel.
;; ----------------------------------------------------------------------------

(map (lambda (spell) (apply mk-spell spell)) spells)
