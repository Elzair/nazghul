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

(define (mk-spell tag name sprite cast-handler magic-words level context 
                  reagents)
  (let ((spell-ifc (ifc '() (method 'cast cast-handler))))
    (set! spell-ifcs (cons spell-ifc spell-ifcs))
    (kern-add-spell (mk-obj-type tag name sprite layer-none spell-ifc)
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

(define (cast-missile-spell caster range missile)
  (let* ((from (kern-obj-get-location caster))
         (to (kern-ui-target from range)))
    (if (not (null? to))
        (kern-fire-missile missile from to))
    to))

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
  (cond ((null? target)
         (kern-print "No effect!\n")
         nil)
        (else ((kobj-ifc target) signal target caster))))

(define (cast-summon-spell origin mk-critter count)
  (define (run-loop n)
    (if (= n 0) nil
        (let* ((critter (kern-obj-set-temporary (mk-critter) #t))
               (loc (pick-loc origin critter)))
          (cond ((null? loc) nil)
                (else
                 (kern-obj-put-at critter loc)
                 (run-loop (- n 1)))))))
  (run-loop count))

(define (cast-bimodal caster proc)
  (define (cast-it target)
    (cond ((null? target) nil)
          (else (proc target))))
  (let ((loc (kern-obj-get-location caster)))
  (if (kern-place-is-wilderness? (loc-place loc))
      (cast-it (kern-ui-select-party-member))
      (cast-it (ui-target loc 2 kern-obj-is-char?)))))
  

(define (cast-heal-spell caster dice)
  (cast-bimodal caster
                (lambda (target)
                  (kern-obj-heal target (kern-dice-roll dice)))))

;; ----------------------------------------------------------------------------
;; Wind spell support
;; ----------------------------------------------------------------------------

(define (get-line origin dir n)
  (if (= n 0) nil
      (cons origin
            (get-line (loc-offset origin dir) dir (- n 1)))))

(define (get-cone-vert origin depth dy)
  (let ((place (loc-place origin)))
    (define (get-lines x y n h)
      (if (< h 0) nil
          (cons (filter (lambda (a) (kern-in-los? origin a))
                        (get-line (mk-loc place x y) east n))
                (get-lines (if (= x 0) 0 (- x 1))
                           (+ y dy) 
                           (+ n (if (= x 0) 1 2))
                           (- h 1)))))
    (get-lines (loc-x origin)
               (loc-y origin)
               1 
               depth)))

(define (get-cone-horz origin depth dx)
  (let ((place (loc-place origin)))
    (define (get-lines x y n h)
      (if (< h 0) nil
          (cons (get-line (mk-loc place x y) south n)
                (get-lines (+ x dx)
                           (if (= y 0) 0 (- y 1))
                           (+ n (if (= y 0) 1 2))
                           (- h 1)))))
    (get-lines (loc-x origin)
               (loc-y origin)
               1 
               depth)))

(define (get-cone origin depth dir)
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
          (define (is-field? kobj) (eqv? field-type (kern-obj-get-type kobj)))
          (define (rmfield loc)
            (if (> (kern-dice-roll "2d20") 16)
                (let ((fields (filter is-field? (kern-get-objects-at loc))))
                  (cond ((null? fields) nil)
                        (else
                         (kern-obj-remove (car fields))
                         (kern-obj-destroy (car fields)))))))
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
  (display "cast-wind-spell2")(newline)
  (define (dropfield loc)
    (if (kern-is-valid-location? loc)
        (proc loc)))
  (define (doline line)
    (map dropfield line)
    (kern-map-repaint))
  (let ((lines (get-cone origin depth dir)))
    (cond ((null? lines) nil)
          (else
           (map doline (cdr lines))
           (kern-map-repaint)))))
  
;; ----------------------------------------------------------------------------
;; All the spell cast handlers are listed here. These are the procedures that
;; get called whenever a spell is cast.
;; ----------------------------------------------------------------------------

(define (an-nox kspell caster)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (kern-obj-remove-effect target ef_poison))))

(define (an-zu kspell caster)
  (let ((target (kern-ui-select-party-member)))
    (if (not (null? target))
        (begin
          (kern-obj-remove-effect target ef_sleep)
          (kern-char-set-sleep target #f)))))

(define (grav-por kspell caster)
  (cast-missile-spell caster 8 t_arrow))

(define (in-lor kspell caster)
  (kern-obj-add-effect caster ef_light nil))

(define (mani kspell caster)
  (cast-heal-spell caster "2d20"))

(define (an-sanct kspell caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'unlock (ui-target loc 1 (mk-ifc-query 'unlock)))))
  
(define (sanct kspell caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'lock (ui-target loc 1 (mk-ifc-query 'lock)))))

(define (an-xen-corp kspell caster)
  (define (is-undead-char? kobj)
    (and (kern-obj-is-char? kobj)
         (species-is-undead? (kern-char-get-species kobj))))
  (define (repel kchar)
    (kern-char-set-fleeing kchar #t))
  (let ((all-kobjs (kern-place-get-objects (car (kern-obj-get-location caster)))))
    (cond ((null? all-kobjs) 
           (kern-print "Odd, Nobody here!\n"))
          (else (let ((all-undead-combatants (filter is-undead-char? all-kobjs)))
                  (cond ((null? all-undead-combatants) 
                         (kern-print "No undead here!\n"))
                        (else (map repel all-undead-combatants)))))))
  #t) ;; always succeeds

(define (in-wis kspell caster)
  (let ((loc (kern-obj-get-location caster)))
    (kern-print "You are in " (kern-place-get-name (car loc)) ":lat=" (cadr loc) " long=" (caddr loc) "\n")))

(define (kal-xen kspell caster)
  (cast-summon-spell (kern-obj-get-location caster)
                     (lambda () (kern-mk-stock-char sp_snake 
                                                    nil s_snake 
                                                    " a snake" 
                                                    faction-player
                                                    nil))
                     (kern-dice-roll "1d4")))

(define (rel-hur kspell caster)
  (let ((dir (ui-get-direction)))
    (cond ((null? dir) nil)
          (else (kern-set-wind dir (kern-dice-roll "20d6"))))))

(define (in-nox-por kspell caster)
  (cast-missile-spell caster 8 t_poison_bolt))
  
(define (in-flam-grav kspell caster)
  (cast-field-spell caster F_fire))

(define (in-nox-grav kspell caster)
  (cast-field-spell caster F_poison))

(define (in-zu-grav kspell caster)
  (cast-field-spell caster F_sleep))

(define (vas-flam kspell caster)
  (cast-missile-spell caster 8 t_fireball))

(define (vas-lor kspell caster)
  (kern-obj-add-effect caster ef_great_light nil))

(define (an-grav kspell caster)
  (define (is-field? kobj)
    (is-field-type? (kern-obj-get-type kobj)))
  (let ((field (ui-get-adjacent (kern-obj-get-location caster) is-field?)))
    (cond ((null? field) nil)
          (else 
           (kern-print "Dispelled field!\n")
           (kern-obj-remove field)
           (kern-obj-destroy field)
           (kern-map-repaint)
           ))))

(define (uus-por kspell caster)
  (cast-teleport-spell caster up))

(define (des-por kspell caster)
  (cast-teleport-spell caster down))

(define (in-sanct-grav kspell caster)
  (cast-field-spell caster F_energy))

(define (in-sanct kspell caster)
  (let ((party (kern-char-get-party caster)))
    (if (null? party) (kern-obj-add-effect caster ef_protection nil)
        (kern-obj-add-effect party ef_protection nil))))

(define (wis-quas kspell caster)
  (kern-add-reveal 50))

(define (in-ex-por kspell caster)
  (let ((loc (kern-obj-get-location caster)))
    (display "in-ex-por")(newline)
    (cast-signal-spell caster 'magic-unlock (ui-target loc 1 (mk-ifc-query 'magic-unlock)))))
  
(define (an-ex-por kspell caster)
  (let ((loc (kern-obj-get-location caster)))
    (cast-signal-spell caster 'magic-lock (ui-target loc 1 (mk-ifc-query 'magic-lock)))))

(define (in-bet-xen kspell caster)
  (cast-summon-spell (kern-obj-get-location caster)
                     (lambda () (kern-mk-stock-char sp_insect nil s_insects "an insect swarm" faction-player nil))
                     (kern-dice-roll "1d6")))

(define (in-zu kspell caster)
  (let ((hostiles (all-hostiles caster)))
    (cond ((null? hostiles) 
           (kern-print "No hostiles here!\n"))
          (else (map apply-sleep hostiles)))))

(define (vas-mani kspell caster)
  (cast-heal-spell caster "4d20+20"))

(define (rel-tym kspell caster)
  (kern-add-quicken (kern-dice-roll "3d6")))

;; ----------------------------------------------------------------------------
;; Sixth Circle
;; ----------------------------------------------------------------------------
(define (in-an kspell caster) (kern-add-magic-negated (kern-dice-roll "3d6")))

(define (wis-an-ylem kspell caster) (kern-add-xray-vision (kern-dice-roll "3d6")))

(define (an-xen-exe kspell caster)
   (let ((target (ui-target (kern-obj-get-location caster) 
                            8 
                            (lambda (kobj) (kern-obj-is-char? kobj)))))
     (if (null? target) 
         result-no-target
         (if (kern-obj-add-effect target 
                                  ef_charm 
                                  (charm-mk (kern-being-get-current-faction caster)))
             result-ok
             result-no-effect))))

(define (in-vas-por-ylem kspell caster)
  (define (tremor kchar)
    (display "tremor")(newline)
    (cond ((kern-char-is-asleep? kchar) (kern-char-set-sleep kchar #f))
          ((> (kern-dice-roll "1d4") 1)
           (kern-char-set-sleep kchar #t)
           (kern-obj-apply-damage kchar "knocked down" tremor-damage))
          (else nil)))
  (define (loop n foes)
    (display "loop:")(display n)(newline)
    (display "foes:")(display foes)(newline)
    (if (not (= n 0))
        (begin
          (kern-map-set-jitter #t)
          (kern-map-repaint)
          (map tremor foes)
          (loop (- n 1) foes))))
  (define (wakeup kchar) (kern-char-set-sleep kchar #f))
  (display "in-vas-por-ylem: entry")(newline)
  (let ((foes (all-hostiles caster)))
    (display "in-vas-por-ylem:")(display foes)(newline)
    (loop 20 foes)
    (kern-map-set-jitter #f)
    (map wakeup foes)))

(define (quas-an-wis kspell caster)
  (define (confuse kchar)
    (if (> (kern-dice-roll "2d20") 16)
        (kern-obj-add-effect kchar ef_charm (charm-mk faction-none))))
  (let ((foes (all-hostiles caster)))
    (cond ((null? foes) (kern-print "No hostiles here!\n"))
          (else
           (map confuse foes))))
  result-ok)

;; ----------------------------------------------------------------------------
;; Seventh Circle
;; ----------------------------------------------------------------------------

(define (in-nox-hur kspell caster)
  (define (poison-foe kobj)
    (if (is-hostile? caster kobj)
        (apply-poison kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   poison-foe
                   F_poison))

(define (in-zu-hur kspell caster)
  (define (lullaby-foe kobj)
    (if (is-hostile? caster kobj)
        (apply-sleep kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   lullaby-foe
                   F_sleep))

(define (in-quas-corp kspell caster)
  (define (repel kchar)
    (kern-char-set-fleeing kchar #t))
  (let ((foes (all-hostiles caster)))
    (cond ((null? foes) (kern-print "No hostiles here!\n"))
          (else
           (map repel foes)))))
  

(define (in-quas-wis kspell caster)
  (kern-map-set-peering #t)
  (kern-map-repaint)
  (kern-print "Hit a key when done gazing...\n");
  (ui-waitkey)
  (kern-map-set-peering #f)
  (kern-map-repaint))

(define (sanct-lor kspell caster)
  (define (hide target)
    (kern-obj-add-effect target ef_invisibility nil))
  (cast-bimodal caster hide))
  
(define (in-quas-xen kspell caster)
  (let ((target (ui-target (kern-obj-get-location caster) 1 kern-obj-is-char?)))
    (if (null? target) nil
        (let* ((clone (kern-obj-clone target))
               (loc (pick-loc (kern-obj-get-location target) clone)))
          (kern-char-set-alignment clone (kern-obj-get-alignment caster))
          (kern-obj-put-at clone loc)))))
          

;; ----------------------------------------------------------------------------
;; Eighth Circle
;; ----------------------------------------------------------------------------

(define (in-flam-hur kspell caster)
  (define (flambe-foe kobj)
    (if (is-hostile? caster kobj)
        (burn kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   flambe-foe
                   F_fire))

(define (in-vas-grav-corp kspell caster)
  (define (energize-foe kobj)
    (if (is-hostile? caster kobj)
        (burn kobj)))
  (cast-wind-spell (kern-obj-get-location caster)
                   energize-foe
                   F_energy))

(define (an-tym kspell caster)
  (kern-add-time-stop 512))

(define (kal-xen-corp kspell caster)
  (define (mk-skeleton)
    (let ((skeleton (kern-mk-stock-char sp_skeleton 
                                        oc_raider
                                        s_skeleton
                                        " a skeleton" 
                                        nil)))
      (kern-being-set-base-faction skeleton faction-player)))
  (cast-summon-spell (kern-obj-get-location caster)
                     mk-skeleton
                     (kern-dice-roll "1d4"))
  result-ok)

(define (xen-corp kspell caster)
  (cast-missile-spell caster 6 deathball))

(define (in-mani-corp kspell caster)
  (let ((target (kern-ui-select-party-member)))
    (if (null? target) nil
        (kern-char-resurrect target))))

(define (vas-rel-por kspell caster)
  (define (rmgate kobj)
    (moongate-close gate)
    (kern-obj-remove gate)
    (kern-obj-destroy gate))
  (let ((loc (kern-ui-target (kern-obj-get-location caster) 1)))
    (if (null? loc) nil
        (let ((gate (summon-moongate 'ord)))
          (kern-obj-put-at gate loc)
          (moongate-open gate)))))

(define (kal-xen-nox kspell caster)
  (define (mk-aligned-slime)
    (mk-slime (kern-being-get-current-faction caster)))
  (cast-summon-spell (kern-obj-get-location caster) 
                     mk-aligned-slime
                     (kern-dice-roll "1d4")))

;; ----------------------------------------------------------------------------
;; This is the table of spells.
;; ----------------------------------------------------------------------------

;; shorter alias
(define s_magic_door s_magically_locked_solid_wood_door_in_stone)

(define spells
  (list
   ;;    tag          name                sprite          handler      code L context      mixture
   ;;    ==========   ==============      ==============  =======      ==== = ===========  =====================================
   (list 'an_nox      "An Nox spell"      s_field_poison  an-nox      "AN"  1 context-any  (list garlic ginseng))
   (list 'an_zu       "An Zu spell"       s_field_sleep   an-zu       "AZ"  1 context-any  (list garlic ginseng))
   (list 'grav_por    "Grav Por spell"    s_arrow         grav-por    "GP"  1 context-town (list spider_silk black_pearl))
   (list 'in_lor      "In Lor spell"      s_torch         in-lor      "IL"  1 context-any  (list sulphorous_ash))
   (list 'mani        "Mani spell"        s_red_potion    mani        "M"   1 context-any  (list ginseng spider_silk))

   (list 'an_sanct    "An Sanct spell"    s_door          an-sanct    "AS"  2 context-town (list sulphorous_ash blood_moss))
   (list 'sanct       "Sanct spell"       s_door_locked   sanct       "S"   2 context-town (list sulphorous_ash spider_silk))
   (list 'an_xen_corp "An Xen Corp spell" s_skeleton      an-xen-corp "AXC" 2 context-town (list garlic sulphorous_ash))
   (list 'in_wis      "In Wis spell"      s_scroll2       in-wis      "IW"  2 context-any  (list nightshade))
   (list 'kal_xen     "Kal Xen spell"     s_snake         kal-xen     "KX"  2 context-town (list spider_silk mandrake))
   (list 'rel_hur     "Rel Hur spell"     s_ship          rel-hur     "RH"  2 context-any  (list sulphorous_ash blood_moss))
   (list 'in_nox_por  "In Nox Por spell"  s_green_magbolt in-nox-por  "INP" 2 context-town (list nightshade blood_moss black_pearl))

   (list 'in_flam_grav "In Flam Grav spell" s_field_fire   in-flam-grav "IFG" 3 context-town (list sulphorous_ash black_pearl spider_silk))
   (list 'in_nox_grav  "In Nox Grav spell"  s_field_poison in-nox-grav  "ING" 3 context-town (list nightshade black_pearl spider_silk))
   (list 'in_zu_grav   "In Zu Grav spell"   s_field_sleep  in-zu-grav   "IZG" 3 context-town (list ginseng black_pearl spider_silk))
   (list 'vas_flam     "Vas Flam"           s_red_magbolt  vas-flam     "VF"  3 context-town (list sulphorous_ash black_pearl))
   (list 'vas_lor      "Vas Lor"            s_gold_explosion vas-lor    "VL"  3 context-any (list mandrake sulphorous_ash))

   (list 'an_grav       "An Grav spell"       s_magic         an-grav       "AG"  4 context-any (list black_pearl sulphorous_ash))
   (list 'uus_por       "Uus Por spell"       s_ladder_up     uus-por       "UP"  4 context-any (list blood_moss spider_silk))
   (list 'des_por       "Des Por spell"       s_ladder_down   des-por       "DP"  4 context-any (list blood_moss spider_silk))
   (list 'in_sanct_grav "In Sanct Grav spell" s_field_energy  in-sanct-grav "ISG" 4 context-town (list mandrake black_pearl spider_silk))
   (list 'in_sanct      "In Sanct spell"      s_large_shield  in-sanct      "IS"  4 context-any (list sulphorous_ash ginseng garlic))
   (list 'wis_quas      "Wis Quas spell"      s_magic         wis-quas      "WQ"  4 context-any (list nightshade spider_silk))

   (list 'in_ex_por   "In Ex Por spell"   s_magic_door    in-ex-por   "IEP" 5 context-any  (list sulphorous_ash blood_moss))
   (list 'an_ex_por   "An Ex Por spell"   s_magic_door    an-ex-por   "AEP" 5 context-any  (list sulphorous_ash blood_moss garlic))
   (list 'in_bet_xen  "In Bet Xen spell"  s_insects       in-bet-xen  "IBX" 5 context-town (list spider_silk blood_moss sulphorous_ash))
   (list 'in_zu       "In Zu spell"       s_corpse        in-zu       "IZ"  5 context-town (list nightshade spider_silk black_pearl))
   (list 'vas_mani    "Vas Mani spell"    s_red_potion    vas-mani    "VM"  5 context-any  (list mandrake spider_silk ginseng))
   (list 'rel_tym     "Rel Tym spell"     s_magic         rel-tym     "RT"  5 context-any  (list sulphorous_ash blood_moss spider_silk))

   (list 'in_an           "In An spell"           s_magic              in-an           "IA"   6 context-any  (list garlic mandrake sulphorous_ash))
   (list 'wis_an_ylem     "Wis An Ylem spell"     s_magic              wis-an-ylem     "WAY"  6 context-any  (list mandrake sulphorous_ash))
   (list 'an_xen_exe      "An Xen Exe spell"      s_happy_monster_face an-xen-exe      "AXE"  6 context-town (list black_pearl nightshade spider_silk))
   (list 'in_vas_por_ylem "In Vas Por Ylem spell" s_mountains          in-vas-por-ylem "IVPY" 6 context-town (list mandrake blood_moss sulphorous_ash))
   (list 'quas_an_wis     "Quas An Wis spell"     s_magic              quas-an-wis     "QAW"  6 context-town (list mandrake nightshade))

   (list 'in_nox_hur   "In Nox Hur spell"   s_field_poison  in-nox-hur   "INH" 7 context-town (list nightshade sulphorous_ash blood_moss))
   (list 'in_zu_hur    "In Zu Hur spell"    s_field_poison  in-zu-hur    "IZH" 7 context-town (list mandrake ginseng blood_moss))
   (list 'in_quas_corp "In Quas Corp spell" s_magic         in-quas-corp "IQC" 7 context-town (list nightshade mandrake garlic))
   (list 'in_quas_wis  "In Quas Wis spell"  s_magic         in-quas-wis  "IQW" 7 context-any  (list nightshade mandrake))
   (list 'sanct_lor    "Sanct Lor spell"    s_null          sanct-lor    "SL"  7 context-any  (list nightshade mandrake blood_moss))
   (list 'xen_corp     "Xen Corp spell"     s_corpse        xen-corp     "XC"  7 context-town (list nightshade black_pearl))
   (list 'in_quas_xen  "In Quas Xen spell"  s_magic         in-quas-xen  "IQX" 7 context-town (list nightshade mandrake sulphorous_ash spider_silk
                                                                                                    blood_moss ginseng))
   (list 'kal_xen_nox      "Kal Xen Nox spell"      s_slime        kal-xen-nox      "KXN"  8 context-town (list spider_silk mandrake nightshade))


   (list 'in_flam_hur      "In Flam Hur spell"      s_field_fire   in-flam-hur      "IFH"  8 context-town (list mandrake sulphorous_ash blood_moss))
   (list 'in_vas_grav_corp "In Vas Grap Corp spell" s_field_energy in-vas-grav-corp "IVGC" 8 context-town (list mandrake sulphorous_ash nightshade))
   (list 'an_tym           "An Tym spell"           s_magic        an-tym           "AT"   8 context-any (list mandrake garlic blood_moss))
   (list 'kal_xen_corp     "Kal Xen Corp spell"     s_skeleton     kal-xen-corp     "KXC"  8 context-town (list spider_silk mandrake nightshade))
   (list 'in_mani_corp     "In Mani Corp spell"     s_magic        in-mani-corp     "IMC"  8 context-any (list garlic ginseng spider_silk 
                                                                                                               sulphorous_ash blood_moss mandrake))
   (list 'vas_rel_por      "Vas Rel Por spell"      s_moongate_full vas-rel-por     "VRP"  8 context-any (list sulphorous_ash mandrake black_pearl))
   ))

;; ----------------------------------------------------------------------------
;; Now rip through the list of spells, adding them to the kernel.
;; ----------------------------------------------------------------------------

(map (lambda (spell) (apply mk-spell spell)) spells)
