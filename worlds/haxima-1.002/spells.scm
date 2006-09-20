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


  
;; ============================================================================
;; Wind spell support
;; ============================================================================

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

;; ----------------------------------------------------------------------------
;; Now rip through the list of spells, adding them to the kernel.
;; ----------------------------------------------------------------------------

(map (lambda (spell) (apply mk-spell spell))
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
      (list 'ylem_an_ex    "Ylem An Ex spell"    ylem-an-ex    "YAE"  1 context-town (list spider_silk black_pearl))

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
      (list 'bet_flam_hur  "Bet Flam Hur spell"  bet-flam-hur  "BFH"  2 context-town (list black_pearl sulphorous_ash blood_moss))

      ;; Third Circle
      (list 'in_flam_grav  "In Flam Grav spell"  in-flam-grav  "IFG" 3 context-town (list sulphorous_ash black_pearl spider_silk))
      (list 'in_nox_grav   "In Nox Grav spell"   in-nox-grav   "ING" 3 context-town (list nightshade black_pearl spider_silk))
      (list 'in_zu_grav    "In Zu Grav spell"    in-zu-grav    "IZG" 3 context-town (list ginseng black_pearl spider_silk))
      (list 'vas_flam      "Vas Flam"            vas-flam      "VF"  3 context-town (list sulphorous_ash black_pearl))
      (list 'vas_lor       "Vas Lor"             vas-lor       "VL"  3 context-any (list mandrake sulphorous_ash))
      (list 'in_flam_sanct "In Flam Sanct spell" in-flam-sanct "IFS" 3 context-any (list garlic sulphorous_ash t_royal_cape))

      ;; Fourth Circle
      (list 'an_grav       "An Grav spell"       an-grav       "AG"  4 context-any (list black_pearl sulphorous_ash))
      ;;(list 'uus_por       "Uus Por spell"       uus-por       "UP"  4 context-any (list blood_moss spider_silk))
      ;;(list 'des_por       "Des Por spell"       des-por       "DP"  4 context-any (list blood_moss spider_silk))
      (list 'in_sanct_grav "In Sanct Grav spell" in-sanct-grav "ISG" 4 context-town (list mandrake black_pearl spider_silk))
      (list 'in_sanct      "In Sanct spell"      in-sanct      "IS"  4 context-any (list sulphorous_ash ginseng garlic))
      (list 'wis_quas      "Wis Quas spell"      wis-quas      "WQ"  4 context-any (list nightshade sulphorous_ash))
      (list 'bet_por       "Bet Por spell"       bet-por       "BP"  4 context-town  (list black_pearl blood_moss))
      (list 'vas_sanct_nox  "Vas Sanct Nox spell"  vas-sanct-nox  "VSN" 3 context-any (list mandrake nightshade garlic))

      ;; Fifth Circle
      (list 'in_ex_por   "In Ex Por spell"   in-ex-por   "IEP" 5 context-any  (list sulphorous_ash blood_moss))
      (list 'an_ex_por   "An Ex Por spell"   an-ex-por   "AEP" 5 context-any  (list sulphorous_ash blood_moss garlic))
      (list 'in_bet_xen  "In Bet Xen spell"  in-bet-xen  "IBX" 5 context-town (list spider_silk blood_moss sulphorous_ash))
      (list 'in_zu       "In Zu spell"       in-zu       "IZ"  5 context-town (list nightshade spider_silk ginseng))
      (list 'vas_mani    "Vas Mani spell"    vas-mani    "VM"  5 context-any  (list mandrake spider_silk ginseng))
      (list 'rel_tym     "Rel Tym spell"     rel-tym     "RT"  5 context-any  (list sulphorous_ash blood_moss mandrake))

      ;; Sixth Circle
      (list 'in_an           "In An spell"           in-an           "IA"   6 context-any  (list garlic mandrake sulphorous_ash))
      (list 'wis_an_ylem     "Wis An Ylem spell"     wis-an-ylem     "WAY"  6 context-any  (list mandrake sulphorous_ash))
      (list 'an_xen_ex      "An Xen Ex spell"      an-xen-ex      "AXE"  6 context-town (list black_pearl nightshade spider_silk))
      (list 'in_vas_por_ylem "In Vas Por Ylem spell" in-vas-por-ylem "IVPY" 6 context-town (list mandrake blood_moss sulphorous_ash))
      (list 'quas_an_wis     "Quas An Wis spell"     quas-an-wis     "QAW"  6 context-town (list mandrake nightshade))
      (list 'vas_uus_ylem    "Vas Uus Ylem spell"    vas-uus-ylem    "VUY"  6 context-wilderness (list mandrake blood_moss spider_silk))
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

