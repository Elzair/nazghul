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
(define (mk-spell tag name cast-handler magic-words level context sprite
                  reagents)
  (let ((spell-ifc (ifc obj-ifc (method 'cast cast-handler))))
    (set! spell-ifcs (cons spell-ifc spell-ifcs))
    (kern-add-spell (mk-obj-type tag name nil layer-none spell-ifc)
                    magic-words 
                    level  ;; level
                    level  ;; mana cost
                    context 
                    (+ (/ level 2) 1) ;; action point cost
                    sprite ;; sprite (FIXME)
                    reagents
                    )))


  
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

;; Spell sprite set
(kern-mk-sprite-set 'ss_spells 32 32 8 8 0 0 "spells.png")

(define (mk-sprite tag offset)
  (kern-mk-sprite tag ss_spells 1 offset #f 0))

(mk-sprite 's_an_nox 0)
(mk-sprite 's_an_zu 1)
(mk-sprite 's_grav_por 2)
(mk-sprite 's_in_lor 3)
(mk-sprite 's_mani 4)
(mk-sprite 's_wis_sanct 5)
(mk-sprite 's_an_sanct_ylem 6)
(mk-sprite 's_ylem_an_ex 7)
(mk-sprite 's_sanct_nox 8)
(mk-sprite 's_an_sanct 9)
(mk-sprite 's_sanct 10)
(mk-sprite 's_an_xen_corp 11)
(mk-sprite 's_in_wis 12)
(mk-sprite 's_kal_xen 13)
(mk-sprite 's_rel_hur 14)
(mk-sprite 's_in_nox_por 15)
(mk-sprite 's_an_xen_bet 16)
(mk-sprite 's_bet_flam_hur 17)
(mk-sprite 's_in_flam_grav 18)
(mk-sprite 's_in_nox_grav 19)
(mk-sprite 's_in_zu_grav 20)
(mk-sprite 's_vas_flam 21)
(mk-sprite 's_vas_lor 22)
(mk-sprite 's_in_flam_sanct 23)
(mk-sprite 's_an_grav 24)
(mk-sprite 's_in_sanct_grav 25)
(mk-sprite 's_in_sanct 26)
(mk-sprite 's_wis_quas 27)
(mk-sprite 's_bet_por 28)
(mk-sprite 's_vas_sanct_nox 29)
(mk-sprite 's_in_ex_por 30)
(mk-sprite 's_an_ex_por 31)
(mk-sprite 's_in_bet_xen 32)
(mk-sprite 's_in_zu 33)
(mk-sprite 's_vas_mani 34)
(mk-sprite 's_rel_tym 35)
(mk-sprite 's_in_an 36)
(mk-sprite 's_wis_an_ylem 37)
(mk-sprite 's_an_xen_ex 38)
(mk-sprite 's_in_vas_por_ylem 39)
(mk-sprite 's_quas_an_wis 40)
(mk-sprite 's_vas_uus_ylem 41)
(mk-sprite 's_in_rel_por 42)
(mk-sprite 's_vas_por 43)
(mk-sprite 's_in_nox_hur 44)
(mk-sprite 's_in_zu_hur 45)
(mk-sprite 's_in_quas_corp 46)
(mk-sprite 's_in_quas_wis 47)
(mk-sprite 's_sanct_lor 48)
(mk-sprite 's_xen_corp 49)
(mk-sprite 's_in_quas_xen 50)
(mk-sprite 's_kal_xen_nox 51)
(mk-sprite 's_in_flam_hur 52)
(mk-sprite 's_in_vas_grav_corp 53)
(mk-sprite 's_an_tym 54)
(mk-sprite 's_kal_xen_corp 55)
(mk-sprite 's_in_mani_corp 56)
(mk-sprite 's_vas_rel_por 57)

;; ----------------------------------------------------------------------------
;; Now rip through the list of spells, adding them to the kernel.
;; ----------------------------------------------------------------------------

;;         tag          name                handler      code L context    sprite  mixture
;;         ==========   ==============      =======      ==== = =========  ======  =======
;; First Circle
(mk-spell 'an_nox      "An Nox spell"      an-nox      "AN"  1 context-any  s_an_nox (list garlic ginseng))
(mk-spell 'an_zu       "An Zu spell"       an-zu       "AZ"  1 context-any  s_an_zu (list garlic ginseng))
(mk-spell 'grav_por    "Grav Por spell"    grav-por    "GP"  1 context-town s_grav_por (list sulphorous_ash black_pearl))
(mk-spell 'in_lor      "In Lor spell"      in-lor      "IL"  1 context-any  s_in_lor (list sulphorous_ash))
(mk-spell 'mani        "Mani spell"        mani        "M"   1 context-any  s_mani (list ginseng spider_silk))
(mk-spell 'wis_sanct   "Wis Sanct spell"   wis-sanct   "WS"  1 context-town s_wis_sanct (list sulphorous_ash))
(mk-spell 'an_sanct_ylem "An Sanct Ylem spell" an-sanct-ylem "ASY" 1 context-town s_an_sanct_ylem (list blood_moss))
(mk-spell 'ylem_an_ex    "Ylem An Ex spell"    ylem-an-ex    "YAE"  1 context-town s_ylem_an_ex (list spider_silk black_pearl))

;; Second Circle
(mk-spell 'sanct_nox   "Sanct Nox spell"   sanct-nox   "SN"  2 context-any  s_sanct_nox (list nightshade garlic))
(mk-spell 'an_sanct    "An Sanct spell"    an-sanct    "AS"  2 context-town s_an_sanct (list sulphorous_ash blood_moss))
(mk-spell 'sanct       "Sanct spell"       sanct       "S"   2 context-town s_sanct (list sulphorous_ash spider_silk))
(mk-spell 'an_xen_corp "An Xen Corp spell" an-xen-corp "AXC" 2 context-town s_an_xen_corp (list garlic sulphorous_ash))
(mk-spell 'in_wis      "In Wis spell"      in-wis      "IW"  2 context-any  s_in_wis (list nightshade))
(mk-spell 'kal_xen     "Kal Xen spell"     kal-xen     "KX"  2 context-town s_kal_xen (list spider_silk mandrake))
(mk-spell 'rel_hur     "Rel Hur spell"     rel-hur     "RH"  2 context-any  s_rel_hur (list sulphorous_ash blood_moss))
(mk-spell 'in_nox_por  "In Nox Por spell"  in-nox-por  "INP" 2 context-town s_in_nox_por (list nightshade blood_moss black_pearl))
(mk-spell 'an_xen_bet  "An Xen Bet spell"  an-xen-bet  "AXB" 2 context-town s_an_xen_bet (list spider_silk garlic))
(mk-spell 'bet_flam_hur  "Bet Flam Hur spell"  bet-flam-hur  "BFH"  2 context-town s_bet_flam_hur (list black_pearl sulphorous_ash blood_moss))

;; Third Circle
(mk-spell 'in_flam_grav  "In Flam Grav spell"  in-flam-grav  "IFG" 3 context-town s_in_flam_grav (list sulphorous_ash black_pearl spider_silk))
(mk-spell 'in_nox_grav   "In Nox Grav spell"   in-nox-grav   "ING" 3 context-town s_in_nox_grav (list nightshade black_pearl spider_silk))
(mk-spell 'in_zu_grav    "In Zu Grav spell"    in-zu-grav    "IZG" 3 context-town s_in_zu_grav (list ginseng black_pearl spider_silk))
(mk-spell 'vas_flam      "Vas Flam"            vas-flam      "VF"  3 context-town s_vas_flam (list sulphorous_ash black_pearl))
(mk-spell 'vas_lor       "Vas Lor"             vas-lor       "VL"  3 context-any s_vas_lor (list mandrake sulphorous_ash))
(mk-spell 'in_flam_sanct "In Flam Sanct spell" in-flam-sanct "IFS" 3 context-any s_in_flam_sanct (list garlic sulphorous_ash t_royal_cape))

;; Fourth Circle
(mk-spell 'an_grav       "An Grav spell"       an-grav       "AG"  4 context-any s_an_grav (list black_pearl sulphorous_ash))
;;(mk-spell 'uus_por       "Uus Por spell"       uus-por       "UP"  4 context-any nil (list blood_moss spider_silk))
;;(mk-spell 'des_por       "Des Por spell"       des-por       "DP"  4 context-any nil (list blood_moss spider_silk))
(mk-spell 'in_sanct_grav "In Sanct Grav spell" in-sanct-grav "ISG" 4 context-town s_in_sanct_grav (list mandrake black_pearl spider_silk))
(mk-spell 'in_sanct      "In Sanct spell"      in-sanct      "IS"  4 context-any s_in_sanct (list sulphorous_ash ginseng garlic))
(mk-spell 'wis_quas      "Wis Quas spell"      wis-quas      "WQ"  4 context-any s_wis_quas (list nightshade sulphorous_ash))
(mk-spell 'bet_por       "Bet Por spell"       bet-por       "BP"  4 context-town  s_bet_por (list black_pearl blood_moss))
(mk-spell 'vas_sanct_nox  "Vas Sanct Nox spell"  vas-sanct-nox  "VSN" 3 context-any s_vas_sanct_nox (list mandrake nightshade garlic))

;; Fifth Circle
(mk-spell 'in_ex_por   "In Ex Por spell"   in-ex-por   "IEP" 5 context-any  s_in_ex_por (list sulphorous_ash blood_moss))
(mk-spell 'an_ex_por   "An Ex Por spell"   an-ex-por   "AEP" 5 context-any  s_an_ex_por (list sulphorous_ash blood_moss garlic))
(mk-spell 'in_bet_xen  "In Bet Xen spell"  in-bet-xen  "IBX" 5 context-town s_in_bet_xen (list spider_silk blood_moss sulphorous_ash))
(mk-spell 'in_zu       "In Zu spell"       in-zu       "IZ"  5 context-town s_in_zu (list nightshade spider_silk ginseng))
(mk-spell 'vas_mani    "Vas Mani spell"    vas-mani    "VM"  5 context-any  s_vas_mani (list mandrake spider_silk ginseng))
(mk-spell 'rel_tym     "Rel Tym spell"     rel-tym     "RT"  5 context-any  s_rel_tym (list sulphorous_ash blood_moss mandrake))

;; Sixth Circle
(mk-spell 'in_an           "In An spell"           in-an           "IA"   6 context-any  s_in_an (list garlic mandrake sulphorous_ash))
(mk-spell 'wis_an_ylem     "Wis An Ylem spell"     wis-an-ylem     "WAY"  6 context-any  s_wis_an_ylem (list mandrake sulphorous_ash))
(mk-spell 'an_xen_ex      "An Xen Ex spell"      an-xen-ex      "AXE"  6 context-town s_an_xen_ex (list black_pearl nightshade spider_silk))
(mk-spell 'in_vas_por_ylem "In Vas Por Ylem spell" in-vas-por-ylem "IVPY" 6 context-town s_in_vas_por_ylem (list mandrake blood_moss sulphorous_ash))
(mk-spell 'quas_an_wis     "Quas An Wis spell"     quas-an-wis     "QAW"  6 context-town s_quas_an_wis (list mandrake nightshade))
(mk-spell 'vas_uus_ylem    "Vas Uus Ylem spell"    vas-uus-ylem    "VUY"  6 context-wilderness s_vas_uus_ylem (list mandrake blood_moss spider_silk))
(mk-spell 'in_rel_por      "In Rel Por spell"      in-rel-por      "IRP"  6 context-town s_in_rel_por (list black_pearl blood_moss spider_silk))
(mk-spell 'vas_por         "Vas Por spell"         vas-por         "VP"   6 context-wilderness s_vas_por (list mandrake black_pearl blood_moss))

;; Seventh Circle
(mk-spell 'in_nox_hur   "In Nox Hur spell"   in-nox-hur   "INH" 7 context-town s_in_nox_hur (list nightshade sulphorous_ash blood_moss))
(mk-spell 'in_zu_hur    "In Zu Hur spell"    in-zu-hur    "IZH" 7 context-town s_in_zu_hur (list mandrake ginseng blood_moss))
(mk-spell 'in_quas_corp "In Quas Corp spell" in-quas-corp "IQC" 7 context-town s_in_quas_corp (list nightshade mandrake garlic))
(mk-spell 'in_quas_wis  "In Quas Wis spell"  in-quas-wis  "IQW" 7 context-any  s_in_quas_wis (list nightshade mandrake))
(mk-spell 'sanct_lor    "Sanct Lor spell"    sanct-lor    "SL"  7 context-any  s_sanct_lor (list nightshade mandrake blood_moss))
(mk-spell 'xen_corp     "Xen Corp spell"     xen-corp     "XC"  7 context-town s_xen_corp (list nightshade black_pearl))
(mk-spell 'in_quas_xen  "In Quas Xen spell"  in-quas-xen  "IQX" 7 context-town s_in_quas_xen (list nightshade mandrake sulphorous_ash spider_silk
                                                                                 blood_moss ginseng))

;; Eighth Circle
(mk-spell 'kal_xen_nox      "Kal Xen Nox spell"      kal-xen-nox      "KXN"  8 context-town s_kal_xen_nox (list spider_silk mandrake nightshade))
(mk-spell 'in_flam_hur      "In Flam Hur spell"      in-flam-hur      "IFH"  8 context-town s_in_flam_hur (list mandrake sulphorous_ash blood_moss))
(mk-spell 'in_vas_grav_corp "In Vas Grav Corp spell" in-vas-grav-corp "IVGC" 8 context-town s_in_vas_grav_corp (list mandrake sulphorous_ash nightshade))
(mk-spell 'an_tym           "An Tym spell"           an-tym           "AT"   8 context-any s_an_tym (list mandrake garlic blood_moss))
(mk-spell 'kal_xen_corp     "Kal Xen Corp spell"     kal-xen-corp     "KXC"  8 context-town s_kal_xen_corp (list spider_silk mandrake nightshade))
(mk-spell 'in_mani_corp     "In Mani Corp spell"     in-mani-corp     "IMC"  8 context-any s_in_mani_corp (list garlic ginseng spider_silk 
                                                                                             sulphorous_ash blood_moss mandrake))
(mk-spell 'vas_rel_por      "Vas Rel Por spell"      vas-rel-por     "VRP"  8 context-any s_vas_rel_por (list sulphorous_ash mandrake black_pearl))

