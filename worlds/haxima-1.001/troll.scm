(define troll-base-hp       20)
(define troll-critical-hp   10)
(define troll-melee-weapon  t_hands)
(define troll-ranged-weapon t_thrown_boulder)
(define troll-speed         speed-human)
(define troll-ripup-boulder-ap (* 2 troll-speed))

;; ----------------------------------------------------------------------------
;; Trick: make a "troll corpse" container type and use it as the troll's
;; container. When the troll dies the kernel will drop the troll's container,
;; making it look like the troll corpse is a container.
;; ----------------------------------------------------------------------------
(mk-obj-type 'troll-corpse-type "troll corpse" s_troll_corpse
             layer-container nil)


;;----------------------------------------------------------------------------
;; Species declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-species 'sp_troll      ;; tag: script variable name
                 "troll"        ;; name: used to display name in the UI
                 14             ;; strength: limits armament weight
                 6              ;; intelligence: (just reported in stats)
                 12             ;; dexterity: used to avoid traps on chests
                 troll-speed    ;; speed: action points per turn
                 10             ;; vision radius: in tiles
                 mmode-walk     ;; movement mode
                 troll-base-hp  ;; base hp: hit points at level zero
                 2              ;; hp multiplier: extra hp per level
                 0              ;; base mp: mana points at level zero
                 0              ;; mp multiplier: extra mana points per level
                 s_troll_corpse ;; sleep sprite
                 t_hands        ;; natural weapon: used when unarmed
                 #t             ;; visible: can be seen
                 sound-damage   ;; damage sound
                 sound-walking  ;; walking sound
                 humanoid-slots ;; slots: hands
                 nil            ;; native spells: currently unused
                 )

;;----------------------------------------------------------------------------
;; Occupation declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-occ 'oc_troll            ;; tag
             "raider"             ;; name 
             0.0                  ;; magic 
             2                    ;; base hp
             2                    ;; hp per level 
             0                    ;; base mp
             0                    ;; mp per level 
             2                    ;; hit_mod 
             -1                   ;; def_mod 
             2                    ;; dam_mod 
             -1                   ;; arm_mod
             troll-corpse-type    ;; container (needed for items)
             nil                  ;; typical traps on the container
             ;; readied:
             (list troll-ranged-weapon)
             ;; items: typical equipment
             (list (list troll-ranged-weapon 100 3)
                   )
             )

;;----------------------------------------------------------------------------
;; Constructor
;;----------------------------------------------------------------------------
(define (mk-troll faction)
  (let ((troll (kern-mk-stock-char sp_troll 
                                   oc_troll
                                   s_troll ;; no troll sprite yet
                                   "a troll" 
                                   'troll-ai)))
    (kern-being-set-base-faction troll faction)
    troll ;; return the kernel object
    ))

(define (mk-hill-troll)
  (mk-troll faction-hill-troll))

(define (char-is-troll? kchar)
  (eqv? (kern-char-get-species kchar) sp_troll))


;;----------------------------------------------------------------------------
;; Troll AI
;;----------------------------------------------------------------------------
(define (troll-is-critical? ktroll)
  (< (kern-char-get-hp ktroll) troll-critical-hp))

(define (troll-wander ktroll) (wander ktroll))
(define (troll-flee ktroll) (flee ktroll))

(define (troll-foes-in-weapon-range ktroll karms kfoes)
  (display "troll-foes-in-weapon-range")(newline)
  (all-in-range (kern-obj-get-location ktroll) 
                (kern-arms-type-get-range karms)
                 kfoes))

(define (weaker? a b)
  (< (kern-char-get-hp a) (kern-char-get-hp b)))

(define (troll-pick-target ktroll foes)
  (foldr (lambda (a b) (if (weaker? a b) a b))
         (car foes) 
         (cdr foes)))

(define (troll-pathfind-foe ktroll foes)
  (display "troll-pathfind-foe")(newline)
  (let ((ktarg (troll-pick-target ktroll foes)))
    (if (notnull? ktarg)
        (pathfind ktroll (kern-obj-get-location (troll-pick-target ktroll 
                                                                   foes))))))

(define (troll-attack ktroll karms foes)
  (kern-char-attack ktroll 
                    karms
                    (troll-pick-target ktroll 
                                       foes)))

;; Given an "origin" location and a list of locations, find the location in the
;; list closest to the coordinates.
(define (loc-closest origin lst)
  (if (null? lst) nil
      (foldr (lambda (a b) (if (loc-closer? a b origin) a b))
             (car lst)
             (cdr lst))))

(define (troll-stronger? ktroll foes)
  (> (kern-char-get-strength ktroll)
     (foldr (lambda (a b) (+ a (kern-char-get-strength b))) 
            0 
            foes)))

(define (troll-has-ranged-weapon? ktroll)
  (in-inventory? ktroll troll-ranged-weapon))

;; troll-get-ammo -- give troll a boulder and convert terrain to grass
(define (troll-get-terrain-ammo ktroll coords)
  (display "troll-get-terrain-ammo")(newline)
  (kern-obj-add-to-inventory ktroll troll-ranged-weapon 1)
  (kern-place-set-terrain coords t_grass)
  (kern-map-repaint)
  (kern-obj-dec-ap ktroll troll-ripup-boulder-ap)
  )

;; ----------------------------------------------------------------------------
;; troll-get-loose-ammo -- search the objects at the location for ammo and give
;; it to the th character
;; ----------------------------------------------------------------------------
(define (troll-get-loose-ammo ktroll loc)
  (kobj-get-at ktroll loc troll-ranged-weapon))

;; ----------------------------------------------------------------------------
;; troll-terrain-is-ammo -- true iff the given location's terrain can be
;; converted by a troll into ammo
;; ----------------------------------------------------------------------------
(define (troll-terrain-is-ammo? coords)
  (eqv? t_boulder (kern-place-get-terrain coords)))

;; ----------------------------------------------------------------------------
;; troll-find-nearest-ammo -- return the closest location with ammo objects or
;; with terrain that can be converted to ammo objects.
;; ----------------------------------------------------------------------------
(define (troll-find-nearest-ammo ktroll)
  (define (check loc)
    (define (scanobjlst lst)
      (foldr (lambda (a b) 
               (or a (eqv? (kern-obj-get-type b) troll-ranged-weapon)))
             #f
             lst))
    (if (troll-terrain-is-ammo? loc)
        loc
        (if (scanobjlst (kern-place-get-objects-at loc))
            loc
            nil)))
  (let* ((loc (kern-obj-get-location ktroll))
         (rad (kern-obj-get-vision-radius ktroll))
         (coords (search-rect (loc-place loc)
                              (- (loc-x loc) (/ rad 2))
                              (- (loc-y loc) (/ rad 2))
                              (* 1 rad)
                              (* 1 rad)
                              check)))
    (loc-closest loc coords)))

;; ----------------------------------------------------------------------------
;; troll-get-ammo -- given the location of an ammo object or terrain that can
;; be converted to ammo, have the troll get the ammo
;; ----------------------------------------------------------------------------
(define (troll-get-ammo ktroll loc)
  (display "troll-get-ammo")(newline)
  (if (troll-terrain-is-ammo? loc)
      (troll-get-terrain-ammo ktroll loc)
      (troll-get-loose-ammo ktroll loc)))

;; ----------------------------------------------------------------------------
;; troll-hunt-for-ammo2 -- find the nearest available ammo and pathfind to it
;; or pick it up. Returns false iff none available.
;; ----------------------------------------------------------------------------
(define (troll-hunt-for-ammo2 ktroll)
  (let ((nearest (profile troll-find-nearest-ammo ktroll))
        (kloc (kern-obj-get-location ktroll)))
    (display "nearest=")(display nearest)(newline)
    (if (null? nearest)
        #f
        (begin
          (do-or-goto ktroll nearest troll-get-ammo)
          #t))))

(define (display-objs lst)
  (if (null? lst)
      (newline)
      (begin
        (display (kern-obj-get-name (car lst)))
        (display " ")
        (display-objs (cdr lst)))))

;; ----------------------------------------------------------------------------
;; troll-ai -- combat ai for a troll npc. Called repeatedly by the kernel on
;; the troll's turn until the troll is out of ap.
;; ----------------------------------------------------------------------------
(define (troll-ai ktroll)
  (newline)(display "troll-ai")(newline)
  (let ((foes (all-visible-hostiles ktroll)))
    (display "foes: ")(display-objs foes)
    (if (null? foes)
        (troll-wander ktroll)
        (if (troll-is-critical? ktroll) 
            (troll-flee ktroll)
            (let ((melee-targs (troll-foes-in-weapon-range ktroll 
                                                           troll-melee-weapon 
                                                           foes)))
              (display "troll-ai:melee-targs=")(display melee-targs)
              (newline)
              (if (null? melee-targs)
                  (if (troll-has-ranged-weapon? ktroll)
                      (let 
                          ((ranged-foes 
                            (troll-foes-in-weapon-range ktroll
                                                        troll-ranged-weapon
                                                        foes)))
                        (display "troll-ai:ranged-foes=")(display ranged-foes)
                        (newline)
                        (if (null? ranged-foes)
                            (troll-pathfind-foe ktroll foes)
                            (troll-attack ktroll troll-ranged-weapon 
                                          ranged-foes)))
                      (or (troll-hunt-for-ammo2 ktroll)
                          (troll-pathfind-foe ktroll foes)))
                  (if (troll-stronger? ktroll melee-targs)
                      (troll-attack ktroll troll-melee-weapon melee-targs)
                      (evade ktroll melee-targs))))))))
