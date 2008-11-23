;;----------------------------------------------------------------------------
;; Skill procedures
;;
;; Skill procedures should not do any requirements-checking because the kernel
;; checks all requirements before allowing them to be called. Skill procedures
;; should always return (ie, evaluate to) one of the standard result-* codes
;; (eg, result-ok, result-no-target, etc... see naz.scm).

(define (skill-jump kactor)
  (define (range)
    (let ((x (* (occ-ability-stracro kactor) (kern-obj-get-ap kactor))))
      (cond ((> x 1000) 3) ;; inconceivable!
            ((> x 500) 2)
            ((> x 150) 1)
            (else 0))))
  (if (has-effect? kactor ef_fatigue)
      result-not-now
      (cast-ui-ranged-loc powers-jump
                          kactor
                          (range)
                          0)))

(define (skill-sprint kactor)
  (if (has-effect? kactor ef_fatigue)
      result-not-now
      (let* ((origin (kern-obj-get-location kactor))
             (kplace (loc-place origin))
             (sprint-max-range (occ-ability-stracro kactor))
             (sprint-max-cost (* sprint-max-range (kern-obj-get-ap kactor)))
             )
        ;;(println "max range " sprint-max-range ", max cost " sprint-max-cost)
        (define (too-far? origin dest)
          (let ((path (line (loc-x origin) (loc-y origin) 
                            (loc-x dest) (loc-y dest))))
            (let ((cost (foldr (lambda (d xy)
                                 (+ d 
                                    (kern-place-get-movement-cost (mk-loc kplace
                                                                          (car xy) 
                                                                          (cdr xy)) 
                                                                  kactor)
                                    ))
                               0
                               path)))
              (> cost sprint-max-cost))))
        (define (checkloc x y)
          (let ((dest (mk-loc kplace x y)))
            (and (kern-place-is-passable dest kactor)
                 (not (occupied? dest))
                 (kern-in-los? origin dest)
                 (not (too-far? origin dest))
                 )))
        (cast-ui-template-loc powers-sprint
                              kactor
                              (kern-mk-templ origin sprint-max-range 'checkloc)
                              0))))

(define (skill-wriggle kactor)
  ;; fixme: use smart target that only suggests viable locations?
  (cast-ui-ranged-loc powers-wriggle kactor 1 0))

(load "disarm-trap.scm")
  
(define (skill-stealth kactor)
  (kern-obj-add-effect kactor ef_stealth nil)
  result-ok)

(define (skill-butcher kactor)
  (cast-ui-ranged-any powers-butcher
                      kactor 1 (occ-ability-crafting kactor)
                      (mk-ifc-query 'butcher)))

(define (skill-pickpocket kactor)
  (cast-ui-basic-ranged-spell powers-pickpocket 
                              kactor 
                              1 
                              (occ-ability-thief kactor)
                              ))

;;----------------------------------------------------------------------------
;; Skill declarations
;;
;; (kern-mk-skill <tag>
;;                <name>
;;                <description>
;;                <ap-consumed>
;;                <mp-consumed>
;;                <can-use-in-wilderness?>
;;                <is-passive?>
;;                <yusage-proc>
;;                <yusage-special-check-proc>
;;                <list-of-required-tools>
;;                <list-of-required-consumables>)

(define (mk-skill name description relative-ap-cost mp-cost use-in-wilderness
                  is-passive yusage-proc yusage-special-check-proc list-of-required-tools list-of-required-consumables)
  (kern-mk-skill name description (* base-skill-ap relative-ap-cost) mp-cost use-in-wilderness
                 is-passive yusage-proc yusage-special-check-proc list-of-required-tools list-of-required-consumables))
					

(define sk_jump
  (mk-skill "Jump" "Jump over impassable terrain"
            0
            0 
            #f
            #f
            'skill-jump
            nil
            nil
            nil
            ))

(define sk_arm_trap
  (mk-skill "Arm Trap" "Allows character to use beartraps and caltrops"
            0
            0
            #f
            #t
            nil
            nil
            nil
            nil
            ))

(define sk_sprint
  (mk-skill "Sprint" "Move quickly, in a straight line, for a short distance"
            0
            0
            #f
            #f
            'skill-sprint
            nil
            nil
            nil
            nil
            ))

(define sk_wriggle
  (mk-skill "Wriggle" "Squeeze through tight spots"
            base-move-ap   ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-wriggle ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            (list (list t_grease 1)) ;; material
            ))

(define sk_disarm_trap
  ;; fixme: should some special tools be required?
  (mk-skill "Disarm Trap" "Disarm a trap on a door or chest"
            0              ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-disarm-trap ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_stealth
  (mk-skill "Stealth" "Avoid detection"
            base-move-ap   ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-stealth ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_reach
  (mk-skill "Reach" "Handle objects more than one tile away"
            base-move-ap   ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #t             ;; passive?
            nil            ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_butcher
  (mk-skill "Butcher" "Turn an animal corpse into food or materials"
            0              ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-butcher ;; yusage 
            nil            ;; yusage check
            nil            ;; tools (fixme: add knife)
            nil            ;; material
            ))

(define sk_pickpocket
  (mk-skill "Pickpocket" "Take something from an NPC"
            base-move-ap   ;; ap
            0              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-pickpocket ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

;;----------------------------------------------------------------------------
;; Skill Set declarations
;;
;; The number preceeding the skill name is the minimum level needed to use the
;; skill.

(define sks_warrior
  (kern-mk-skill-set "Warrior" (list
                                (list 1 sk_jump)
                                (list 1 sk_sprint)
                                )))

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list 
                               (list 1 sk_sprint)
                               (list 4 sk_disarm_trap)
                               (list 5 sk_jump)
                               (list 6 sk_arm_trap)
                               (list 7 sk_wriggle)
                               (list 7 sk_reach)
                               (list 8 sk_pickpocket)
                               (list 8 sk_stealth)
                               )))

(define sks_wright
  (kern-mk-skill-set "Wright" (list 
                               (list 1 sk_butcher)
                               )))

(define sks_wanderer 
  (kern-mk-skill-set "Wanderer" (list 
                               (list 2 sk_sprint)
                               (list 8 sk_disarm_trap)
                               (list 10 sk_jump)
                               (list 12 sk_arm_trap)
                               (list 13 sk_wriggle)
                               (list 14 sk_reach)
                               (list 15 sk_pickpocket)
                               (list 16 sk_stealth)
                               (list 2 sk_butcher)
                               )))
