;;----------------------------------------------------------------------------
;; Skill procedures
;;
;; Skill procedures should not do any requirements-checking because the kernel
;; checks all requirements before allowing them to be called. Skill procedures
;; should always return (ie, evaluate to) one of the standard result-* codes
;; (eg, result-ok, result-no-target, etc... see naz.scm).

(define (skill-jump kactor)
  (cast-ui-ranged-loc powers-jump
                      kactor
                      2
                      0))

(define (skill-detect-trap kactor)
  (cast-ui-ranged-any powers-detect-traps
                      kactor 1 (occ-ability-thief kactor)
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 'get-traps)))
                      ))

(define (skill-sprint kactor)
  (if (has-effect? kactor ef_fatigue)
      result-not-now
      (let* ((origin (kern-obj-get-location kactor))
             (kplace (loc-place origin))
             (sprint-max-range (occ-ability-stracro kactor))
             (sprint-max-cost (* sprint-max-range (kern-obj-get-ap kactor)))
             )
        (println "max range " sprint-max-range ", max cost " sprint-max-cost)
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

(define (skill-disarm-trap kactor)
  ;; fixme: only checks topmost item. Also, this disarms ALL traps, regardless
  ;; of whether or not they have been detected, so it should involve some cost
  ;; or risk.
  (cast-ui-ranged-any powers-disarm-traps
                      kactor 1 (occ-ability-whitemagic kactor)
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 'rm-traps)))
                      ))
  
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
            2
            1 
            #f
            #f
            'skill-jump
            nil
            nil
            nil
            ))

(define sk_detect_trap
  (mk-skill "Detect Trap" "Check if a door or chest is trapped"
            2
            2
            #f
            #f
            'skill-detect-trap
            nil
            nil
            nil
            ))

(define sk_arm_trap
  (mk-skill "Arm Trap" "Allows character to use beartraps and caltrops"
            2
            2
            #f
            #t
            nil
            nil
            nil
            nil
            ))

(define sk_sprint
  (mk-skill "Sprint" "Move quickly, in a straight line, for a short distance"
            1
            1
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
            1              ;; ap
            1              ;; mp
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
            1              ;; ap
            1              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-disarm-trap ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_stealth
  (mk-skill "Stealth" "Avoid detection"
            1              ;; ap
            1              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-stealth ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_reach
  (mk-skill "Reach" "Handle objects more than one tile away"
            1              ;; ap
            1              ;; mp
            #f             ;; wilderness?
            #t             ;; passive?
            nil            ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_reach
  (mk-skill "Reach" "Handle objects more than one tile away"
            1              ;; ap
            1              ;; mp
            #f             ;; wilderness?
            #t             ;; passive?
            nil            ;; yusage 
            nil            ;; yusage check
            nil            ;; tools
            nil            ;; material
            ))

(define sk_butcher
  (mk-skill "Butcher" "Turn an animal corpse into food or materials"
            1              ;; ap
            1              ;; mp
            #f             ;; wilderness?
            #f             ;; passive?
            'skill-butcher ;; yusage 
            nil            ;; yusage check
            nil            ;; tools (fixme: add knife)
            nil            ;; material
            ))

(define sk_pickpocket
  (mk-skill "Pickpocket" "Take something from an NPC"
            1              ;; ap
            1              ;; mp
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

(define sks_wrogue
  (kern-mk-skill-set "Wrogue" (list 
                               (list 1 sk_sprint)
                               (list 3 sk_detect_trap)
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
                               (list 6 sk_detect_trap)
                               (list 8 sk_disarm_trap)
                               (list 10 sk_jump)
                               (list 12 sk_arm_trap)
                               (list 13 sk_wriggle)
                               (list 14 sk_reach)
                               (list 15 sk_pickpocket)
                               (list 16 sk_stealth)
                                 
                               (list 2 sk_butcher)
                               )))
