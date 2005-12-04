;;----------------------------------------------------------------------------
;; pgroup -- one type of npc within an npc party type
(define (pgroup-mk npct dice)
  (list 'pgroup npct dice))
(define (pgroup-npct pgrp) (cadr pgrp))
(define (pgroup-dice pgrp) (caddr pgrp))
(define (pgroup-size pgrp)
  (* (kern-dice-roll (pgroup-dice pgrp))
     (length (kern-party-get-members (kern-get-player)))
     ))
(define (pgroup-generate pgrp)
  (define (loop n)
    (if (<= n 0)
        nil
        (cons (mk-npc (pgroup-npct pgrp) (calc-level))
              (loop (- n 1)))))
  (loop (pgroup-size pgrp)))

;;----------------------------------------------------------------------------
;; ptype -- npc party type
(define (ptype-mk name sprite faction . groups)
  (list 'ptype sprite faction groups name))
(define (ptype-sprite ptype) (cadr ptype))
(define (ptype-faction ptype) (caddr ptype))
(define (ptype-groups ptype) (cadddr ptype))
(define (ptype-name ptype) (list-ref ptype 4))
(define (ptype-generate ptype)
  (let ((kparty (kern-mk-party)))
    (kern-being-set-name kparty (ptype-name ptype))
    (kern-obj-set-sprite kparty (ptype-sprite ptype))
    (kern-being-set-base-faction kparty (ptype-faction ptype))
    (map (lambda (pgroup)
           (map (lambda (kchar)
                  (kern-party-add-member kparty kchar))
                (pgroup-generate pgroup)))
         (ptype-groups ptype))
    kparty
    ))

;;----------------------------------------------------------------------------
;; mk-npc-party
(define (mk-npc-party ptype) (ptype-generate (eval ptype)))

;;----------------------------------------------------------------------------
;; NPC PARTY TYPES
(define forest-goblin-party 
  (ptype-mk "goblin hunting party" s_orc faction-forest-goblin
            (pgroup-mk 'forest-goblin-stalker "1d2")
            (pgroup-mk 'forest-goblin-hunter  "1d2-1")
            ))

(define bandit-party 
  (ptype-mk "bandit gang" s_brigand faction-outlaw
            (pgroup-mk 'bandit "1d2")
            ))

(define troll-party 
  (ptype-mk "troll with goblin slaves" s_troll faction-troll
            (pgroup-mk 'troll "1")
            (pgroup-mk 'forest-goblin-stalker "1d3-1")
            ))

(define ranger-party 
  (ptype-mk "ranger patrol" s_companion_ranger faction-green-tower
            (pgroup-mk 'ranger "1d2")
            ))

(define green-slime-party 
  (ptype-mk "ooze of slimes" s_slime faction-monster
            (pgroup-mk 'green-slime "1d3")
            ))

(define militia-party 
  (ptype-mk "militia squad" s_guard faction-glasdrin
            (pgroup-mk 'halberdier "1d2")
            (pgroup-mk 'crossbowman "1d2-1")
            (pgroup-mk 'medik "1d2-1")
            ))

(define dryad-party 
  (ptype-mk "dryad grove" s_reaper faction-monster
            (pgroup-mk 'dryad "1")
            (pgroup-mk 'wolf "1d3-1")
            ))

(define wolf-party 
  (ptype-mk "wolf pack" s_wolf faction-monster
            (pgroup-mk 'wolf "1d3-1")
            ))

(define skeleton-party 
  (ptype-mk "skeleton brigade" s_skeleton faction-monster
            (pgroup-mk 'skeletal-warrior "1d2")
            (pgroup-mk 'skeletal-spear-thrower "1d3-1")
            ))

(define lich-party 
  (ptype-mk "lich with undead servants" s_lich faction-monster
            (pgroup-mk 'lich "1")
            (pgroup-mk 'skeletal-warrior "1d2-1")
            (pgroup-mk 'skeletal-spear-thrower "1d2-1")
            ))

(define wisp-party 
  (ptype-mk "parliament of wisps" s_wisp faction-none
            (pgroup-mk 'wisp "1d3")
            ))

(define ghast-party 
  (ptype-mk "haunt of ghasts" s_ghost faction-monster
            (pgroup-mk 'ghast "1d3")
            ))

(define dragon-party
  (ptype-mk "lone dragon" s_dragon faction-monster
            (pgroup-mk 'dragon "1")
            ))
