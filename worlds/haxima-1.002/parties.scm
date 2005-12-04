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
(define (ptype-mk sprite faction . groups)
  (list 'ptype sprite faction groups))
(define (ptype-sprite ptype) (cadr ptype))
(define (ptype-faction ptype) (caddr ptype))
(define (ptype-groups ptype) (cadddr ptype))
(define (ptype-generate ptype)
  (let ((kparty (kern-mk-party)))
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
(define forest-goblins (ptype-mk s_orc 
                                 faction-forest-goblin
                                 (pgroup-mk 'forest-goblin-stalker "1d2")
                                 (pgroup-mk 'forest-goblin-hunter  "1d2-1")))
