(define mana-to-summon-green-slimes 3) ;; possible dup of spell table entry for kal xen nox
(define ap-to-summon-green-slimes 3) 
(define range-of-acid-spray 2) ;; dup of arms table entry

(define (summon-green-slimes kchar)
  (and (> (kern-char-get-mana kchar) mana-to-summon-green-slimes)
       (begin
         (kal-xen-nox kal_xen_nox kchar)
         (kern-char-dec-mana kchar mana-to-summon-green-slimes)
         (kern-obj-dec-ap kchar ap-to-summon-green-slimes)
         #t)))

(define (attack-a-target-with-acid-spray kchar hostiles)
  (and (<= (distance kchar (car hostiles)) range-of-acid-spray)
       (begin
         (kern-char-attack kchar t_acid_spray (car hostiles))
         #t)))

(define (yellow-slime-ai kchar)
  (let ((hostiles (all-visible-hostiles kchar)))
    (if (null? hostiles) (kern-obj-wander kchar)
        (begin
          (or (summon-green-slimes kchar)
              (attack-a-target-with-acid-spray kchar hostiles)
              (pathfind-to-a-target kchar hostiles)
              (kern-obj-wander kchar))))))
