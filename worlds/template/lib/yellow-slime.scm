(define range-of-acid-spray 2) ;; dup of arms table entry

(define (attack-a-target-with-acid-spray kchar hostiles)
  ;(display "attack-a-target-with-acid-spray")(newline)
  (and (<= (distance kchar (car hostiles)) range-of-acid-spray)
       (begin
         (kern-char-attack kchar t_acid_spray (car hostiles))
         #t)))

(define (yellow-slime-ai kchar)
  ;(display "yellow-slime-ai")(newline)
  (let ((hostiles (all-visible-hostiles kchar)))
    ;(display "hostile: ")(display hostiles)(newline)
    (if (null? hostiles)
        #f
        (or (ai-summon kchar summon-slimes)
            (attack-a-target-with-acid-spray kchar hostiles)))))
