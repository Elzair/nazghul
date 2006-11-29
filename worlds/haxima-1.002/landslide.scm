;; landslide -- shake the map, drop some boulders, and damage any non-flying,
;; non-phasing beings that get hit.
(define (drop-rock-on kbeing)
  (cond ((and (not (can-fly? kbeing))
              (not (can-phase? kbeing)))
         (kern-log-msg (kern-obj-get-name kbeing)
                       " hit by falling rock!")
         (kern-obj-apply-damage kbeing 
                                "hit"
                                (kern-dice-roll "1d10")))
        ))

(define (drop-boulder x loc)
  (cond ((<= (modulo (random-next) 100) 50)
         (kern-place-set-terrain loc t_boulder)         
         (map drop-rock-on (get-beings-at loc))
         )))

(define (landslide kbeing x y w h)
  (kern-log-msg "LANDSLIDE!")
  (shake-map 10)
  (foldr-rect (loc-place (kern-obj-get-location kbeing))
             x
             y
             w
             h
             drop-boulder
             #f)
  #t)

(define (mk-landslide x y w h)
  (mk-step-trig 'landslide x y w h))