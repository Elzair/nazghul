(define (mk-spawnpoint type max) (list type max 0))
(define (spawnpoint-type kobj) (car (kobj-gob-date kobj))
(define (spawnpoint-max kobj) (cadr (kobj-gob-date kobj))
(define (spawnpoint-count kobj) (caddr (kobj-gob-date kobj))


(define (spawnpoint-exec kspawnpoint)
  (let ((kobj ((spawnpoint-type kspawnpoint))))
    (kobj-put-at kobj (kobj-loc kspawnpoint))
    (

(define (spawnpoint-timer kspawnpoint)
  (spawnpoint-exec kspawnpoint)
  (set-timer 10
             spawnpoint-timer 
             kspawnpoint))

(define (spawnpoint-on-entry kspawnpoint)
  (spawnpoint-exec kspawnpoint)
  (set-timer (modulo (random-next 10)) 
             spawnpoint-timer 
             kspawnpoint))

(define spawnpoint-ifc
  (ifc '()
       (method 'on-entry spawnpoint-on-entry)))

(mk-ktype 't_spawnpoint
          nil
          nil
          layer-none
          spawnpoint-ifc)
                  

(define (spawnpoint type max)
  (let ((kobj (mk-kobj 't_spawnpoint 1))
        (gob '(type max 0 )))
    (kern-obj-set-visible kobj #f)
    (bind kobj gob)))

(spawnpoint mk-troll 2)
