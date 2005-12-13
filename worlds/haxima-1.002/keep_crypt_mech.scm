;;----------------------------------------------------------------------------
;; Special mech for the Keep Crypt
;;
;;    When the lever is pulled, this mech engages. Every turn it picks a
;; random location from its region. If the location does not contain a being
;; but does contain a corpse it removes the corpse and creates a skeletal
;; warrior in its place.
;;----------------------------------------------------------------------------

(define (kcm-mk area)
  (list #f area))
(define (kcm-set-on! kcm) (set-car! kcm #t))
(define (kcm-on? kcm) (car kcm))
(define (kcm-area kcm) (cadr kcm))

(define (kcm-exec-main kcm kplace)

  (define (pick-loc)
    (let ((area (kcm-area kcm)))
      (let ((x (+ (rect-x area)
                  (modulo (random-next) (rect-w area))))
            (y (+ (rect-y area)
                  (modulo (random-next) (rect-h area)))))
        (mk-loc kplace x y))))

  (define (get-corpse-at loc)
    ;;(display "get-corpse-at")(newline)
    (let ((corpses (find-object-types-at loc t_corpse)))
      (if (null? corpses)
          nil
          (car corpses))))
  
  (let* ((loc (pick-loc))
         (corpse (get-corpse-at loc)))
    
    ;;(display "loc:")(display loc)(newline)
    ;;(display "corpse:")(display corpse)(newline)
    
    (define (corpse-at? loc)
      ;;(display "corpse-at?")(newline)
      (not (null? corpse)))
    
    (define (remove-corpse loc)
      ;;(display "remove-corpse")(newline)
      (kern-obj-remove corpse))
    
    (define (mk-skeleton)
      (if (> (modulo (random-next) 2) 0)
          (mk-npc 'skeletal-warrior (kern-dice-roll "1d3+4"))
          (mk-npc 'skeletal-spear-thrower (kern-dice-roll "1d3+4"))))

    (define (put-skeleton loc)
      ;;(display "put-skeleton")(newline)
      (kern-obj-put-at (mk-skeleton) loc))
    
    (define (good? loc)
      ;;(display "good?")(newline)
      (and (not (being-at? loc))
           (corpse-at? loc)))
    
    (define (change-corpse-to-skeleton loc)
      ;;(display "change-corpse-to-skeleton")(newline)
      (kern-log-msg "You have disturbed the dead!")
      (remove-corpse loc)
      (put-skeleton loc))
    
    (if (good? loc)
        (change-corpse-to-skeleton loc))))

(define (kcm-exec kself)
  (let ((kcm (kobj-gob-data kself)))
    (if (and (kcm-on? kcm)
             (> (modulo (random-next) 20) 16))
        (kcm-exec-main kcm
                       (loc-place (kern-obj-get-location kself))))))

(define (kcm-on kself ksender)
  (kcm-set-on! (kobj-gob-data kself)))
        
(define kcm-ifc
  (ifc '()
       (method 'on kcm-on)
       (method 'exec kcm-exec)))

(mk-obj-type 't_kcm nil nil layer-none kcm-ifc)

(define (mk-kcm area)
  (bind (kern-mk-obj t_kcm 1)
        (kcm-mk area)))
