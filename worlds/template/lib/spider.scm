;; Local variables
(define spider-melee-weapon t_hands)

;; Remapped display and newline to local procs so they can be disabled/enabled
;; for debug more conveniently
; (define (spider-display . args) 
;   (display (kern-get-ticks))
;   (display ":")
;   (apply display args))
; (define (spider-newline) (newline))

(define (spider-display . args) )
(define (spider-newline) )

;; ----------------------------------------------------------------------------
;; Spider Egg
;; 
;; ----------------------------------------------------------------------------

(define spider-egg-hatch-time 10)
(define (spider-egg-gob-mk) (list spider-egg-hatch-time))
(define (spider-egg-hatch-timer gob) (car gob))
(define (spider-egg-set-hatch-timer! gob val) (set-car! gob val))
(define (spider-egg-hatch-timer-expired? gob) (= 0 (spider-egg-hatch-timer gob)))
(define (spider-egg-dec-hatch-timer! gob)
  (spider-egg-set-hatch-timer! gob
                               (- (spider-egg-hatch-timer gob)
                                  1)))

;; spider-egg-disturbed - obsolete function that would return true if any
;; neighboring tiles contained non-spiders. I discontinued it because it made
;; eggs run too slowly (about 30ms per). Replaced it with a simple egg timer.
(define (spider-egg-disturbed kegg)
  (spider-display "spider-egg-disturbed")(spider-newline)
  (define (check val loc)
    ;;(display "loc:")(display loc)(newline)
    (or val
        (foldr (lambda (a b) (or a
                                 (and (obj-is-char? b)
                                      (not (is-spider? b)))))
               #f
               (kern-get-objects-at loc))))
  (let ((loc (kern-obj-get-location kegg)))
    (kern-fold-rect (loc-place loc)
                    (- (loc-x loc) 2)
                    (- (loc-y loc) 2)
                    5
                    5
                    check
                    #f)))

(define (spider-egg-hatch kegg)
  (spider-display "spider-egg-hatch")(spider-newline)
  (kern-log-msg "A spider hatches!")
  (kern-obj-put-at (mk-npc 'giant-spider (calc-level)) (kern-obj-get-location kegg))
  (kern-obj-remove kegg))

(define (spider-egg-exec kegg)
  (let ((gob (kobj-gob-data kegg)))
    (if (spider-egg-hatch-timer-expired? gob)
        (spider-egg-hatch kegg)
        (spider-egg-dec-hatch-timer! gob))))

(define spider-egg-ifc
  (ifc '()
       (method 'exec spider-egg-exec)))

(mk-obj-type 'spider-egg-type
             "spider egg"
             s_magic
             layer-item
             spider-egg-ifc)

(define (mk-spider-egg)
  (bind (kern-mk-obj spider-egg-type 1)
        (spider-egg-gob-mk)))

;; ----------------------------------------------------------------------------
;; Spider "Skills"
;; ----------------------------------------------------------------------------

(define (suck-hp kspider ktarg amount)
  (kern-log-msg (kern-obj-get-name kspider) 
                " sucks the juices from " 
                (kern-obj-get-name ktarg))
  (let ((amount (min amount (kern-char-get-hp ktarg))))
    (kern-obj-apply-damage ktarg nil amount)
    (kern-obj-heal kspider amount)))

(define (spider-paralyze ktarg)
  (spider-display "spider-paralyze")(spider-newline)
  (paralyze ktarg))

(define (ensnare-loc loc)
  (spider-display "ensnare-loc")(spider-newline)
  (kern-obj-put-at (kern-mk-obj web-type 1) loc))



;; ----------------------------------------------------------------------------
;; Spider AI
;; ----------------------------------------------------------------------------
(define (spider-is-aggressive? kspider)
  (> (kern-char-get-hp kspider)
     (/ (* 4 (kern-char-get-max-hp kspider)) 5)))

(define (is-queen-spider? kspider)
  (eqv? (kern-char-get-species kspider) sp_queen_spider))

(define (spider-try-to-lay-egg kspider)
  (spider-display "spider-try-to-lay-egg")(spider-newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (and (not (is-object-type-at? loc spider-egg-type))
             (> (kern-dice-roll "1d20") 18))
        (kern-obj-put-at (mk-spider-egg) loc))))

(define (spider-no-hostiles kspider)
  (spider-display "spider-no-hostiles")(spider-newline)
  (let ((loc (kern-obj-get-location kspider)))
    (if (not (is-object-type-at? loc web-type))
        (ensnare-loc loc))
    (if (is-queen-spider? kspider)
        (spider-try-to-lay-egg kspider)))
  (wander kspider))

(define (is-helpless? kchar)
  (or (kern-char-is-asleep? kchar)
      (is-ensnared? kchar)
      (is-paralyzed? kchar)))

(define (spider-attack-helpless-foe kspider kfoe)
  (define (attack kspider coords)
    (spider-display "spider-attack")(spider-newline)
    (if (is-paralyzed? kfoe)
        (suck-hp kspider kfoe (kern-dice-roll "1d6"))
        (spider-paralyze kfoe)))
  (spider-display "spider-attack-helpless-foe")(spider-newline)
  (do-or-goto kspider (kern-obj-get-location kfoe) attack))

(define (spider-foe-in-range-of-web-spew? kspider kfoe)
  (spider-display "spider-foe-in-range-of-web-spew?")(spider-newline)
  (< (kern-get-distance (kern-obj-get-location kspider)
                        (kern-obj-get-location kfoe))
     (/ (kern-char-get-level kspider) 2)))

(define (spider-pathfind-to-foe kspider kfoe)
  (spider-display "spider-pathfind-to-foe")(spider-newline)
  (pathfind kspider (kern-obj-get-location kfoe)))

(define (spider-try-to-spew-web kspider foe)
  (spider-display "spider-try-to-spew-web")(spider-newline)
  (if (and (can-use-ability? web-spew kspider)
           (spider-foe-in-range-of-web-spew? kspider foe))
      (use-ability web-spew kspider foe)
      (spider-attack-helpless-foe kspider foe)))

(define (spider-no-helpless-foes kspider foes)
  (spider-display "spider-no-helpless-foes")(spider-newline)
  (if (is-queen-spider? kspider)
      (spider-try-to-spew-web kspider (closest-obj 
                                            (kern-obj-get-location kspider)
                                            foes))
      (if (spider-is-aggressive? kspider)
          (spider-attack-helpless-foe kspider 
                                      (closest-obj 
                                       (kern-obj-get-location kspider)
                                       foes))
          (evade kspider foes))))

(define (spider-hostiles kspider foes)
  (spider-display "spider-hostiles")(spider-newline)
  (let ((helpless-foes (filter is-helpless? foes)))
    (if (null? helpless-foes)
        (spider-no-helpless-foes kspider foes)
        (spider-attack-helpless-foe kspider 
                                         (closest-obj 
                                          (kern-obj-get-location kspider)
                                          helpless-foes)))))

(define spider-bad-fields
  (filter (lambda (x) (and (not (eqv? x web-type))
                           (not (eqv? x F_web_perm))))
          all-field-types))

(define (spider-ai kspider)
  (spider-display "spider-ai")(spider-newline)
  (or (get-off-bad-tile? kspider)
      (let ((foes (all-visible-hostiles kspider)))
        (if (null? foes)
            (spider-no-hostiles kspider)
            (spider-hostiles kspider foes))
        #t)))
