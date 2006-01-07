;; difficulty class of spawned npc party -- mean player party level +/-1
(define (difficulty) 
  (max 1
       (+ (mean-player-party-level)
          (kern-dice-roll "1d3-2")
          )))

(define (filter-out-difficult-ptypes ptypes)
  (let ((dc (difficulty)))
    ;;(println "filter-out-difficult-ptypes:dc=" dc)
    (filter (lambda (ptype)
              ;;(println " ptype:" (ptype-name ptype))
              (and (<= (ptype-dc ptype) dc)
                   (>= (ptype-dc ptype) (- dc 4))))
            ptypes)))

(define (lookup-ptype n ptypes)
  (if (null? ptypes)
      nil
      (begin
        ;;(println " lookup:n=" n " ptype=" (car ptypes))
        (if (< n (ptype-scarcity (car ptypes)))
            (car ptypes)
            (lookup-ptype (- n 
                             (ptype-scarcity (car ptypes)))
                          (cdr ptypes))))))

(define (select-ptype ptypes)
  (let* ((mod (foldr (lambda (x ptype)
                       (+ x (ptype-scarcity ptype)))
                     0
                     ptypes))
         (n (modulo (random-next) mod)))
    ;;(println "  mod=" mod " n=" n)
    (lookup-ptype n ptypes)))

(define (edge-spawn-exec kwm)

  ;;(println "edge-spawn-exec")

  (define (get-ptype loc)
    ;;(println "  get-ptype:loc=" loc)
    (let ((ptypes (terrain-to-ptypes (kern-place-get-terrain loc))))
      ;;(println " ptypes:" ptypes)
      (if (null? ptypes)
          nil
          (let ((ptypes (map eval ptypes)))
            (let ((ptypes (filter-out-difficult-ptypes ptypes)))
              ;;(println "  filtered ptypes: " ptypes)
              (if (null? ptypes)
                  nil
                  (select-ptype ptypes)))))))

  (define (try-to-spawn-at loc)
    (let ((ptype (get-ptype loc)))
      ;;(println " try-to-spawn-at:ptype=" ptype)
      (if (not (null? ptype))
          (let ((kparty (ptype-generate ptype)))
            ;; note: must put the party on the map (thus giving it a refcount)
            ;; before setting ttl
            (kern-obj-put-at kparty loc)
            (kern-obj-set-ttl kparty 50)
            ))))

  (define (pick-edge-tile)
    (let* ((ww 9)
           (wh 9)
           (ploc (kern-obj-get-location (kern-get-player)))
           (kplace (loc-place ploc))
           (x (loc-x ploc))
           (y (loc-y ploc)))
      (case (modulo (random-next) 4)
        ((0) (random-loc kplace (- x ww) (- y wh) (+ 1 (* 2 ww)) 1)) ; north
        ((1) (random-loc kplace (- x ww) (+ y wh) (+ 1 (* 2 ww)) 1)) ; south
        ((2) (random-loc kplace (+ x ww) (- y wh) 1 (+ 1 (* 2 wh)))) ; east
        ((3) (random-loc kplace (- x ww) (- y wh) 1 (+ 1 (* 2 wh)))) ; west
        )))

  (define (roll-to-spawn?) 
    (>= (modulo (random-next) 100) 96))

  (if (and (kern-place-is-wilderness? (loc-place (kern-obj-get-location (kern-get-player))))
           (roll-to-spawn?))
      (try-to-spawn-at (pick-edge-tile)))

  )

(define edge-spawn-ifc
  (ifc nil
       (method 'exec edge-spawn-exec)))

(mk-obj-type 't_edge_spawn nil nil layer-none edge-spawn-ifc)

(define (mk-edge-spawn-generator)
  (kern-obj-set-visible (kern-mk-obj t_edge_spawn 1) #f)
  )
