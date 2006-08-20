
(define (edge-spawn-exec kwm)

  ;;(println "edge-spawn-exec")

  (define (get-ptype loc)
    (println "get-ptype:" loc)
    (terrain-to-ptype (kern-place-get-terrain loc)
                      (mean-player-party-level)))

  (define (try-to-spawn-at loc)
    (let ((ptype (get-ptype loc)))
      ;;(println " try-to-spawn-at:ptype=" ptype)
      (if (not (null? ptype))
          (let ((kparty (ptype-generate ptype)))
            ;; note: must put the party on the map (thus giving it a refcount)
            ;; before setting ttl
            ;; FIXME: what if loc is invalid? will put-at fail? will ttl then crash?
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
    (>= (modulo (random-next) 100) 98))

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
