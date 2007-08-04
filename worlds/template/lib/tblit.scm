;;----------------------------------------------------------------------------
;; tblit.scm - a terrain-blitting mech
;;----------------------------------------------------------------------------

;; Some convenient maps for blitting
(kern-mk-map
 'm_hall_section 3 3 pal_expanded
 (list
  ",, ,, ,,"
  ",, ,, ,,"
  ",, ,, ,,"
  ))

(kern-mk-map
 'm_deck_section 3 3 pal_expanded
 (list
  "ee ee ee"
  "ee ee ee"
  "ee ee ee"
  ))


;; A curried wrapper for kern-blit-map (note that place may legitimately be
;; null during startup)
(define (blit-map place x y w h map)
  (if (notnull? place)
      (kern-blit-map (kern-place-map place) x y map 0 0 w h)))
                         
;; Syntactic sugar to set blitter state
(define (tblit-mk place-tag x y w h map-tag) (list place-tag x y w h map-tag))

;; Do the blit upon receiving an "on" signal.
(define (tblit-on kobj)
  (apply blit-map (map safe-eval (kobj-gob-data kobj))))

;; Blit mechs are not visible.
(define (tblit-init kobj)
  (kern-obj-set-visible kobj #f))

;; A blitter mech responds to an "on" signal by executing the blit
(define tblit-ifc
  (ifc '()
       (method 'on tblit-on)
       (method 'init tblit-init)))

;; The kernel object type of a blitter
(mk-obj-type 't_terrain_blitter '() '() layer-mechanism 
             tblit-ifc)

;; Constructor
(define (mk-tblitter place-tag x y w h map-tag)
  (bind (kern-mk-obj t_terrain_blitter 1)
        (tblit-mk place-tag x y w h map-tag)))
