;; A wind bridge appears to rotates in the wind. It blits a map to lay down the
;; bridge terrain. Position the object in the upper left corner of where the
;; bridge map should be blitted.

(kern-mk-map
 'm_wind_bridge_ns 9 9 pal_expanded
 (list
  "vv vv vv vv ee vv vv vv vv "
  "vv vv vv vv ee vv vv vv vv "
  "vv vv vv vv ee vv vv vv vv "
  "vv vv vv ee ee ee vv vv vv "
  "vv vv vv ee oo ee vv vv vv "
  "vv vv vv ee ee ee vv vv vv "
  "vv vv vv vv ee vv vv vv vv "
  "vv vv vv vv ee vv vv vv vv "
  "vv vv vv vv ee vv vv vv vv "
  ))

(kern-mk-map
 'm_wind_bridge_ew 9 9 pal_expanded
 (list
  "vv vv vv vv vv vv vv vv vv "
  "vv vv vv vv vv vv vv vv vv "
  "vv vv vv vv vv vv vv vv vv "
  "vv vv vv ee ee ee vv vv vv "
  "ee ee ee ee oo ee ee ee ee "
  "vv vv vv ee ee ee vv vv vv "
  "vv vv vv vv vv vv vv vv vv "
  "vv vv vv vv vv vv vv vv vv "
  "vv vv vv vv vv vv vv vv vv "
  ))

(define (wind-bridge-mk) (list here))
(define (wind-bridge-facing gob) (car gob))
(define (wind-bridge-set-facing! gob val) (set-car! gob val))

(define (wind-bridge-exec kobj)
  (let ((wind (kern-get-wind))
        (gob (gob kobj))
        (loc (kern-obj-get-location kobj))
        )
    (println "wind=" wind "gob=" gob "loc=" loc)
    (define (blit map)
      (kern-blit-map (kern-place-map (loc-place loc))
                     (loc-x loc)
                     (loc-y loc)
                     map 0 0 9 9))
    (cond ((and (or (= wind north)
                    (= wind south))
                (not (= (wind-bridge-facing gob) north)))
           (blit m_wind_bridge_ns)
           (wind-bridge-set-facing! gob north)
           )
          ((and (or (= wind east)
                    (= wind west))
                (not (= (wind-bridge-facing gob) east)))
           (blit m_wind_bridge_ew)
           (wind-bridge-set-facing! gob south)
           ))))

(define wind-bridge-ifc
  (ifc '()
       (method 'exec wind-bridge-exec)
       ))

;; Make a kernel portcullis type
(mk-obj-type 't_wind_bridge "wind bridge" nil layer-mechanism wind-bridge-ifc)

;; Define a constructor
(define (mk-wind-bridge)
  (bind (make-invisible (kern-mk-obj t_wind_bridge 1))
        (wind-bridge-mk)))
