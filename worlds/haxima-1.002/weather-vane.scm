;; A weather vane points away from the wind direction

(define (weather-vane-exec kobj)
  (kern-obj-set-facing kobj
                       (kern-get-wind)))

(define weather-vane-ifc
  (ifc '()
       (method 'exec weather-vane-exec)
       ))

;; Make a kernel portcullis type
(mk-obj-type 't_weather_vane "weather vane" s_weather_vane layer-mechanism
             weather-vane-ifc)

;; Define a constructor
(define (mk-weather-vane)
  (kern-mk-obj t_weather_vane 1))
