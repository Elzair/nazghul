
(kern-mk-sound 'sound-damage         "damage.wav")
(kern-mk-sound 'sound-walking        "walk.wav")
;(kern-mk-sound 'sound-splashing      "rowing.wav")
(kern-mk-sound 'sound-splashing      "water2.wav")
(kern-mk-sound 'sound-squishing      "walk.wav") ;; fixme
(kern-mk-sound 'sound-moongate-enter "enter_moongate.wav")
(kern-mk-sound 'sound-cannon-fire    "cannon.wav")
(kern-mk-sound 'sound-clock          "ticktock.wav")
(kern-mk-sound 'sound-river          "river.wav")

;; Aliases
(define sound-ship-move sound-splashing)

;; ambient sound 'object'

(define ambience-ifc
    (ifc '()
         (method 'exec
                 (lambda (ksound)
                   (kern-sound-play-ambient (eval (gob ksound)) (kern-obj-get-location ksound))
                   ))	
         ))

(mk-obj-type 't_ambience nil
             '()
             layer-mechanism ambience-ifc)

(define (mk-ambient-sound soundtag)
  (let ((ksound (kern-mk-obj t_ambience 1)))
    (bind ksound soundtag)
    ksound))
