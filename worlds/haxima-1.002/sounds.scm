
(kern-mk-sound 'sound-damage         "damage.wav")
(kern-mk-sound 'sound-walking        "walk.wav")
;(kern-mk-sound 'sound-splashing      "rowing.wav")
(kern-mk-sound 'sound-splashing      "water2.wav")
(kern-mk-sound 'sound-squishing      "walk.wav") ;; fixme
(kern-mk-sound 'sound-moongate-enter "enter_moongate.wav")
(kern-mk-sound 'sound-cannon-fire    "cannon.wav")
(kern-mk-sound 'sound-clock          "ticktock.wav")
(kern-mk-sound 'sound-clock-chime    "gong.wav")
(kern-mk-sound 'sound-river          "river.wav")
(kern-mk-sound 'sound-wind          "wind_1a.wav")
(kern-mk-sound 'sound-missile          "swish.wav")
(kern-mk-sound 'sound-explode          "boom.wav")
(kern-mk-sound 'sound-lightning        "lightning.wav")
(kern-mk-sound 'sound-fireblast       "fireblast.wav")

;; Aliases
(define sound-ship-move sound-splashing)

;; ambient sound 'object'

(define ambience-ifc
    (ifc '()
         (method 'exec
                 (lambda (ksound)
                   (kern-sound-play-ambient (eval (gob ksound)) (kern-obj-get-location ksound))
                   ))
         (method 'on-entry
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

(println "esounds")

(define music (list 0 nil nil))

(define (music-play-track file)
	(println "music-play: " file)
	(set-car! (cdr music) file)
	(kern-music-play file)
)

(define (music-handler code filelist)
	(if (not (equal? (car music) code))
		(begin
			(set-car! music code)
			(set-car! (cddr music) filelist)
			(if (or (null? (cadr music))
						(not (in-text-list? (cadr music) filelist)))
				(music-play-track (random-select (caddr music)))
			)
		)))

(define (music-battle)
	(music-handler 1 (list "dragon-slayer.mid" "dragon-quest.mid" "into-battle.mid"))
	)
	
(define (music-shard)
	(music-handler 2 (list "fair-camelot.mid" "game-music2.mid" "medieval-quest.mid"))
	)
	
(define (music-places)
	(music-handler 3 (list "fair-camelot.mid" "game-music2.mid" "medieval-quest.mid"))
	)

(music-shard)

(define (music-change-handler player)
	(if (not (null? (caddr music)))
			(music-play-track (random-select (caddr music)))
		)
	)
	
(kern-set-music-handler music-change-handler)