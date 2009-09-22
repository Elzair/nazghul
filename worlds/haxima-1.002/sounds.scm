
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

;;==========================================================================
;; music

(define default-music "music/ballad.ogg")

(define (music-list . entries)
	(if (null? entries)
		(list default-music)
		entries
	)
)

(load "music/music.scm")

(println ml-battle-music)

(define music (list nil nil))

(define (music-play-track file)
	(set-car! music file)
	(kern-music-play file)
)

(define (music-handler filelist)
	(set-car! (cdr music) filelist)
	(if (or (null? (car music))
				(not (in-text-list? (car music) filelist)))
		(music-play-track (random-select (cadr music)))
	))
		
(define (music-cue filelist)
	(set-car! (cdr music) filelist)
	)
	
(define (music-cue-ref listref)
	(let ((cuelist (safe-eval listref)))
		(if (not (null? cuelist))
			(music-cue cuelist))
	))
	
(define (music-fake-current)
	(if (not (null? (cadr music)))
			(set-car! music (car (cadr music)))
		))
		
(music-handler (list default-music))

(define (music-change-handler player)
	(if (not (null? (cadr music)))
			(music-play-track (random-select (cadr music)))
		)
	)
	
(kern-add-hook 'music_change_hook 'music-change-handler)
	
;;combo play + cue, with sensible eval and null behaviour
(define (music-set-pair immediate therafter)
	(let* ((imm (safe-eval immediate))
			(ther (safe-eval therafter))
			(cuelist (if (null? imm)
								nil
								ther))
			(playlist (if (null? imm)
								ther
								imm))
			)
		(if (not (null? playlist))
			(music-handler playlist))
		(if (not (null? cuelist))
			(music-cue cuelist))
	))


;;==================================================================================
;; interactive music handler

(mk-obj-type 't_sounddata nil nil layer-none nil)

;; use kern-set-combat-state-listener to call this
;; do it on system startup too (kern-set-gamestart-hook)
(define (music-on-combat-change player)
  (let ((playerloc (player-member-loc)))
    (if (notnull? playerloc)
        (let ((dataslist (kplace-get-objects-of-type playerloc t_sounddata)))
          (if (notnull? dataslist)
              (let* ((sounddata (gob (car dataslist)))
                     (oldstate (car sounddata))
                     (newstate (null? (all-hostiles (car (kern-party-get-members player)))))
                     )
                (set-car! sounddata newstate)
                (if newstate
                    (if oldstate
                        (music-set-pair nil (list-ref sounddata 1))
                        (music-set-pair (list-ref sounddata 4) (list-ref sounddata 1))
                        )
                    (if oldstate
                        (music-set-pair (list-ref sounddata 2) (list-ref sounddata 3))
                        (music-set-pair nil (list-ref sounddata 3))
                        )
                    )
                ))
          ))))
    
;; use place entry hooks to call this
(define (music-on-combat-entry playerloc player)
	(let ((dataslist (kplace-get-objects-of-type playerloc t_sounddata)))
		(if (not (null? dataslist))
			(let ((sounddata (gob (car dataslist)))
					(newstate (null? (all-hostiles (car (kern-party-get-members player)))))
					)
				(set-car! sounddata newstate)
				(if newstate
					(music-set-pair nil (list-ref sounddata 1))
					(music-set-pair (list-ref sounddata 2) (list-ref sounddata 3))
				)		
				(music-fake-current)		
			))
	))		

;; use this to make data object
(define (mk-sounddata normal engagement combat victory)
	(bind (kern-obj-set-visible (kern-mk-obj t_sounddata 1) #f)
		(list #t normal engagement combat victory)
	))
	
;;normal combat music entries
(define (mk-basic-musicdata noncombatml)
	(mk-sounddata noncombatml 'ml-battle-intro 'ml-battle-music 'ml-battle-over))
	
;;world music entries dont use combat stuff
(define (mk-world-musicdata noncombatml)
	(mk-sounddata noncombatml nil noncombatml nil))
	
;; do-it-all method- adds an object and the hook to a place
(define (mk-place-music place noncombatml)
	(kern-obj-put-at (mk-basic-musicdata noncombatml) (list place 0 0))
	(kern-place-add-on-entry-hook place 'music-on-combat-entry))
	
