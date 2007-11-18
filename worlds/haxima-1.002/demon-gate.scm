(kern-load "nossifer.scm")

;; constants
(define demon-gate-x 6)
(define demon-gate-y 9)

;; demon gate gob
(define (mk-demon-gate-gob) (list 8 #f #f))
(define (demon-gate-unlock gob) (set-car! gob (- (car gob) 1)))
(define (demon-gate-completely-unlocked? gob) (= (car gob) 0))
(define (demon-gate-opened? gob) (cadr gob))
(define (demon-gate-opened! gob) (set-car! (cdr gob) #t))
(define (end-game-played? gob) (caddr gob))
(define (end-game-played! gob) (set-car! (cddr gob) #t))


;; demon gate procs
(define (summon-nossifer kplace)
  (kern-log-msg "SOMETHING EMERGES")
  (let ((knpc (mk-nossifer)))
    (kern-obj-put-at knpc
                     (mk-loc kplace
                             demon-gate-x
                             demon-gate-y))
    (kern-map-repaint)
    (kern-sleep 2000)
    (kern-conv-begin knpc)
  ))

(define (open-demon-gate kplace)
  (let* ((loc (mk-loc kplace
                      demon-gate-x
                      demon-gate-y))
         (gate (mk-moongate nil))
         (stages (list (list '()                       0)
                       (list s_blackgate_quarter        32)
                       (list s_blackgate_half           64)
                       (list s_blackgate_three_quarters 96)
                       (list s_blackgate_full           128))))
    (kern-map-flash 100)
    (kern-sleep 2000)
    (kern-log-msg "A DARK GATE OPENS")
    (kern-obj-put-at gate loc)
    (moongate-animate gate stages)
    (kern-sleep 2000)
    (summon-nossifer kplace)
    (kern-obj-remove gate)
    ))


(define (demon-gate-on kgate kchar)
  (let ((dgate (gob kgate)))
    (demon-gate-unlock dgate)))

(define (nossifer-vanquished?)
  (= 0 (num-hostiles (kern-get-player))))

(define (end-game)
  (kern-map-flash 1000)
  (kern-map-repaint)
  (kern-log-msg "**************************")
  (kern-sleep 2000)
  (kern-log-msg "Nossifer and his minions are defeated.")
  (kern-sleep 2000)
  (kern-log-msg "The Demon Gate is closed.")
  (kern-sleep 2000)
  (kern-log-msg "Will it open again?")
  (kern-sleep 2000)
  (kern-log-msg "Is the Wanderer trapped on the Shard forever?")
  (kern-sleep 2000)
  (kern-log-msg "Is he, perhaps, a Wanderer no more?")
  (kern-sleep 2000)
  (kern-log-msg "Is it time he became...")
  (kern-sleep 4000)
  (kern-log-msg "...a Conquerer?")
  (kern-sleep 2000)
  (kern-log-msg "Find out in Haxima II: Conquerer")
  (kern-log-msg "**************************")

  (kern-log-msg "*** CONGRATULATIONS ***")
  (kern-log-msg "You have finished the game!")
  (kern-log-msg "Press any key to exit.")
  (kern-ui-waitkey)

  (kern-end-game)
  )

(define (demon-gate-exec kgate)
	(let ((dgate (gob kgate)))
		(if (and (not (end-game-played? dgate))
				(demon-gate-opened? dgate)
				(nossifer-vanquished?))
			(begin
				(end-game)
				(end-game-played! dgate)
			))
		(println (demon-gate-completely-unlocked? dgate) " "
					(demon-gate-opened? dgate) " " (nossifer-vanquished?))
		(if (and (demon-gate-completely-unlocked? dgate)
				(not (demon-gate-opened? dgate)))
			(begin
				(open-demon-gate (get-place kgate))
				(demon-gate-opened! dgate)
			)
		)
	))
        

;; demon gate type ifc
(define demon-gate-ifc
  (ifc '()
       (method 'on demon-gate-on)
       (method 'exec demon-gate-exec)
       ))

;; demon gate type
(mk-obj-type 't_demon_gate nil nil layer-none demon-gate-ifc)

;; demon gate ctor
(define (mk-demon-gate)
  (kern-tag 'demon-gate
            (bind (kern-obj-set-ignore-time-stop (kern-mk-obj t_demon_gate 1)
                                                 #t)
                  (mk-demon-gate-gob))))
