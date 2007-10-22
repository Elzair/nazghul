
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deeps random map


;probability for edge terrains... out of 83
(define voidgap-terrain-edges
	(list
		(list 20 'm_deeptempl_wall #f)
		(list 25 'm_deeptempl_water #f)
		(list 30 'm_deeptempl_hole #f)
		(list 33 'm_deeptempl_lava #f)
		(list 35 'm_deeptempl_swamp #t)		
		(list 100 'm_deeptempl_passage #t)
	))

;probability for area terrains... out of 149
(define voidgap-terrain-area
	(list
		(list 40 'm_deeptempl_wall)
		(list 55 'm_deeptempl_water)
		(list 70 'm_deeptempl_hole)		
		(list 75 'm_deeptempl_lava)		
		(list 80 'm_deeptempl_swamp)		
		(list 150 'm_deeptempl_passage)
	))

;map areas replaced by the various blitting ops
(define voidgap-room-blitstats
	(prmap-mk-blitstats 19 19 3 4 3))


;parameters to random number generators
(define (voidgap-rno) (+ (kern-dice-roll "1d70" 33)))
(define voidgap-random-type-ew (prmap-mk-prng-params (voidgap-rno) (voidgap-rno) 0 (voidgap-rno) 83))
(define voidgap-random-type-ns (prmap-mk-prng-params (voidgap-rno) (voidgap-rno) 0 (voidgap-rno) 83))
(define voidgap-random-type-area (prmap-mk-prng-params (voidgap-rno) (voidgap-rno) 0 (voidgap-rno) 149))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; critter lists

(define voidgap-monster-types
	(list 
		(list 
			(list 100 'bat)
		)
		(list
			(list 100 'rat)
			(list 200 'bat)
		)
		(list
			(list 100 'giant-spider)
			(list 150 'queen-spider)
		)
		(list
			(list 100 'cave-goblin-slinger)
			(list 200 'cave-goblin-berserker)
			(list 250 'cave-goblin-priest)
		)
		(list
			(list 50 'cave-goblin-slinger-m)
			(list 100 'cave-goblin-berserker-m)
			(list 300 'troll-m)
			(list 350 'troll-geomancer-m)
		)
		(list
			(list 100 'green-slime)
			(list 200 'yellow-slime)
		)	
		(list
			(list 100 'zorn)
		)
		(list
			(list 20 'ghast)
			(list 200 'demon)
		)
		(list
			(list 50 'cave-goblin-slinger-m)
			(list 100 'cave-goblin-berserker-m)
			(list 150 'troll-m)
			(list 350 'gint-warrior-m)
			(list 400 'gint-mage-m)
		)
		(list
			(list 50 'cave-goblin-slinger-m)
			(list 100 'headless)
			(list 150 'cave-goblin-berserker-m)
			(list 200 'troll-m)
			(list 250 'gint-warrior-m)
			(list 350 'gazer)
		)
		(list
			(list 25 'cave-goblin-slinger-m)
			(list 50 'cave-goblin-berserker-m)
			(list 300 'dragon)
		)
		(list
			(list 20 'ghast)
			(list 70 'skeletal-warrior)
			(list 120 'skeletal-spear-thrower)
			(list 150 'craven-archer)
			(list 300 'death-knight)
			(list 350 'demon)
		)
		(list
			(list 100 'skeletal-warrior)
			(list 200 'skeletal-spear-thrower)
			(list 250 'lich)
		)
		(list 
			(list 100 'headless)
			(list 150 'skeletal-warrior)
			(list 200 'skeletal-spear-thrower)
			(list 220 'craven-archer)
			(list 250 'death-knight)
			(list 260 'demon)
			(list 330 'warlock)
		)
	)
)

(define voidgap-group-types
	(list
		(list 10 0 100 "1d4+3") ;bats
		(list 20 1 100 "1d4+3") ;rats
		(list 30 1 200 "1d6+4") ;bats n rats
		(list 40 5 100 "1d3+2") ;slime
		(list 50 2 100 "1d4+3") ;spiders
		(list 60 3 100 "1d3+1") ;goblins
		(list 70 2 110 "1d4+3") ;spiders 1q
		(list 80 13 100 "1d6+4") ;headless
		(list 90 2 150 "1d6+4") ;spiders +qs
		(list 100 5 200 "1d6+3") ;slime+
		(list 110 3 200 "1d6+4") ;goblin war
		(list 120 7 20 "1d6+1")  ;ghost
		(list 130 11 120 "1d6+4") ;skels
		(list 140 4 300 "1d6+3") ;trolls
		(list 150 6 100 "1d2")   ;zorn
		(list 160 8 350 "1d6+3") ;gint
		(list 170 7 22 "1d6+1")  ;ghost + d
		(list 180 11 300 "2d4+4") ;deathknights
		(list 190 11 310 "2d4+4") ;deathknights +d
		(list 200 3 210 "1d6+5") ;goblin war +p
		(list 210 7 200 "1d3")   ;demons
		(list 220 3 250 "2d4+4") ;goblin tribe
		(list 230 4 350 "1d6+5") ;trolls +m
		(list 240 8 400 "2d4+4") ;gint + m
		(list 250 10 300 "1d4")  ;dragon
		(list 260 10 80 "1d6+3") ;dragon + gob	
		(list 270 9 260 "1d6+3") ;gazer
		(list 280 13 330 "2d4+4") ;warlock		
		(list 290 12 205 "2d4+4") ;lich	
		(list 300 9 320 "2d4+4") ;gazers
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handles

(define (voidgap-link-factory x y z map dir)
		(list 'm_deeptempl_break nil)
		)

(define (voidgap-room-custom-hardlinks rxloc ryloc rzloc hardlinks)
	(vector-merge
		(prmap-room-hardlinkentry-get rxloc ryloc rzloc hardlinks)
		(vector
			(if (> ryloc 1) (list nil 'm_deeptempl_wall) nil)
			(if (< rxloc -7) (list nil 'm_deeptempl_wall) nil)
			(if (> rxloc 1) (list nil 'm_deeptempl_wall) nil)
			(if (< ryloc -7) (list nil 'm_deeptempl_wall) nil)
		)
	))	
		
(define (voidgap-room-init-contents kplace roomdata)
	(let* (
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(distance (sqrt (+ (* rxloc rxloc) (* ryloc ryloc))))
		)
		(if (and (null? (kern-place-get-beings kplace))
				(< (kern-dice-roll "1d100") 
					(min 75 (+ 25 (* 15 (sqrt distance))))))
			(begin 
			(map (lambda (monster)
				(begin 

					(prmap-room-addmonster kplace monster)))
				(prmap-mk-monster-group voidgap-group-types voidgap-monster-types 
					(string-append "1d" (number->string (+ 100 (ceiling (* 40 (sqrt distance))))))
					(+ 200 (* 30 (sqrt distance)))))
					)
		)
	))

(define (voidgap-init-cohesion mapdata)
	(if (list-ref mapdata 10)
		(begin
			;;(kern-log-msg "begin cohesion check")
			(prmap-ensure-cohesion mapdata -5 0 -5 0 0 voidgap-link-factory)
			;;(kern-log-msg "end cohesion check")
			(mutable-list-set mapdata 10 #f)
		)))
		
(define (voidgap-room-handle-deeps kplace kplayer)
	(let* (
		(roomdata (get-roomdata kplace))
		(mapdata (prmap-get-mapdata (eval 'p_voidgap_1)))
		)
		(prmap-room-freeze-current mapdata)
		(prmap-room-init-neighbors kplace roomdata)
		(prmap-room-init-links kplace roomdata mapdata)
		(prmap-room-cleanout kplace)
		(prmap-room-thaw kplace mapdata)		
		(prmap-room-blit-map kplace roomdata mapdata)
		(voidgap-room-init-contents kplace roomdata)
	))

(define (voidgap-room-handle-start kplace kplayer)
	(let* (
		(roomdata (get-roomdata (eval 'p_voidgap_1)))
		(mapdata (prmap-get-mapdata (eval 'p_voidgap_1)))
		)
		(voidgap-init-cohesion mapdata)
		(prmap-room-freeze-current mapdata)
		(prmap-roomdata-setxyz roomdata -1 0 0)
		(kern-place-set-neighbor west kplace (eval 'p_voidgap_1))
	))	
	
(define (voidgap-room-handle-end kplace kplayer)
	(let* (
		(roomdata (get-roomdata (eval 'p_voidgap_1)))
		(mapdata (prmap-get-mapdata (eval 'p_voidgap_1)))
		)
		(voidgap-init-cohesion mapdata)
		(prmap-room-freeze-current mapdata)
		(prmap-roomdata-setxyz roomdata -5 -5 0)
		(voidgap-room-init-contents kplace roomdata)
		(kern-place-set-neighbor east kplace (eval 'p_voidgap_1))
	))	
	