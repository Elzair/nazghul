;utility for searching {sum-of-probability, object} lists
(define (get-numbered-elem list value)
	(if (>= (car (car list)) value)
		(cdar list)
		(get-numbered-elem (cdr list) value)
		)
	)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; blit stats

; areamargin: from edge of room to area
; edgemargin: from corner of room to each side of edge tile
; edgewidth: from edge of room to inner side of edge tile

(define (prmap-mk-blitstats roomwidth roomheight areamargin edgemargin edgewidth)
	(list
		(list edgemargin 0 (- roomwidth edgemargin edgemargin) edgewidth)
		(list 0 edgemargin edgewidth (- roomheight edgemargin edgemargin))
		(list (- roomwidth edgewidth) edgemargin edgewidth (- roomheight edgemargin edgemargin))
		(list edgemargin (- roomheight edgewidth) (- roomwidth edgemargin edgemargin) edgewidth)
		(list areamargin areamargin (- roomwidth areamargin areamargin) (- roomheight areamargin areamargin))
	))
	
(define (prmap-blitstats-north data)
	(list-ref data 0))
(define (prmap-blitstats-west data)
	(list-ref data 1))
(define (prmap-blitstats-east data)
	(list-ref data 2))
(define (prmap-blitstats-south data)
	(list-ref data 3))
(define (prmap-blitstats-area data)
	(list-ref data 4))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Directions	

; order is N W E S
(define (get-cardinal-ref list dir)
	(begin
	(list-ref list
		(/ (- dir 1) 2))
		))

(define prmap-room-offsets
	(list 
		(list 0 1)
		(list -1 0)
		(list 1 0)
		(list 0 -1)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic room data

(mk-obj-type 't_roomdata nil nil layer-none nil)

; returns roomdata object or nil if none
(define (get-roomdata kplace)
	(let ((dataslist (kplace-get-objects-of-type kplace t_roomdata)))
		(if (equal? (length dataslist) 0)
			nil
			(gob (car dataslist)))))
			

; replaces any roomdatas in place with given data
(define (set-roomdata kplace data)
	(begin
		; remove any/all previous data
		(map (lambda (rdataobj)
				(kern-obj-remove rdataobj))
			(kplace-get-objects-of-type kplace t_roomdata))
		;add new t_roomdata
		(kern-obj-put-at
			(bind (kern-obj-set-visible (kern-mk-obj t_roomdata 1) #f) data)
			(list kplace 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random map roomdata

(define (prmap-roomdata-x data)
	(list-ref data 0))
(define (prmap-roomdata-y data)
	(list-ref data 1))
(define (prmap-roomdata-current data)
	(list-ref data 2))
(define (prmap-roomdata-prev data)
	(list-ref data 3))
(define (prmap-roomdata-rooms data)
	(list-ref data 4))

(define (prmap-roomdata-setxy data x y)
	(set-car! data x)
	(set-car! (cdr data) y))
	
;prev becomes current, current is cleared
(define (prmap-roomdata-pushcurrent data)
	(let ((curdat (list-tail data 2)))
		(set-car! (cdr curdat) (car curdat))
		(set-car! curdat #f)))

(define (prmap-roomdata-setcurrent data cur)
	(let ((curdat (list-tail data 2)))
		(set-car! curdat cur)))

(define (prmap-mk-roomdata room x y current prev rooms)
	(set-roomdata room (list x y current prev rooms))
	)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Room linking

; 2 dimensional map - 5 rooms

;  2   5   4   1   3                   
; 513 324 132 245 451                          
;  4   1   5   3   2 

(define (prmap-linkrooms-2d room1-tag room2-tag room3-tag room4-tag room5-tag)
	(prmap-mk-roomdata (eval room1-tag) 0 0 #f #f (list room2-tag room5-tag room3-tag room4-tag))
	(prmap-mk-roomdata (eval room2-tag) 0 0 #f #f (list room5-tag room3-tag room4-tag room1-tag))
	(prmap-mk-roomdata (eval room3-tag) 0 0 #f #f (list room4-tag room1-tag room2-tag room5-tag))
	(prmap-mk-roomdata (eval room4-tag) 0 0 #f #f (list room1-tag room2-tag room5-tag room3-tag))
	(prmap-mk-roomdata (eval room5-tag) 0 0 #f #f (list room3-tag room4-tag room1-tag room2-tag))
	)

; 3 dimensional map - 7 rooms

;   2 6     3 7     5 4     1 5     6 2     7 3     4 1
; 5 1 3   6 2 4   1 3 7   2 4 6   7 5 1   4 6 2   3 7 5
; 7 4     5 1     6 2     3 7     4 3     1 5     2 6

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Map paramater storage

(mk-obj-type 't_mapdata nil nil layer-none nil)

; returns roomdata object or nil if none
(define (prmap-get-mapdata kplace)
	(let ((dataslist (kplace-get-objects-of-type kplace t_mapdata)))
		(if (equal? (length dataslist) 0)
			nil
			(gob (car dataslist)))))
			

; replaces any roomdatas in place with given data
(define (prmap-set-mapdata kplace data)
	(begin
		; remove any/all previous data
		(map (lambda (rdataobj)
				(kern-obj-remove rdataobj))
			(kplace-get-objects-of-type kplace t_mapdata))
		;add new t_mapdata
		(kern-obj-put-at
			(bind (kern-obj-set-visible (kern-mk-obj t_mapdata 1) #f) data)
			(list kplace 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map random seed values

(define (prmap-mk-prng-params xscale yscale offset modulus)
	(list xscale yscale offset modulus))

(define (prmap-prng-param-xscale data)
	(list-ref data 0))
(define (prmap-prng-param-yscale data)
	(list-ref data 1))
(define (prmap-prng-param-offset data)
	(list-ref data 2))
(define (prmap-prng-param-modulus data)
	(list-ref data 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random map parameter data

(define (prmap-mk-mapdata nsparams ewparams areaparams edgemaps areamaps blitstats) 
	(list nsparams ewparams areaparams edgemaps areamaps blitstats))

(define (prmap-params-nsparams params)
	(list-ref params 0))
(define (prmap-params-ewparams params)
	(list-ref params 1))
(define (prmap-params-areaparams params)
	(list-ref params 2))
(define (prmap-params-edgemaps params)
	(list-ref params 3))
(define (prmap-params-areamaps params)
	(list-ref params 4))
(define (prmap-params-blitstats params)
	(eval (list-ref params 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random template handling

(define (prmap-get-template rxloc ryloc maptype)
	(let* (
			(xmult (list-ref maptype 0)) 
			(ymult (list-ref maptype 1)) 
			(addfactor (list-ref maptype 2)) 
			(modfactor (list-ref maptype 3)) 
			(maplist (eval(list-ref maptype 4)))
			(mapnumber (modulo (+ (* rxloc xmult) (* ryloc ymult) addfactor) modfactor))
		)
		;get the map from the first entry with value greater than mapnumber		
		(cadr
			(car (filter (lambda (listentry)
				(> (car listentry) mapnumber))
					maplist))
			)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deeps fixed map enabling

(define (prmap-room-mklink dir target maptepl)
	(let* ((node (list target maptepl)))
		(get-cardinal-ref
			(list
				(list node nil nil nil)
				(list nil node nil nil)
				(list nil nil node nil)
				(list nil nil nil node)
			)
			dir
		)
	))
	
(define (prmap-room-gethardlink xloc yloc linksdata)
	(let (
		(matchinglink
			(filter (lambda (listentry)
				(and
					(equal? (car listentry) xloc)
					(equal? (cadr listentry) yloc)
				))
				linksdata)
			)
		)
		(if (equal? (length matchinglink) 0)
			(list nil nil nil nil)
			(caddr (car matchinglink)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x y location tracking

;sets x and y location of new rooms based on current location and direction to new room
;also updates current -> prev -> normal status
(define (prmap-room-setxy roomslist dir rxloc ryloc)
	(let* (
			(kplace (eval (get-cardinal-ref roomslist dir)))
			(roomdata (get-roomdata kplace))
			(offsets (get-cardinal-ref prmap-room-offsets dir))
			(rxoff (+ rxloc (car offsets)))
			(ryoff (+ ryloc (cadr offsets)))
		)
		(if (not (prmap-roomdata-current roomdata))
			(prmap-roomdata-setxy roomdata rxoff ryoff))
		(prmap-roomdata-pushcurrent roomdata)
		))

;set xy stuff for all neighboring rooms, also sets current as current
(define (prmap-room-init-neighbors kplace roomdata)
	(let* (
			(rooms (prmap-roomdata-rooms roomdata))
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
		)
		(map (lambda (dir)
			(prmap-room-setxy rooms dir rxloc ryloc))
			(list north west east south))
		(prmap-roomdata-setcurrent roomdata #t)
		;debugging map
		(if #f
			(begin
				(kern-log-msg rxloc)
				(kern-log-msg ryloc))
		)
	))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Room linking

; links in neighbors, or rooms from hardlink list, as appropriate
(define (prmap-room-init-links kplace roomdata hardlinkdata)
	(let* (
			(rooms (prmap-roomdata-rooms roomdata))
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(linkinfo (prmap-room-gethardlink rxloc ryloc hardlinkdata))
		)
		(map (lambda (dir)
			;roomlinktarget is hardlink target if it exists, else regular neighbor
			(let* (
				(thishardlink (get-cardinal-ref linkinfo dir))
				(hardlinktarget (if (null? thishardlink)
									nil
									(car thishardlink)))
				(roomlinktarget (if (null? hardlinktarget)
									(get-cardinal-ref rooms dir)
									hardlinktarget))
				)
				(kern-place-set-neighbor dir kplace (eval roomlinktarget))
			))
			(list north west east south))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map construction	
	
; do a single map blit
(define (prmap-do-map-blit destmap srcmap blitstats)
	(let (
			(xloc (list-ref blitstats 0))
			(yloc (list-ref blitstats 1))
			(wid (list-ref blitstats 2))
			(hgt (list-ref blitstats 3))
		)	
		(kern-blit-map destmap xloc yloc
				(eval srcmap)
				xloc yloc wid hgt)
	))
	
; blit map for area and all sides. uses hard linked sides if given
; todo generalise random-type s
(define (prmap-room-blit-map kplace roomdata hardlinkdata mapdata)
	(let* (
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(linkinfo (prmap-room-gethardlink rxloc ryloc hardlinkdata))
			(blitstats (prmap-params-blitstats mapdata))
			(destmap (kern-place-map kplace))
			(rmapdata (list
				(list 0 1 deep-random-type-ns)
				(list 0 0 deep-random-type-ew)
				(list 1 0 deep-random-type-ew)
				(list 0 0 deep-random-type-ns)
				))
		)
		(prmap-do-map-blit destmap
			(prmap-get-template rxloc ryloc deep-random-type-area)
			(list-ref blitstats 4))
		(map (lambda (dir)
			;roomlinktarget is hardlink target if it exists, else regular neighbor
			(let* (
					(thishardlink (get-cardinal-ref linkinfo dir))
					(thisrmapdata (get-cardinal-ref rmapdata dir))
					(thisx (+ rxloc (car thisrmapdata)))
					(thisy (+ ryloc (cadr thisrmapdata)))
					(thisrtype (caddr thisrmapdata))
					(linkmap (if (null? thishardlink)
								(prmap-get-template thisx thisy thisrtype)
								(cadr thishardlink)))
				)
				(prmap-do-map-blit destmap linkmap
					(get-cardinal-ref blitstats dir))
			))
			(list north west east south))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monster handling

(define (prmap-reduce-level level remaining)
	(max 1
		(- level (/ (* level (kern-dice-roll "1d100")) 50 (max remaining 2)))
		)
	)
	
(define (prmap-get-monster-group quantity grouptype level typelev)
	(cons (mk-npc (car (get-numbered-elem grouptype typelev)) (ceiling (/ level 100)))
		(if (> quantity 0)
			(prmap-get-monster-group (- quantity 1) grouptype
				(prmap-reduce-level level quantity)
				(prmap-reduce-level typelev quantity))
			nil)
	))
	
(define (prmap-mk-monster-group group-types monster-types dice level)
	(let* (
			(grouptype (get-numbered-elem group-types (kern-dice-roll dice)))
			(grouplist (list-ref monster-types (car grouptype)))
		)
		(prmap-get-monster-group (kern-dice-roll (caddr grouptype))
			grouplist level (cadr grouptype))
	))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object handling


(define (prmap-room-cleanout kplace)
	(map (lambda (obj)
		(if (or (equal? (kern-obj-get-type obj) t_roomdata) (equal? (kern-obj-get-type obj) t_mapdata))
			nil
			(begin 
				(kern-obj-remove obj))
		))
		(kern-place-get-objects kplace))
	)
	
(define (prmap-room-addmonster kplace kchar)
	(kern-obj-put-at kchar 
		(random-loc-place-iter kplace 
			(lambda (loc)
				(and (passable? loc kchar)
					(not (is-bad-terrain-at? loc))
					(not (any-object-types-at? loc all-field-types))
					(not (occupied? loc))
				))
		15))
	)

; clears out old objects, and creates new ones
;    this one is rather specific to the endless deeps. need generic version
(define (prmap-room-init-contents kplace roomdata)
	(let* (
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(distance (sqrt (+ (* rxloc rxloc) (* ryloc ryloc))))
		)
		(prmap-room-cleanout kplace)
		(if (< (kern-dice-roll "1d100") 
				(min 75 (+ 25 (* 15 (sqrt distance)))))
			(begin 
			(map (lambda (monster)
				(begin 

					(prmap-room-addmonster kplace monster)))
				(prmap-mk-monster-group deep-group-types deep-monster-types 
					(string-append "1d" (number->string (min 300 (ceiling (* 120 (sqrt distance))))))
					(+ 400 (* 100 (sqrt distance)))))
					)
		)
	))
