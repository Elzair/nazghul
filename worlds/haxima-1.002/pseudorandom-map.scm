;utility for searching {sum-of-probability, object} lists
(define (get-numbered-elem list value)
	(if (>= (car (car list)) value)
		(cdar list)
		(get-numbered-elem (cdr list) value)
		)
	)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Directions	

; order is N W E S
(define (get-cardinal-ref list dir)
	(begin
	(list-ref list
		(/ (- dir 1) 2))
		))

(define deep-room-offsets
	(list 
		(list 0 1)
		(list -1 0)
		(list 1 0)
		(list 0 -1)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic room data

(mk-obj-type 't_roomdata nil nil layer-none monman-ifc)

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

(define (deeps-roomdata-x data)
	(list-ref data 0))
(define (deeps-roomdata-y data)
	(list-ref data 1))
(define (deeps-roomdata-current data)
	(list-ref data 2))
(define (deeps-roomdata-prev data)
	(list-ref data 3))
(define (deeps-roomdata-rooms data)
	(list-ref data 4))

(define (deeps-roomdata-setxy data x y)
	(set-car! data x)
	(set-car! (cdr data) y))

;prev becomes current, current is cleared
(define (deeps-roomdata-pushcurrent data)
	(let ((curdat (list-tail data 2)))
		(set-car! (cdr curdat) (car curdat))
		(set-car! curdat #f)))

(define (deeps-roomdata-setcurrent data cur)
	(let ((curdat (list-tail data 2)))
		(set-car! curdat cur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random template handling

(define (deep-get-template rxloc ryloc maptype)
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

(define (deep-room-mklink dir target maptepl)
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
	
(define deep-room-hardlinks
  (list 
  	(list 0 1    (deep-room-mklink south 'p_lost_garrison 'm_deeptempl_passage))
	(list 1 1    (deep-room-mklink south nil 'm_deeptempl_wall))
  	(list -1 0   (deep-room-mklink east 'p_lost_garrison 'm_deeptempl_passage))
	(list 2 0    (deep-room-mklink west nil 'm_deeptempl_wall))
  	(list -1 -1  (deep-room-mklink east nil 'm_deeptempl_wall))
	(list 2 -1   (deep-room-mklink west nil 'm_deeptempl_wall))
	(list 0 -2   (deep-room-mklink north nil 'm_deeptempl_wall))
	(list 1 -2   (deep-room-mklink north nil 'm_deeptempl_wall))
  ))
	
(define (deep-room-gethardlink xloc yloc linksdata)
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
(define (deep-room-setxy roomslist dir rxloc ryloc)
	(let* (
			(kplace (eval (get-cardinal-ref roomslist dir)))
			(roomdata (get-roomdata kplace))
			(offsets (get-cardinal-ref deep-room-offsets dir))
			(rxoff (+ rxloc (car offsets)))
			(ryoff (+ ryloc (cadr offsets)))
		)
		(if (not (deeps-roomdata-current roomdata))
			(deeps-roomdata-setxy roomdata rxoff ryoff))
		(deeps-roomdata-pushcurrent roomdata)
		))

;set xy stuff for all neighboring rooms, also sets current as current
(define (deep-room-init-neighbors kplace roomdata)
	(let* (
			(rooms (deeps-roomdata-rooms roomdata))
			(rxloc (deeps-roomdata-x roomdata))
			(ryloc (deeps-roomdata-y roomdata))
		)
		(map (lambda (dir)
			(deep-room-setxy rooms dir rxloc ryloc))
			(list north west east south))
		(deeps-roomdata-setcurrent roomdata #t)
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
(define (deep-room-init-links kplace roomdata hardlinkdata)
	(let* (
			(rooms (deeps-roomdata-rooms roomdata))
			(rxloc (deeps-roomdata-x roomdata))
			(ryloc (deeps-roomdata-y roomdata))
			(linkinfo (deep-room-gethardlink rxloc ryloc hardlinkdata))
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
(define (deep-do-map-blit destmap srcmap blitstats)
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
(define (deep-room-blit-map kplace roomdata hardlinkdata blitstats)
	(let* (
			(rxloc (deeps-roomdata-x roomdata))
			(ryloc (deeps-roomdata-y roomdata))
			(linkinfo (deep-room-gethardlink rxloc ryloc hardlinkdata))
			(destmap (kern-place-map kplace))
			(rmapdata (list
				(list 0 1 deep-random-type-ns)
				(list 0 0 deep-random-type-ew)
				(list 1 0 deep-random-type-ew)
				(list 0 0 deep-random-type-ns)
				))
		)
		(deep-do-map-blit destmap
			(deep-get-template rxloc ryloc deep-random-type-area)
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
								(deep-get-template thisx thisy thisrtype)
								(cadr thishardlink)))
				)
				(deep-do-map-blit destmap linkmap
					(get-cardinal-ref blitstats dir))
			))
			(list north west east south))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monster handling

(define (deep-reduce-level level remaining)
	(max 1
		(- level (/ (* level (kern-dice-roll "1d100")) 50 (max remaining 2)))
		)
	)
	
(define (deep-get-monster-group quantity grouptype level typelev)
	(cons (mk-npc (car (get-numbered-elem grouptype typelev)) (ceiling (/ level 100)))
		(if (> quantity 0)
			(deep-get-monster-group (- quantity 1) grouptype
				(deep-reduce-level level quantity)
				(deep-reduce-level typelev quantity))
			nil)
	))
	
(define (deep-mk-monster-group group-types monster-types dice level)
	(let* (
			(grouptype (get-numbered-elem group-types (kern-dice-roll dice)))
			(grouplist (list-ref monster-types (car grouptype)))
		)
		(deep-get-monster-group (kern-dice-roll (caddr grouptype))
			grouplist level (cadr grouptype))
	))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object handling


(define (deep-room-cleanout kplace)
	(map (lambda (obj)
		(if (equal? (kern-obj-get-type obj) t_roomdata)
			nil
			(begin 
				(kern-obj-remove obj))
		))
		(kern-place-get-objects kplace))
	)
	
(define (deep-room-addmonster kplace kchar)
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
(define (deep-room-init-contents kplace roomdata)
	(let* (
			(rxloc (deeps-roomdata-x roomdata))
			(ryloc (deeps-roomdata-y roomdata))
			(distance (sqrt (+ (* rxloc rxloc) (* ryloc ryloc))))
		)
		(deep-room-cleanout kplace)
		(if (< (kern-dice-roll "1d100") 
				(min 75 (+ 25 (* 15 (sqrt distance)))))
			(begin 
			(map (lambda (monster)
				(begin 

					(deep-room-addmonster kplace monster)))
				(deep-mk-monster-group deep-group-types deep-monster-types 
					(string-append "1d" (number->string (min 300 (ceiling (* 120 (sqrt distance))))))
					(+ 400 (* 100 (sqrt distance)))))
					)
		)
	))
