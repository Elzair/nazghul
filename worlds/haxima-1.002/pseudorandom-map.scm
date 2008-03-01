;utility for searching {sum-of-probability, object} lists
(define (get-numbered-elem alist value)
	(if (>= (car (car alist)) value)
		(cdar alist)
		(get-numbered-elem (cdr alist) value)
		)
	)
	
;list utilities
;return list with given element swapped
(define (list-swap alist entry index)
	(if (null? alist)
		nil
		(if (zero? index)
			(cons entry (cdr alist))
			(cons (car alist)
				(list-swap (cdr alist) entry (- index 1)))
			)))
			
;return list with nulls defaulting to another list
(define (list-merge newlist defaultlist)
	(if (null? newlist)
		nil
		(cons (if (null? (car newlist))
					(car defaultlist)
					(car newlist))
				(list-merge (cdr newlist) (cdr defaultlist))
		)))
		
;;vector-merge: return copy of a vector with nulls defaulting to another vector
(define (vector-merge-visitor instance index newvec defvec)
	(if (<= 0 index)
		(let ((element (vector-ref newvec index))) 
			(if (null? element)
				(vector-set! instance index (vector-ref defvec index))
				(vector-set! instance index element)
			)
			(vector-merge-visitor instance (- index 1) newvec defvec)
		)
		instance
	))

(define (vector-merge newvec defaultvec)
	(vector-merge-visitor
		(make-vector (vector-length newvec) nil)
			(- (vector-length newvec) 1)
			newvec defaultvec)
		)
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mutable list support
;; workarounds for list building commands dying on empty lists

(define (mk-mutable-list alist)
	(if (null? alist)
		(list 'mutable-list-placeholder)
		alist))

(define (mutable-list-insert alist index entry)
	(cond
		((equal? (car alist) 'mutable-list-placeholder)
			(if (zero? index)
				(set-car! alist entry)
				(begin
					(set-car! alist nil)
					(set-cdr! alist (list 'mutable-list-placeholder))
					(mutable-list-insert (cdr alist) (- index 1) entry)
				))
		)
		((zero? index)
			(set-cdr! alist (cons (car alist) (cdr alist)))
			(set-car! alist entry)
			)
		((null? (cdr alist))
			(set-cdr! alist (list 'mutable-list-placeholder))
			(mutable-list-insert (cdr alist) (- index 1) entry)
			)
		(#t
			(mutable-list-insert (cdr alist) (- index 1) entry))
	))
			
		

(define (mutable-list-set alist index entry)
	(cond
		((equal? (car alist) 'mutable-list-placeholder)
			(if (zero? index)
				(set-car! alist entry)
				(begin
					(set-car! alist nil)
					(set-cdr! alist (list 'mutable-list-placeholder))
					(mutable-list-set (cdr alist) (- index 1) entry)
				))
		)
		((zero? index)
			(set-car! alist entry)
			)
		((null? (cdr alist))
			(set-cdr! alist (list 'mutable-list-placeholder))
			(mutable-list-set (cdr alist) (- index 1) entry)
			)
		(#t
			(mutable-list-set (cdr alist) (- index 1) entry))
	))

;; exercise for the reader: reimplement as balanced tree

(define (mutable-pairlist-get! alist index)
	(cond
		((equal? (car alist) 'mutable-list-placeholder)
			(set-car! alist (list index 'mutable-list-placeholder))
			(cdar alist)
		)
		((< (caar alist) index)
			(if (null? (cdr alist))
				(begin
					(set-cdr! alist (list (list index 'mutable-list-placeholder)))
					(cdadr alist)
				)
				(mutable-pairlist-get! (cdr alist) index)
			))
		((equal? (caar alist) index)
			(cdar alist)
			)
		((> (caar alist) index)
			(set-cdr! alist (cons (car alist) (cdr alist)))
			(set-car! alist (list index 'mutable-list-placeholder))
			(cdar alist)
			)
	))
	
(define (mutable-pairlist-get alist index)
	(cond
		((null? alist)
			nil
			)
		((equal? (car alist) 'mutable-list-placeholder)
			nil
			)		
		((< (caar alist) index)
			(mutable-pairlist-get (cdr alist) index)
			)
		((> (caar alist) index)
			nil
			)
		(#t ;;(equal? (caar alist) index)
			(cdar alist)
			)
	))
	
(define (mutable-pairlist-expand-placeholder alist)
	(if (equal? (car alist) 'mutable-list-placeholder)
		(set-car! alist (list 'mutable-list-placeholder))
			)
	alist)

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

(define prmap-room-offsets
	(vector 
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

(define (prmap-roomdata-tag data)
	(list-ref data 0))
(define (prmap-roomdata-x data)
	(list-ref data 1))
(define (prmap-roomdata-y data)
	(list-ref data 2))
(define (prmap-roomdata-z data)
	(list-ref data 3))
(define (prmap-roomdata-rooms data)
	(list-ref data 4))

(define (prmap-roomdata-setxyz data x y z)
	(set-car! (cdr data) x)
	(set-car! (cddr data) y)
	(set-car! (cdddr data) z))

(define (prmap-mk-roomdata room-tag x y z rooms)
	(set-roomdata (eval room-tag) (list room-tag x y z rooms))
	)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Room linking

; 2 dimensional map - 5 rooms

;  2   5   4   1   3                   
; 513 324 132 245 451                          
;  4   1   5   3   2 

(define (prmap-linkrooms-2d room1-tag room2-tag room3-tag room4-tag room5-tag)
	(prmap-mk-roomdata room1-tag 0 0 0 (list room2-tag room5-tag room3-tag room4-tag))
	(prmap-mk-roomdata room2-tag 0 0 0 (list room5-tag room3-tag room4-tag room1-tag))
	(prmap-mk-roomdata room3-tag 0 0 0 (list room4-tag room1-tag room2-tag room5-tag))
	(prmap-mk-roomdata room4-tag 0 0 0 (list room1-tag room2-tag room5-tag room3-tag))
	(prmap-mk-roomdata room5-tag 0 0 0 (list room3-tag room4-tag room1-tag room2-tag))
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

(define (prmap-mk-prng-params xscale yscale zscale offset modulus)
	(list xscale yscale zscale offset modulus))

(define (prmap-prng-param-xscale data)
	(list-ref data 0))
(define (prmap-prng-param-yscale data)
	(list-ref data 1))
(define (prmap-prng-param-zscale data)
	(list-ref data 2))
(define (prmap-prng-param-offset data)
	(list-ref data 3))
(define (prmap-prng-param-modulus data)
	(list-ref data 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random map parameter data

(define (prmap-mk-mapdata mapname nsparams ewparams areaparams edgemaps areamaps blitstats hardlinkfunction) 
	(list mapname nsparams ewparams areaparams edgemaps areamaps blitstats nil hardlinkfunction (mk-mutable-list nil)))

(define (prmap-params-mapname params)
	(list-ref params 0))
(define (prmap-params-nsparams params)
	(list-ref params 1))
(define (prmap-params-ewparams params)
	(list-ref params 2))
(define (prmap-params-areaparams params)
	(list-ref params 3))
(define (prmap-params-edgemaps params)
	(list-ref params 4))
(define (prmap-params-areamaps params)
	(list-ref params 5))
(define (prmap-params-blitstats params)
	(eval (list-ref params 6)))
	
(define (prmap-params-current params)
	(let ((data (list-ref params 7)))
		(if (null? data)
			nil
			(eval data)
		)))
		
(define (prmap-params-set-current params room)
	(let ((curdat (list-tail params 7)))
		(set-car! curdat room)))
	
(define (prmap-params-hardlinkfunction params)
	(let ((candidatefn (list-ref params 8)))
		(eval
			(if (null? candidatefn)
				'prmap-room-hardlinkentry-get
				candidatefn
		))))
(define (prmap-params-hardlinks params)
	(list-ref params 9)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random template handling

(define (prmap-get-template rxloc ryloc rzloc maptype maplistref)
	(let* (
			(xmult (prmap-prng-param-xscale maptype)) 
			(ymult (prmap-prng-param-yscale maptype)) 
			(zmult (prmap-prng-param-zscale maptype)) 
			(addfactor (prmap-prng-param-offset maptype)) 
			(modfactor (prmap-prng-param-modulus maptype)) 
			(maplist (eval maplistref))
			(mapnumber (modulo (+ (* rxloc xmult) (* ryloc ymult) (* rzloc zmult) addfactor) modfactor))
		)
		;get the map from the first entry with value greater than mapnumber		
		(cdr 
			(car (filter (lambda (listentry)
				(> (car listentry) mapnumber))
					maplist))
			)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hardlinks- fixed rooms enabling

(define (prmap-room-hardlinkentry-get! xloc yloc zloc linksdata)
	(let ((linkentry 
			(mutable-pairlist-get!
			(mutable-pairlist-get!
			(mutable-pairlist-get! linksdata zloc) yloc ) xloc )
			))
		(if (equal? (car linkentry) 'mutable-list-placeholder)
			(set-car! linkentry (vector nil nil nil nil)))
		(car linkentry)
	))

(define (prmap-room-hardlink-set! xloc yloc zloc linksdata dir target maptemplate passable hooklist)
	(vector-set! (prmap-room-hardlinkentry-get! xloc yloc zloc linksdata)
		(cardinal-dir-num dir)
		(cons target (cons maptemplate (cons passable hooklist)))
	))
	
(define (prmap-room-hardlinkentry-get xloc yloc zloc linksdata)
	(let ((linkentry 
			(mutable-pairlist-get
			(mutable-pairlist-get
			(mutable-pairlist-get linksdata zloc) yloc ) xloc )
			))
		(if (null? linkentry)
			(vector nil nil nil nil)
			(car linkentry))
	))

(define (prmap-room-getmaphardlink xloc yloc zloc mapdata)
	(apply (prmap-params-hardlinkfunction mapdata) (list xloc yloc zloc (prmap-params-hardlinks mapdata)))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x y location tracking

;sets x and y location of new rooms based on current location and direction to new room
(define (prmap-room-setxyz roomslist dir rxloc ryloc rzloc)
	(let* (
			(kplace (eval (get-cardinal-lref roomslist dir)))
			(roomdata (get-roomdata kplace))
			(offsets (get-cardinal-ref prmap-room-offsets dir))
			(rxoff (+ rxloc (car offsets)))
			(ryoff (+ ryloc (cadr offsets)))
		)
		(prmap-roomdata-setxyz roomdata rxoff ryoff rzloc)
		))

;set xy stuff for all neighboring rooms
(define (prmap-room-init-neighbors kplace roomdata)
	(let* (
			(rooms (prmap-roomdata-rooms roomdata))
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(rzloc (prmap-roomdata-z roomdata))
		)
		(map (lambda (dir)
			(prmap-room-setxyz rooms dir rxloc ryloc rzloc))
			(list north west east south))
		;debugging map
		(if #f
			(begin
				(kern-log-msg (string-append 
					(number->string rxloc) " "
					(number->string ryloc) " "
					(number->string rzloc) " "))
				(println "loc : " rxloc " " ryloc " " rzloc)
				)
		)
	))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Room linking

; links in neighbors, or rooms from hardlink list, as appropriate
(define (prmap-room-init-links kplace roomdata mapdata)
	(let* (
			(rooms (prmap-roomdata-rooms roomdata))
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(rzloc (prmap-roomdata-z roomdata))
			(linkinfo (prmap-room-getmaphardlink rxloc ryloc rzloc mapdata))
		)
		(map (lambda (dir)
			;roomlinktarget is hardlink target if it exists, else regular neighbor
			(let* (
				(thishardlink (get-cardinal-ref linkinfo dir))
				(hardlinktarget (if (null? thishardlink)
									nil
									(car thishardlink)))
				(roomlinktarget (if (null? hardlinktarget)
									(get-cardinal-lref rooms dir)
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
(define (prmap-room-blit-map kplace roomdata mapdata)
	(let* (
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(rzloc (prmap-roomdata-z roomdata))
			(linkinfo (prmap-room-getmaphardlink rxloc ryloc rzloc mapdata))
			(blitstats (prmap-params-blitstats mapdata))
			(destmap (kern-place-map kplace))
			(rmapdata (vector
				(list 0 1 (prmap-params-nsparams mapdata))
				(list 0 0 (prmap-params-ewparams mapdata))
				(list 1 0 (prmap-params-ewparams mapdata))
				(list 0 0 (prmap-params-nsparams mapdata))
				))
			(areamap-choice (prmap-get-template rxloc ryloc rzloc (prmap-params-areaparams mapdata) (prmap-params-areamaps mapdata)))
		)
		(prmap-do-map-blit destmap
			(car areamap-choice)
			(prmap-blitstats-area blitstats))
		(if (> (length areamap-choice) 1)
			(apply (eval (cadr areamap-choice)) (list kplace))
			)
		(map (lambda (dir)
			;roomlinktarget is hardlink target if it exists, else regular neighbor
			(let* (
					(thishardlink (get-cardinal-ref linkinfo dir))
					(thisrmapdata (get-cardinal-ref rmapdata dir))
					(thisx (+ rxloc (car thisrmapdata)))
					(thisy (+ ryloc (cadr thisrmapdata)))
					(thisrtype (caddr thisrmapdata))
					(linkmap (if (or (null? thishardlink) (null? (car (cdr thishardlink))))
								(prmap-get-template thisx thisy rzloc thisrtype (prmap-params-edgemaps mapdata))
								(cdr thishardlink)))
				)
				(prmap-do-map-blit destmap (car linkmap)
					(get-cardinal-lref blitstats dir))
				(if (> (length linkmap) 2)
					(map (eval (get-cardinal-lref (cddr linkmap) dir)) (list kplace))
					)
			))
			(list north west east south))
	))
	
; blit map for all sides. uses hard linked sides if given
;; todo refactor duplicate code!
(define (prmap-room-blit-map-edges kplace roomdata mapdata)
	(let* (
			(rxloc (prmap-roomdata-x roomdata))
			(ryloc (prmap-roomdata-y roomdata))
			(rzloc (prmap-roomdata-z roomdata))
			(linkinfo (prmap-room-getmaphardlink rxloc ryloc rzloc mapdata))
			(blitstats (prmap-params-blitstats mapdata))
			(destmap (kern-place-map kplace))
			(rmapdata (vector
				(list 0 1 (prmap-params-nsparams mapdata))
				(list 0 0 (prmap-params-ewparams mapdata))
				(list 1 0 (prmap-params-ewparams mapdata))
				(list 0 0 (prmap-params-nsparams mapdata))
				))
		)
		(map (lambda (dir)
			;roomlinktarget is hardlink target if it exists, else regular neighbor
			(let* (
					(thishardlink (get-cardinal-ref linkinfo dir))
					(thisrmapdata (get-cardinal-ref rmapdata dir))
					(thisx (+ rxloc (car thisrmapdata)))
					(thisy (+ ryloc (cadr thisrmapdata)))
					(thisrtype (caddr thisrmapdata))
					;;(temp (println "thl " thishardlink))
					(linkmap (if (or (null? thishardlink) (null? (car (cdr thishardlink))))
								(prmap-get-template thisx thisy rzloc thisrtype (prmap-params-edgemaps mapdata))
								(cdr thishardlink)))
				)
                          ;;(println "lm " linkmap)
				(prmap-do-map-blit destmap (car linkmap)
					(get-cardinal-lref blitstats dir))
				(if (> (length linkmap) 2)
					(map (eval (get-cardinal-lref (cddr linkmap) dir)) (list kplace))
					)
			))
			(list north west east south))
	)
	;;(println "done")
	)

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

(define (prmap-room-roomkey target mapname)
	(let* ((roomdata (get-roomdata target))
				(x (prmap-roomdata-x roomdata))
				(y (prmap-roomdata-y roomdata))
				(z (prmap-roomdata-z roomdata)))
		(string-append mapname "|" 
			(number->string x) "|" 
			(number->string y) "|"
			(number->string z))
	))

;; TODO this should be done by layer, not type! (add kern method?)

(define (prmap-room-freezable-type obj)
	(let ((objtype (kern-obj-get-type obj)))
	(not (in-text-list? objtype (list t_roomdata t_mapdata t_sounddata))
	))
	)

(define (prmap-room-freeze-current mapdata)
	(let ((current (prmap-params-current mapdata)))
		(if (not (null? current))
			(let* ((roomkey (prmap-room-roomkey current (prmap-params-mapname mapdata))))
                          ;;(println "freeze: " roomkey)
				(map (lambda (obj)
					(if (prmap-room-freezable-type obj)
						(let*
							((loc (kern-obj-get-location obj))
								(x (car (cdr loc)))
								(y (car (cddr loc))))
							(kern-obj-freeze obj roomkey x y)
							(kern-obj-remove obj)
						)
						nil
					))
					(kern-place-get-objects current))
				(prmap-params-set-current mapdata nil)
			)
		)
		))

(define (prmap-room-thaw kplace mapdata)
	(let ((roomkey (prmap-room-roomkey kplace (prmap-params-mapname mapdata))))
          ;;(println " thaw: " roomkey)
		(do 
			((obj (kern-obj-thaw roomkey kplace) (kern-obj-thaw roomkey kplace)))
			((null? obj))
                  ;;(println "obj " (kern-obj-get-name obj))
		)	
		(prmap-params-set-current mapdata (prmap-roomdata-tag (get-roomdata kplace)))
	))

(define (prmap-room-cleanout kplace)
	(map (lambda (obj)
		(if (prmap-room-freezable-type obj)
			(begin 
				(kern-obj-remove obj))
			nil
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

; creates new objects for a room
;    this one is rather specific to the endless deeps. need generic version
(define (prmap-room-init-contents kplace roomdata)
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
				(prmap-mk-monster-group deep-group-types deep-monster-types 
					(string-append "1d" (number->string (min 300 (ceiling (* 120 (sqrt distance))))))
					(+ 400 (* 100 (sqrt distance)))))
					)
		)
	))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map cohesion checking

(define (vector-2d-make width height fill)
    (let ((2dvec (make-vector height nil)))
        (do ((index (- height 1) (- index 1)))
            ((< index 0) 2dvec)
          (vector-set! 2dvec index (make-vector width fill)))
	))
	
(define (vector-2d-get table cursor)
	(vector-ref (vector-ref table (vector-ref cursor 1)) (vector-ref cursor 0))
	)

(define (vector-2d-get-off table xmin ymin cursor)
	(vector-ref (vector-ref table (- (vector-ref cursor 1) ymin)) (- (vector-ref cursor 0) xmin))
	)
	
(define (vector-2d-set-off table xmin ymin cursor entry)
	(vector-set! (vector-ref table (- (vector-ref cursor 1) ymin)) (- (vector-ref cursor 0) xmin) entry)
	)

;; ensures that an area of man is completely connected adding new links as necessary
;; (linkfactory x y z mapdata dir) should return an appropriate hardlink (list template hooks)
(define (prmap-ensure-cohesion mapdata xmin xmax ymin ymax zloc linkfactory)
	(let* ((width (- xmax xmin -1))
			(height (- ymax ymin -1))
			(table (vector-2d-make width height 'unk))
			(checking-cursor (vector 0 0))
			(checking-end (vector 0 0))
			(working-cursor (vector xmin ymin))
			;; flags are: (most southwesterly exploration dir), 
			;; (have finished checking loop),
			;; (have finished checking whole maze)
			(flags (vector #f #f #f))
			(rmapdata (vector
					(list 0 1 (prmap-params-nsparams mapdata))
					(list 0 0 (prmap-params-ewparams mapdata))
					(list 1 0 (prmap-params-ewparams mapdata))
					(list 0 0 (prmap-params-nsparams mapdata))
					))
			)
			
		(define (prmap-cohesion-check-neighbor linkinfo xloc yloc zloc dir)
			(let* (
				(thishardlink (get-cardinal-ref linkinfo dir))
				(thisrmapdata (get-cardinal-ref rmapdata dir))
				(thisx (+ xloc (car thisrmapdata)))
				(thisy (+ yloc (cadr thisrmapdata)))
				(thisrtype (caddr thisrmapdata))
				(linkmap (if (or (null? thishardlink) (null? (car (cdr thishardlink))))
						(prmap-get-template thisx thisy zloc thisrtype (prmap-params-edgemaps mapdata))
						(cdr thishardlink))
						))
					(cadr linkmap)
				))
				
		(define (link-neighbor linkinfo xloc yloc zloc dir)
			(let* ((posoff (get-cardinal-ref prmap-room-offsets dir))
					(xoff (- xmin (car posoff)))
					(yoff (- ymin (cadr posoff)))
					)
				(if (and (equal? (vector-2d-get-off table xoff yoff working-cursor) 'unk)
						(prmap-cohesion-check-neighbor linkinfo xloc yloc zloc dir)
						)
					(begin
						(vector-2d-set-off table xoff yoff working-cursor 'lin)
						(vector-set! flags 0 dir)
						))
				))
				
		(define (check-hardlink xoff yoff dir)
			(null? (get-cardinal-ref
				(prmap-room-getmaphardlink 
					(+ (vector-ref checking-cursor 0) xoff xmin) 
					(+ (vector-ref checking-cursor 1) yoff ymin) 
						zloc mapdata)
					dir)
				))
				
		(define (mk-linkpair xoff yoff dir1 dir2)
			(let ((linkhere (apply linkfactory 
					(list
						(+ (vector-ref checking-cursor 0) xmin)
						(+ (vector-ref checking-cursor 1) ymin)
						zloc mapdata dir1)
					))
				(linkthere (apply linkfactory 
					(list
						(+ (vector-ref checking-cursor 0) xoff xmin)
						(+ (vector-ref checking-cursor 1) yoff ymin) 
						zloc mapdata dir2))
					))
				(prmap-room-hardlink-set! 
					(+ (vector-ref checking-cursor 0) xmin)
					(+ (vector-ref checking-cursor 1) ymin)
					zloc (prmap-params-hardlinks mapdata) dir1
					nil (car linkhere) #t (cadr linkhere))
				(prmap-room-hardlink-set! 
					(+ (vector-ref checking-cursor 0) xoff xmin)
					(+ (vector-ref checking-cursor 1) yoff ymin) 
					zloc (prmap-params-hardlinks mapdata) dir2
					nil (car linkthere) #t (cadr linkthere))
				)
			(vector-2d-set-off table 0 0 checking-cursor 'lin)
			(vector-set! working-cursor 0 (+ (vector-ref checking-cursor 0) xmin))
			(vector-set! working-cursor 1 (+ (vector-ref checking-cursor 1) ymin))
			(vector-set! flags 1 #t)
			)	
			
		(vector-set! (vector-ref table 0) 0 'lin)
				
		;; loop on fixing links till all checked
		(do 
			()
			((vector-ref flags 2))
			
;; single continuity working pass
;;
;; start from northwest corner (or continue from where a new area was linked in)
;;
;; we are at the west end of the south row of the potentially linked part of the maze
;; if we are at a linked cell:
;;    mark any accessible neighbors as linked
;;    mark the current cell explored
;; if we added any linked cells to the south or west, backtrack to those cells
;;    (so we are always looking at the west end of the southmost unknown row)
;; otherwise continue east or to the start of the next row
		
			(do
				()
				((> (vector-ref working-cursor 1) ymax))
				(vector-set! flags 0 east)
				(if (equal? (vector-2d-get-off table xmin ymin working-cursor) 'lin)
					(begin
						(vector-2d-set-off table xmin ymin working-cursor 'exp)
						(let* ((xloc (vector-ref working-cursor 0))
								(yloc (vector-ref working-cursor 1))
								(linkinfo (prmap-room-getmaphardlink xloc yloc zloc mapdata)))
							(if (< (vector-ref working-cursor 1) ymax)
								(link-neighbor linkinfo xloc yloc zloc north))
							(if (< (vector-ref working-cursor 0) xmax)
								(link-neighbor linkinfo xloc yloc zloc east))
							(if (> (vector-ref working-cursor 0) xmin)
								(link-neighbor linkinfo xloc yloc zloc west))
							(if (> (vector-ref working-cursor 1) ymin)
								(link-neighbor linkinfo xloc yloc zloc south))
						)))
				(cond ((equal? (vector-ref flags 0) south)
						(vector-set! working-cursor 1 (- (vector-ref working-cursor 1) 1)))
					((equal? (vector-ref flags 0) west)
						(vector-set! working-cursor 0 (- (vector-ref working-cursor 0) 1)))
					((< (vector-ref working-cursor 0) xmax)
						(vector-set! working-cursor 0 (+ (vector-ref working-cursor 0) 1)))
					(#t
						(vector-set! working-cursor 0 xmin)
						(vector-set! working-cursor 1 (+ 1 (vector-ref working-cursor 1))))
				)
			)
		
;; single checking working pass
;;
;; start from an initially random point in the maze
;;
;; from here iterate through each cell in each row of the maze.
;; when the current cells status is not the same as that of the cell to the north or east,
;;    then we are on a boundary between explored and unexplored, so
;;    create a hardlink joining the cells and stop the current pass
;; mark any explored cell with explored neighbors so it can be ignored on later passes
;; when the cursor reaches the northeast point, continue from the southwest
;; the entire maze is linked when the cursor reaches where it started from 
	
			(vector-set! flags 1 #f)
			;; checking end is tile before current checking cursor
			;;   looping if need be
			(vector-set! checking-cursor 0 (- (kern-dice-roll (mkdice 1 width)) 1))
			(vector-set! checking-cursor 1 (- (kern-dice-roll (mkdice 1 height)) 1))
			(cond ((> (vector-ref checking-cursor 0) 0)
				(vector-set! checking-end 0 (- (vector-ref checking-cursor 0) 1))
				(vector-set! checking-end 1 (vector-ref checking-cursor 1))
				)
				((> (vector-ref checking-cursor 1) 0)
					(vector-set! checking-end 0 (- width 1))
					(vector-set! checking-end 1 (- (vector-ref checking-cursor 1) 1))
					)
				(#t
					(vector-set! checking-end 0 (- width 1))
					(vector-set! checking-end 1 (- height 1))
					))
	
			(do
				()
				((vector-ref flags 1))
				(let ((currentstatus (vector-2d-get-off table 0 0 checking-cursor))	)
				;;(println "checking " checking-cursor)
				(cond
					((equal? (vector-2d-get-off table 0 0 checking-cursor) 'chk))
					;; check east
					((and (< (vector-ref checking-cursor 0) (- width 1))
							(not (equal? (vector-2d-get-off table -1 0 checking-cursor) currentstatus))
							(not (equal? (vector-2d-get-off table -1 0 checking-cursor) 'chk))
							(check-hardlink 0 0 east)
							(check-hardlink 1 0 west)
							)
						(mk-linkpair 1 0 east west)
						;;(println "mklink east " working-cursor)
						)
					;; check north
					((and (< (vector-ref checking-cursor 1) (- height 1))
							(not (equal? (vector-2d-get-off table 0 -1 checking-cursor) currentstatus))
							(not (equal? (vector-2d-get-off table 0 -1 checking-cursor) 'chk))
							(check-hardlink 0 0 north)
							(check-hardlink 0 1 south)
							)
						(mk-linkpair 0 1 north south)
						;;(println "mklink north " working-cursor)
						)
					((equal? (vector-2d-get-off table 0 0 checking-cursor) 'exp)
						(vector-2d-set-off table 0 0 checking-cursor 'chk))
				)
				(cond
					;; done
					((and (equal? (vector-ref checking-cursor 0) (vector-ref checking-end 0))
						(equal? (vector-ref checking-cursor 1) (vector-ref checking-end 1)))
						(vector-set! flags 1 #t)
						(vector-set! flags 2 #t)
						)
					;; next cell east
					((< (vector-ref checking-cursor 0) (- width 1))
						(vector-set! checking-cursor 0 (+ (vector-ref checking-cursor 0) 1)))
					;; next row
					((< (vector-ref checking-cursor 1) (- height 1))
						(vector-set! checking-cursor 0 0)
						(vector-set! checking-cursor 1 (+ 1 (vector-ref checking-cursor 1))))
					;; loop to sw
					(#t
						(vector-set! checking-cursor 0 0)
						(vector-set! checking-cursor 1 0))
				)
			))
			;;(println "tab " table)
		)
	))
