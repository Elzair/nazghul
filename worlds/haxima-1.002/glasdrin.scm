;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_glasdrin 31 31 pal_expanded
	(list
		"xx xx xx xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx xx xx xx "
		"xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx cc ++ cc xx "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc xx "
		"xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx "
		"xx xx ,, xx xx xx .A .R .M .S ]] xx ,, ,, ,, ,, ,, ,, ,, xx .M .E .D .I .C .K xx xx ,, xx xx "
		".. xx ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, pp ,, ,, ,, pp ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
		".. xx ,, xx ,, xx @@ @@ @@ @@ @@ xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
		".. xx ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, pp ,, ,, ,, pp ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
		".. xx ,, xx xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
		".. xx ,, xx xx xx xx xx ,, sA xx xx ,, pp ,, ,, ,, pp ,, xx xx xx ,, sH xx xx xx xx ,, xx .. "
		".. xx ,, xx ,, ,, xx .. cc .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx cc .. xx ,, ,, xx ,, xx .. "
		".. xx ,, xx ,, ,, ,, cc cc xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx cc cc ,, ,, ,, xx ,, xx .. "
		".. xx ,, xx xx xx xx .. cc xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx cc .. xx xx xx xx ,, xx .. "
		".. xx ,, xx ,, ,, xx .. cc xx ,, ,, ,, xx w+ ,, w+ xx ,, ,, ,, xx cc .. xx ,, ,, xx ,, xx .. "
		".. xx ,, xx ,, ,, ,, cc cc xx ,, ,, ,, xx cc cc cc xx ,, ,, ,, xx cc cc ,, ,, ,, xx ,, xx .. "
		".. xx ,, xx xx xx xx .. cc xx xx xx xx x! cc cc cc x! xx xx xx xx cc .. xx xx xx xx ,, xx .. "
		".. xx ,, ,, cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ,, ,, xx .. "
		".. xx ,, xx xx xx xx xx xx xx .. .. .. .. cc cc cc t3 tt tt t5 xx xx xx xx xx xx xx ,, xx .. "
		".. xx ,, xx .H .O .T .E .L xx .. .. .. .. cc cc cc tt ~3 ~5 tt xx .H .O .L .Y ]] xx ,, xx .. "
		".. xx ,, xx ,, ,, ,, ,, ,, ,, .. .. .. .. cc cc cc tt ~a ~c tt xx .G .R .A .I .L xx ,, xx .. "
		".. xx ,, xx @@ @@ @@ @@ @@ sI .. .. .. .. cc cc cc ta tt tt tc ws ,, ,, ,, ,, ,, xx ,, xx .. "
		".. xx ,, xx ,, ,, ,, ,, ,, ,, cc cc cc cc cc cx cc cc cc cc cc ,, ,, ,, ,, ,, ,, xx ,, xx .. "
		".. xx ,, xx x! ,, ,, ,, ,, ws bb .. .. bb cc cc cc bb bb bb bb ws ,, ,, 00 ,, && xx ,, xx .. "
		".. xx ,, xx xx ,, xx ,, xx xx .. .. .. .. cc cc cc bb .. .. .. sT ,, ,, 00 ,, ,, xx ,, xx .. "
		".. xx ,, xx ,, ,, xx ,, ,, xx .. .. .. .. cc cc cc bb .. tC t7 xx ,, ,, ,, ,, ,, xx ,, xx .. "
		".. xx ,, xx ,, ,, xx ,, ,, xx .. .. .. bb cc cc cc bb .. tb tc xx ,, [[ @@ ]] xx xx ,, xx .. "
		"xx xx ,, xx xx xx xx xx xx xx xx xx xx x! cc w+ cc x! xx xx xx xx ,, ,, ,, ,, xx xx ,, xx xx "
		"xx ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"xx ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, xx "
		"xx xx xx xx xx .. .. .. .. xx xx xx xx xx cc cc cc xx xx xx xx xx .. .. .. .. xx xx xx xx xx "
	)

)

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "patch.scm")
(kern-load "angela.scm")
(kern-load "jess.scm")
(kern-load "chester.scm")
(kern-load "steward.scm")
(kern-load "ini.scm")
(kern-load "jeffreys.scm")
(kern-load "statue-of-justice.scm")
(kern-load "janice.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_glasdrin     ; tag
 "Glasdrin"      ; name
 s_keep          ; sprite
 m_glasdrin      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list  ;; objects:

  (put (mk-monman) 0 0)

  ;; npcs
  (put (mk-patch) 0 0)
  (put (mk-angela) 0 0)
  (put (mk-jess) 0 0)
  (put (mk-chester) 0 0)
  (put (mk-steward) 0 0)
  (put (mk-ini) 0 0)
  (put (mk-jeffreys) 0 0)
  (put (mk-janice) 0 0)
  (put (mk-bull) 19 24)

  ;; guards
  (put (guard-pt 'glasdrin-halberdier) 15 25)
  (put (guard-pt 'glasdrin-halberdier) 14 12)
  (put (guard-pt 'glasdrin-halberdier) 16 12)
  (put (guard-pt 'glasdrin-crossbowman) 12 28)
  (put (guard-pt 'glasdrin-crossbowman) 18 28)

  ;; doors
  (put (mk-windowed-door) 21 21)
  (put (mk-locked-door) 24 11)
  (put (mk-locked-door) 24 14)
  (put (mk-locked-door)  6 11)
  (put (mk-locked-door)  6 14)
  (put (mk-locked-door) 11 11)
  (put (mk-locked-door) 19 11)
  (put (mk-windowed-door) 8 9)
  (put (mk-door) 5 5)
  (put (mk-door) 5 7)
  (put (mk-windowed-door) 15 13)

  ;; hospital
  (put (mk-windowed-door) 22 9)
  

  ;; wall doors
  (put (mk-locked-door) 3 16)
  (put (mk-locked-door) 27 16)
  (put (mk-locked-door) 2 4)
  (put (mk-locked-door) 4 2)
  (put (mk-door) 26 28)
  (put (mk-door) 28 26)

  ;; inn
  (put (mk-windowed-door) 9 21)
  (put (mk-door) 9 19)
  (put (kern-tag 'glasdrin-inn-room-1-door (mk-locked-door)) 5 23)
  (put (kern-tag 'glasdrin-inn-room-2-door (mk-locked-door)) 7 23)
  (put (mk-bed) 4 25)
  (put (mk-bed) 8 25)
  (put (mk-clock) 4 21)

  ;; NPC beds
  (put (mk-bed) 26 10)
  (put (mk-bed)  4 13)
  (put (mk-bed) 26 13)
  (put (mk-bed)  4 10)
  (put (mk-bed) 11 14)
  (put (mk-bed) 29 28)
  (put (mk-bed) 19 14)
  (put (mk-bed)  1 29)

  ;; ladder down to prison
  (put (mk-ladder-down 'p_prison 6 4) 2 2)

  ;; Stewardess's chest
  (put (let* ((kchest (make-invisible (mk-chest nil '((1 t_stewardess_journal))))))
         (kcontainer-lock-with-key kchest 't_stewardess_chest_key)
         kchest)
       10 14)

  ;; Statue of justice
  (put (kern-mk-obj t_statue_of_justice 1) 15 21)
 
  )


 ;; on-entry hook
 (list 'on-entry-to-dungeon-room 
       'lock-inn-room-doors
       )
 (list ;; edge entrances
 	(list northwest 16 30)
 	(list northeast 14 30)
 )
)

(mk-place-music p_glasdrin 'ml-large-town)
