(kern-load "gamestart-mech.scm")
(kern-load "gamestart-statues.scm")

(kern-mk-place 'p_char_setup "the Path"
  s_shrine ;; sprite
  (kern-mk-map 'm_char_setup 19 19 pal_expanded
	(list
	  "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
	  "x! @@ @@ .C .H .O .O .S .E @@ @@ @@ .Y .O .U .R @@ @@ x! "
	  "xx @@ @@ @@ @@ @@ @@ .P .A .T .H @@ @@ @@ @@ @@ @@ @@ xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ++ ,, ,, ,, ,, ,, ,, ,, ++ ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, cx cx cx ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, cx cx cx ,, ,, ,, ,, ,, ,, ,, cx cx cx ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
	  "x! ,, ,, +s ,, ,, ,, ,, pp cc pp ,, ,, ,, ,, +s ,, ,, x! "
	  "xx ,, ,, ,, ,, ,, ,, pp ,, cc ,, pp ,, ,, ,, ,, ,, ,, xx "
	  "xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx "
	)
	)

  #f #t #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil
  
 ;; *** contents of the place ***
  (list
   (put (mk-step-trig 'get-player-name nil) 9 16)
   (put (kern-tag 'start-gate (mk-start-portal 'start-actual-game)) 9 4)
   (put (kern-mk-obj F_illum_perm 1) 3 1)
   (put (kern-mk-obj F_illum_perm 1) 15 1)
   (put (kern-mk-obj F_illum_perm 1) 9 1)
   (put (kern-mk-obj F_illum_perm 1) 10 4)
   (put (kern-mk-obj F_illum_perm 1) 8 4)
   )

  nil ;; hooks
  nil

)
 
(define (obj-line objfactory yloc xloc xmax)
	(kern-obj-put-at (objfactory xloc) (list p_char_setup xloc yloc))
	(if (< xloc xmax)
		(obj-line objfactory yloc (+ xloc 1) xmax)
	))  
 
;; Note: start-gate must be a tag to survive saving/reloading.
(set-roomdata p_char_setup (list 6 6 6 'start-gate))

(obj-line (lambda (unused)
	(mk-step-trig 'one-off-message "A portal beckons on the far side of the room" "intromes"))
	15 8 10)
	
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-light-lamps nil "lamps"))
	14 7 11)
	
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 7 15))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 11 15))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 7 16))
(kern-obj-put-at (mk-step-trig 'gamestart-light-lamps nil "lamps") (list p_char_setup 11 16))

(define (mk-start-statue tag name sprite conv)
  (let ((kchar (bind 
                (kern-mk-char 
                 tag            ; tag
                 name             ; name
                 sp_statue         ; species
                 nil              ; occ
                 sprite     ; sprite
                 faction-men      ; starting alignment
                 0 0 0            ; str/int/dex
                 999 0              ; hp mod/mult
                 0 0              ; mp mod/mult
                 max-health ; hp
                 0                   ; xp
                 max-health ; mp
                 0
                 9
                 #f               ; dead
                 conv         ; conv
                 nil           ; sched
                 'ankh-ai              ; special ai
                 nil              ; container
                 nil              ; readied
                 )
                nil)))
    (kern-char-set-known kchar #t)
    ))

(kern-obj-put-at (mk-start-statue 'str_statue "Statue of Might" s_str_statue 'gs-str-conv) (list p_char_setup 4 10))
(kern-obj-put-at (mk-start-statue 'dex_statue "Statue of Agility" s_dex_statue 'gs-dex-conv) (list p_char_setup 9 8))
(kern-obj-put-at (mk-start-statue 'int_statue "Statue of Wisdom" s_int_statue 'gs-int-conv) (list p_char_setup 14 10))

(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'str_statue "statspeak"))
	10 1 6)
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'dex_statue "statspeak"))
	9 6 12)
(obj-line (lambda (unused)
	(mk-step-trig 'gamestart-statue-speak 'int_statue "statspeak"))
	10 12 17)

	
;;(gamestart-field-circle F_fire_perm p_char_setup 4 10 4)
;;(gamestart-field-circle F_acid_perm p_char_setup 9 8 4)
;;(gamestart-field-circle F_energy_perm p_char_setup 14 10 4)

