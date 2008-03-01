(kern-load "mesmeme.scm")
(kern-load "jake.scm")
(kern-load "slywan.scm")
(kern-load "tooth.scm")
(kern-load "tetzl.scm")

(kern-mk-place
 'p_kun "Kun" s_town
 (kern-mk-map 
  nil 19 19 pal_expanded
	(list
		"xx xx xx xx xx xx xx xx xx xx xx {{ {2 .. {4 {{ ^^ ^^ ^^ "
		"xx [[ .C .A .N .T .I .N .A ]] xx {{ {2 .. {4 {{ {{ ^^ ^^ "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ {2 .. {4 {{ {{ {{ ^^ "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ {2 .. {4 {{ ^^ ^^ ^^ "
		"xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ {2 .. {4 {{ ^^ ^^ {{ "
		"xx ,, ,, 00 ,, ,, ,, 00 ,, ,, xx {{ {2 .. {4 {{ {{ {{ {{ "
		"xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ {2 .. .. {1 {1 {1 {1 "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ {2 .. .. .. .. .. .. "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ {2 .. .. .. {8 .. {8 "
		"xx xx ws xx xx ,, sT xx ws xx xx {{ {2 .. .. bb {{ bb {{ "
		".. .. .. .. .. .. .. .. .. .. .. {1 .. .. {4 {{ {{ {{ ^^ "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb {{ ^^ ^^ "
		"{8 .. {8 .. .. .. .. {8 .. .. .. .. .. .. {4 {{ ^^ ^^ ^^ "
		"{{ bb {{ bb .. .. {4 {{ xx xx xx xx sE ,, xx xx xx xx xx "
		"{{ {{ {{ {{ {2 .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"^^ {{ {{ bb .. .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"^^ ^^ {{ {{ {2 .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"^^ ^^ ^^ {{ {2 .. {4 {{ xx .D .U .T .Y @@ .F .R .E .E xx "
		"^^ ^^ ^^ {{ {2 .. {4 {{ xx xx xx xx xx xx xx xx xx xx xx "
	)
  )
 #f      ; wraps
 #f      ; underground
 #f      ; large-scale (wilderness)
 #f      ; tmp combat place
 nil     ; subplaces
 nil     ; neighbors
 
 ;; objects
 (list

  ;; npcs
  (put (mk-mesmeme) 0 0)
  (put (mk-jake) 0 0)
  (put (mk-slywan) 0 0)
  (put (mk-tooth) 0 0)
  (put (mk-tetzl) 0 0)

  (put (mk-windowed-door) 5 9)
  (put (mk-door) 13 13)
  )
 
 ;; on-entry hooks
 nil

 ;; edge entrances
 (list
  (list north 5 18)
  (list south 13 0)
  (list west 18 7)
  (list east 0 11)
  (list northwest 18 9)
  (list southeast 0 10)
  (list southwest 15 0)
  (list northeast 3 18)
  )
 )

(mk-place-music p_kun 'ml-small-town)
