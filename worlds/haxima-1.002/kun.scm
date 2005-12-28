(kern-load "mesmeme.scm")
(kern-load "jake.scm")
(kern-load "slywan.scm")
(kern-load "tooth.scm")

(kern-mk-place
 'p_kun "Kun" s_town
 (kern-mk-map 
  nil 19 19 pal_expanded
  (list
      "xx xx xx xx xx xx xx xx xx xx xx {{ .. .. .. {{ ^^ ^^ ^^ "
      "xx [[ .C .A .N .T .I .N .A ]] xx {{ .. .. .. {{ {{ ^^ ^^ "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ .. .. .. {{ {{ {{ ^^ "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ .. .. .. {{ ^^ ^^ ^^ "
      "xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ .. .. .. {{ ^^ ^^ {{ "
      "xx ,, ,, 00 ,, ,, ,, 00 ,, ,, xx {{ .. .. .. {{ {{ {{ {{ "
      "xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ .. .. .. .. .. .. .. "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ .. .. .. .. .. .. .. "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ .. .. .. .. .. .. .. "
      "xx xx ws xx xx ,, xx xx ws xx xx {{ .. .. .. bb {{ bb {{ "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. {{ {{ {{ ^^ "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb {{ ^^ ^^ "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. {{ ^^ ^^ ^^ "
      "{{ bb {{ bb .. .. .. {{ xx xx xx xx xx ,, xx xx xx xx xx "
      "{{ {{ {{ {{ .. .. .. {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ {{ {{ bb .. .. .. {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ ^^ {{ {{ .. .. .. {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ ^^ ^^ {{ .. .. .. {{ xx .D .U .T .Y @@ .F .R .E .E xx "
      "^^ ^^ ^^ {{ .. .. .. {{ xx xx xx xx xx xx xx xx xx xx xx "
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
  )
 )
