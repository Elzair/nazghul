(kern-mk-map
 'm_ancient_derelict 19 19 pal_expanded
	(list
			"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
			"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
			"^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ "
			"^^ {{ {{ bb {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {3 {5 {{ {{ ^^ "
			"{{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {3 #A bb {5 {{ ^^ "
			"{{ {{ {{ {3 {1 {5 {{ {{ {{ ^^ ^^ ^^ {{ {2 ee .. {4 {{ ^^ "
			"{{ {{ {3 .. .. #A #r #E {{ ^^ ^^ #A {{ {2 .. ee #C {{ ^^ "
			"{1 {1 .. .. .. .. ee ee {{ {{ ^^ {{ {3 .. .. {4 {{ ^^ ^^ "
			".. .. #E .. ee ee ee ee ee {{ {{ {{ ee ee .. #C ^^ ^^ ^^ "
			".. .. bb ee ee ee ee ee ee ee {1 ee ee ee ee #F {1 {5 ^^ "
			".. .. #B .. ee ee ee ee ee ee ee ee ee ee .. ee #C .. {1 "
			".. .. .. .. .. ee ee ee ee ee ee .. .. #D #r .. #C .. .. "
			"{8 {8 bb .. #D #r #G #H #D .. #r #r .. .. .. .. .. bb .. "
			"{{ {{ {a .. .. .. .. .. .. bb .. .. .. .. .. .. .. .. .. "
			"{{ {{ {{ {a {8 {8 .. .. .. .. .. .. .. {8 {8 {8 bb .. .. "
			"^^ {{ bb {{ {{ {{ {a {8 .. .. .. .. {c {{ {{ {{ {{ {a {8 "
			"^^ ^^ {{ {{ {{ {{ {{ {{ {2 .. .. {c {{ {{ {{ {{ {{ {{ {{ "
			"^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {2 .. {4 {{ ^^ ^^ ^^ {{ {{ {{ {{ "
			"^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {2 .. {4 {{ ^^ ^^ ^^ ^^ ^^ {{ {{ "
	)
 )

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_ancient_derelict ; tag
 "Ancient Derelict"   ; name
 s_void_ship          ; sprite
 m_ancient_derelict  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-monman) 0 0)
  (put (spawn-pt 'wisp) 7 9)
  (put (spawn-pt 'wisp) 10 9)
  (put (kern-mk-obj t_power_core 1) 12 5)
  (put (mk-corpse2
        '(
         (1 t_staff)
         (1 t_vas_rel_por_scroll)
         )) 9 10)
  
  )

 (list 'on-entry-to-dungeon-room) ; hooks
 (list ; edge entrances
 	(list southeast 0 4)
 	(list northeast 0 14)
 )
 )

(mk-place-music p_ancient_derelict 'ml-outdoor-adventure)
