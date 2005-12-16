(kern-mk-map
 'm_ancient_derelict 19 19 pal_expanded
 (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ "
      "^^ {{ {{ bb {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ .. .. {{ {{ ^^ "
      "{{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ .. #> bb .. {{ ^^ "
      "{{ {{ {{ .. .. .. {{ {{ {{ ^^ ^^ ^^ {{ .. ee .. .. {{ ^^ "
      "{{ {{ .. .. .. #> #> #> {{ ^^ ^^ #> {{ .. .. ee #> {{ ^^ "
      ".. .. .. .. .. .. ee ee {{ {{ ^^ {{ .. .. .. .. {{ ^^ ^^ "
      ".. .. #> .. ee ee ee ee ee {{ {{ {{ ee ee .. #> ^^ ^^ ^^ "
      ".. .. bb ee ee ee ee ee ee ee .. ee ee ee ee #> .. .. ^^ "
      ".. .. #> .. ee ee ee ee ee ee ee ee ee ee .. ee #> .. .. "
      ".. .. .. .. .. ee ee ee ee ee ee .. .. #> #> .. #> .. .. "
      ".. .. bb .. #> #> #> #> #> .. #> #> .. .. .. .. .. bb .. "
      "{{ {{ .. .. .. .. .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "{{ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. "
      "^^ {{ bb {{ {{ {{ .. .. .. .. .. .. .. {{ {{ {{ {{ .. .. "
      "^^ ^^ {{ {{ {{ {{ {{ {{ .. .. .. .. {{ {{ {{ {{ {{ {{ {{ "
      "^^ ^^ ^^ {{ ^^ ^^ ^^ {{ .. .. .. {{ ^^ ^^ ^^ {{ {{ {{ {{ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ .. .. .. {{ ^^ ^^ ^^ ^^ ^^ {{ {{ "
  )
 )

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_ancient_derelict ; tag
 "Ancient Derelict"   ; name
 s_ship              ; sprite
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
        (mk-contents
         (add-content 1 't_staff)
         (add-content 1 't_vas_rel_por_scroll)
         )) 9 10)
  
  )

 (list 'on-entry-to-dungeon-room) ; hooks
 nil ; edge entrances
 )
