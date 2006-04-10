;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_void_temple 19 19 pal_expanded
	(list
		"cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
		"cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
		"cc cc t3 tt tt *3 *1 *5 bb .. bb *3 *1 *5 tt tt t5 cc cc "
		"cc cc tt tt *3 ** *. ** *5 cc *3 ** ** ** *5 tt tt cc cc "
		"cc cc tt *3 *. ** ** ** *4 cc *2 ** ** *. *. *5 tt cc cc "
		"cc cc *3 ** ** *. ** ** *c cc *a ** ** ** ** *. *5 cc cc "
		"cc cc *2 *. *. ** *. *c bb .. bb *a *. ** *. ** *4 cc cc "
		".. cc *a ** *. ** *c .g .. .. .. .h *a ** ** ** *c cc .. "
		".. .. bb *a *8 *c bb .. .. .. .. .. bb *a *8 *c bb .. .. "
		".. .. .. cc cc cc .. .. .. .. .. .. .. cc cc cc .. .. .. "
		".. .. bb *3 *1 *5 bb .. .. .. .. .. bb *3 *1 *5 bb .. .. "
		".. cc *3 ** ** ** *5 .j .. .. .. .l *3 ** *. ** *5 cc .. "
		"cc cc *2 *. *. ** ** *5 bb .. bb *3 ** *. *. *. *4 cc cc "
		"cc cc *a *. *. ** ** *. *5 cc *3 ** ** *. ** ** *c cc cc "
		"cc cc tt *a ** *. ** *. *4 cc *2 *. *. *. *. *c tt cc cc "
		"cc cc tt tt *a *. *. ** *c cc *a ** *. *. *c tt tt cc cc "
		"cc cc ta tt tt *a *8 *c bb .. bb *a *8 *c tt tt tc cc cc "
		"cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
		"cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
	)
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_void_temple ; tag
 "Void Temple"   ; name
 s_shrine              ; sprite
 m_void_temple  ; map
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
  (put (kern-mk-obj t_rune_d 1) 9 9)

  (put (kern-mk-obj F_sleep_perm 1)  15 9)
  (put (kern-mk-obj F_fire_perm 1)   14 9)
  (put (kern-mk-obj F_energy_perm 1) 13 9)

  (put (kern-mk-obj F_sleep_perm 1)  3 9)
  (put (kern-mk-obj F_fire_perm 1)   4 9)
  (put (kern-mk-obj F_energy_perm 1) 5 9)

  (put (kern-mk-obj F_sleep_perm 1)  9 3)
  (put (kern-mk-obj F_fire_perm 1)   9 4)
  (put (kern-mk-obj F_energy_perm 1) 9 5)

  (put (kern-mk-obj F_sleep_perm 1)  9 15)
  (put (kern-mk-obj F_fire_perm 1)   9 14)
  (put (kern-mk-obj F_energy_perm 1) 9 13)

  (put (spawn-pt 'wisp) 7 9)
  (put (spawn-pt 'wisp) 9 7)
  (put (spawn-pt 'wisp) 11 9)
  (put (spawn-pt 'wisp) 9 11)

  )

 nil ; hooks
 nil ; edge entrances
 )
