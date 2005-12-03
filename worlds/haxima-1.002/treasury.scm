(mk-dungeon-room
 'p_treasury2 "Lost Treasury of Luximene"
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx ,T ,R ,E ,A ,S ,U ,R ,Y @@ xx xx xx xx xx "
      "xx xx xx xx xx @@ @@ @@ ,O @@ ,F @@ @@ @@ xx xx xx xx xx "
      "xx xx xx xx xx @@ ,L ,U ,X ,I ,M ,E ,N ,E xx xx xx xx xx "
      "xx xx xx xx xx cc cc cc ,, cc ,, cc cc cc xx xx xx xx xx "
      "xx xx xx xx xx cc pp ,, ,, ,, ,, ,, pp cc xx xx xx xx xx "
      "xx xx xx xx xx cc ,, ,, ,, cc ,, ,, ,, cc xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, cc cc cc ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, cc cc ,, cc cc ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, cc cc cc ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx ,, ,, ,, cc ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-ladder-down 'p_treasury 9 9) 9 9)

 ;; special treasures
 (put (mk-chest 
       'bomb-trap
       (list (list 1 t_eldritch_blade 1)
             (list 1 t_armor_plate_4)
             (list 1 t_iron_helm_4)
             (list 1 t_doom_staff)
             ))
      9 5)
 
 )

(define (can-drop? loc)
  (and (is-floor? loc)
       (loc-is-empty? loc)))

;; piles of gold
(put-random-stuff p_treasury2
                  (mk-rect 5 5 9 9)
                  can-drop?
                  (lambda (loc)
                    (kern-obj-put-at (kern-mk-obj t_gold_coins (kern-dice-roll "5d20")) loc))
                  20)
                  
;; random mundane treasures
(put-random-stuff p_treasury2
                  (mk-rect 5 5 9 9)
                  can-drop?
                  (lambda (loc)
                    (kern-obj-put-at (mk-treasure-chest) loc))
                  5)

;; some gems to add sparkle   
(put-random-stuff p_treasury2
                  (mk-rect 5 5 9 9)
                  can-drop?
                  (lambda (loc)
                    (kern-obj-put-at (kern-mk-obj t_gem 1) loc))
                  10)

;; a couple of corpses
(put-random-stuff p_treasury2
                  (mk-rect 5 5 9 9)
                  can-drop?
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse2 (mk-treasure-list 
                                                  (+ 1
                                                     (modulo (random-next) 
                                                             3))))
                                     loc))
                  3)