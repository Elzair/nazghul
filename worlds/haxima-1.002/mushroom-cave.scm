;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_mushroom_cave 16 16 pal_expanded
 (list
  ;;                               1  1  1  1  1  1 
  ;; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5 
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ";  ;;  0
  " rr %% %% rr rr tt .. .. .. tt rr rr rr {{ rr rr ";  ;;  1
  " rr %% rr rr || .. .. .. .. .. || rr rr tt {{ rr ";  ;;  2
  " rr %% rr tt %% bb .. .. .. bb %% || rr .. tt rr ";  ;;  3
  " rr %% rr %% %% %% %% .. .. %% %% tt rr .. tt rr ";  ;;  4
  " rr rr rr tt %% bb %% .. .. bb %% %% rr .. {{ rr ";  ;;  5
  " rr .. || || %% %% .. .. .. %% %% %% rr {{ {{ rr ";  ;;  6
  " rr .. rr || %% bb .. %% %% bb %% %% rr {{ {{ rr ";  ;;  7
  " rr tt rr tt %% .. .. .. %% %% tt tt rr .. {{ rr ";  ;;  8
  " rr tt rr %% %% bb .. .. .. bb || || rr .. {{ rr ";  ;;  9
  " rr tt rr rr tt %% .. .. .. tt || rr rr .. tt rr ";  ;; 10
  " rr .. tt rr rr rr .. .. .. rr rr rr .. .. tt rr ";  ;; 11
  " rr .. .. .. && rr rr .. rr rr .. .. .. tt || rr ";  ;; 12
  " rr .. .. .. .. .. rr rr rr {{ .. .. tt || rr rr ";  ;; 13
  " rr rr tt .. .. rr rr rr rr rr tt tt || rr rr rr ";  ;; 14
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ";  ;; 15
  ))


;;----------------------------------------------------------------------------
;; Yellow slime generator
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_mushroom_cave "Mushroom Cave" s_dungeon 
 m_mushroom_cave
 #f  ;; is-wrapping?
 #t  ;; is-underground?
 #f  ;; is-wilderness? 
 #f  ;; is-tmp-combat-place?
 nil ;; subplaces
 nil ;; neighbors

 ;; objects
 (list
  (put (mk-step-gen 600 
                    1 
                    'is-yellow-slime? 
                    'mk-yellow-slime-verbose 
                    (list "A yellow slime rises from the ooze!")
                    (list 'p_mushroom_cave 7 7)) 
       7 
       9)
  (put (mk-ladder-up 'p_shard 78 74) 7 12)
  (put (kern-mk-obj t_royal_cape 1) 4 14)
  )

 nil ;; hooks
 nil ;; edge entrances
)
