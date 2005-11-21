;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(mk-dungeon-room
 'p_mushroom_cave "Mushroom Cave"
 (list
  ;;                               1  1  1  1  1  1 
  ;; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5 
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr";  ;;  0
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr";  ;;  0
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr";  ;;  0
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr";  ;;  0
  " rr %% %% rr rr tt .. .. .. tt rr rr rr {{ rr rr rr rr rr";  ;;  1
  " rr %% rr rr || .. .. .. .. .. || rr rr tt {{ rr rr rr rr";  ;;  2
  " rr %% rr tt %% bb .. .. .. bb %% || rr .. tt rr rr rr rr";  ;;  3
  " rr %% rr %% %% %% %% .. .. %% %% tt rr .. tt rr rr rr rr";  ;;  4
  " rr rr rr tt %% bb %% .. .. bb %% %% rr .. {{ rr rr rr rr";  ;;  5
  " rr .. || || %% %% .. .. .. %% %% %% rr {{ {{ rr rr rr rr";  ;;  6
  " rr .. rr || %% bb .. %% %% bb %% %% rr {{ {{ rr rr rr rr";  ;;  7
  " rr tt rr tt %% .. .. .. %% %% tt tt rr .. {{ rr rr rr rr";  ;;  8
  " rr tt rr %% %% bb .. .. .. bb || || rr .. {{ rr rr rr rr";  ;;  9
  " rr tt rr rr tt %% .. .. .. tt || rr rr .. tt rr rr rr rr";  ;; 10
  " rr .. tt rr rr rr .. .. .. rr rr rr .. .. tt rr rr rr rr";  ;; 11
  " rr .. .. .. && rr rr .. rr rr .. .. .. tt || rr rr rr rr";  ;; 12
  " rr .. .. .. .. .. rr rr rr {{ .. .. tt || rr rr rr rr rr";  ;; 13
  " rr rr tt .. .. rr rr rr rr rr tt tt || rr rr rr rr rr rr";  ;; 14
  " rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr";  ;; 15
  )
 (put (spawn-pt 'yellow-slime) 7 7)
 (put (mk-ladder-up 'p_shard 78 74) 7 12)
 (put (kern-mk-obj t_royal_cape 1) 4 14)
 )
