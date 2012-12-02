;; The initial diplomacy table at the start of the game. Each entry defines the
;; attitude of the row to the column. Note that attitudes are not necessarily
;; symmetric. Negative values are hostile, positive are friendly.
;;
;; Note: factions should always be allied with themselves in order for
;; summoning AI to work properly.

(kern-mk-dtable
 ;;      non pla 
 (list   2   0   ) ;; none
 (list   0   2   ) ;; player
)
