;;----------------------------------------------------------------------------
;; test/player.scm - basic player setup
;;
;; This will create a basic player character and party for testing. Test
;; scripts can customize it by adding party member and equipment afterwards.
;; ----------------------------------------------------------------------------

(kern-mk-char 
 'ch_wanderer   ; tag
 "The Wanderer" ; name
 sp_human       ; species
 oc_wanderer    ; occupation
 s_wanderer     ; sprite
 faction-player ; starting alignment
 6 6 6          ; starting str/int/dex
 pc-hp-off      ; base max hit points
 pc-hp-gain     ; max hit points gained per level
 pc-mp-off      ; base mana points
 pc-mp-gain     ; mana points gained per level
 max-health     ; max hit points (kernel will trim based on level)
 0              ; hit points (kernel will set to max shortly)
 max-health     ; max mana points (kernel will trim based on level)
 0              ; mana points (kernel will set to max shortly)
 1              ; character level
 #f             ; dead?
 nil            ; conversation proc
 nil            ; schedule
 nil            ; special ai
 nil            ; personal inventory
 nil            ; readied armaments
 )

;;----------------------------------------------------------------------------
;; Player Party
;;----------------------------------------------------------------------------


(bind 
 (kern-mk-player
  'player                     ; tag
  s_wanderer         ; sprite
  "Walk"                      ; movement description
  sound-walking               ; movement sound
  1                           ; food
  0                           ; gold
  (* 60 60 5)                 ; turns to next meal (5 hours)
  nil                         ; formation
  m_campsite                  ; campsite map
  nil                         ; campsite formation
  nil                         ; vehicle
  ;; inventory
  (kern-mk-inventory nil)
  nil ;; party members (should be nil for initial load file)
  )
 (tbl-mk))


;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)
