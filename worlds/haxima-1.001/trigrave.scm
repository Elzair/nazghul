;;----------------------------------------------------------------------------
;; Trigrave
;;
;; This town is at the heart of the starting region. It's a frontier town in
;; the province of a lord who rules from the south, so don't expect anything
;; too fancy. Here the player will find the basic shops and amenities needed to
;; get by.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Map
;;
;; This is the terrain map for the place. It may be altered at run-time so it
;; must be saved and loaded with every session.
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_lusty_juggs 14 10 pal_expanded
 (list
  " x  x  x  x  x  x  x  x  x  x  x  x  x  x "
  " x x!  @ .L .U .S .T .Y x!  x cc cc cc  x "
  "ws cc cc  0 cc cc  0 cc cc  x cc cc cc  x "
  " x cc cc  0 cc cc  0 cc cc x!  x cc  x  x "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc  x "
  "cc cc cc cc cc cc cc cc cc cc cc cc  &  x "
  " x cc cc  0 cc cc  0 cc cc x! cc cc  &  x "
  "ws cc cc  0 cc cc  0 cc cc  x cc cc cc  x "
  " x x!  @ .J .U .G .S  @ x!  x cc cc cc  x "
  " x  x  x  x  x  x  x  x  x  x  x ws  x  x "
  )
)

(kern-mk-map
 'm_johns_irony 7 12 pal_expanded
 (list
  " x  x  x  x  x  x  x "
  " x .J .O .H .N .S  x "
  " x .I .R .O .N .Y  x "
  "ws cc cc cc cc cc  x "
  " x cc cc cc cc cc  x "
  "cc cc cc cc cc __  x "
  " x cc cc cc cc  !  x "
  "ws cc cc cc cc cc  x "
  " x  x cc  x cc  x  x "
  " x cc cc x! cc cc  x "
  " x cc cc  x cc cc  x "
  " x  x  x  x  x  x  x "
  ))

(kern-mk-map 
 'm_quiet_inn 13 10 pal_expanded
 (list
  " x  x  x  x  x  x  x  x  x  x  x  x  x "
  " x cc cc x! .Q .U .I .E .T x! cc cc  x "
  "cc cc cc cc cc cc cc cc cc cc cc cc  x "
  " x  x  x  x  @ .I .N .N  @  x  x  x  x "
  " x cc cc  x cc cc cc cc cc  x cc cc  x "
  " x cc cc cc cc cc cc cc cc cc cc cc  x "
  " x  x  x x! cc cc cc cc cc x!  x  x  x "
  " x cc cc cc cc cc cc cc cc cc cc cc  x "
  " x cc cc  x cc cc cc cc cc  x cc cc  x "
  " x  x  x  x ws  x cc  x ws  x  x  x  x "
  )
 )

(kern-mk-map 
 'm_trigrave 32 32 pal_expanded
 (list
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. ..  b ..  b .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  " b ..  b ..  b .. .. .. .. ..  b ..  b ..  b .. .. .. .. .. .. ..  b ..  b ..  b ..  b ..  b .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  " b ..  b ..  b ..  b ..  b .. .. .. .. .. .. ..  b ..  b ..  b ..  b ..  b ..  b ..  b ..  b .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. ..  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  )
 )

;;----------------------------------------------------------------------------
;; Zones
;;
;; Zones are rectangular areas defined as scheme variables. They can be
;; referred to by NPC schedules and other parts of the script. They cannot be
;; defined in this file, however, and must be kern-loaded, for a couple of
;; reasons:
;;
;; 1. When the kernel saves and reloads a game, the reloaded game will not load
;; this file (this one, right here). The reason is that everything in this file
;; defines an initial game state, and the game will change as it plays. When
;; the kernel saves it will save all of this state as part of a single file.
;;
;; 2. When the kernel saves a game it will not save the zones because it
;; doesn't know about them.
;;
;; 3. The kern-load procedure tells the kernel that when it reloads a game it
;; needs to reload the given file. Think of the zone file as read-only data,
;; whereas this file contains read/write data.
;;----------------------------------------------------------------------------
;;(kern-load "trigrave-zones.scm")

;;----------------------------------------------------------------------------
;; NPCs
;;
;; NPC's are defined in two steps. Step 1 is to kern-load their read-only
;; definition file (this file includes their conversation, schedule,
;; constructor, etc). Step 2 is to call kern-mk-char to instantiate them.
;;----------------------------------------------------------------------------
;(kern-load "ilya.scm")
; (bind 
;  (kern-mk-char 'ch_ilya ; tag
;                "Ilya"              ; name
;                sp_human            ; species
;                nil                 ; occ
;                s_child             ; sprite
;                faction-men         ; starting alignment
;                0 10 5              ; str/int/dex
;                0 0                 ; hp mod/mult
;                0 0                 ; mp mod/mult
;                30 0 9 9            ; hp/xp/mp/lvl
;                'ilya-conv          ; conv
;                sch_ilya           ; sched
;                nil                 ; special ai
;                nil                 ; container
;                nil                 ; readied
;                )
;  (ilya-mk #f #f))

;;----------------------------------------------------------------------------
;; Place
;;
;; The place definition instantiates the place.
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_trigrave     ; tag
 "Trigrave"      ; name
 s_town           ; sprite

 ;; Use a composite map. Blit the buildings over the basic terrain map. This
 ;; makes it easy to move the buildings.
 (kern-blit-map 
  (kern-blit-map 
   (kern-blit-map 
    m_trigrave 17 21 
    m_lusty_juggs 0 0 14 10)
   24 1
   m_johns_irony 0 0 7 12)
  1 1 m_quiet_inn 0 0 13 10)
 
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 nil ; objects
 nil ; hooks
 (list  ;; edge entrances
  (list south 18 0)
  (list north  12 31)
  )
 )
