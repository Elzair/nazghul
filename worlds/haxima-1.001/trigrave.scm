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
;;
;; This is a "composite" map. The individual building maps are defined
;; separately and then blitted onto the terrain map.
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_lusty_juggs 14 10 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx x! @@ .L .U .S .T .Y x! xx cc cc cc xx "
  "ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx "
  "xx cc cc 00 cc cc 00 cc cc x! xx cc xx xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc && xx "
  "xx cc cc 00 cc cc 00 cc cc x! cc cc && xx "
  "ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx "
  "xx x! @@ .J .U .G .S @@ x! xx cc cc cc xx "
  "xx xx xx xx xx xx xx xx xx xx xx ws xx xx "
  )
)

(kern-mk-map
 'm_iron_works 7 12 pal_expanded
 (list
  "xx xx xx xx xx xx xx "
  "xx .I .R .O .N @@ xx "
  "xx .W .O .R .K .S xx "
  "ws cc cc cc cc cc xx "
  "xx cc cc cc cc cc xx "
  "cc cc cc cc cc __ xx "
  "xx cc cc cc cc !! xx "
  "ws cc cc cc cc cc xx "
  "xx xx cc xx cc xx xx "
  "xx cc cc x! cc cc xx "
  "xx cc cc xx cc cc xx "
  "xx xx xx xx xx xx xx "
  ))

(kern-mk-map 
 'm_quiet_inn 13 10 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx cc cc x! .Q .U .I .E .T x! cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx xx @@ .I .N .N @@ xx xx xx xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx x! cc cc cc cc cc x! xx xx xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx xx xx xx ws xx cc xx ws xx xx xx xx "
  )
 )

(kern-mk-map
 'm_dry_goods 7 10 pal_expanded
 (list
  " xx xx xx xx xx xx xx "
  " xx @@ .D .R .Y @@ xx "
  " xx .G .O .O .D .S xx "
  " cc cc cc cc cc cc ws "
  " xx @@ @@ @@ @@ @@ xx "
  " xx cc cc cc cc cc xx "
  " xx cc cc cc cc cc cc "
  " xx cc cc cc cc cc xx "
  " xx cc cc cc cc cc ws "
  " xx xx xx xx xx xx xx "
  )
 )


(kern-mk-map 
 'm_trigrave 32 32 pal_expanded
 (list
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt .. tt tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt bb .. bb tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt "
  "bb tt bb tt bb .. .. .. .. .. bb tt bb tt bb .. .. .. .. .. .. .. bb tt bb tt bb tt bb tt bb tt "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  "bb tt bb tt bb tt bb tt bb .. .. .. .. .. .. .. bb tt bb tt bb tt bb tt bb tt bb tt bb tt bb tt "
  "tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt .. .. .. .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt bb .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "tt tt tt tt tt tt tt tt tt tt tt .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
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
(kern-load "trigrave-zones.scm")

;;----------------------------------------------------------------------------
;; NPCs
;;
;; NPC's are defined in two steps. Step 1 is to kern-load their read-only
;; definition file (this file includes their conversation, schedule,
;; constructor, etc). Step 2 is to call kern-mk-char to instantiate them.
;;----------------------------------------------------------------------------
(kern-load "jim.scm")
(bind 
 (kern-mk-char 'ch_jim ; tag
               "Jim the Blacksmith" ; name
               sp_human            ; species
               nil                 ; occ
               s_townsman          ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               'jim-conv          ; conv
               sch_jim           ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (jim-mk))

(kern-load "gwen.scm")
(bind 
 (kern-mk-char 'ch_jim ; tag
               "Gwen the Innkeeper" ; name
               sp_human            ; species
               nil                 ; occ
               s_gray_robed_humanoid ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               'gwen-conv          ; conv
               sch_gwen           ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (gwen-mk))

;;----------------------------------------------------------------------------
;; Place
;;
;; The place definition instantiates the place.
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_trigrave     ; tag
 "Trigrave"      ; name
 s_town          ; sprite

 ;; Blit the buildings over the basic terrain map.
 (if #f
     m_trigrave
     (blit-maps m_trigrave
                (list 17 21 m_lusty_juggs 0 0 14 10)
                (list 24 1  m_iron_works 0 0 7  12)
                (list 1  1  m_quiet_inn   0 0 13 10)
                (list 1 21  m_dry_goods   0 0 7  10)))
 
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list ; objects

  ;; characters
  (list ch_jim 15 15)

  )
 nil ; hooks
 (list  ;; edge entrances
  (list south 18 0)
  (list north  12 31)
  )
 )
