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
 'm_quiet_inn 13 11 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx cc cc x! .G .R .A .Y @@ x! cc cc xx "
  "xx cc cc x! @@ .D .O .V .E x! cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx x! @@ .I .N .N @@ x! xx xx xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx x! cc cc cc cc cc x! xx xx xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx xx xx xx ws x! cc x! ws xx xx xx xx "
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
               "Jim"                ; name
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
 (kern-mk-char 'ch_gwen ; tag
               "Gwen"               ; name
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

(kern-load "chanticleer.scm")
(bind 
 (kern-mk-char 'ch_chant ; tag
               "Chanticleer"          ; name
               sp_human            ; species
               nil                 ; occ
               s_townsman          ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               'chant-conv         ; conv
               sch_chant           ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (chant-mk))

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
                (list 24 1  m_iron_works  0 0 7  12)
                (list 1  1  m_quiet_inn   0 0 13 11)
                (list 1 21  m_dry_goods   0 0 7  10)))
 
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list ; objects

  ;; characters
  (list ch_jim  15 15)
  (list ch_gwen 15 15  )
  (list ch_chant 15 15)

  ;; Inn
  (list (kern-tag 'trigrave-inn-room-1-door (mk-locked-door))  4 7)
  (list (kern-tag 'trigrave-inn-room-2-door (mk-locked-door))  4 9)
  (list (kern-tag 'trigrave-inn-room-3-door (mk-locked-door)) 10 9)
  (list (kern-tag 'trigrave-inn-room-4-door (mk-locked-door)) 10 7)
  (list (mk-locked-door) 1 4)
  (list (mk-door)  4  4)
  (list (mk-door) 10  4)
  (list (mk-door)  7 11)
  (list (mk-bed)   2  6)
  (list (mk-bed)  12  6)
  (list (mk-bed)   2  9)
  (list (mk-bed)  12  9)
  (list (mk-bed)  12  2)

  ;; General store
  (list (mk-locked-door)  1 24)
  (list (mk-door)         7 27)
  
  ;; Iron Works
  (list (mk-door) 24 6)
  (list (mk-locked-door) 26 9)
  (list (kern-tag 'tiw-portcullis (mk-portcullis)) 28 9)
  (list (mk-lever 'tiw-portcullis) 25 10)

  ;; Lusty Juggs (tavern)
  (list (mk-door) 28 24)
  (list (kern-tag 'tlj-d-1 (mk-connected-door 'tlj-d-2)) 17 25)
  (list (kern-tag 'tlj-d-2 (mk-connected-door 'tlj-d-1)) 17 26)

  )
 (list 'trigrave-entry) ;; hooks
 (list  ;; edge entrances
  (list south 18 0)
  (list north  12 31)
  )
 )

;; ----------------------------------------------------------------------------
;; The entry hooks must be kern-loaded from a separate file, since they are
;; read-only and not saved with the session.
;; ----------------------------------------------------------------------------
(kern-load "trigrave-entry.scm")
