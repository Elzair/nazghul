;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")

;;----------------------------------------------------------------------------
;; Load the read-only game data. See the note on 'kern-load' vs 'load' above.
;;----------------------------------------------------------------------------

(kern-load "game.scm")

(kern-mk-map 
 'm_campsite 7 7 pal_expanded
 (list
  "b  b  .. .. .. b  b "
  "b  .. .. .. .. .. b "
  ".. .. .. .. .. .. .."
  ".. .. .. &  .. .. .."
  ".. .. .. .. .. .. .."
  "b  .. .. .. .. .. b "
  "b  b  .. .. .. b  b "  
  ))

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

(bind 
 (kern-mk-char 'ch_shroom ; tag
               "Shroom"              ; name
               sp_human              ; species
               oc_druid              ; occ
               s_companion_druid     ; sprite
               align-town            ; starting alignment
               0 10 0                ; str/int/dex
               0 0                   ; hp mod/mult
               0 0                   ; mp mod/mult
               0 0                   ; hit mod def mod
               0 0                   ; dam mod arm mod
               30 0 150 9            ; hp/xp/mp/lvl
               'shroom-conv          ; conv
               sch_shroom            ; sched
               nil                   ; special ai
               (list t_dagger))      ; readied
 (shroom-mk #f #f))

(kern-mk-char 'ch_olin ; tag
              "Olin the Ghast"      ; name
              sp_ghast              ; species
              nil                   ; occ
              s_ghost               ; sprite
              align-town            ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 240 8           ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil)                  ; readied

(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              align-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 240 8           ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              (list t_rpg))         ; readied

(kern-mk-char 'ch_slurmok ; tag
              "Slurmok"             ; name
              sp_yellow_slime       ; species
              oc_wizard             ; occ
              s_yellow_slime        ; sprite
              align-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 240 8           ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              (list t_dagger))      ; readied

(kern-obj-add-effect ch_slurmok ef_poison_immunity nil)

;;----------------------------------------------------------------------------
;; Player
;;----------------------------------------------------------------------------
(kern-mk-player
 'player                     ; tag
 s_companion_fighter         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 1000                        ; food
 500                         ; gold
 (+ align-player align-town) ; alignment
 nil                         ; formation
 m_campsite                  ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle

 
 ;; inventory
 (kern-mk-container
  nil ;; type
  nil ;; trap
  ;; contents:
  (list
   
   ;; spells
   (list 10 test_recur)
   (list 10 vas_rel_por)
   (list 2 an_xen_corp)
   (list 2 kal_xen)
   (list 2 in_nox_por)
   (list 2 an_sanct)
   (list 2 sanct)
   (list 2 an_xen_exe)
   (list 1 in_zu)
   (list 10 in_lor)
   (list 3 vas_lor)
   (list 3 in_sanct)
   (list 1 sanct_lor)
   (list 1 in_flam_hur)
   (list 1 in_vas_por_ylem)

   (list 1 t_dagger)
   (list 100 t_arrow)
   (list 25 t_oil)
   (list 10 garlic)
   (list 10 ginseng)
   (list 10 spider_silk)
   (list 10 black_pearl)
   (list 10 sulphorous_ash)
   (list 10 blood_moss)
   (list 10 nightshade)
   (list 10 mandrake)
   (list 1 t_poison_immunity_potion)

   (list 1 the-goblin-lexicon)
   ))
 
 ;; party members
 (list ch_thorald_greybeard
       ch_slurmok
       )
 )

;;----------------------------------------------------------------------------
;; Maps
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_dark_passage 32 32 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx"
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ ~~ xx xx xx xx xx"
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx"
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ ~~ ~~ xx xx xx xx xx xx"
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx xx xx"
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ ~~ ~~ ~~ xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~ xx xx xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~ ~~ ~~ xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx ,, ,, ,, xx xx ,, ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ~~ ,, ,, ,, xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ~~ ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ,, ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ,, ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ~~ ~~ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx xx ~~ ~~ xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx xx ~~ xx xx xx xx .. .. .. .. .. .. .. xx xx xx xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx ~~ ~~ xx xx xx .. .. .. .. .. .. .. .. .. xx xx xx xx ,, ,, ,, xx xx xx"
  "xx xx xx xx xx xx xx xx ~~ xx xx xx xx .. .. .. .. .. .. .. .. .. xx xx xx xx ,, ,, ,, ,, ,, xx"
  "xx xx xx xx xx xx xx ~~ ~~ xx xx xx xx .. .. .. .. .. .. .. .. .. xx xx xx xx ,, ,, ,, ,, ,, xx"
  "xx xx xx xx xx xx xx ~~ ~~ xx xx xx xx ~~ .. .. .. .. .. .. .. .. xx xx xx xx ,, ,, ,, ,, ,, xx"
  "xx xx xx xx xx xx xx xx ~~ ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. .. .. xx xx xx xx ,, ,, ,, ,, ,, xx"
  "xx xx xx xx xx xx xx xx xx xx xx xx xx ~~ .. .. .. .. .. .. .. xx xx xx xx xx ,, ,, ,, ,, ,, xx"
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx"
  ))

(kern-mk-map
 'm_green_tower 64 64 pal_expanded
 (list
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || ||  r  r  r  r  r  r  r || || || || || || || || || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| ||  r  r  r  r  r || || ||  r  r  r  & cc cc cc cc  r || || || || || || || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || ||  x  x  x  x  x  x  x  x  x tt tt || || || || || "
  "|| ||  r cc cc cc  r || || ||  r cc  r cc cc cc cc cc  r || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || ||  x .. .. .. .. .. .. ..  x tt tt tt || || || || "
  "|| ||  r cc cc cc  r || || ||  r cc cc cc cc cc cc cc  r || || || || || || || || || tt tt  b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
  "|| ||  r cc cc cc  r || || ||  r cc  r cc cc cc  r  r  r || || || || || || || || tt tt tt tt .. /7 ..  b tt || || || || || || || || || || /7 tt  x  @  @  @  @  @  @  @  x tt tt tt || || || || "
  "|| ||  r  r cc  r  r || || ||  r cc  r cc cc cc cc cc  r || || || || || || || || tt tt tt  b .. /7 .. tt tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. ..  x tt tt || || || || || "
  "|| || || || /7 || || || || || || /7  r  r  r  r  r  r  r || || || || || || || tt tt tt tt tt .. /7 .. tt tt || || || || || || || || || || /7 tt  x .S .H .R .O .O .M .S  x || || || || || || || "
  "|| || || || /8 /d /d /d /1 /d /d /a || || || || || || || || || || || || || || tt tt tt tt  b .. /7 ..  b tt || || || || || || || || || tt /7 tt  x  x  x  x  x  x  x  x  x || || || || || || || "
  "|| || || || || || || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt tt .. /7 .. tt tt || || || || || || || || tt tt /7 tt  x .. .. ..  x .. .. ..  x || || || || || || || "
  "|| || ||  r  r  r || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt  b .. /7 ..  b tt || || || || || || || || tt tt /8 /d .. .. .. ..  ? .. .. ..  ? tt tt tt tt tt tt tt "
  "||  r  r  r cc  r  r || /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt tt .. /7 .. tt tt || || || || tt tt tt tt tt tt tt tt  x .. .. tt  x .. .. ..  x || || || || || || || "
  "||  r cc cc cc cc  r  r /7 || || || || || || || || || || /7 || || || || tt tt tt tt tt tt  b .. /7 ..  b tt || || || || tt .. tt tt tt || || ||  x  x ws  x  x  x ..  x  x || || || || || || || "
  "||  r cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt tt tt tt .. .. tt || || || || || || || || || || || || || || || || || || || || "
  "||  r  r cc cc cc  r  r || || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt  b .. /7 ..  b tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
  "|| ||  r  r  &  r  r || tt || || || tt tt  b || || tt || /7 || tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| || ||  r  r  r || tt tt tt || tt tt .. tt || || tt  b /7  b ..  b tt  b tt  b tt  b tt .. .. /7 .. .. tt  b tt  b tt tt  b tt  b tt  b tt tt || || || || || || || || || || || || || || || || "
  "|| || || tt || || || || tt || tt tt .. .. .. tt || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt tt tt || || || || || || tt tt .. tt || ||  b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
  "|| || || tt || || || || || tt tt tt tt tt tt || || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || tt tt tt tt tt tt tt || || ||  b .. /7 .. .. ..  b  d  b  d  b  d .. .. .. /7 .. .. ..  b tt tt  b tt  b tt .. .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || || tt tt tt .. tt tt tt || || || || tt .. /7 .. ..  d  d  d  d  d  d  d  b .. .. /7 .. ..  b tt tt tt tt tt tt  b tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || ||  b tt tt .. .. .. tt tt tt || || ||  b .. /7 ..  b  d || || || || ||  d  x w+  x cc  x w+  x tt || || || || || tt tt .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt .. .. .. .. .. tt tt || || || || .. /7 ..  d  d || || || ||  x w+  x cc cc cc cc cc  x  x  x || || || || tt  b .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt tt .. .. .. tt tt tt || || ||  b .. /7 ..  b  d || ||  x  x  x cc cc cc cc cc cc cc cc cc  x  x  x || || tt tt .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt tt tt .. tt tt tt || || || || || .. /7 ..  d  d ||  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x || tt  b .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt tt tt tt || tt tt tt tt  b || || tt tt ||  b .. /7 ..  b  d ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt || || || || || tt tt tt || || || || || || tt .. /7 ..  d  d  x  x cc cc cc  x x!  x  x cc  x  x x!  x cc cc cc  x  x tt  b .. /7 ..  b tt || || || || || || || || || || || || || || tt "
  "tt tt tt || || || || || || || || || || || || || tt  b .. /7 ..  b  d  x cc cc cc  x  x || tt .. cc .. tt ||  x  x cc cc cc  x tt tt .. /7 .. tt tt || || || || || || || || || || || || || || tt "
  " b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. /7 ..  d  b  x cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc  x  b .. .. /7 ..  b tt || || || || || || || || || || || || || || tt "
  ".. ..  b tt  b tt  b tt  b tt  b tt  b tt  b  b .. .. .. /7 .. .. .. w+ cc cc cc  x tt ..  b .. cc ..  b .. tt  x cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. ..  x cc cc cc  x .. .. .. .. cc .. .. .. ..  x cc cc cc  x .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. ..  x cc cc cc  x .. .. .. .. cc .. .. .. ..  x cc cc cc  x .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. ..  b tt  b tt  b tt  b tt  b tt  b tt tt tt  b .. .. /7 .. .. .. w+ cc cc cc  x tt ..  b .. cc ..  b .. tt  x cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  " b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. ..  b  x cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc  x  b .. .. /7 ..  b tt || || || || || || || || || || || tt tt tt tt "
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tt .. /7 .. .. tt  x cc cc cc  x  x || tt .. cc .. tt ||  x  x cc cc cc  x tt .. .. /7 .. tt tt || || || || || || || || || || || || || || tt "
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt  b .. /7 ..  b tt  x  x cc cc cc  x x!  x  x cc  x  x x!  x cc cc cc  x  x tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || || || tt tt tt tt tt tt tt || || tt tt .. /7 .. tt tt ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || tt tt tt tt tt tt || || tt  b .. /7 ..  b tt ||  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt tt .. /7 .. tt tt || ||  x  x  x cc cc cc cc cc cc cc cc cc  x  x  x || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt || || || ||  x w+  x cc cc cc cc cc  x  x  x || || || || tt  b .. /7 ..  b tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || ||  r  r tt tt .. /7 .. tt tt || || || || || tt  x w+  x cc  x w+  x tt || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt  b .. /7 ..  b tt tt tt tt tt tt tt  b .. .. /7 .. ..  b tt tt tt tt tt tt tt  b .. /7 ..  b tt tt tt tt tt tt tt || || || || || || || || || "
  "|| || || || || || || tt tt tt tt || || || || || tt tt .. /7 .. .. tt  b tt  b tt  b .. .. .. .. /7 .. .. .. ..  b tt  b tt  b tt .. .. /7 .. .. tt  b tt  b tt  b tt || || || || || || || || || "
  "|| || || || || tt  r tt  r  r tt  r  r || || || tt  b .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || || || || || || || "
  "|| || ||  r  r tt tt tt || || tt tt  r  r  r || tt tt .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 ..  b tt || || || || || || || || || "
  "|| || ||  r || || || tt tt tt || tt tt ||  r || ||  b .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || "
  "|| || || || tt tt tt tt .. .. tt || tt tt ||  r  r || || tt  b tt  b tt  b tt  b tt  b tt b  .. /7 .. tt  b tt  b tt  b tt  b tt  b tt  b tt  b tt tt  b /4 /2  b tt || || || || || || || || || "
  "||  r  r || tt tt || .. ..  b .. tt tt tt tt ||  r  r || tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt tt tt tt tt tt tt tt tt tt  x  x  x  x ws  x cc cc  x ws  x  x  x  x || || || || || "
  "|| || || || tt tt .. .. .. .. .. tt .. tt tt || ||  r || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc  x || || || || || "
  "|| tt tt tt tt .. .. .. .. .. .. .. .. .. tt tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc  x || || || || || "
  "|| tt tt tt ..  b .. .. .. .. .. .. ..  b .. tt .. tt tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc  0 cc cc cc cc cc cc  0 cc cc  x || || || || || "
  "|| || || tt .. .. .. .. ..  a .. .. .. .. .. tt ||  r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || ||  x cc cc  0 cc cc  &  & cc cc  0 cc cc  x || || || || || "
  "||  r || tt tt tt tt .. .. .. .. .. .. tt tt || ||  r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || ||  x  x  x cc cc  0 cc cc cc cc cc cc  0 cc cc  x  x  x || || || "
  "||  r || || || .. tt .. .. .. .. .. .. tt tt ||  r || || ||  r  r tt  r  r || || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
  "||  r  r || tt tt tt  b .. .. ..  b .. tt tt ||  r || ||  r  r tt tt tt  r  r || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
  "|| ||  r || tt tt tt tt tt .. .. .. .. tt ||  r  r || ||  r tt tt || tt ||  r || || || tt tt .. /7 .. tt || || || || || || || ||  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || || "
  "|| || || || || || || || tt tt tt tt tt || tt  r || || ||  r tt || || || ||  r || || || tt tt .. /7 .. || || || || || || || ||  x  x cc  x  x  @  @  @  @  @  @  @  @  @  @  x  x cc  x  x || || "
  "|| || ||  r  r || || || tt tt tt || tt tt ||  r || || ||  r tt tt || || ||  r || || || tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x || || "
  "|| || || ||  r  r ||  r  r  r ||  r  r  r  r || || || ||  r  r || || || tt  r || || || tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x .W .H .I .T .E  @ .S .T .A .G  x cc cc cc  x || || "
  "|| || || || || || || || || || || || || || || || || || || ||  r  r  r  r tt || || || tt tt tt .. /7 .. tt || || || || || || ||  x cc cc cc  x  x  x  x  x  x  x  x  x  x  x  x cc cc cc  x || || "
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || ||  x  x  x  x  x || || || || || || || || || ||  x  x  x  x  x || || "
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "))

(kern-mk-map
 'm_pit_of_death 8 8 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx"
  "xx !  !  !  !  !  !  xx"
  "xx !  !  !  !  !  !  xx"
  "xx !  !  !  !  !  !  xx"
  "xx !  !  !  !  !  !  xx"
  "xx !  !  !  !  !  !  xx"
  "xx !  !  !  !  !  !  xx"
  "xx xx xx xx xx xx xx xx"
))

(kern-mk-map
 'm_green_tower_lower 64 64 pal_expanded
 (list
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ || || || ||  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || || ||  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || ||  r  r  r  r  r  r  r  x  x  x  x x!  x  x x!  x  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x tt  x tt tt || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ || || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. ..  x tt tt tt || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ || || ||  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. tt tt tt tt || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r  r  r  @ tt  @ tt  x tt tt tt || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x  x  x x! cc cc cc cc x!  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. ..  x tt tt || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r  r tt tt tt tt tt tt  x || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ ~~ || ||  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x tt  x  x || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ || ||  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r  r  r  r  r .. .. ..  x .. .. ..  x || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ || ||  r  r  r  r  r  r  r  x  x  x x! cc cc cc cc x!  x  x  x  r  r  r  r  r  r  r tt tt .. .. .. ..  ? .. .. ..  ? tt tt tt tt tt tt tt "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. ~~ ~~ ~~  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x  r  r  r  r  r  r || || tt  x .. .. ..  x .. .. ..  x || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || ~~  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc  ? ||  r  r  r  r  r  x  x tt  x  x tt  x  x  x ..  x  x || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || || ~~  r  r  r  r  r  r  r  x cc cc  x cc cc cc cc  x cc cc  x tt  r ~~ ~~ ~~ ~~ ~~ || tt || || || || || || || || || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || || ~~ ~~  r  r  r  r  r  r  r  x  x  x  x x! cc cc x!  x  x  x  x  x  x ~~  x  x -- ~~ ~~ .. || || || || || || || || || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || || ~~ || .. || tt ~~ ~~ ~~ .. .. .. ..  x cc cc cc cc  x ~~ ~~ ~~ -- __ __ __  x -- -- ~~ ~~ || || || || || || || || || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ ~~ || .. || tt ~~ tt ~~ .. .. ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ -- __ __ __  x __ -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ || || || ~~ ~~ ~~ ~~ || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ tt  b ..  b .. ~~ tt ~~ .. .. ~~ tt  x cc cc cc cc  x cc cc ~~ -- __ __ __  x __ -- ~~ ~~ || || || || || || ~~ || || || ~~ || || ~~ ~~ || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r ~~ ~~ .. .. .. .. ~~ .. ~~ ~~ ~~ ~~ tt  x x! cc cc x!  x cc cc ~~ -- -- -- --  x -- ~~ ~~ tt tt || || || || || || || || || ~~ || || tt ~~ ~~ "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r || ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. ..  x cc cc cc cc  x cc cc ~~ ~~ ~~ ~~ ~~  x ~~ ~~ .. || tt || || || || || ~~ ~~ ~~ ~~ ~~ || tt tt tt || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r .. .. .. .. .. .. .. ..  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc  x .. .. .. tt tt || || || || || || || || || || || || tt || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc  x cc cc cc cc cc cc cc x! .. .. ..  b tt || || || || || || tt || || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x x! cc cc x!  x ,C ,I ,S ,T ,E ,R ,N  x .. .. .. tt tt || tt || || || tt tt tt || || || || || || || "
  " r  r  r  r  r  r  r  r  r  r  r  x  x  x  x  x  x  x  x  x  x  r  r  r  r  r  r  x  x  x  x cc cc cc cc  x  x  x  x  x  x  x  x  x .. .. ..  b tt tt tt tt || || || tt || || ||  r ||  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x ,B ,A ,R ,R ,A ,C ,K ,S  x  r  r  r  r  x  x  x cc cc  x cc cc cc cc  x cc cc  x  x  x cc cc  ? .. .. .. tt tt || tt || || || || || || || ||  r tt tt cc  r "
  " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  r  x  x  x cc cc cc cc  x cc cc cc cc  x cc cc cc cc  x  x  x  x .. .. ..  b tt || || tt  r  r  r  r  r  r  r  r cc cc cc  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  r  x cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc  x  x .. .. .. tt tt || || tt  r cc cc cc cc cc cc cc cc cc cc  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x  x .. .. .. tt tt || || tt  r cc  r  r  r  r  r  r cc cc cc  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x  x x!  x  x  x x!  x  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x ,C ,O ,U ,N ,C ,I ,L  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x  x  x cc  x  x  x x! cc cc cc cc x!  x  x  x cc  x  x  x  x  x cc cc cc cc cc cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x  x x!  x cc cc  x x!  x  x  x x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  0  0  0 cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  0  0  0 cc cc  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x  x x!  x cc cc  x x!  x  x  x x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x  x  x  x cc  x  x  x x! cc cc cc cc x!  x  x  x cc  x  x  x  x  x cc cc cc cc cc cc &   x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  x  x  x x!  x  x  x x! xx  x cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  x cc cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc cc  ? cc cc cc cc cc cc cc cc cc cc  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r x! cc cc cc cc cc cc cc cc x!  r  x  x cc cc cc cc cc cc  x cc cc cc cc  x cc cc cc cc cc cc  x  x  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc  x  r  r  x cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  x  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x ,T ,R ,A ,I ,N ,I ,N ,G  x  r  r  x  x  x cc cc cc cc  x cc cc cc cc  x cc cc cc cc  x  x  x  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  x  x  x  x  x  x  x  x  x  x  r  r  r  r  x  x  x cc cc  x cc cc cc cc  x cc cc  x  x  x  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x cc cc cc cc  x  x  x  x  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x  x  x  x x! cc cc x!  x  x  x  x  x  x  x  x  x  x  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc  x  [  @  @  ]  x  r  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc  x  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r ..  r  r  r  r  r  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  x cc cc  0 cc cc  0 cc cc  0 cc cc cc cc cc cc cc  &  x  r  r  r  r  r  r cc  r  r  r  r  r  r  r  r  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc  &  x  r  r  r  r  r  r cc  r ,C ,R ,Y ,P ,T  r  r  r  r  r "
  " r ..  p ..  p ..  p ..  p ..  p ..  p ..  p ..  p .. ..  r  r  r  r  r  r  r  r  x cc cc cc cc cc cc cc cc cc cc cc  x cc cc cc cc  x  x  r  r  r  r  r cc cc cc cc cc cc cc cc cc  r  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  x ,D ,I ,N ,I ,N ,G  @ ,H ,A ,L ,L  x  [  @  @  ]  x  r  r  r  r  r  r cc  r  r cc cc cc cc cc cc cc  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  x  x x!  x  x  x x!  x  x  x x!  x  x  x  x  x  x  x  r  r  r  r  r  r cc  r cc cc cc cc cc cc cc  r  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r cc cc cc  r cc cc cc  r  r  r  r "
  " r ..  p ..  p ..  p ..  p ..  p ..  p ..  p ..  p .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
  " r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r cc cc cc  r cc cc cc  r  r  r  r "
  " r ..  r  r  r  r  r  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r cc  r  r  r cc  r  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc  r cc cc cc  r cc cc cc  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r cc  r  r  r  r  r "
  " r .. .. .. .. .. ..  r .. .. .. .. .. .. .. .. .. .. ..  r .. .. .. .. ..  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc cc cc  r cc cc cc  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r cc  r  r  r cc  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "
  " r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r  r "))

(kern-mk-map 
 'm_wilderness 40 40 pal_expanded
 (list
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- .. .. .. .. .. .. .. .. .. -- -- -- -- -- __ __ __ "
  "__ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt tt .. {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ tt tt .. .. .. %% {{ -- __ __ __ "
  "__ __ __ -- -- -- -- -- -- .. .. .. tt tt tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. .. -- __ __ __ "
  "__ __ __ -- .. .. .. .. tt tt !  tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. .. -- -- .. __ "
  "__ __ __ -- .. .. tt tt tt || {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt .. .. .. .. .. -- .. .. "
  ".. __ __ -- .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ {{ {{ {{ tt .. .. .. .. -- .. .. "
  ".. __ __ -- .. tt tt {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ tt tt tt {{ {{ {{ {{ {{ {{ {{ tt .. /0 /d /d -- /d /a "
  ".. __ __ -- .. .. tt tt {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ tt tt tt .. .. .. .. .. .. .. .. .. .. /6 .. .. -- .. .. "
  ".. -- -- -- .. .. .. .. tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ tt tt tt tt .. .. /7 .. .. -- -- .. "
  "__ -- .. .. .. .. .. .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {{ {{ {{ {{ tt .. .. /7 .. .. .. -- .. "
  "__ -- .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. /7 .. .. .. -- __ "
  "__ -- .. .. .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. /7 .. .. .. -- __ "
  "__ -- .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. /7 .. .. -- -- __ "
  "__ -- .. .. .. .. {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. .. .. /7 .. -- -- __ __ "
  "-- -- .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt tt tt tt .. .. .. .. tt tt tt /7 tt -- -- __ __ "
  "-- -- tt tt tt tt .. tt tt tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. tt tt tt tt || || /7 || || -- -- -- "
  "|| ~~ || || tt tt .. .. tt tt tt tt .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. tt tt || || || || || /8 /2 || || ~~ tt "
  "|| ~~ ~~ || || tt .. .. tt tt .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt .. .. tt || || || || || || || /7 || || ~~ ~~ "
  ".. .. ~~ .. .. .. .. tt tt tt tt tt tt tt .. .. .. {{ {{ ^^ {{ {{ {{ .. .. .. .. tt || || || b  tt b  tt /b .. || || ~~ "
  ".. || ~~ ~~ || || tt tt tt tt ~~ ~~ ~~ -- -- -- .. .. {{ {{ {{ -- -- -- .. .. .. tt tt || || tt tt .. .. .. .. || ~~ ~~ "
  ".. || || ~~ tt tt tt tt .. %% %% ~~ ~~ -- __ -- -- .. .. .. .. .. -- -- .. .. .. .. tt || || b  tt b  tt .. .. ~~ ~~ .. "
  "|| || || ~~ tt tt tt .. .. .. %% %% ~~ -- __ __ -- -- -- -- .. .. -- -- -- .. .. .. tt || || || || || || .. .. ~~ .. .. "
  "|| || || ~~ tt tt .. .. .. %% .. %% %% -- __ __ __ __ __ -- -- -- -- -- .. .. .. .. tt tt tt || || || || || -- -- -- || "
  "|| || || ~~ .. .. .. %% %% .. %% %% ~~ -- __ __ __ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt -- -- -- -- __ -- -- "
  "-- || -- -- ~~ ~~ %% %% ~~ %% %% ~~ -- -- __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ -- "
  "-- -- -- __ __ -- -- -- ~~ ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ -- -- -- %% %% %% %% -- -- __ __ ^^ ^^ ^^ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ -- -- -- .. .. .. -- -- -- -- -- -- ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ -- -- -- ^^ {{ ^^ {{ {{ {{ ^^ -- ^^ ^^ ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ -- -- {{ {{ {{ ^^ {{ ^^ -- {{ {{ ^^ ^^ -- -- __ __ __ __ __ -- ^^ ^^ ^^ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ ^^ -- ^^ ^^ ^^ ^^ -- -- __ __ __ __ __ -- -- !  ^^ ^^ ^^ ^^ -- __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !  !  !  ^^ !  -- -- __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !  !  !  ^^ -- __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !  !  ^^ ^^ ^^ ^^ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !  ^^ ^^ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ __ __ "
  ))

(load "gregors-hut.scm")

;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(kern-mk-place 'p_dark_passage    ; tag
               "A Dark Passage"   ; name
               s_dungeon          ; sprite
               m_dark_passage     ; map
               #f                 ; wraps
               #t                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               nil ; objects
               nil ; hooks
               )

(kern-mk-place 'p_pit_of_death
               "Pit of Death"
               s_dungeon
               m_pit_of_death
               #f
               #t
               #f
               #f
               nil
               nil
               nil
               (list 'pit-of-death-pre-entry-hook)
               )

(kern-mk-place 'p_green_tower_lower
               "Beneath GreenTower"
               nil ; sprite
               m_green_tower_lower
               #f  ; wraps
               #t  ; underground
               #f  ; wilderness
               #f  ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               ;; objects:
               (list
                (list ch_olin 59 32)
                (list (mk-ladder-up 'p_green_tower 32 32) 32 32)
                ) ;; end of objects
               nil ; hooks
               )

(kern-mk-place 'p_green_tower
               "Town of GreenTower"
               s_keep          ; sprite
               m_green_tower
               #f  ; wraps
               #f  ; underground
               #f  ; wilderness
               #f  ; tmp combat place
               nil ; subplaces

               ;; neighbors
               (list
                (list p_green_tower_lower down)
                )

               ;;
               ;; Objects
               ;;
               (list 

                ;; Tower
                (list (kern-tag 'portcullis-1 (mk-portcullis)) 37 32)
                (list (mk-door) 41 32)
                (list (mk-door) 23 32)
                (list (mk-door) 32 22)
                (list (mk-door) 32 42)
                (list (mk-lever 'portcullis-1) 36 31)
                (list (mk-ladder-down 'p_green_tower_lower 32 32) 32 32)

                ;; White Stag
                (list (kern-tag 'ws-d-1 (mk-connected-door 'ws-d-2)) 51 49)
                (list (kern-tag 'ws-d-2 (mk-connected-door 'ws-d-1)) 52 49)

                ;; Shroom's
                (list (mk-door) 48  4)
                (list (mk-door) 48  6)
                (list (mk-door) 48  10)
                (list (mk-locked-door) 54  12)
                (list (mk-trap-door 'p_green_tower_lower 54 11) 54 11)
                (list ch_shroom 61 31)

                ;; Huts
                (list (mk-door) 11 6)
                (list (mk-door)  4 6)
                (list (mk-door)  7 13)

                ;; Misc
                (list (kern-mk-obj t_dagger 1) 63 32)
                (list (kern-mk-obj t_green_potion 2) 63 32)
                (list (kern-mk-obj t_mushroom 1) 63 32)
                (list (kern-tag 'gt-b-1 
                                (mk-tblitter 'p_green_tower 60 30 5 5 
                                             'm_campsite)) 62 32)
                (list (mk-lever 'gt-b-1) 63 31)
                (list (kern-tag 'mg-3 (mk-moongate 'ord)) 60 32)
                (list (kern-tag 'mg-4 (mk-moongate 'ord)) 20 32)
                (list (kern-mk-obj F_sleep_perm 1) 63 33)
                (list (mk-teleporter 'p_pit_of_death 5 5) 60 32)
                )
               (list 'green-tower-pre-entry-hook)
               )

(kern-mk-place 'p_wilderness
               "The Great Wild"
               nil          ; sprite
               m_wilderness
               #t
               #f
               #t
               #f                 ; tmp combat place
               (list
                (list p_green_tower 35 23)
                (list p_gregors_hut 3 17)
                )
               nil ; neighbors
               (list
                (list player 3  18)
                (list (kern-mk-party t_skeleton_brigade align-monster nil) 25 19)
                (list (kern-mk-party t_slime_glob align-monster nil) 36 20)
                (list (kern-mk-obj F_fire 1) 34 23)
                (list (kern-mk-obj F_poison 1) 34 22)
                (list (kern-mk-obj F_sleep_perm 1) 34 24)
                (list (kern-mk-party t_goblin_horde align-monster (mk-ship)) 3 28)
                (list (kern-mk-obj t_goblin_generator 1) 20 20)
                (list (kern-tag 'mg-1 (mk-moongate 'ord)) 32 23)
                (list (kern-tag 'mg-2 (mk-moongate 'ord)) 35 25)
                (list (mk-ship) 36 26)
                )
               nil ; hooks
               )

(define hour 12)
(define minutes 45)
(define time-in-minutes (+ (* hour 60) minutes))

(kern-set-clock 
 0 ; year
 0 ; month
 0 ; week
 0 ; day
 hour  ; hour
 minutes ; minutes(
 )


(kern-mk-astral-body
 'sun              ; tag
 "Fyer (the sun)"  ; name
 1                 ; relative astronomical distance 
 1                 ; minutes per phase (n/a for sun)
 (/ (* 24 60) 360) ; minutes per degree
 0                 ; initial arc
 0                 ; initial phase
 '()               ; script interface
 ;; phases:
 (list 
  (list s_sun 255 "full")
  )
 )

;;----------------------------------------------------------------------------
;; Lumis is the source gate, which means it opens the source moongates on its
;; phases. We designate this by using the source-moon-ifc as its ifc.
;;----------------------------------------------------------------------------
(mk-moon 'lumis  ; tag
         "Lumis" ; name
         2       ; hours per phase
         24      ; hours per revolution
         90      ; initial arc
         0       ; initial phase
         'source-moon-ifc ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-1 'mg-2 'mg-3 'mg-4
               ))

;;----------------------------------------------------------------------------
;; Ord is the destination gate, which means its phase decides the destination
;; when the player steps through a moongate. We designate this by giving it a
;; nil ifc. Note that its gates do not need to be listed in the same order as
;; Lumis. In fact, they don't even need to be the same set of gates.
;;----------------------------------------------------------------------------
(mk-moon 'ord    ; tag
         "Ord"   ; name
         1       ; hours per phase
         12      ; hours per revolution
         180     ; initial arc
         1       ; initial phase
         nil     ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-1 'mg-2 'mg-3 'mg-4
               ))
