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
(kern-load "camping-map.scm")

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

(bind 
 (kern-mk-char 'ch_shroom ; tag
               "Shroom"              ; name
               sp_human              ; species
               oc_druid              ; occ
               s_companion_druid     ; sprite
               faction-men            ; starting alignment
               0 10 0                ; str/int/dex
               0 0                   ; hp mod/mult
               0 0                   ; mp mod/mult
               30 0 9 9            ; hp/xp/mp/lvl
               #f                  ; dead
               'shroom-conv          ; conv
               sch_shroom            ; sched
               nil                   ; special ai
               nil                   ; container
               (list t_dagger))      ; readied
 (shroom-mk #f #f))

(kern-mk-char 'ch_olin ; tag
              "Olin the Ghast"      ; name
              sp_ghast              ; species
              nil                   ; occ
              s_ghost               ; sprite
              faction-men            ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              240 0 9 8           ; hp/xp/mp/lvl
               #f                  ; dead
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil                   ; container
              nil)                  ; readied

(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              faction-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              240 0 8 8           ; hp/xp/mp/lvl
               #f                  ; dead
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil                   ; container
              (list t_rpg))         ; readied

(kern-mk-char 'ch_slurmok ; tag
              "Slurmok"             ; name
              sp_yellow_slime       ; species
              oc_wizard             ; occ
              s_yellow_slime        ; sprite
              faction-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              240 0 5 7             ; hp/xp/mp/lvl
               #f                  ; dead
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil                   ; container
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
 0                           ; turns to next meal
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
   (list 1 in_an)
   (list 1 wis_an_ylem)
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
   (list 1 in_mani_corp)

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
 
 ;; party members (should be nil for initial load file)
 nil
 )

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_thorald_greybeard)
(kern-party-add-member player ch_slurmok)

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
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || rr rr rr rr rr rr rr || || || || || || || || || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| || rr rr rr rr rr || || || rr rr rr && cc cc cc cc rr || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || xx xx xx xx xx xx xx xx xx tt tt || || || || || "
  "|| || rr cc cc cc rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || xx .. .. .. .. .. .. .. xx tt tt tt || || || || "
  "|| || rr cc cc cc rr || || || rr cc cc cc cc cc cc cc rr || || || || || || || || || tt tt bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
  "|| || rr cc cc cc rr || || || rr cc rr cc cc cc rr rr rr || || || || || || || || tt tt tt tt .. /7 .. bb tt || || || || || || || || || || /7 tt xx @@ @@ @@ @@ @@ @@ @@ xx tt tt tt || || || || "
  "|| || rr rr cc rr rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || tt tt tt bb .. /7 .. tt tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. .. xx tt tt || || || || || "
  "|| || || || /7 || || || || || || /7 rr rr rr rr rr rr rr || || || || || || || tt tt tt tt tt .. /7 .. tt tt || || || || || || || || || || /7 tt xx .S .H .R .O .O .M .S xx || || || || || || || "
  "|| || || || /8 /d /d /d /1 /d /d /a || || || || || || || || || || || || || || tt tt tt tt bb .. /7 .. bb tt || || || || || || || || || tt /7 tt xx xx xx xx xx xx xx xx xx || || || || || || || "
  "|| || || || || || || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt tt .. /7 .. tt tt || || || || || || || || tt tt /7 tt xx .. .. .. xx .. .. .. xx || || || || || || || "
  "|| || || rr rr rr || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt bb .. /7 .. bb tt || || || || || || || || tt tt /8 /d .. .. .. .. ?? .. .. .. ?? tt tt tt tt tt tt tt "
  "|| rr rr rr cc rr rr || /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt tt .. /7 .. tt tt || || || || tt tt tt tt tt tt tt tt xx .. .. tt xx .. .. .. xx || || || || || || || "
  "|| rr cc cc cc cc rr rr /7 || || || || || || || || || || /7 || || || || tt tt tt tt tt tt bb .. /7 .. bb tt || || || || tt .. tt tt tt || || || xx xx ws xx xx xx .. xx xx || || || || || || || "
  "|| rr cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt tt tt tt .. .. tt || || || || || || || || || || || || || || || || || || || || "
  "|| rr rr cc cc cc rr rr || || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt bb .. /7 .. bb tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
  "|| || rr rr && rr rr || tt || || || tt tt bb || || tt || /7 || tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
  "|| || || rr rr rr || tt tt tt || tt tt .. tt || || tt bb /7 bb .. bb tt bb tt bb tt bb tt .. .. /7 .. .. tt bb tt bb tt tt bb tt bb tt bb tt tt || || || || || || || || || || || || || || || || "
  "|| || || tt || || || || tt || tt tt .. .. .. tt || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt tt tt || || || || || || tt tt .. tt || || bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
  "|| || || tt || || || || || tt tt tt tt tt tt || || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || tt tt tt tt tt tt tt || || || bb .. /7 .. .. .. bb dd bb dd bb dd .. .. .. /7 .. .. .. bb tt tt bb tt bb tt .. .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || || tt tt tt .. tt tt tt || || || || tt .. /7 .. .. dd dd dd dd dd dd dd bb .. .. /7 .. .. bb tt tt tt tt tt tt bb tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || bb tt tt .. .. .. tt tt tt || || || bb .. /7 .. bb dd || || || || || dd xx w+ xx cc xx w+ xx tt || || || || || tt tt .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt .. .. .. .. .. tt tt || || || || .. /7 .. dd dd || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt bb .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt tt .. .. .. tt tt tt || || || bb .. /7 .. bb dd || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || tt tt tt tt .. tt tt tt || || || || || .. /7 .. dd dd || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt tt tt tt || tt tt tt tt bb || || tt tt || bb .. /7 .. bb dd || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || tt || || || || || tt tt tt || || || || || || tt .. /7 .. dd dd xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || tt "
  "tt tt tt || || || || || || || || || || || || || tt bb .. /7 .. bb dd xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt tt .. /7 .. tt tt || || || || || || || || || || || || || || tt "
  "bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. /7 .. dd bb xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || || || || tt "
  ".. .. bb tt bb tt bb tt bb tt bb tt bb tt bb bb .. .. .. /7 .. .. .. w+ cc cc cc xx tt .. bb .. cc .. bb .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. bb tt bb tt bb tt bb tt bb tt bb tt tt tt bb .. .. /7 .. .. .. w+ cc cc cc xx tt .. bb .. cc .. bb .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
  "bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. .. bb xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || tt tt tt tt "
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tt .. /7 .. .. tt xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt .. .. /7 .. tt tt || || || || || || || || || || || || || || tt "
  "tt tt || || || || tt tt tt tt tt tt tt tt || || tt bb .. /7 .. bb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || || || tt tt tt tt tt tt tt || || tt tt .. /7 .. tt tt || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || tt tt tt tt tt tt || || tt bb .. /7 .. bb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt tt .. /7 .. tt tt || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || rr rr tt tt .. /7 .. tt tt || || || || || tt xx w+ xx cc xx w+ xx tt || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
  "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt tt tt tt tt tt tt bb .. .. /7 .. .. bb tt tt tt tt tt tt tt bb .. /7 .. bb tt tt tt tt tt tt tt || || || || || || || || || "
  "|| || || || || || || tt tt tt tt || || || || || tt tt .. /7 .. .. tt bb tt bb tt bb .. .. .. .. /7 .. .. .. .. bb tt bb tt bb tt .. .. /7 .. .. tt bb tt bb tt bb tt || || || || || || || || || "
  "|| || || || || tt rr tt rr rr tt rr rr || || || tt bb .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || || || || || || || "
  "|| || || rr rr tt tt tt || || tt tt rr rr rr || tt tt .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 .. bb tt || || || || || || || || || "
  "|| || || rr || || || tt tt tt || tt tt || rr || || bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || "
  "|| || || || tt tt tt tt .. .. tt || tt tt || rr rr || || tt bb tt bb tt bb tt bb tt bb tt bb  .. /7 .. tt bb tt bb tt bb tt bb tt bb tt bb tt bb tt tt bb /4 /2 bb tt || || || || || || || || || "
  "|| rr rr || tt tt || .. .. bb .. tt tt tt tt || rr rr || tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt tt tt tt tt tt tt tt tt tt xx xx xx xx ws xx cc cc xx ws xx xx xx xx || || || || || "
  "|| || || || tt tt .. .. .. .. .. tt .. tt tt || || rr || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
  "|| tt tt tt tt .. .. .. .. .. .. .. .. .. tt tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
  "|| tt tt tt .. bb .. .. .. .. .. .. .. bb .. tt .. tt tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx || || || || || "
  "|| || || tt .. .. .. .. .. aa .. .. .. .. .. tt || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc && && cc cc 00 cc cc xx || || || || || "
  "|| rr || tt tt tt tt .. .. .. .. .. .. tt tt || || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || xx xx xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx xx xx || || || "
  "|| rr || || || .. tt .. .. .. .. .. .. tt tt || rr || || || rr rr tt rr rr || || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
  "|| rr rr || tt tt tt bb .. .. .. bb .. tt tt || rr || || rr rr tt tt tt rr rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
  "|| || rr || tt tt tt tt tt .. .. .. .. tt || rr rr || || rr tt tt || tt || rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
  "|| || || || || || || || tt tt tt tt tt || tt rr || || || rr tt || || || || rr || || || tt tt .. /7 .. || || || || || || || || xx xx cc xx xx @@ @@ @@ @@ @@ @@ @@ @@ @@ @@ xx xx cc xx xx || || "
  "|| || || rr rr || || || tt tt tt || tt tt || rr || || || rr tt tt || || || rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || "
  "|| || || || rr rr || rr rr rr || rr rr rr rr || || || || rr rr || || || tt rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx .W .H .I .T .E @@ .S .T .A .G xx cc cc cc xx || || "
  "|| || || || || || || || || || || || || || || || || || || || rr rr rr rr tt || || || tt tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx || || "
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || xx xx xx xx xx || || || || || || || || || || xx xx xx xx xx || || "
  "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "))

(kern-mk-map
 'm_pit_of_death 8 8 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx"
  "xx !! !! !! !! !! !! xx"
  "xx !! !! !! !! !! !! xx"
  "xx !! !! !! !! !! !! xx"
  "xx !! !! !! !! !! !! xx"
  "xx !! !! !! !! !! !! xx"
  "xx !! !! !! !! !! !! xx"
  "xx xx xx xx xx xx xx xx"
))

(kern-mk-map
 'm_green_tower_lower 64 64 pal_expanded
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ ~~ || || || || rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ || || || || || rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ || || || || rr rr rr rr rr rr rr xx xx xx xx x! xx xx x! xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx tt xx tt tt || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ || || || || rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. xx tt tt tt || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ ~~ || || || rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc xx rr rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. .. tt tt tt tt || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || ~~ || || || rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx rr rr rr rr rr rr rr rr rr rr rr rr rr @@ tt @@ tt xx tt tt tt || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || ~~ || || || rr rr rr rr rr rr rr xx xx xx x! cc cc cc cc x! xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. .. .. xx tt tt || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || ~~ || || || rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx rr rr rr rr rr rr rr rr rr rr rr tt tt tt tt tt tt xx || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || ~~ ~~ || || rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc xx rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx tt xx xx || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || ~~ || || rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx rr rr rr rr rr rr rr rr rr rr .. .. .. xx .. .. .. xx || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || ~~ || || rr rr rr rr rr rr rr xx xx xx x! cc cc cc cc x! xx xx xx rr rr rr rr rr rr rr tt tt .. .. .. .. ?? .. .. .. ?? tt tt tt tt tt tt tt "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr .. .. ~~ ~~ ~~ rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx rr rr rr rr rr rr || || tt xx .. .. .. xx .. .. .. xx || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || || ~~ rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc ?? || rr rr rr rr rr xx xx tt xx xx tt xx xx xx .. xx xx || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || || ~~ rr rr rr rr rr rr rr xx cc cc xx cc cc cc cc xx cc cc xx tt rr ~~ ~~ ~~ ~~ ~~ || tt || || || || || || || || || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || || ~~ ~~ rr rr rr rr rr rr rr xx xx xx xx x! cc cc x! xx xx xx xx xx xx ~~ xx xx -- ~~ ~~ .. || || || || || || || || || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || || ~~ || .. || tt ~~ ~~ ~~ .. .. .. .. xx cc cc cc cc xx ~~ ~~ ~~ -- __ __ __ xx -- -- ~~ ~~ || || || || || || || || || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || ~~ ~~ || .. || tt ~~ tt ~~ .. .. ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ -- __ __ __ xx __ -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ || || || ~~ ~~ ~~ ~~ || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || ~~ tt bb .. bb .. ~~ tt ~~ .. .. ~~ tt xx cc cc cc cc xx cc cc ~~ -- __ __ __ xx __ -- ~~ ~~ || || || || || || ~~ || || || ~~ || || ~~ ~~ || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ~~ ~~ .. .. .. .. ~~ .. ~~ ~~ ~~ ~~ tt xx x! cc cc x! xx cc cc ~~ -- -- -- -- xx -- ~~ ~~ tt tt || || || || || || || || || ~~ || || tt ~~ ~~ "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr || ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. .. xx cc cc cc cc xx cc cc ~~ ~~ ~~ ~~ ~~ xx ~~ ~~ .. || tt || || || || || ~~ ~~ ~~ ~~ ~~ || tt tt tt || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. .. .. .. .. .. rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc xx .. .. .. tt tt || || || || || || || || || || || || tt || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc xx cc cc cc cc cc cc cc x! .. .. .. bb tt || || || || || || tt || || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx x! cc cc x! xx ,C ,I ,S ,T ,E ,R ,N xx .. .. .. tt tt || tt || || || tt tt tt || || || || || || || "
  "rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr xx xx xx xx cc cc cc cc xx xx xx xx xx xx xx xx xx .. .. .. bb tt tt tt tt || || || tt || || || rr || rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx ,B ,A ,R ,R ,A ,C ,K ,S xx rr rr rr rr xx xx xx cc cc xx cc cc cc cc xx cc cc xx xx xx cc cc ?? .. .. .. tt tt || tt || || || || || || || || rr tt tt cc rr "
  "rr rr rr rr rr rr rr rr rr rr rr x! cc cc cc cc cc cc cc cc x! rr rr xx xx xx cc cc cc cc xx cc cc cc cc xx cc cc cc cc xx xx xx xx .. .. .. bb tt || || tt rr rr rr rr rr rr rr rr cc cc cc rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr rr xx cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc xx xx .. .. .. tt tt || || tt rr cc cc cc cc cc cc cc cc cc cc rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx .. .. .. tt tt || || tt rr cc rr rr rr rr rr rr cc cc cc rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx cc cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc cc xx xx xx x! xx xx xx x! xx xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx cc cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc cc xx xx ,C ,O ,U ,N ,C ,I ,L xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx xx xx xx cc xx xx xx x! cc cc cc cc x! xx xx xx cc xx xx xx xx xx cc cc cc cc cc cc cc xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx xx x! xx cc cc xx x! xx xx xx x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc 00 00 00 cc cc xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc 00 00 00 cc cc xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx xx x! xx cc cc xx x! xx xx xx x! cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc cc cc cc x! cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx xx xx xx cc xx xx xx x! cc cc cc cc x! xx xx xx cc xx xx xx xx xx cc cc cc cc cc cc && xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr x! cc cc cc cc cc cc cc cc x! rr xx cc cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc cc xx xx xx x! xx xx xx x! xx xx cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr xx cc cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc cc ?? cc cc cc cc cc cc cc cc cc cc rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr x! cc cc cc cc cc cc cc cc x! rr xx xx cc cc cc cc cc cc xx cc cc cc cc xx cc cc cc cc cc cc xx xx rr rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc xx rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx ,T ,R ,A ,I ,N ,I ,N ,G xx rr rr xx xx xx cc cc cc cc xx cc cc cc cc xx cc cc cc cc xx xx xx rr rr rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx rr rr rr rr xx xx xx cc cc xx cc cc cc cc xx cc cc xx xx xx rr rr rr rr rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx cc cc cc cc xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx x! cc cc x! xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc xx [[ @@ @@ ]] xx rr rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc xx xx rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr xx cc cc 00 cc cc 00 cc cc 00 cc cc cc cc cc cc cc && xx rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr xx cc cc 00 cc cc 00 cc cc 00 cc cc cc cc cc cc cc && xx rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr .. rr rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr xx cc cc 00 cc cc 00 cc cc 00 cc cc cc cc cc cc cc && xx rr rr rr rr rr rr cc rr rr rr rr rr rr rr rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr x! cc cc cc cc cc cc cc cc cc cc cc x! cc cc cc cc && xx rr rr rr rr rr rr cc rr ,C ,R ,Y ,P ,T rr rr rr rr rr "
  "rr .. pp .. pp .. pp .. pp .. pp .. pp .. pp .. pp .. .. rr rr rr rr rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc xx cc cc cc cc xx xx rr rr rr rr rr cc cc cc cc cc cc cc cc cc rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr xx ,D ,I ,N ,I ,N ,G @@ ,H ,A ,L ,L xx [[ @@ @@ ]] xx rr rr rr rr rr rr cc rr rr cc cc cc cc cc cc cc rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr xx xx x! xx xx xx x! xx xx xx x! xx xx xx xx xx xx xx rr rr rr rr rr rr cc rr cc cc cc cc cc cc cc rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr cc rr rr rr cc rr rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr cc cc cc rr cc cc cc rr rr rr rr "
  "rr .. pp .. pp .. pp .. pp .. pp .. pp .. pp .. pp .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr cc rr rr rr cc rr rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr cc cc cc rr cc cc cc rr rr rr rr "
  "rr .. rr rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr cc rr rr rr cc rr rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc rr cc cc cc rr cc cc cc rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr rr cc rr rr rr rr rr "
  "rr .. .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc cc cc rr cc cc cc rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr cc rr rr rr cc rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "))

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
  "__ __ __ -- .. .. .. .. tt tt !! tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. .. -- -- .. __ "
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
  ".. .. ~~ .. .. .. .. tt tt tt tt tt tt tt .. .. .. {{ {{ ^^ {{ {{ {{ .. .. .. .. tt || || || bb tt bb tt /b .. || || ~~ "
  ".. || ~~ ~~ || || tt tt tt tt ~~ ~~ ~~ -- -- -- .. .. {{ {{ {{ -- -- -- .. .. .. tt tt || || tt tt .. .. .. .. || ~~ ~~ "
  ".. || || ~~ tt tt tt tt .. %% %% ~~ ~~ -- __ -- -- .. .. .. .. .. -- -- .. .. .. .. tt || || bb tt bb tt .. .. ~~ ~~ .. "
  "|| || || ~~ tt tt tt .. .. .. %% %% ~~ -- __ __ -- -- -- -- .. .. -- -- -- .. .. .. tt || || || || || || .. .. ~~ .. .. "
  "|| || || ~~ tt tt .. .. .. %% .. %% %% -- __ __ __ __ __ -- -- -- -- -- .. .. .. .. tt tt tt || || || || || -- -- -- || "
  "|| || || ~~ .. .. .. %% %% .. %% %% ~~ -- __ __ __ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt -- -- -- -- __ -- -- "
  "-- || -- -- ~~ ~~ %% %% ~~ %% %% ~~ -- -- __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ -- "
  "-- -- -- __ __ -- -- -- ~~ ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ -- -- -- %% %% %% %% -- -- __ __ ^^ ^^ ^^ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ -- -- -- .. .. .. -- -- -- -- -- -- ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ -- -- -- ^^ {{ ^^ {{ {{ {{ ^^ -- ^^ ^^ ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ -- -- {{ {{ {{ ^^ {{ ^^ -- {{ {{ ^^ ^^ -- -- __ __ __ __ __ -- ^^ ^^ ^^ __ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ ^^ -- ^^ ^^ ^^ ^^ -- -- __ __ __ __ __ -- -- !! ^^ ^^ ^^ ^^ -- __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !! !! !! ^^ !! -- -- __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !! !! !! ^^ -- __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !! !! ^^ ^^ ^^ ^^ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !! ^^ ^^ __ __ __ __ __ __ "
  "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ __ __ "
  ))

(load "P_terrain_test.scm")

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
               nil ; edge entrances
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
               nil ; edge entrances
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
               nil ; edge entrances
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
                (list (kern-mk-obj cure-poison-potion 2) 63 32)
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
               nil ; edge entrances
               )

(define road-map
  (list
   (list 'r8 'rd 'rd 'r2)
   (list '.. '.. '.. 'r7)
   (list '.. '.. '.. 'r7)
   (list '.. '.. '.. 'r7)
   (list 'rd 'rd 'rd 'ra)))

(kern-mk-place 'p_wilderness
               "The Great Wild"
               nil          ; sprite
               m_wilderness
               #t
               #f
               #t
               #f                 ; tmp combat place
               (list
                (list p_green_tower  35 23)
                (list p_terrain_test  3 15)  ;; For test purposes
                )
               nil ; neighbors
               (list
                (list player 36 23)
                (list (kern-mk-party t_skeleton_brigade faction-monster nil) 25 19)
                (list (kern-mk-party t_slime_glob faction-monster nil) 36 20)
                (list (kern-mk-obj F_fire 1) 34 23)
                (list (kern-mk-obj F_poison 1) 34 22)
                (list (kern-mk-obj F_sleep_perm 1) 34 24)
                (list (kern-mk-party t_goblin_horde faction-orks (mk-ship)) 3 28)
                (list (mk-generator t_orc_generator) 20 20)
                (list (kern-tag 'mg-1 (mk-moongate 'ord)) 32 23)
                (list (kern-tag 'mg-2 (mk-moongate 'ord)) 35 25)
                (list (mk-ship) 36 26)
                (list (mk-bridge east) 2 22)
                (list (mk-bridge east) 37 25)
                )

               nil ; hooks
               nil ; edge entrances
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

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;           none play men orks accu mons
 (dtable-row  0    0    0   0   -1    -2) ;; none
 (dtable-row  0    2    1  -2   -1    -2) ;; player
 (dtable-row -1    1    2  -1   -2    -2) ;; men
 (dtable-row -1   -2   -1   2   -1    -2) ;; orks
 (dtable-row -1   -1   -1  -1    2    -2) ;; accursed
 (dtable-row -2   -2   -2  -2   -2     0) ;; monsters
 )

(kern-load "pre-entry-hooks.scm")
