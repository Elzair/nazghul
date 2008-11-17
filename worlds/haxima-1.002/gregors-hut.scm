(kern-mk-map 
 'm_gregors_hut 32 32 pal_expanded
	(list
		"|| || || || tt tc .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. tC tt tt tt tt "
		"|| || || || tt tB .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. t3 tt tt tt tt tt tt "
		"|| || tt tt tt t5 .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tC tt || || || tt tt tt "
		"|| tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tC t3 tt || || || tt tt tt "
		"tt tt tt tt tt tt .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. tC t3 tt || || tt || || tt t# "
		"tc t# .. t% tt tt tA .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. tb tt tt || tt tt tt || tt .. "
		".. .. .. .. ta tt tt td .. .. .. .. .. .. .. .. /7 .. .. .. .. .. t% ta tt || || tt || || tt .. "
		".. .. .. .. /0 /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /d /2 ta tt || || || tt tc .. "
		".. .. .. /0 /a t3 tt t5 tA .. .. .. .. .. .. .. .. .. .. .. .. .. .. /8 /d /d /2 ta tt tt t# .. "
		".. .. .. /7 t3 tt tt tt t5 rr rr wr rr rr rr wr rr rr rr rr rr bb bb bb bb bb /8 /d /2 te .. .. "
		".. .. .. /7 ta tt tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr .. .. .. .. .. bb .. /7 .. .. .. "
		".. .. .. /7 t% ta tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr t7 tA .. .. .. bb .. /7 .. .. .. "
		".. .. .. /7 .. t% tt || || rr cc cc [[ @@ ]] cc cc rr cc cc rr tt t5 tA .. .. bb .. /7 .. .. .. "
		".. .. .. /7 .. .. ta || |C rr cc cc cc cc cc cc cc rr cc cc rr tt tt t5 tA .. bb .. /7 .. .. .. "
		".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc cc cc cc rr tt tt || td .. bb .. /7 .. .. .. "
		".. .. .. /7 .. .. wr cc cc rr rr cc rr && rr rr rr rr rr rr rr ta tt tc t# .. bb .. /7 .. .. .. "
		"/d /d /d /6 .. .. rr cc cc cc cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /4 /d /d /d "
		".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /7 t3 tt tt "
		".. .. .. /7 .. .. rr cc cc rr cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb t7 /7 tt tt tt "
		".. .. .. /7 .. .. wr cc cc cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb te /7 tt tc %3 "
		".. .. .. /7 .. .. rr rr rr rr rr wr rr cc rr wr rr rr .. rr rr bb bb bb bb bb /0 /d /a te %3 %% "
		".. .. .. /7 .. .. .. .. .. .. .. tb td /7 .. tb tt td /4 /d /d /d /d /d /d /d /a t3 td %3 %% %% "
		".. .. .. /7 .. .. .. .. .. .. .. .. .. /7 .. .. .. .. /7 .. .. .. .. .. tC t3 tt tc %3 %% ~3 ~9 "
		".. .. .. /8 /d /d /d /d /d /d /d /d /d /9 /d /1 /d /d /a .. .. .. .. tC t3 tt tt %3 ~3 ~9 ~c %% "
		".. .. .. .. .. .. .. t3 tt t5 .. .. .. .. .. /7 .. .. t7 tA .. .. tb tt tt tt tc %% ~6 %% %% %% "
		"tt t5 tA .. .. .. .. tt tt tt tA .. .. .. .. /7 .. .. tt tt t5 tA .. t% te ~3 ~9 ~9 ~c %% %% %% "
		"|| tt t5 tA .. .. .. ta tt tt t5 .. .. .. .. /7 .. .. ta tt tt tt t5 .. .. ~6 .. tf %a %c t7 .. "
		"|| || tt t5 tA .. .. t% ta tt tc .. .. .. .. /7 .. .. t% tt tt tt tc ~3 ~~ -4 .. .. .. t3 tt .. "
		"|| || tt tt t5 tA .. .. .. .. .. .. .. .. .. /8 /2 .. .. tt tt tt ~3 ~8 ~8 ~c t3 t5 .. ta |C .. "
		"|| || || tt tt t5 tA .. .. .. .. .. .. .. .. .. /7 .. .. tt tt tc ~6 t3 tt tt tt tt tA .. .. .. "
		"|| || || || || tt t5 tA .. .. .. .. .. .. .. .. /7 .. .. tt tt ~3 ~c tt t# tD tt tt |% .. .. .. "
		"|| || || || || || tt t5 .. .. .. .. .. .. .. .. /7 .. .. tt tt ~6 t3 tt .. t3 tt || || .. .. .. "
	)
 )

(kern-load "ilya.scm")

(bind 
 (kern-mk-char 'ch_ilya ; tag
               "Ilya"              ; name
               sp_human            ; species
               nil                 ; occ
               s_child             ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
               #f                  ; dead
               'ilya-conv          ; conv
               sch_ilya           ; sched
               'townsman-ai                 ; special ai
               (mk-inventory (list (list 1 t_sword)))                 ; container
               nil                 ; readied
               )
 (ilya-mk #f #f))

;;(kern-terrain-map-blend m_gregors_hut t_shoals tset_water tset_shore)


(kern-mk-place 'p_gregors_hut     ; tag
               "Gregor's Hut"     ; name
               s_hamlet           ; sprite
               m_gregors_hut      ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               ;;objects
               (list
                
                ;; characters
                (list ch_ilya   15 15)

                ;; mechanisms
                (list (mk-door-in-rock) 13 20)
                (list (mk-door-in-rock)  9 19)
                (list (mk-door-in-rock)  9 16)
                (list (mk-door-in-rock) 11 15)
                (list (mk-door-in-rock) 17 14)
                (list (mk-door-in-rock) 18 20)
                (list (mk-clock) 10 10)
                (list (mk-bridge 'east) 25 26)
                (list (mk-ambient-sound 'sound-river) 25 26)
                (put (mk-bed) 7 18)
                (put (mk-bed) 7 16)
                (put (mk-npc 'chicken 5) 13 23)
                (put (mk-npc 'chicken 5) 14 24)
                
                )
               nil ; hooks
               nil ; edge entrances
               )

(mk-place-music p_gregors_hut 'ml-small-town)
