;;----------------------------------------------------------------------------
;; Abandoned Farm
;;
;; The abandoned farm is a wrecked homestead. Not long ago (weeks or at most
;; months) renegade trolls attacked, killing the husband and wife who lived
;; here. On start-of-game the trolls are still here, but they are locked in a
;; life-and-death battle with a group of giant wood spiders.
;;
;; The farm has a cellar which contains a quest item. The farm itself should
;; contain the bones or corpses of the man and woman, who were eaten by the
;; trolls. Their remains should lie near the troll's campsite. I'd like to make
;; burying their remains a quest item if I can, since the woman is Gregor's
;; daughter and Ilya's mother.
;;
;; One or two very frightened sheep might also remain, and quite a few have
;; been eaten by the trolls.
;;----------------------------------------------------------------------------

(kern-mk-map 
 'm_abandoned_farm 32 32 pal_expanded
 (list
		"tt tt || || || || || || || || || || || tt tt .. .. .. tt || || || || || || || || tt tt tt tt tt "
		"tt tt || || || || || || || || || || || || tt .. .. .. ta || || || || tt || || || || tt tt bb tt "
		"|| || || || || || || || || || tt || || || tt tA .. .. t% tt || || tt tt tt || || || || tt tt tt "
		"|| || || || || || || || || tt tt tt || || tt t5 .. .. .. tt || tt tt tt tt tt || || || tt tt tt "
		"|| || || || || || || || || || tt || || || tt tc .. .. .. tt || || tt tt tt tt || || || || || tt "
		"|| || || || || || || || || || || || || || tt t# .. .. tC tt || || || tt || tt tt tt || || || || "
		"|| || || || || || || || || || || || || tt tt .. .. .. t3 tt tt || || || || tt || tt tt tt tt || "
		"|| || || tt tt || || || || || tt tt tt tt tc .. .. tC tt tt tt tt tt || tt tt || || || tt tt tt "
		"|| || || tt tt tt tt tt tt tt tc t# .. .. .. .. .. tb tt tt tt tt tt tt tt || || || tt tt bb tt "
		"|| || || tt tt tt tt tt tt tc t# .. bb bb bb bb bb bb bb bb tD ta tt tt tt tt tt || || tt tt tt "
		"|| || tt tt t# .. .. .. .. .. .. bb .. .. .. .. .. .. tb tt td t& te bb ta tt tt tt || || tt || "
		"|| || tt tt .. bb rr rr wr rr rr rr .. .. .. .. .. .. .. .. .. .. t% tf bb tt tt tt || || || || "
		"|| || tt tt .. rr .. cc cc cc cc rr .. .. .. .. .. .. .. .. .. && .. .. bb tt tt tt || || || || "
		"|| tt tt tt .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb tt bb ta || || || || "
		"tt tt tt tc .. wr cc cc cc cc cc wr .. .. .. .. .. .. .. .. .. bb .. tf bb tt td t# .. t% |A || "
		"t% ta tc t# .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb te t# .. .. .. t% |A "
		".. .. .. .. .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. tC tf tA .. .. "
		".. .. .. .. .. rr rr rr cc rr rr bb rr cc rr .. .. .. .. .. .. .. .. .. bb .. tC t7 bb t7 tA .. "
		"t5 tA .. .. .. rr cc cc cc cc cc cc .. cc rr .. .. .. .. .. .. .. .. .. bb .. t3 tt tt tt tt tt "
		"tt t5 tA .. .. wr cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. bb .. tt tt tt tt tt bb "
		"tt tt t5 .. .. rr cc cc cc cc cc .. cc cc rr rr rr rr wr rr rr bb rr bb .. tC tt tt tt tt tt tt "
		"tt tt tt tA .. rr cc cc cc [[ @@ ]] cc cc rr cc cc cc cc cc .. .. rr .. tC t3 tt || || || tt tt "
		"|| || tt t5 .. wr cc cc cc cc cc cc cc cc rr cc cc cc cc cc cc cc rr .. t3 tt || || || || || tt "
		"|| || tt tt .. rr cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc wr tC tt || || || || || || tt "
		"|| || || tt .. rr rr rr cc cc cc cc cc cc rr cc cc cc cc cc cc .. rr t3 tt || || || tt || || || "
		"|| || || tt .. rr cc cc cc cc cc [[ @@ ]] rr cc cc cc cc cc .. .. bb tt tt || || || || || || || "
		"|| || || tt .. rr rr rr rr wr rr rr rr rr rr rr rr rr wr rr rr rr rr tt tt || || || || || || tt "
		"|| || || tt tA .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tC t3 tt tt tt || || || || || tt tt "
		"|| || tt tt tt tt tt tt tt tt tt tt tt t5 tA .. .. .. tC t3 tt tt tt tt tt tt || || || || tt tt "
		"|| || tt tt || || || || || || || || tt tt t5 tA .. tC t3 tt tt || || || tt tt || || || || tt tt "
		"|| || || || || || || || || || || || || tt tt t5 .. t3 tt tt || || || || || || || || || tt tt tt "
		"|| || || || || || || || || || || || || || tt tt .. tt tt || || || tt || || || || || tt tt tt tt "
 ))


(kern-mk-place 'p_abandoned_farm  ; tag
               "Abandoned Farm"   ; name
               s_hamlet           ; sprite
               m_abandoned_farm   ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil                ; subplaces
               nil                ;; neighbors

               ;; objects
               (list
                (put (mk-monman) 0 0)
                (put (spawn-pt 'queen-spider) 9 23)
                (put (spawn-pt 'troll) 19 13)
                (put (spawn-pt 'troll) 19 14)
		(put (kern-mk-obj t_spell_book_enchantment_curses 1) 10 12)  ;; Belonged to Ilya's mother
                (list (mk-door-in-rock) 13 17)
                (list (mk-door-in-rock) 7 25)
                (list (mk-ladder-down 'p_abandoned_cellar 6 25) 6 25)
				(list (mk-broken-clock s_clock_hand_s s_clock_hand_ne "The clock reads 6:12") 6 23)
                )
               (list 'on-entry-to-dungeon-room) ; hooks
               nil ; edge entrances
               )

(mk-place-music p_abandoned_farm 'ml-outdoor-adventure)
