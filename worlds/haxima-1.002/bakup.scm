;; save.scm -- a nazghul session file
;; Load the standard definitions file
(load "naz.scm")

(kern-load "game.scm")

(kern-load "camping-map.scm")

(kern-load "zones.scm")

(kern-load "runes.scm")

(kern-load "prices.scm")

(kern-load "special.scm")

(kern-load "town-entry.scm")

(kern-load "endless-deeps-mech.scm")

(kern-load "gregor.scm")

(kern-load "kalcifax.scm")

(kern-load "ilya.scm")

(kern-mk-place 'p_gregors_hut "Gregor's Hut"
  s_hamlet ;; sprite
  (kern-mk-map
    nil     32 32 pal_expanded
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
      ".. .. .. /7 t3 tt tt tt t5 rr rr ws rr rr rr ws rr rr rr rr rr bb bb bb bb bb /8 /d /2 te .. .. "
      ".. .. .. /7 ta tt tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr .. .. .. .. .. bb .. /7 .. .. .. "
      ".. .. .. /7 t% ta tt tt tt rr cc cc cc cc cc cc cc rr cc cc rr t7 tA .. .. .. bb .. /7 .. .. .. "
      ".. .. .. /7 .. t% tt || || rr cc cc [[ @@ ]] cc cc rr cc cc rr tt t5 tA .. .. bb .. /7 .. .. .. "
      ".. .. .. /7 .. .. ta || |C rr cc cc cc cc cc cc cc rr cc cc rr tt tt t5 tA .. bb .. /7 .. .. .. "
      ".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc cc cc cc rr tt tt || td .. bb .. /7 .. .. .. "
      ".. .. .. /7 .. .. ws cc cc rr rr cc rr && rr rr rr rr rr rr rr ta tt tc t# .. bb .. /7 .. .. .. "
      "/d /d /d /6 .. .. rr cc cc cc cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /4 /d /d /d "
      ".. .. .. /7 .. .. rr rr rr rr cc cc cc cc cc cc cc rr .. .. rr .. .. .. .. .. bb .. /7 t3 tt tt "
      ".. .. .. /7 .. .. rr cc cc rr cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb t7 /7 tt tt tt "
      ".. .. .. /7 .. .. ws cc cc cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. bb te /7 tt tc %3 "
      ".. .. .. /7 .. .. rr rr rr rr rr ws rr cc rr ws rr rr .. rr rr bb bb bb bb bb /0 /d /a te %3 %% "
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
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_gregors_hut
    (list
      (bind
        (kern-mk-char
          'ch_ilya
          "Ilya"
          sp_human
          nil
          s_child
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'ilya-conv
          sch_ilya
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
          #f
        )
      ) ;; bind
    7 18)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    13 20)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 19)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 16)
    (list
      (bind
        (kern-mk-char
          'ch_gregor
          "Gregor"
          sp_human
          nil
          s_townsman
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'gregor-conv
          sch_gregor
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    7 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    11 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    17 14)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    18 20)
    (list
      (bind
        (kern-mk-obj tf_ew_bridge 1
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    25 26)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    7 18)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    7 16)
  ) ;; end of objects in p_gregors_hut
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 31 31) ;; Northwest
    (list 1 16 31) ;; North
    (list 2 0 31) ;; Northeast
    (list 3 31 16) ;; West
    (list 4 16 16) ;; Here
    (list 5 0 16) ;; East
    (list 6 31 0) ;; Southwest
    (list 7 16 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_gregors_hut

(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map
    nil     23 28 pal_expanded
    (list
      "^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^d t3 tt tt tt td ^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^5 "
      "^^ {{ {{ {{ ^^ ^^ ^^ ^c t3 tt tt || || ^b ^^ ^^ {{ {{ {{ ^^ {{ {{ ^^ "
      "^^ {{ {{ {{ {{ ^^ t3 tt tt || tt tt || |% ^a ^^ {C t7 {{ ^^ ^^ {{ ^^ "
      "^^ {{ {{ {{ {{ {C tt || || || || tt || || |% ^e t3 tt {A {{ ^^ {{ ^^ "
      "^^ ^^ {{ {{ {C t3 || || || || tt tt tt || || tt tt tt t5 {A {{ {{ ^^ "
      "^^ ^^ ^^ ^^ t3 tt || || || tt tt tt tt tt tt tt tt tt tt td {{ ^^ ^^ "
      "^^ ^^ ^^ ^^ || || || || tt tt tc bb ta tt tt tt tt tt || ^b ^^ ^^ ^c "
      "^^ ^^ ^^ ^^ || || || tt tt bb .. .. .. bb tt tt tt || || |% ^a ^c |& "
      "^^ ^^ ^^ ^c || || tt tt tt td .. .. .. tb tt tt tt tt || || || || || "
      "^^ ^c |# || || || tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt "
      "^^ |# || || || || tt tc .. .. .. .. .. .. .. ta tt tt tt tt tt tt tc "
      "{{ |A || || || || tt bb .. .. .. .. .. .. .. bb tt tt tt tt tt tc {& "
      "{{ {% |A || || tt tt t5 .. .. .. .. .. .. .. t3 tt tc {# {% te {# {{ "
      "^^ {{ {{ {{ {% ta tt tt bb .. .. .. .. .. bb tt tc {# {{ {{ {{ {{ {{ "
      "^^ ^^ ^^ {{ {{ {% ta tt tt td .. .. .. tb tt tt {# ^^ ^^ {{ {{ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ {% ta tt bb .. .. .. bb tt tt {A ^^ ^^ ^^ {{ {{ ^^ "
      "^^ ^^ {7 ^^ ^^ ^^ {{ {% tt td .. .. .. tb tt tt td {{ ^^ ^^ {{ {{ ^^ "
      "^^ {3 .. {1 {5 ^^ {{ {{ tt bb .. .. .. bb tt tt {B {{ {{ {{ {{ ^^ ^^ "
      "^^ {2 .. .. {4 ^^ {{ {{ tt td .. .. .. tb tt tt tt tt t5 {A ^^ ^^ ^^ "
      "^^ {a .. {8 {c ^^ {{ {{ tt bb .. .. .. bb te bb te bb tt t5 ^^ ^^ ^^ "
      "^^ ^^ {e ^^ ^^ ^^ {{ {{ tt td .. .. .. .. .. .. .. .. ta tt ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt bb .. .. .. .. .. .. .. .. bb tt ^^ ^^ ^^ "
      "^^ ^^ ,H ,A ,I ,L ^^ ^^ tt t5 .. .. .. .. .. .. .. .. tb tt ^a ^^ ^^ "
      "^^ {3 .. .. .. .. {5 ^^ ta tc bb .. bb t7 bb .. .. .. bb tt t5 ^a ^^ "
      "^^ {2 .. .. .. .. .. {9 .. .. .. tC t3 tt td .. .. .. tb tt tt t5 ^e "
      "^^ {2 .. .. .. .. {4 ^^ t3 || tt tt tt tt bb .. .. .. bb tt tt tt t5 "
      "^^ ,S ,E ,E ,K ,E ,R ^^ || || || tt tt tc .. .. .. .. .. ta tt tt tt "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ta || tt tt tc bb .. .. .. .. .. bb ta tt tc "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_moongate_clearing
    (list
      (kern-tag 'black-gate
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            '()
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    11 11)
  ) ;; end of objects in p_moongate_clearing
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 22 27) ;; Northwest
    (list 1 16 27) ;; North
    (list 2 0 27) ;; Northeast
    (list 3 22 10) ;; West
    (list 4 11 14) ;; Here
    (list 5 0 11) ;; East
    (list 6 22 0) ;; Southwest
    (list 7 11 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_moongate_clearing

(kern-mk-place 'p_abandoned_cellar "Musty Cellar"
  nil ;; sprite
  (kern-mk-map
    nil     32 32 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr {{ {{ {{ rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. {{ {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. .. .. .. .. .. .. .. {{ {{ rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. .. .. .. .. .. .. .. .. .. {{ rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. {{ {{ {{ .. .. .. .. {{ rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr {{ {{ .. .. .. {{ rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. {{ {{ rr rr rr "
      "rr rr rr rr rr x! .. x! rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ {{ rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr x! .. x! rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. .. .. .. {{ {{ rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ .. .. .. .. .. {{ {{ rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ .. {{ {{ {{ rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_abandoned_farm "Abandoned Farm"
        s_hamlet ;; sprite
        (kern-mk-map
          nil           32 32 pal_expanded
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
            "|| || tt tt .. bb rr rr ws rr rr rr .. .. .. .. .. .. .. .. .. .. t% tf bb tt tt tt || || || || "
            "|| || tt tt .. rr .. cc cc cc cc rr .. .. .. .. .. .. .. .. .. && .. .. bb tt tt tt || || || || "
            "|| tt tt tt .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb tt bb ta || || || || "
            "tt tt tt tc .. ws cc cc cc cc cc ws .. .. .. .. .. .. .. .. .. bb .. tf bb tt td t# .. t% |A || "
            "t% ta tc t# .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb te t# .. .. .. t% |A "
            ".. .. .. .. .. rr cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. tC tf tA .. .. "
            ".. .. .. .. .. rr rr rr cc rr rr bb rr cc rr .. .. .. .. .. .. .. .. .. bb .. tC t7 bb t7 tA .. "
            "t5 tA .. .. .. rr cc cc cc cc cc cc .. cc rr .. .. .. .. .. .. .. .. .. bb .. t3 tt tt tt tt tt "
            "tt t5 tA .. .. ws cc cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. .. bb .. tt tt tt tt tt bb "
            "tt tt t5 .. .. rr cc cc cc cc cc .. cc cc rr rr rr rr ws rr rr bb rr bb .. tC tt tt tt tt tt tt "
            "tt tt tt tA .. rr cc cc cc [[ @@ ]] cc cc rr cc cc cc cc cc .. .. rr .. tC t3 tt || || || tt tt "
            "|| || tt t5 .. ws cc cc cc cc cc cc cc cc rr cc cc cc cc cc cc cc rr .. t3 tt || || || || || tt "
            "|| || tt tt .. rr cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ws tC tt || || || || || || tt "
            "|| || || tt .. rr rr rr cc cc cc cc cc cc rr cc cc cc cc cc cc .. rr t3 tt || || || tt || || || "
            "|| || || tt .. rr cc cc cc cc cc [[ @@ ]] rr cc cc cc cc cc .. .. bb tt tt || || || || || || || "
            "|| || || tt .. rr rr rr rr ws rr rr rr rr rr rr rr rr ws rr rr rr rr tt tt || || || || || || tt "
            "|| || || tt tA .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tC t3 tt tt tt || || || || || tt tt "
            "|| || tt tt tt tt tt tt tt tt tt tt tt t5 tA .. .. .. tC t3 tt tt tt tt tt tt || || || || tt tt "
            "|| || tt tt || || || || || || || || tt tt t5 tA .. tC t3 tt tt || || || tt tt || || || || tt tt "
            "|| || || || || || || || || || || || || tt tt t5 .. t3 tt tt || || || || || || || || || tt tt tt "
            "|| || || || || || || || || || || || || || tt tt .. tt tt || || || tt || || || || || tt tt tt tt "
          )
        )
        #f #f #f #f
        ;; subplaces
        nil
        nil ;; neighbors
        (list ;; objects in p_abandoned_farm
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'troll
              )
            ) ;; bind
          19 14)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          13 17)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          7 25)
          (list
            (bind
              (kern-mk-obj t_ladder_down 1
                ;; hooks
                (list
                )
              )
              (list
                'p_abandoned_cellar
                6
                25
              )
            ) ;; bind
          6 25)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'queen-spider
              )
            ) ;; bind
          9 23)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'troll
              )
            ) ;; bind
          19 13)
        ) ;; end of objects in p_abandoned_farm
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 31 31) ;; Northwest
          (list 1 16 31) ;; North
          (list 2 0 31) ;; Northeast
          (list 3 31 16) ;; West
          (list 4 16 16) ;; Here
          (list 5 0 16) ;; East
          (list 6 31 0) ;; Southwest
          (list 7 16 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_abandoned_farm

    9)
  ) ;; end neighbors of p_abandoned_cellar
  (list ;; objects in p_abandoned_cellar
    (list
      (kern-mk-field web-type 20)    7 13)
    (list
      (kern-mk-field web-type 20)    7 14)
    (list
      (kern-mk-field web-type 20)    7 15)
    (list
      (kern-mk-field web-type 20)    7 16)
    (list
      (kern-mk-field web-type 20)    8 13)
    (list
      (kern-mk-field web-type 20)    9 12)
    (list
      (kern-mk-field web-type 20)    9 13)
    (list
      (kern-mk-field web-type 20)    9 14)
    (list
      (kern-mk-field web-type 20)    10 12)
    (list
      (kern-mk-field web-type 20)    10 13)
    (list
      (kern-mk-field web-type 20)    10 14)
    (list
      (kern-mk-field web-type 20)    11 12)
    (list
      (kern-mk-field web-type 20)    11 13)
    (list
      (kern-mk-field web-type 20)    11 14)
    (list
      (kern-mk-field web-type 20)    9 16)
    (list
      (kern-mk-field web-type 20)    10 16)
    (list
      (kern-mk-field web-type 20)    10 17)
    (list
      (kern-mk-field web-type 20)    10 16)
    (list
      (kern-mk-field web-type 20)    11 17)
    (list
      (kern-mk-field web-type 20)    11 18)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    5 13)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    6 15)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    7 12)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    9 14)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    11 14)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    10 16)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    11 18)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          6
          0 0 0
          0 0
          0 0
          26 512
          5 3
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll
          #f
          #f
          '()
        )
      ) ;; bind
    11 12)
    (list
      (kern-mk-obj t_puska 1
        ;; hooks
        (list
        )
      )
    10 18)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_abandoned_farm
          6
          25
        )
      ) ;; bind
    6 25)
    (list
      (kern-mk-field web-type 20)    5 12)
    (list
      (kern-mk-field web-type 20)    5 13)
    (list
      (kern-mk-field web-type 20)    5 14)
    (list
      (kern-mk-field web-type 20)    6 11)
    (list
      (kern-mk-field web-type 20)    6 12)
    (list
      (kern-mk-field web-type 20)    6 13)
    (list
      (kern-mk-field web-type 20)    6 14)
    (list
      (kern-mk-field web-type 20)    6 15)
    (list
      (kern-mk-field web-type 20)    7 12)
  ) ;; end of objects in p_abandoned_cellar
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 31 31) ;; Northwest
    (list 1 16 31) ;; North
    (list 2 0 31) ;; Northeast
    (list 3 31 16) ;; West
    (list 4 16 16) ;; Here
    (list 5 0 16) ;; East
    (list 6 31 0) ;; Southwest
    (list 7 16 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_abandoned_cellar

p_abandoned_farm
(kern-load "slimy-cavern-zones.scm")

(kern-load "roland.scm")

(kern-mk-place 'p_slimy_cavern "Slimy Cavern"
  nil ;; sprite
  (kern-mk-map
    nil     16 32 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. rr "
      "rr rr rr rr rr .. .. && .. .. rr rr .. .. .. rr "
      "rr rr rr rr bb .. .. .. .. .. bb rr rr .. rr rr "
      "rr bb .. .. bb .. .. .. .. .. bb .. .. .. rr rr "
      "rr bb .. .. .. bb bb .. bb bb .. .. .. .. .. rr "
      "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr rr bb .. .. .. .. .. .. .. .. .. rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. rr .. .. rr "
      "rr .. .. .. .. .. .. rr rr .. .. rr rr .. .. rr "
      "rr .. rr .. rr bb rr rr rr rr .. rr rr .. rr rr "
      "rr %7 rr .. .. .. rr .. .. rr .. rr rr .. rr rr "
      "~~ b~ b~ rr .. .. rr .. .. .. .. .. .. .. rr rr "
      "rr ~a b~ ~4 .. .. rr rr .. rr rr .. rr rr rr rr "
      "rr rr %a ~a ~9 ~1 b~ rr rr rr rr .. rr rr rr rr "
      "rr .. .. .. .. ~a b~ b~ rr rr .. .. .. rr rr rr "
      "rr .. rr .. %f rr b~ ~~ bb %7 .. .. .. rr rr rr "
      "rr .. rr rr rr rr rr b~ ~~ ~5 %5 .. .. %7 rr rr "
      "rr .. rr rr rr .. .. rr %% ~a b~ ~9 b~ ~1 ~5 rr "
      "rr .. .. .. rr .. %b %% %% %% %c .. %% b~ ~8 ~~ "
      "rr rr rr .. rr .. .. rr rr .. .. .. %e rr rr rr "
      "rr rr rr .. rr rr rr rr rr .. .. .. .. .. bb rr "
      "rr rr .. .. .. rr rr .. .. .. rr .. .. .. .. rr "
      "rr .. .. .. .. .. .. .. rr rr rr .. .. .. .. rr "
      "rr .. .. .. .. rr rr rr rr rr rr rr .. .. rr rr "
      "rr rr .. .. rr rr rr .. .. .. rr rr rr .. rr rr "
      "rr rr rr .. rr rr .. .. .. .. .. rr rr .. rr rr "
      "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
      "rr .. rr rr rr rr .. .. .. .. .. rr .. rr .. rr "
      "rr .. rr rr rr rr rr .. .. .. rr rr .. .. .. rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_slimy_cavern
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    10 18)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    13 18)
    (list
      (bind
        (kern-mk-char
          'ch_roland
          "Roland"
          sp_human
          oc_warrior
          s_human_knight
          2
          16 10 16
          25 5
          0 0
          53 512
          5 3
          #f ;; dead?
          'roland-conv
          nil
          'roland-ai
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
          #f
          #f
        )
      ) ;; bind
    14 1)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 2 t_food)
          (list 1 t_2h_sword)
          (list 1 t_armor_chain)
          (list 1 t_iron_helm)
        )
        ;; hooks
        (list
        )
      )
    9 2)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_shard
          13
          8
        )
      ) ;; bind
    8 30)
    (list
      (bind
        (kern-mk-obj tf_ns_bridge 1
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    4 15)
    (list
      (bind
        (kern-mk-obj tf_ns_bridge 1
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    11 19)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    13 3)
    (list
      (bind
        (kern-mk-char
          nil
          "blackguard"
          sp_human
          oc_wrogue
          s_brigand
          8
          0 0 0
          0 0
          0 0
          12 256
          4 2
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'self-destruct-trap
            ;; contents
            (list
              (list 8 t_bolt)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_sword
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "2d6-2"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_picklock
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                    (list
                      50
                      "1d10"
                      't_arrow
                    )
                    (list
                      10
                      "1"
                      't_in_ex_por_scroll
                    )
                    (list
                      10
                      "1"
                      't_wis_quas_scroll
                    )
                    (list
                      5
                      "1"
                      't_sanct_lor_scroll
                    )
                    (list
                      5
                      "1"
                      't_an_tym_scroll
                    )
                    (list
                      5
                      "1"
                      't_vas_rel_por_scroll
                    )
                    (list
                      20
                      "1"
                      't_mana_potion
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      10
                      "1d3"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'blackguard
          #f
          #f
          '()
        )
      ) ;; bind
    5 4)
    (list
      (bind
        (kern-mk-char
          nil
          "blackguard"
          sp_human
          oc_wrogue
          s_brigand
          8
          0 0 0
          0 0
          0 0
          12 256
          4 2
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'sleep-trap
            ;; contents
            (list
              (list 1 t_sword)
              (list 4 t_bolt)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "2d6-2"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_picklock
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                    (list
                      50
                      "1d10"
                      't_arrow
                    )
                    (list
                      10
                      "1"
                      't_in_ex_por_scroll
                    )
                    (list
                      10
                      "1"
                      't_wis_quas_scroll
                    )
                    (list
                      5
                      "1"
                      't_sanct_lor_scroll
                    )
                    (list
                      5
                      "1"
                      't_an_tym_scroll
                    )
                    (list
                      5
                      "1"
                      't_vas_rel_por_scroll
                    )
                    (list
                      20
                      "1"
                      't_mana_potion
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      10
                      "1d3"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'blackguard
          #f
          #f
          '()
        )
      ) ;; bind
    9 4)
    (list
      (bind
        (kern-mk-char
          nil
          "blackguard"
          sp_human
          oc_wrogue
          s_brigand
          8
          0 0 0
          0 0
          0 0
          12 256
          4 2
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'sleep-trap
            ;; contents
            (list
              (list 1 t_sword)
              (list 7 t_bolt)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "2d6-2"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_picklock
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                    (list
                      50
                      "1d10"
                      't_arrow
                    )
                    (list
                      10
                      "1"
                      't_in_ex_por_scroll
                    )
                    (list
                      10
                      "1"
                      't_wis_quas_scroll
                    )
                    (list
                      5
                      "1"
                      't_sanct_lor_scroll
                    )
                    (list
                      5
                      "1"
                      't_an_tym_scroll
                    )
                    (list
                      5
                      "1"
                      't_vas_rel_por_scroll
                    )
                    (list
                      20
                      "1"
                      't_mana_potion
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      10
                      "1d3"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'blackguard
          #f
          #f
          '()
        )
      ) ;; bind
    9 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    9 17)
  ) ;; end of objects in p_slimy_cavern
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 15 31) ;; Northwest
    (list 1 8 31) ;; North
    (list 2 0 31) ;; Northeast
    (list 3 15 16) ;; West
    (list 4 8 16) ;; Here
    (list 5 0 16) ;; East
    (list 6 15 0) ;; Southwest
    (list 7 8 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_slimy_cavern

(kern-load "jim.scm")

(kern-load "gwen.scm")

(kern-load "chanticleer.scm")

(kern-load "earl.scm")

(kern-load "miggs.scm")

(kern-mk-place 'p_trigrave "Trigrave"
  s_town ;; sprite
  (kern-mk-map
    nil     32 32 pal_expanded
    (list
      "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
      "tt xx xx xx xx xx xx xx xx xx xx xx xx xx tt tt td .. .. .. tb tt tt tt xx xx xx xx xx xx xx tt "
      "tt xx cc cc x! .G .R .A .Y @@ x! cc cc xx tt tt bb .. .. .. bb tt tt tt xx .I .R .O .N @@ xx tt "
      "tt xx cc cc x! @@ .D .O .V .E x! cc cc xx tt tt td .. .. .. tb tt tt tt xx .W .O .R .K .S xx tt "
      "tt cc cc cc cc cc cc cc cc cc cc cc cc xx tt tt bb .. .. .. bb ta tt tt ws cc cc cc cc cc xx tt "
      "tt xx xx xx x! @@ .I .N .N @@ x! xx xx xx tt tt td .. .. .. .. .. bb ta sA cc cc cc cc cc xx tt "
      "tt xx cc cc xx cc cc cc cc cc xx cc cc xx tt tt bb .. .. .. .. .. .. tD cc cc cc cc cc __ xx tt "
      "tt xx cc cc cc cc cc cc cc cc cc cc cc xx tt tt td .. .. .. .. .. bb t3 xx cc cc cc cc !! xx tt "
      "tt xx xx xx x! cc cc cc cc cc x! xx xx xx tt tt bb .. .. .. bb t3 tt tt ws cc cc cc cc cc xx tt "
      "tt xx cc cc cc cc cc cc cc cc cc cc cc xx tt tt td .. .. .. tb tt tt tt xx xx cc xx cc xx xx tt "
      "tt xx cc cc xx cc cc cc cc cc xx cc cc xx tt tt bb .. .. .. bb tt tt tt xx cc cc x! cc cc xx tt "
      "tt xx xx xx xx ws x! cc x! ws xx xx xx xx tt tt td .. .. .. tb tt tt tt xx cc cc xx cc cc xx tt "
      "tt tt tt tt tt tc bb .. bb tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt xx xx xx xx xx xx xx tt "
      "tt tt tt tt tc sI .. .. .. ta tt tt tt tt tt tc .. .. .. .. .. ta tt tt tt tt tt tt tt tt tt tt "
      "bb te bb te bb .. .. .. .. .. bb te bb te bb .. .. .. .. .. .. .. bb te bb te bb te bb te bb ta "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "bb t7 bb t7 bb t7 bb t7 bb .. .. .. .. .. .. .. bb t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb t3 "
      "tt tt tt tt tt tt tt tt tt t5 .. .. .. .. .. t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
      "tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
      "tt xx xx xx xx xx xx xx tt tt td .. .. .. tb tt tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx tt "
      "tt xx @@ .D .R .Y @@ xx tt tt bb .. .. .. bb tt tt xx x! @@ .L .U .S .T .Y x! xx cc cc cc xx tt "
      "tt xx .G .O .O .D .S xx tt tt td .. .. .. .. ta tt ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx tt "
      "tt cc cc cc cc cc cc ws tt tt bb .. .. .. .. .. bb sT cc cc 00 cc cc 00 cc cc x! xx cc xx xx tt "
      "tt xx @@ @@ @@ @@ @@ xx tt tt td .. .. .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc cc xx tt "
      "tt xx cc cc cc cc cc sE bb .. .. .. .. .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc && xx tt "
      "tt xx cc cc cc cc cc cc .. .. .. .. .. .. .. .. bb xx cc cc 00 cc cc 00 cc cc x! cc cc && xx tt "
      "tt xx cc cc cc cc cc xx bb .. .. .. .. .. .. t3 tt ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx tt "
      "tt xx cc cc cc cc cc ws tt tt td .. .. .. bb tt tt xx x! @@ .J .U .G .S @@ x! xx cc cc cc xx tt "
      "tt xx xx xx xx xx xx xx tt tt bb .. .. .. tb tt tt xx xx xx xx xx xx xx xx xx xx xx ws xx xx tt "
      "tt tt tt tt tt tt tt tt tt tt t5 .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_trigrave
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    1 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    10 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    7 11)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    2 6)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    12 6)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    12 9)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    12 2)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    2 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_stun_wand)
          (list 3 t_xen_corp_scroll)
          (list 3 t_in_vas_por_ylem_scroll)
          (list 5 t_vas_mani_scroll)
        )
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    2 10)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    1 24)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    7 27)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    24 6)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    26 9)
    (list
      (kern-tag 'tiw-portcullis
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    28 9)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tiw-portcullis
          #f
          '()
        )
      ) ;; bind
    25 11)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    25 10)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_scratched_shield)
          (list 1 t_armor_plate)
          (list 1 t_iron_helm)
        )
        ;; hooks
        (list
        )
      )
    28 11)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 100 t_gold_coins)
          (list 3 t_dagger)
          (list 3 t_mace)
          (list 3 t_sword)
        )
        ;; hooks
        (list
        )
      )
    29 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    28 24)
    (list
      (kern-tag 'tlj-d-1
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            'tlj-d-2
            #f
            #f
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    17 25)
    (list
      (kern-tag 'tlj-d-2
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            'tlj-d-1
            #f
            #f
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    17 26)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    27 22)
    (list
      (bind
        (kern-mk-char
          'ch_jim
          "Jim"
          sp_human
          nil
          s_townsman
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'jim-conv
          sch_jim
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    18 23)
    (list
      (bind
        (kern-mk-char
          'ch_gwen
          "Gwen"
          sp_human
          nil
          s_gwen
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'gwen-conv
          sch_gwen
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    5 4)
    (list
      (bind
        (kern-mk-char
          'ch_chant
          "Chanticleer"
          sp_human
          nil
          s_chanticleer
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'chant-conv
          sch_chant
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          0
        )
      ) ;; bind
    18 23)
    (list
      (bind
        (kern-mk-char
          'ch_earl
          "Earl"
          sp_human
          nil
          s_townsman
          2
          0 10 5
          0 0
          0 0
          19 0
          9 9
          #f ;; dead?
          'earl-conv
          sch_earl
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    2 9)
    (list
      (bind
        (kern-mk-char
          'ch_miggs
          "Miggs"
          sp_human
          nil
          s_fat_townswoman
          2
          12 10 8
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'miggs-conv
          sch_miggs
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    27 25)
    (list
      (kern-tag 'trigrave-inn-room-1-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    4 7)
    (list
      (kern-tag 'trigrave-inn-room-2-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    4 9)
    (list
      (kern-tag 'trigrave-inn-room-3-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    10 9)
    (list
      (kern-tag 'trigrave-inn-room-4-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    10 7)
  ) ;; end of objects in p_trigrave
  (list ;; on-entry-hooks
    'lock-inn-room-doors
  )
  (list ;; edge entrances
    (list 0 31 31) ;; Northwest
    (list 1 12 31) ;; North
    (list 2 0 31) ;; Northeast
    (list 3 31 16) ;; West
    (list 4 16 16) ;; Here
    (list 5 0 16) ;; East
    (list 6 31 0) ;; Southwest
    (list 7 18 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_trigrave

(kern-load "warritrix.scm")

(kern-mk-place 'p_lost_halls_4 "Lost Halls South"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr .. .. .. ,, .. rr ,, .. ,, rr rr "
      "rr rr rr rr .. .. .. rr .. .. .. ,, .. ,, ,, ,, ,, ,, rr "
      "rr rr .. .. .. .. .. .. .. .. xx ,, ,, .. ,, ,, ,, ,, rr "
      "rr rr .. .. .. .. .. .. .. .. .. xx rr ,, ,, ,, ,, ,, rr "
      "rr .. .. .. bb .. .. .. .. .. .. xx rr rr rr ,, ,, ,, rr "
      "rr .. .. .. .. .. .. .. bb .. .. rr xx rr rr ,, bb ,, rr "
      "rr rr .. .. .. .. .. .. .. .. .. rr xx rr rr ,, ,, ,, rr "
      "rr rr .. .. .. .. .. .. .. .. rr rr rr xx rr rr ,, ,, ,, "
      "rr rr rr .. .. .. .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx ,, ,, "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ,, .. "
      "rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. rr rr .. rr .. .. .. .. rr "
      "rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr .. rr "
      "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. rr rr rr .. .. rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_lost_halls_1 "Lost Halls Entrance"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
            "rr xx xx rr .. rr rr .. .. rr rr rr rr rr rr rr rr rr rr "
            "rr xx cc cc .. .. .. .. .. .. [[ @@ ]] rr rr rr rr rr rr "
            "rr rr cc cc .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
            "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
            "rr .. .. .. .. .. .. .. .. .. .. && .. .. .. rr .. .. rr "
            "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
            "rr .. .. .. .. .. .. .. .. .. bb .. bb .. .. .. .. .. .. "
            "rr rr .. .. .. .. && .. .. .. .. .. .. .. .. .. .. .. ,, "
            "rr rr rr .. .. .. .. .. .. .. .. %3 %d rr rr xx .. ,, ,, "
            "rr rr rr .. .. .. .. .. .. %b %% %% rr rr xx rr rr ,, ,, "
            "rr rr rr .. .. .. .. .. rr rr ~1 ~~ rr xx rr ,, ,, ,, ,, "
            "rr rr rr rr .. .. rr rr rr rr -- -- xx rr ,, ,, ,, ,, rr "
            "rr rr rr rr .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, rr rr "
            "rr rr rr .. .. .. rr rr rr rr rr xx rr ,, ,, ,, ,, rr rr "
            "rr rr rr .. .. .. .. .. .. .. rr xx rr ,, ,, ,, ,, rr rr "
            "rr rr rr .. .. .. .. .. .. .. rr xx rr rr rr && rr rr rr "
            "rr rr rr rr .. .. rr rr .. .. .. rr rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr .. .. .. ,, rr ,, ,, ,, rr rr rr "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        (list
          (list
            (kern-mk-place 'p_lost_halls_2 "Lost Halls East"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                  "rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr "
                  "rr rr rr rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr "
                  "rr .. rr rr rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr "
                  "rr .. .. .. rr rr rr .. .. .. .. .. .. rr .. .. .. .. rr "
                  "rr .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. rr "
                  "rr .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. rr "
                  ".. .. xx xx .. .. .. .. .. .. .. .. rr rr .. rr rr .. rr "
                  ".. .. .X .. .O .. xx xx xx .. .. rr rr rr .. rr rr .. rr "
                  ",, ,, ,, ,, ,, ,, xx ,, ,, xx xx .. rr rr .. rr rr .. .. "
                  ".. ,, ,, ,, ,, ,, xx ,, ,, ,, ,, xx .. .. .. .. .. ,, rr "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, rr "
                  "rr rr ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx .. rr .. rr rr "
                  "rr rr rr ,, ,, ,, rr rr rr ,, ,, ,, rr xx rr .. .. .. rr "
                  "rr rr rr rr ,, ,, rr rr rr rr ,, ,, rr rr xx .. .. .. rr "
                  "rr rr tb td ,, ,, rr rr rr rr ,, ,, rr rr xx rr .. .. rr "
                  "rr tf t# .. .. ,, rr rr rr rr ,, ,, ,, rr xx rr rr .. rr "
                  "rr xx xx xx .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, xx rr .. rr "
                  "rr xx rr rr rr rr bb ,, ,, ,, ,, ,, rr rr rr xx xx rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              (list
                (list
                  (kern-mk-place 'p_lost_halls_3 "Lost Halls Keep"
                    nil ;; sprite
                    (kern-mk-map
                      nil                       19 19 pal_expanded
                      (list
                        "rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx rr "
                        "rr rr rr rr rr .. .. rr rr rr xx ,L ,A ,R ,D ,E ,R xx rr "
                        "rr rr rr .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr .. .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr rr .. .. .. .. bb rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr rr rr rr .. rr rr rr rr xx xx xx ,, ,, xx xx xx rr "
                        "rr rr rr ,, ,, ,, ,, ,, rr xx xx xx xx ,, ,, xx xx xx rr "
                        "rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr ,, ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr .. ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
                        "rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx xx rr "
                        "rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx rr rr "
                        "rr rr xx xx xx xx rr rr xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
                        "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
                        "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
                        "xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx rr rr "
                      )
                    )
                    #f #t #f #f
                    ;; subplaces
                    nil
                    (list
                      (list
                        (kern-mk-place 'p_lost_halls_6 "Lost Halls End"
                          nil ;; sprite
                          (kern-mk-map
                            nil                             19 19 pal_expanded
                            (list
                              "rr rr rr rr rr rr rr rr .. .. .. bb bb bb bb bb bb rr rr "
                              "rr rr rr rr rr rr rr t3 tt t5 .. bb .. .. .. bb t3 t5 rr "
                              "rr rr rr rr rr rr rr ta tt tc .. .. tf bb .. tC tt tc rr "
                              "rr rr rr rr rr rr rr rr te tB bb .. .. .. tC t3 tc rr rr "
                              "rr rr rr rr rr rr rr rr rr tb td tA .. bb tb tc rr rr rr "
                              "rr rr bb rr rr rr rr rr rr rr rr tf .. .. rr rr rr rr rr "
                              "rr rr rr rr rr rr rr rr rr rr rr .. bb .. rr rr rr rr rr "
                              "rr rr rr rr bb rr rr rr rr rr rr .. .. .. rr rr rr rr rr "
                              "rr rr rr rr rr bb rr rr ,R rr .. .. .. .. rr rr rr rr rr "
                              "rr rr rr rr rr rr rr rr bb tf .. bb .. .. %7 bb rr rr rr "
                              "rr rr rr rr rr rr rr rr .. .. .. .. bb .. %% %d bb rr rr "
                              "rr rr rr rr rr rr rr bb .. .. bb .. .. %3 %% tf %3 ,T rr "
                              "rr rr ,, xx xx rr rr bb .. bb tb t5 .. _b _1 _1 __ rr rr "
                              "rr ,, ,, ,, xx rr ,A %d .. .. t% te .. ~% __ __ __ _5 rr "
                              "rr ,, ,, ,, xx rr rr rr bb .. bb .. .. .. __ ~B ~D _4 rr "
                              "bb ,, ,, ,, xx rr rr rr rr tf %3 __ _1 bb __ __ __ _c rr "
                              "xx bb xx xx xx rr rr rr ,Q %% _3 __ __ __ __ __ bb rr rr "
                              "rr rr rr rr rr rr rr rr rr rr bb _8 _8 _8 _8 bb ,P rr rr "
                              "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                            )
                          )
                          #f #t #f #f
                          ;; subplaces
                          nil
                          (list
                            (list
                              (kern-mk-place 'p_lost_halls_5 "Lost Halls Feast Room"
                                nil ;; sprite
                                (kern-mk-map
                                  nil                                   19 19 pal_expanded
                                  (list
                                    "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
                                    "rr .. .. .. .. rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
                                    "rr .. rr bb .. .. rr .. .. .. .. rr rr rr rr rr rr rr rr "
                                    "rr .. rr rr .. .. rr .. .. .. .. rr rr rr rr rr rr rr rr "
                                    "rr .. .. .. .. rr rr .. .. .. rr rr rr rr cc cc rr rr rr "
                                    "rr rr .. rr rr rr rr .. .. .. rr ,, cc cc cc cc cc cc rr "
                                    "rr .. .. .. rr rr rr .. .. .. .. ,, cc cc cc cc cc cc rr "
                                    ".. .. bb .. rr rr bb .. .. .. .. pp cc cc 00 00 cc cc rr "
                                    ".. .. .. .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
                                    ".. .. .. .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
                                    ".. .. bb .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
                                    ".. .. .. .. rr rr bb .. .. .. .. ,, cc cc 00 00 cc cc rr "
                                    "rr .. .. rr rr rr rr .. .. .. .. pp cc cc 00 00 cc cc rr "
                                    "rr rr .. .. .. .. rr rr .. .. .. ,, cc cc cc cc cc cc rr "
                                    "rr rr .. rr rr .. rr rr rr .. rr ,, cc cc cc cc cc cc rr "
                                    "rr rr .. rr bb .. rr rr rr rr rr rr rr bb cc cc rr rr rr "
                                    "rr rr .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr "
                                    "rr rr rr .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                                    "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                                  )
                                )
                                #f #t #f #f
                                ;; subplaces
                                nil
                                nil ;; neighbors
                                (list ;; objects in p_lost_halls_5
                                  (list
                                    (bind
                                      (kern-mk-obj t_monman 1
                                        ;; hooks
                                        (list
                                        )
                                      )
                                      (list
                                        'monman
                                        (list
                                          0
                                          0
                                          0
                                          -1
                                          6
                                          -1
                                        )
                                      )
                                    ) ;; bind
                                  0 0)
                                  (list
                                    (bind
                                      (kern-mk-obj t_spawn_pt 1
                                        ;; hooks
                                        (list
                                        )
                                      )
                                      (list
                                        'spawn-pt
                                        'giant-spider
                                      )
                                    ) ;; bind
                                  0 9)
                                  (list
                                    (bind
                                      (kern-mk-obj t_spawn_pt 1
                                        ;; hooks
                                        (list
                                        )
                                      )
                                      (list
                                        'spawn-pt
                                        'gint-warrior
                                      )
                                    ) ;; bind
                                  13 8)
                                  (list
                                    (bind
                                      (kern-mk-obj t_spawn_pt 1
                                        ;; hooks
                                        (list
                                        )
                                      )
                                      (list
                                        'spawn-pt
                                        'gint-warrior
                                      )
                                    ) ;; bind
                                  16 10)
                                  (list
                                    (bind
                                      (kern-mk-obj t_spawn_pt 1
                                        ;; hooks
                                        (list
                                        )
                                      )
                                      (list
                                        'spawn-pt
                                        'gint-mage
                                      )
                                    ) ;; bind
                                  14 13)
                                ) ;; end of objects in p_lost_halls_5
                                (list ;; on-entry-hooks
                                  'on-entry-to-dungeon-room
                                )
                                (list ;; edge entrances
                                  (list 0 18 18) ;; Northwest
                                  (list 1 9 18) ;; North
                                  (list 2 0 18) ;; Northeast
                                  (list 3 18 9) ;; West
                                  (list 4 9 9) ;; Here
                                  (list 5 0 9) ;; East
                                  (list 6 18 0) ;; Southwest
                                  (list 7 9 0) ;; South
                                  (list 8 0 0) ;; SoutheastUp
                                )
                              ) ;; end of place p_lost_halls_5

                            3)
                          ) ;; end neighbors of p_lost_halls_6
                          (list ;; objects in p_lost_halls_6
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      1
                                      't_rune_l
                                    )
                                    (list
                                      1
                                      't_armor_chain_4
                                    )
                                    (list
                                      1
                                      't_chain_coif_4
                                    )
                                    (list
                                      1
                                      't_sword_4
                                    )
                                    (list
                                      1
                                      't_shield_4
                                    )
                                    (list
                                      1
                                      't_warritrix_orders
                                    )
                                  )
                                )
                              ) ;; bind
                            12 12)
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      4
                                      't_bolt
                                    )
                                    (list
                                      1
                                      't_food
                                    )
                                  )
                                )
                              ) ;; bind
                            14 2)
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      5
                                      't_bolt
                                    )
                                    (list
                                      1
                                      't_torch
                                    )
                                  )
                                )
                              ) ;; bind
                            13 10)
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      1
                                      't_food
                                    )
                                    (list
                                      4
                                      't_arrow
                                    )
                                  )
                                )
                              ) ;; bind
                            11 7)
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      1
                                      't_gold_coins
                                    )
                                    (list
                                      4
                                      't_arrow
                                    )
                                    (list
                                      2
                                      't_bolt
                                    )
                                  )
                                )
                              ) ;; bind
                            12 8)
                            (list
                              (bind
                                (kern-mk-obj t_corpse 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  (list
                                    (list
                                      1
                                      't_heal_potion
                                    )
                                  )
                                )
                              ) ;; bind
                            8 10)
                            (list
                              (bind
                                (kern-mk-obj t_monman 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'monman
                                  (list
                                    0
                                    0
                                    0
                                    -1
                                    6
                                    -1
                                  )
                                )
                              ) ;; bind
                            0 0)
                            (list
                              (bind
                                (kern-mk-obj t_spawn_pt 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'spawn-pt
                                  'yellow-slime
                                )
                              ) ;; bind
                            16 11)
                          ) ;; end of objects in p_lost_halls_6
                          (list ;; on-entry-hooks
                            'on-entry-to-dungeon-room
                          )
                          (list ;; edge entrances
                            (list 0 18 18) ;; Northwest
                            (list 1 9 18) ;; North
                            (list 2 0 18) ;; Northeast
                            (list 3 18 9) ;; West
                            (list 4 9 9) ;; Here
                            (list 5 0 9) ;; East
                            (list 6 18 0) ;; Southwest
                            (list 7 9 0) ;; South
                            (list 8 0 0) ;; SoutheastUp
                          )
                        ) ;; end of place p_lost_halls_6

                      7)
                    ) ;; end neighbors of p_lost_halls_3
                    (list ;; objects in p_lost_halls_3
                      (list
                        (bind
                          (kern-mk-obj t_corpse 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            (list
                              (list
                                1
                                't_mana_potion
                              )
                              (list
                                1
                                't_food
                              )
                            )
                          )
                        ) ;; bind
                      5 2)
                      (list
                        (bind
                          (kern-mk-obj t_corpse 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            (list
                              (list
                                1
                                't_torch
                              )
                              (list
                                3
                                't_arrow
                              )
                            )
                          )
                        ) ;; bind
                      7 3)
                      (list
                        (bind
                          (kern-mk-obj t_monman 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'monman
                            (list
                              0
                              0
                              0
                              -1
                              6
                              -1
                            )
                          )
                        ) ;; bind
                      0 0)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'green-slime
                          )
                        ) ;; bind
                      9 18)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'gint-warrior
                          )
                        ) ;; bind
                      13 9)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'gint-mage
                          )
                        ) ;; bind
                      14 10)
                    ) ;; end of objects in p_lost_halls_3
                    (list ;; on-entry-hooks
                      'on-entry-to-dungeon-room
                    )
                    (list ;; edge entrances
                      (list 0 18 18) ;; Northwest
                      (list 1 9 18) ;; North
                      (list 2 0 18) ;; Northeast
                      (list 3 18 9) ;; West
                      (list 4 9 9) ;; Here
                      (list 5 0 9) ;; East
                      (list 6 18 0) ;; Southwest
                      (list 7 9 0) ;; South
                      (list 8 0 0) ;; SoutheastUp
                    )
                  ) ;; end of place p_lost_halls_3

                5)
                (list
                  p_lost_halls_5
                7)
              ) ;; end neighbors of p_lost_halls_2
              (list ;; objects in p_lost_halls_2
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                0 9)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'gint-warrior
                    )
                  ) ;; bind
                9 9)
                (list
                  (bind
                    (kern-mk-obj t_corpse 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      (list
                        (list
                          3
                          't_gold_coins
                        )
                        (list
                          1
                          't_torch
                        )
                      )
                    )
                  ) ;; bind
                12 1)
                (list
                  (bind
                    (kern-mk-obj t_corpse 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      (list
                        (list
                          5
                          't_arrow
                        )
                        (list
                          1
                          't_bolt
                        )
                        (list
                          2
                          't_gold_coins
                        )
                      )
                    )
                  ) ;; bind
                14 2)
              ) ;; end of objects in p_lost_halls_2
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_lost_halls_2

          5)
        ) ;; end neighbors of p_lost_halls_1
        (list ;; objects in p_lost_halls_1
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'giant-spider
              )
            ) ;; bind
          9 18)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'troll
              )
            ) ;; bind
          7 8)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'cave-goblin-slinger
              )
            ) ;; bind
          18 8)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'green-slime
              )
            ) ;; bind
          2 2)
          (list
            (bind
              (kern-mk-obj t_ladder_up 1
                ;; hooks
                (list
                )
              )
              (list
                'p_shard
                39
                75
              )
            ) ;; bind
          2 2)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_arrow
                  )
                )
              )
            ) ;; bind
          5 17)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    5
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          9 10)
        ) ;; end of objects in p_lost_halls_1
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_lost_halls_1

    1)
    (list
      p_lost_halls_5
    5)
  ) ;; end neighbors of p_lost_halls_4
  (list ;; objects in p_lost_halls_4
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    5 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    5 13)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'troll
        )
      ) ;; bind
    9 0)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
            (list
              5
              't_bolt
            )
          )
        )
      ) ;; bind
    7 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
            (list
              1
              't_arrow
            )
          )
        )
      ) ;; bind
    7 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              3
              't_bolt
            )
            (list
              1
              't_gem
            )
          )
        )
      ) ;; bind
    8 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_torch
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    5 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_bolt
            )
          )
        )
      ) ;; bind
    4 11)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_lost_halls_4
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_lost_halls_4

p_lost_halls_1
p_lost_halls_6
p_lost_halls_3
p_lost_halls_2
p_lost_halls_5
(kern-load "zane.scm")

(kern-load "enchanter.scm")

(kern-mk-place 'p_enchanters_tower "Enchanters Tower"
  s_keep ;; sprite
  (kern-mk-map
    nil     31 31 pal_expanded
    (list
      "tt tt bb te bb || || tt %a %% %% %% %% %c .. .. .. %a %% %% %% %% %% %% %% %% %% %% %% %% %% "
      "bb te .. t% tb tt || || tt t5 %% %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. bb .. %a %% "
      "td t# .. .. .. .. bb || || tt %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% "
      "bb .. .. .. .. .. .. ta || || %a %% %% bb .. .. .. bb .. bb .. bb .. bb .. bb .. .. .. bb %% "
      "|| .. .. .. .. .. .. .. bb || t5 %% %% %d .. .. .. %b %% %% %% %% %% %% %% %% %5 .. .. .. %% "
      "|| bb .. .. .. .. .. .. .. || || %a %c xx w+ cc w+ xx %e tb tt tt tt tt t5 %a %% bb .. bb %% "
      "|| || .. .. .. .. && .. .. ta || xx xx xx ,, cc ,, xx xx xx || || || tt tt t5 %% .. .. .. %% "
      "tc bb .. .. bb .. .. .. .. xx w+ xx && xx ,, cc ,, xx && xx xx xx || || || tt %% bb .. bb %% "
      "%5 .. .. || || tA .. .. xx xx ,, ,, ,, xx x! cc x! xx ,, ,, xx xx ?? xx || tt %% .. .. .. %% "
      "%% bb .. bb || t5 tA xx xx ,, ,, ,, ,, xx ,, cc ,, ,, ,, ,, xx ,, ,, xx || tt %% bb .. bb %% "
      "%% .. .. || || tt td xx ,, ,, 00 ,, ,, xx ,, cc ,, xx xx xx xx xx xx xx || tc %% .. .. .. %% "
      "%% bb .. bb || || xx xx ,, ,, 00 ,, ,, xx ,, cc ,, xx ,, ,, ,, ,, ,, xx xx %3 %% bb .. bb %% "
      "%% .. .. %f ta || xx ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, ,, && xx %a %% .. .. .. %% "
      "%c bb .. bb %f xx xx xx xx xx xx xx xx xx ,, cc ,, xx xx xx xx xx xx xx xx xx %e bb .. bb %a "
      ".. .. .. .. .. w+ ,, ,, ,, ,, xx ,, ,, x! ,, cc ,, x! ,, ,, xx ,, ,, ,, ,, w+ .. .. .. .. .. "
      ".. .. .. .. .. cc cc cc cc ,, xx ,, ,, ,, ,, cc ,, xx ,, ,, xx ,, cc cc cc cc .. .. .. .. .. "
      ".. .. .. .. .. w+ ,, ,, cc ,, xx ,, ,, xx ,, cc ,, xx ,, ,, xx ,, cc ,, ,, w+ .. .. .. .. .. "
      "%5 bb .. bb %7 xx xx x! cc x! xx xx xx xx ,, cc ,, xx xx xx xx x! cc x! xx xx %7 bb .. bb %3 "
      "%% %% %% %% %% %5 w+ ,, cc ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, cc ,, w+ t7 %a %% %% %% %% "
      "%% %c .. %a %% %% xx xx cc ,, pp ,, ,, pp ,, cc ,, pp ,, ,, pp ,, cc xx xx tt tt tt tt tt tt "
      "%% .. .. .. %% %% %5 w+ cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx t3 || || || || || tt "
      "%% %5 .. %3 %% %% %% xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || || bb te bb || || "
      "%% %% %% %% %% %% %% %5 xx xx x! ,, ,, pp ,, cc ,, pp ,, ,, x! xx xx || || tt td t& tb tt || "
      "%% %c t3 tt tt t5 tA %a %5 xx xx xx ,, ,, ,, cc ,, ,, ,, xx xx xx tb || || bb .. .. .. bb || "
      "%c t3 tL ~3 ~5 tJ t5 tA %a %% %5 xx w+ xx xx cc xx xx w+ xx %3 %% ~5 ta || td .. ++ .. tb || "
      "tt tL ~3 -- -- ~5 tJ t5 .. %% %% %% %5 xx ,, cc ,, xx %3 %% %% ~b ~~ -d || bb .. .. .. bb || "
      "tt ~3 -- __ __ -- ~5 tt .. %% %% %% %% xx ,, cc ,, xx %a %% %% %% ~c %% || || td .. tb || || "
      "tt ~a -- __ __ -- ~c tt .. %% %% %% %% xx w+ cc w+ xx .. bb .. bb %% %c || || bb .. bb || || "
      "tt tH ~a -- -- ~c tG tc .. %e .. %a %% .. .. .. .. .. .. .. .. .. == .. tt tc tA .. t3 || || "
      "%5 ta tH ~a ~c tG tc t# %7 .. .. .. %% bb .. .. .. bb .. bb .. bb %% %5 || bb t7 bb || || || "
      "%% %5 tt tt tt tt t# %3 %% %5 .. %3 %% %5 .. .. .. %3 %% %% %% %% %% %% || || || || || || || "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_enchanters_tower
    (list
      (kern-mk-obj t_doom_staff 1
        ;; hooks
        (list
        )
      )
    19 14)
    (list
      (kern-mk-obj t_xen_corp_scroll 1
        ;; hooks
        (list
        )
      )
    19 15)
    (list
      (kern-mk-obj t_an_tym_scroll 1
        ;; hooks
        (list
        )
      )
    19 16)
    (list
      (kern-mk-obj t_vas_rel_por_scroll 1
        ;; hooks
        (list
        )
      )
    18 14)
    (list
      (kern-mk-obj t_in_vas_por_ylem_scroll 1
        ;; hooks
        (list
        )
      )
    18 15)
    (list
      (kern-mk-obj t_gold_coins 500
        ;; hooks
        (list
        )
      )
    18 16)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    21 11)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    19 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 24)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    22 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    8 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    13 12)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    17 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    17 12)
    (list
      (bind
        (kern-mk-char
          'ch_enchanter
          "Enchanter"
          sp_human
          oc_wizard
          s_companion_wizard
          2
          0 5 0
          10 2
          20 5
          18 0
          10 8
          #f ;; dead?
          'enchanter-conv
          sch_enchanter
          nil
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 10 t_food)
              (list 100 t_arrow)
              (list 1 t_dagger)
              (list 1 t_sword)
              (list 5 t_torch)
              (list 5 t_cure_potion)
              (list 5 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
          )
        )
        (list
          #f
          (list
            #f
            #f
            #f
          )
          (list
            #f
            #f
            #f
          )
          (list
            #f
            #f
            #f
          )
          (list
            #f
            #f
            #f
          )
        )
      ) ;; bind
    18 11)
    (list
      (bind
        (kern-mk-char
          'ch_zane
          "Zane"
          sp_human
          oc_ranger
          s_companion_ranger
          2
          1 0 1
          1 1
          0 0
          18 0
          10 8
          #f ;; dead?
          'zane-conv
          sch_zane
          nil
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 10 t_food)
              (list 100 t_arrow)
              (list 1 t_dagger)
              (list 1 t_sword)
              (list 5 t_torch)
              (list 5 t_cure_potion)
              (list 5 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    4 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    5 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    25 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 27)
    (list
      (kern-tag 'enchtwr-port-4
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    13 15)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'enchtwr-port-4
          #f
          '()
        )
      ) ;; bind
    21 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 10 sulphorous_ash)
          (list 10 ginseng)
          (list 10 garlic)
          (list 10 spider_silk)
          (list 10 blood_moss)
          (list 10 black_pearl)
          (list 10 nightshade)
          (list 10 mandrake)
        )
        ;; hooks
        (list
        )
      )
    11 14)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'spike-trap
        ;; contents
        (list
          (list 10 t_heal_potion)
          (list 10 t_mana_potion)
          (list 5 t_cure_potion)
          (list 5 t_poison_immunity_potion)
          (list 1 t_inv_potion)
        )
        ;; hooks
        (list
        )
      )
    11 15)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'lightning-trap
        ;; contents
        (list
          (list 100 t_gold_coins)
        )
        ;; hooks
        (list
        )
      )
    11 16)
  ) ;; end of objects in p_enchanters_tower
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 30 30) ;; Northwest
    (list 1 15 30) ;; North
    (list 2 0 30) ;; Northeast
    (list 3 30 15) ;; West
    (list 4 15 15) ;; Here
    (list 5 0 15) ;; East
    (list 6 30 0) ;; Southwest
    (list 7 15 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_enchanters_tower

(kern-load "shroom.scm")

(kern-load "gen.scm")

(kern-load "doris.scm")

(kern-load "deric.scm")

(kern-load "jorn.scm")

(kern-load "abe.scm")

(kern-load "kama.scm")

(kern-load "abigail.scm")

(kern-mk-place 'p_green_tower_lower "Beneath Green Tower"
  nil ;; sprite
  (kern-mk-map
    nil     22 38 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr x! rr rr rr .. .. .. rr rr rr "
      "rr cc cc cc cc cc cc cc cc cc rr rr rr rr rr .. .. .. .. .. rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr rr rr .. .. .. rr .. .. .. rr "
      "rr rr rr rr cc cc cc rr rr rr rr rr rr rr rr .. rr rr rr .. rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr x! rr .. .. .. rr .. .. .. rr "
      "rr cc cc cc cc cc cc cc cc cc rr rr rr rr rr .. rr rr rr .. rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr rr rr .. .. .. rr .. .. .. rr "
      "rr rr rr rr cc cc cc rr rr rr rr rr rr rr rr .. rr rr rr .. rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr x! rr .. .. .. rr .. .. .. rr "
      "rr cc cc cc cc cc cc cc cc cc rr rr rr rr rr .. rr rr rr .. rr rr "
      "rr cc cc rr cc cc cc rr cc cc rr rr rr rr .. .. .. rr .. .. .. rr "
      "rr rr rr rr cc cc cc rr rr rr rr rr rr rr rr .. rr rr rr .. rr rr "
      "rr rr rr rr cc cc cc cc cc cc cc rr rr rr .. .. .. rr .. .. .. rr "
      "rr rr rr rr cc cc cc cc cc cc cc cc rr rr rr .. .. .. .. .. rr rr "
      "rr rr rr rr rr cc cc cc cc cc cc cc rr .. .. .. .. .. .. .. .. rr "
      "rr rr rr rr rr rr rr rr xx x! cc x! xx .. rr ,C ,R ,Y ,P ,T rr rr "
      "xx xx xx xx xx xx xx xx xx cc cc cc xx ?? xx xx xx xx xx xx xx rr "
      "xx xx ,T ,A ,L ,O ,S xx xx cc cc cc xx cc cc _! x! _! cc cc xx rr "
      "xx x! cc cc cc cc cc x! xx cc cc cc xx cc cc -- _! -- cc cc xx rr "
      "xx cc cc cc cc cc cc cc xx cc cc cc cc cc cc -- -- -- cc cc xx rr "
      "xx cc cc cc aa cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rr "
      "xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rr "
      "xx x! cc cc cc cc cc x! xx cc cc cc xx ,C ,I ,S ,T ,E ,R ,N xx rr "
      "xx xx xx xx cc xx xx xx x! cc cc cc x! xx xx xx xx xx xx xx xx xx "
      "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
      "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
      "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
      "xx xx xx xx cc xx xx xx x! cc cc cc x! xx xx xx cc xx xx xx xx xx "
      "xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc xx rr "
      "xx cc cc cc cc cc cc cc xx cc cc cc xx cc cc cc cc cc cc cc ?? rr "
      "xx xx cc cc cc x! cc cc xx cc cc cc xx cc cc x! cc cc cc xx xx rr "
      "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
      "rr xx xx xx cc cc cc cc xx cc cc cc xx cc cc cc cc xx xx xx rr rr "
      "rr rr rr xx xx xx cc cc xx cc cc cc xx cc cc xx xx xx rr rr rr rr "
      "rr rr rr rr rr xx xx xx xx cc cc cc xx xx xx xx rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_green_tower "Green Tower"
        s_town ;; sprite
        (kern-mk-map
          nil           64 64 pal_expanded
          (list
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || |C rr rr rr rr rr rr rr || || || || || || || || || || tt td .. /7 .. tb tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| || rr rr rr rr rr || || || rr rr rr && cc cc cc cc rr || || || || || || || || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || xx xx xx xx xx xx xx xx xx tt tt || || || || || "
            "|| || rr cc cc cc rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || || tt tt td .. /7 .. tb tt || || || || || || || || || || || |C xx .. .. .. .. .. .. .. xx tt tt tt || || || || "
            "|| || rr cc cc cc rr || || || rr cc cc cc cc cc cc cc rr || || || || || || || || || tt tt bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
            "|| || rr cc cc cc rr || || || rr cc rr cc cc cc rr rr rr || || || || || || || || tt tt tt td .. /7 .. bb t3 || || || || || || || || || |% /7 tf sR @@ @@ @@ @@ @@ @@ @@ xx tt tt tt || || || || "
            "|| || rr rr cc rr rr || || || rr cc rr cc cc cc cc cc rr || || || || || || || || tt tt tt bb .. /7 .. t3 tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. .. xx tt tt || || || || || "
            "|| || || |% /7 |B || || || || |D /7 rr rr rr rr rr rr rr || || || || || || || tt tt tt tt td .. /7 .. ta tt || || || || || || || || || || /7 t7 xx .S .H .R .O .O .M .S xx || || || || || || || "
            "|| || || || /8 /d /d /d /1 /d /d /a |# || || || || || || || || || || || || || tt tt tt tt bb .. /7 .. bb tt || || || || || || || || || tt /7 tt xx xx xx xx xx xx xx xx xx || || || || || || || "
            "|| || || || || || || |% /7 |# || || || || || || || || || || || || || || || tt tt tt tt tt td .. /7 .. tb tt || || || || || || || || tt tt /7 te xx .. .. .. xx .. .. .. xx || || || || || || || "
            "|| || |C rr rr rr |A || /7 |A || || || || || || || || || || || || || || || tt tt tt tt tt bb .. /7 .. bb tt || || || || || || || || tt tt /8 /d .. .. .. .. ?? .. .. .. ?? tt tt tt tt tt tt tt "
            "|| rr rr rr cc rr rr |E /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt td .. /7 .. tb tt || || || || tt tt tt tt tt tt tt t5 xx .. .. tf xx .. .. .. xx || || || || || || || "
            "|| rr cc cc cc cc rr rr /7 |# || || || || || || || || |% /7 || || || || tt tt tt tt tt tt bb .. /7 .. bb tt || || || || tt t& ta tt tt || || || xx xx ws xx xx xx .. xx xx || || || || || || || "
            "|| rr cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt td .. /7 .. tb tt || tt tt tt tt tA tD tt || || || || || || || || || || || || || || || || || || || || "
            "|| rr rr cc cc cc rr rr |# || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt bb .. /7 .. bb tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
            "|| |% rr rr && rr rr |# tt || || || tt tc bb || || tt || /7 || tt tt tt tt tt tt tt tt tt td .. /7 .. tb tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
            "|| || |% rr rr rr |# tt tt tt || tt tc t& tb || || tt bb /7 bb .. bb te bb te bb te bb te t# .. /7 .. t% te bb te bb ta tc bb te bb te bb ta tt || || || || || || || || || || || || || || || || "
            "|| || || tt || || || || tt || tt tt tB .. tD tt || tc .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb tt || || || || || || || || || || || || || || || "
            "|| || tt tt tt || || || || || || tt t5 tE t3 || || bb .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
            "|| || || tt || || || || || tt tt tt tt tt tt || || td .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. ta tt || || || || || || || || || || || || || || || "
            "|| || || || || || || tt tt tt tt tt tt tt || || || bb .. /7 .. .. .. bb t7 bb t7 bb t7 .. .. .. /7 .. .. .. bb t3 t5 bb t7 bb tf tA .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || || tt tt tc t& ta tt tt || || || || td .. /7 .. .. t3 tt tt tt tt tt tt bb .. .. /7 .. .. bb t3 tt tt tt tt t5 bb t7 .. /7 .. tb tt || || || || || || || || || || || || || || || "
            "|| || || || || bb tt tc t# .. t% ta tt tt || || || bb .. /7 .. bb tt || || || || || tc xx w+ xx cc xx w+ xx ta || || || || || tt tc .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt tB .. .. .. tD tt tt || || || || .. /7 .. tb tt || || || |C xx w+ xx cc cc cc cc cc xx xx xx |A || || || tt bb .. /7 .. tb tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt t5 tA .. tC t3 tt tt || || || bb .. /7 .. bb tt || |C xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx |A || tt td .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || tt tt tt t5 tE t3 tt tt || || || || || .. /7 .. tb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. t3 tt || || || || || || || || || || || || || || || "
            "|| || tt tt tt tt || tt tt tt tt bb || || tt tt || bb .. /7 .. bb tt |C xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |A tt td .. /7 .. ta tt || || || || || || || || || || || || || || || "
            "|| || tt || || || || || tt tt tt || || || || || || td .. /7 .. tb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || tt "
            "tt tt tt || || || || || || || || || || || || || tt bb .. /7 .. bb te xx cc cc cc xx xx xx || .. cc .. tb || xx xx cc cc cc xx ta td .. /7 .. tb tt || || || || || || || || || || || || || || tt "
            "bb ta tt tt tt tt tt tt tt tt tt tt tt tt tt tt tc .. .. /7 .. tf bb xx cc cc cc x! || || || .. cc .. t% ta || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || || || || tt "
            ".. .. bb te bb te bb te bb te bb te bb te bb bb .. .. .. /7 .. .. .. w+ cc cc cc xx te t# bb .. cc .. bb t% te xx cc cc cc w+ .. .. .. /7 .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
            "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
            ".. .. bb t7 bb t7 bb t7 bb t7 bb t7 bb t3 tt t5 bb .. .. /7 .. .. .. w+ cc cc cc xx t7 tA bb .. cc .. bb tC t7 xx cc cc cc w+ .. .. .. /7 .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
            "bb t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 .. /7 .. .. bb xx cc cc cc x! || t5 tA .. cc .. tC t3 || x! cc cc cc xx bb .. .. /7 .. bb tt || || || || || || || || || || || tt tt tt tt "
            "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tc .. /7 .. .. t7 xx cc cc cc xx xx || td .. cc .. tb || xx xx cc cc cc xx t7 .. .. /7 .. tb tt || || || || || || || || || || || || || || tt "
            "tt tt || || || || tt tt tt tt tt tt tt tt || || tt bb .. /7 .. bb tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || tt tt tt tt tt tt tt || || tt td .. /7 .. tb tt |% xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |# tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || tt tt tt tt tt tt || || tt bb .. /7 .. bb tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt td .. /7 .. tb tt || |% xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx |# || tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt || || || |% xx w+ xx cc cc cc cc cc xx xx xx |# || || || tt bb .. /7 .. bb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || rr rr tt td .. /7 .. tb tt || || || || || t5 xx w+ xx cc xx w+ xx t3 || || || || tt tt td .. /7 .. tb tt || || || || || || || || || || || || || || || "
            "|| || || || || || || || || || || || || || || || tt bb .. /7 .. bb tt tt tt tt tt tt tc bb .. .. /7 .. .. bb ta tt tt tt tt tt tt bb .. /7 .. bb tt tt tt tt tt tt tt || || || || || || || || || "
            "|| || || || || || || tt tt tt tt || || || || || tt td .. /7 .. .. te bb te bb te bb .. .. .. .. /7 .. .. .. .. bb te bb te bb te .. .. /7 .. .. te bb te bb te bb tt || || || || || || || || || "
            "|| || || || || tt rr tt rr rr tt rr rr |A || || tt bb .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. t% tb tt || || || || || || || || || "
            "|| || || rr rr tt tt tt || || tt t5 rr rr rr || tt td .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 .. bb tt || || || || || || || || || "
            "|| || || rr |# || || tt tt tt || tt tt |% rr || || bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tb tt || || || || || || || || || "
            "|| || || || tt tt tt tc t# t% ta || tt tt || rr rr || || t5 bb t7 bb t7 bb t7 bb t7 bb t7 bb .. /7 .. t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb tf bb tb td bb /4 /2 bb ta || || || || || || || || || "
            "|| rr rr || tt tt |C t# .. bb t% tt tt tt tt |% rr rr || tt tt tt tt tt tt tt tt tt tt tt t5 .. /7 .. tt tt tt tt tt tt tt tt tt tt tt xx xx xx xx ws sT cc cc sI ws xx xx xx xx || || || || || "
            "|| || || || tt tc t# .. .. .. .. te t& ta tt || |% rr || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
            "|| tt tt tt tc t# .. .. .. .. .. .. .. t% ta tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
            "|| tt tt tt t# bb .. .. .. .. .. .. .. bb t% tt tF ta tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx || || || || || "
            "|| || || tt tA .. .. .. .. aa .. .. .. .. tC tt || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || |C xx cc cc 00 cc cc && && cc cc 00 cc cc xx |A || || || || "
            "|| rr || tt tt tt t5 .. .. .. .. .. .. t3 tt || || rr || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || xx xx xx cc cc 00 cc cc cc cc cc cc 00 cc cc xx xx xx || || || "
            "|| rr |A || || tF tt .. .. .. .. .. .. tt tt || rr || || |C rr rr tt rr rr |A || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
            "|| rr rr || tt tt tt bb .. .. .. bb .. tt tt |C rr || || rr rr tb tt td rr rr || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
            "|| |% rr || tt tt tt tt t5 tA .. .. tC tt || rr rr || || rr tf .! .! .! tf rr || || || tt tt .. /7 .. tt || || || || || || || |C xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx |A || || "
            "|| || || || || || || || tt tt tt tt tt || tt rr |# || || rr .! .! && .! .! rr || || || tt tt .. /7 .. || || || || || || || || xx xx cc xx xx @@ @@ @@ @@ @@ @@ @@ @@ @@ @@ xx xx cc xx xx || || "
            "|| || || rr rr |A || || tt tt tt || tt tt || rr || || || rr tf .! .! .! .! rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || "
            "|| || || |% rr rr || rr rr rr || rr rr rr rr || || || || rr rr .! .! tb t5 rr || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx .W .H .I .T .E @@ .S .T .A .G xx cc cc cc xx || || "
            "|| || || || || || || || || || || || || || || || || || || |% rr rr rr rr tt || || || tt tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx || || "
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tc .. /7 .. ta tt || || || || || || xx xx xx xx xx |# || || || || || || || || |% xx xx xx xx xx || || "
            "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt t# .. /7 .. t% tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "
          )
        )
        #f #f #f #f
        ;; subplaces
        nil
        nil ;; neighbors
        (list ;; objects in p_green_tower
          (list
            (bind
              (kern-char-force-drop                (kern-mk-char
                  'ch_jorn
                  "Jorn"
                  sp_human
                  oc_wrogue
                  s_brigand
                  2
                  0 0 0
                  0 0
                  0 0
                  15 0
                  7 5
                  #f ;; dead?
                  'jorn-conv
                  sch_jorn
                  'spell-sword-ai
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 1 t_skull_ring)
                      (list 67 t_gold_coins)
                      (list 3 t_picklock)
                      (list 3 t_heal_potion)
                    )
                    ;; hooks
                    (list
                    )
                  )
                  (list
                    t_leather_helm_2
                    t_dagger_4
                    t_sword_2
                    t_armor_leather_2
                  )
                  ;; hooks
                  (list
                  )
                )
              #t) ;; kern-char-force-drop
              '()
            ) ;; bind
          46 50)
          (list
            (bind
              (kern-mk-char
                'ch_abe
                "Abe"
                sp_human
                nil
                s_companion_wizard
                2
                0 0 0
                0 0
                0 0
                13 0
                5 3
                #f ;; dead?
                'abe-conv
                sch_abe
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              (list
                #f
              )
            ) ;; bind
          46 50)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #t
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          48 10)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          48 6)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          48 4)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #t
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          54 12)
          (list
            (kern-mk-obj t_bed 1
              ;; hooks
              (list
              )
            )
          51 9)
          (list
            (kern-mk-container
              t_chest
              ;; trap
              'burn-trap
              ;; contents
              (list
                (list 50 sulphorous_ash)
                (list 50 garlic)
                (list 50 ginseng)
                (list 50 blood_moss)
                (list 50 black_pearl)
                (list 50 spider_silk)
                (list 50 mandrake)
                (list 50 nightshade)
              )
              ;; hooks
              (list
              )
            )
          53 9)
          (list
            (kern-mk-container
              t_chest
              ;; trap
              'poison-trap
              ;; contents
              (list
                (list 10 t_cure_potion)
                (list 10 t_heal_potion)
                (list 10 t_mana_potion)
                (list 10 t_poison_immunity_potion)
                (list 1 t_inv_potion)
              )
              ;; hooks
              (list
              )
            )
          54 9)
          (list
            (kern-mk-container
              t_chest
              ;; trap
              'sleep-trap
              ;; contents
              (list
                (list 1 t_armor_leather)
                (list 1 t_sword)
                (list 1 t_shield)
                (list 1 t_leather_helm)
              )
              ;; hooks
              (list
              )
            )
          55 9)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          7 13)
          (list
            (kern-mk-obj t_bed 1
              ;; hooks
              (list
              )
            )
          2 13)
          (list
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              (list
                (list 100 t_arrow)
                (list 1 t_bow)
              )
              ;; hooks
              (list
              )
            )
          4 11)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          11 6)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          16 6)
          (list
            (kern-mk-obj t_bed 1
              ;; hooks
              (list
              )
            )
          17 4)
          (list
            (kern-mk-container
              t_chest
              ;; trap
              'burn-trap
              ;; contents
              (list
                (list 100 t_bolt)
                (list 1 t_crossbow)
                (list 1 t_shield)
                (list 2 t_cure_potion)
                (list 5 t_heal_potion)
              )
              ;; hooks
              (list
              )
            )
          17 6)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          51 49)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          52 49)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          57 59)
          (list
            (kern-tag 'white-stag-door
              (bind
                (kern-mk-obj t_door 1
                  ;; hooks
                  (list
                  )
                )
                (list
                  #f
                  0
                  '()
                  #f
                  #t
                  #f
                  (list
                    (list
                      's_closed_solid_wood_door_in_stone
                      #t
                      5
                    )
                    (list
                      's_open_door_in_stone
                      #f
                      0
                    )
                    (list
                      's_locked_solid_wood_door_in_stone
                      #t
                      5
                    )
                    (list
                      's_magically_locked_solid_wood_door_in_stone
                      #t
                      5
                    )
                  )
                )
              ) ;; bind
            ) ;; kern-tag
          44 58)
          (list
            (kern-mk-obj t_bed 1
              ;; hooks
              (list
              )
            )
          43 61)
          (list
            (kern-mk-obj t_bed 1
              ;; hooks
              (list
              )
            )
          58 61)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #t
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          59 58)
          (list
            (bind
              (kern-mk-obj t_lever 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                'gtl-portcullis-1
                #f
                '()
              )
            ) ;; bind
          29 28)
          (list
            (bind
              (kern-mk-obj t_ladder_down 1
                ;; hooks
                (list
                )
              )
              (list
                'p_green_tower_lower
                10
                26
              )
            ) ;; bind
          32 32)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          32 22)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          41 32)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          32 42)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          23 32)
          (list
            (bind
              (kern-mk-char
                'ch_shroom
                "Shroom"
                sp_human
                nil
                s_companion_druid
                2
                8 14 8
                0 0
                0 0
                13 0
                5 3
                #f ;; dead?
                'shroom-conv
                sch_shroom
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              (list
                #f
                #f
              )
            ) ;; bind
          51 9)
          (list
            (bind
              (kern-mk-char
                'ch_gen
                "Gen"
                sp_human
                oc_ranger
                s_companion_ranger
                2
                12 10 12
                25 5
                0 0
                53 812
                5 3
                #f ;; dead?
                'gen-conv
                sch_gen
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              (list
                #f
                #f
              )
            ) ;; bind
          2 13)
          (list
            (bind
              (kern-mk-char
                'ch_doris
                "Doris"
                sp_human
                nil
                s_townswoman
                2
                0 0 0
                0 0
                0 0
                11 0
                0 1
                #f ;; dead?
                'doris-conv
                sch_doris
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              '()
            ) ;; bind
          47 59)
          (list
            (bind
              (kern-mk-char
                'ch_abigail
                "Abigail"
                sp_forest_goblin
                oc_wrogue
                s_goblin_child
                2
                0 0 0
                0 0
                0 1
                9 128
                7 1
                #f ;; dead?
                'abigail-conv
                sch_abigail
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              '()
            ) ;; bind
          60 61)
          (list
            (bind
              (kern-mk-char
                'ch_kalc
                "Kalcifax"
                sp_human
                oc_wizard
                s_companion_wizard
                2
                0 5 0
                12 2
                1 1
                40 4096
                15 6
                #f ;; dead?
                'kalc-conv
                sch_kalc
                nil
                nil ;; inventory
                (list
                  t_staff
                )
                ;; hooks
                (list
                )
              )
              '()
            ) ;; bind
          46 50)
          (list
            (bind
              (kern-mk-char
                'ch_deric
                "Deric"
                sp_human
                nil
                s_companion_ranger
                2
                0 0 0
                0 0
                0 0
                3 0
                6 4
                #f ;; dead?
                'deric-conv
                sch_deric
                nil
                nil ;; inventory
                nil
                ;; hooks
                (list
                )
              )
              (list
                #f
              )
            ) ;; bind
          13 2)
        ) ;; end of objects in p_green_tower
        (list ;; on-entry-hooks
          'lock-inn-room-doors
        )
        (list ;; edge entrances
          (list 0 63 63) ;; Northwest
          (list 1 32 63) ;; North
          (list 2 0 63) ;; Northeast
          (list 3 63 32) ;; West
          (list 4 32 32) ;; Here
          (list 5 0 32) ;; East
          (list 6 63 0) ;; Southwest
          (list 7 32 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_green_tower

    9)
  ) ;; end neighbors of p_green_tower_lower
  (list ;; objects in p_green_tower_lower
    (list
      (bind
        (kern-mk-char
          'ch_amy
          "Amy"
          sp_human
          oc_wright
          s_companion_tinker
          1
          2 4 4
          25 5
          1 1
          17 55
          5 1
          #f ;; dead?
          'amy-conv
          nil
          nil
          nil ;; inventory
          (list
            t_leather_helm
            t_sling
            t_sword
            t_armor_leather
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    11 25)
    (list
      (bind
        (kern-mk-char
          'ch_kama
          "Kama"
          sp_forest_goblin
          oc_wrogue
          s_fgob_civilian
          2
          2 0 10
          25 5
          0 0
          57 1024
          12 4
          #f ;; dead?
          'kama-conv
          sch_kama
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
          #f
        )
      ) ;; bind
    8 1)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_crypt
          9
          3
        )
      ) ;; bind
    17 1)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              3
              't_arrow
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    14 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    14 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_gold_coins
            )
            (list
              1
              't_gold_coins
            )
            (list
              2
              't_arrow
            )
          )
        )
      ) ;; bind
    14 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              3
              't_gold_coins
            )
            (list
              2
              't_bolt
            )
            (list
              1
              't_arrow
            )
          )
        )
      ) ;; bind
    14 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              5
              't_arrow
            )
            (list
              1
              't_cure_potion
            )
            (list
              3
              't_gold_coins
            )
          )
        )
      ) ;; bind
    16 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    16 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_torch
            )
          )
        )
      ) ;; bind
    16 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    16 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_arrow
            )
            (list
              1
              't_mana_potion
            )
            (list
              3
              't_arrow
            )
          )
        )
      ) ;; bind
    18 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_arrow
            )
            (list
              1
              't_food
            )
            (list
              1
              't_gem
            )
          )
        )
      ) ;; bind
    18 7)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_bolt
            )
          )
        )
      ) ;; bind
    18 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
            (list
              1
              't_food
            )
            (list
              4
              't_bolt
            )
          )
        )
      ) ;; bind
    18 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_mana_potion
            )
          )
        )
      ) ;; bind
    20 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_food
            )
            (list
              1
              't_bolt
            )
          )
        )
      ) ;; bind
    20 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_bolt
            )
            (list
              1
              't_gold_coins
            )
          )
        )
      ) ;; bind
    20 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_arrow
            )
          )
        )
      ) ;; bind
    20 11)
    (list
      (kern-tag 'gtl-portcullis-1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    10 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    12 20)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    4 24)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    4 28)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    8 32)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    12 32)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    16 28)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    15 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    7 29)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    2 32)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    13 29)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    19 29)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            0
            21
            21
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_green_tower
          32
          32
        )
      ) ;; bind
    10 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    3 2)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    3 6)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    3 10)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    7 2)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    7 6)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    7 10)
    (list
      (bind
        (kern-mk-char
          nil
          "rat"
          sp_rat
          nil
          s_rat
          5
          0 0 0
          0 0
          0 0
          7 128
          1 1
          #f ;; dead?
          nil
          nil
          'rat-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      25
                      "1"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'rat
          #f
          #t
          '()
        )
      ) ;; bind
    16 3)
    (list
      (kern-mk-char
        'ch_wanderer
        "The Wanderer"
        sp_human
        oc_wanderer
        s_wanderer
        1
        5 5 5
        25 5
        1 1
        25 217
        5 1
        #f ;; dead?
        nil
        nil
        nil
        nil ;; inventory
        (list
          t_shield
          t_sword
        )
        ;; hooks
        (list
        )
      )
    10 26)
  ) ;; end of objects in p_green_tower_lower
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 21 37) ;; Northwest
    (list 1 11 37) ;; North
    (list 2 0 37) ;; Northeast
    (list 3 21 19) ;; West
    (list 4 11 19) ;; Here
    (list 5 0 19) ;; East
    (list 6 21 0) ;; Southwest
    (list 7 11 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_green_tower_lower

p_green_tower
(kern-mk-place 'p_mushroom_cave "Mushroom Cave"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr %3 %d rr rr tf .. .. .. tf rr rr rr {{ rr rr rr rr rr "
      "rr %% rr rr || t# .. .. .. t% || rr rr tf {A rr rr rr rr "
      "rr %% rr tf %7 bb .. .. .. bb %7 |% rr t% t7 rr rr rr rr "
      "rr %e rr %b %% %% %5 .. .. %b %% te rr .. te rr rr rr rr "
      "rr rr rr t7 %% bb %e .. .. bb %% %5 rr {c {{ rr rr rr rr "
      "rr .. |B || %% %d .. .. .. %b %% %% rr {{ {{ rr rr rr rr "
      "rr .. rr || %% bb .. %b %5 bb %a %c rr {{ {{ rr rr rr rr "
      "rr t7 rr te %% .. .. .. %a %d t3 t5 rr {5 {{ rr rr rr rr "
      "rr tt rr %b %c bb .. .. .. bb || |C rr {4 {{ rr rr rr rr "
      "rr te rr rr tf %f .. .. .. tb || rr rr .. t7 rr rr rr rr "
      "rr t% tf rr rr rr .. .. .. rr rr rr .. tC tt rr rr rr rr "
      "rr .. .. .. && rr rr .. rr rr .. .. tC t3 |C rr rr rr rr "
      "rr .. .. .. .. .. rr rr rr {{ {2 tC t3 |C rr rr rr rr rr "
      "rr rr tf .. .. rr rr rr rr rr tb tt |C rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_mushroom_cave
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_shard
          78
          74
        )
      ) ;; bind
    7 12)
    (list
      (kern-mk-obj t_royal_cape 1
        ;; hooks
        (list
        )
      )
    4 14)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'yellow-slime
        )
      ) ;; bind
    7 7)
  ) ;; end of objects in p_mushroom_cave
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_mushroom_cave

(kern-load "douglas.scm")

(kern-mk-place 'p_shamans_grove "Shaman's Grove"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr t7 bb t7 rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr tb tt tt tt td rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr t7 bb te t& te bb t7 rr rr rr rr rr rr "
      "rr rr rr rr rr rr tt t5 tB aa tD t3 tt rr rr rr rr rr rr "
      "rr rr rr rr rr rr ta tt td tE tb tt tc rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr te bb t7 bb te rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr t3 tt t5 rr rr rr rr rr rr rr rr "
      "rr rr rr t3 tt t5 rr rr || || || rr rr |# || |% rr rr rr "
      "rr rr t3 tc t& ta t5 rr || || || rr |# || || || |% rr rr "
      "rr t3 tc t# .. t% te bb || || || || || || || || || |% rr "
      "rr tt tB .. && .. tD tb || || || || || || tt || || || rr "
      "rr ta t5 tA .. tC t7 bb || || || || || || || || || |C rr "
      "rr rr ta t5 tE t3 tc rr || || || rr |A || || || |C rr rr "
      "rr rr rr ta tt tc rr rr || || || rr rr |A || |C rr rr rr "
      "rr rr rr rr rr rr rr rr || tt || rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr ta tt tc rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_goblin_crossroads "Goblin Crossroads"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "rr rr rr rr rr rr rr {{ {{ ,, ,, {{ rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr {{ ,, {# {{ {{ {{ {{ rr rr rr rr "
            "rr rr rr rr rr rr rr rr {{ {{ ,, {5 {{ rr {{ {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr {{ ,, bb {5 {{ {{ {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr ,, {{ {a {8 {1 {5 {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr rr {{ {{ {{ {a bb {d {{ {{ rr "
            "rr rr rr rr rr rr rr rr rr rr bb {{ {{ {{ {e {{ {{ {{ rr "
            "{{ {{ {{ rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ "
            ",, {{ {{ {a bb rr rr rr rr rr rr {{ {{ {{ {{ {{ ,, {C ,, "
            ",, {E ,, {{ {6 {{ rr rr rr rr rr {{ {{ ,, ,, ,, {A ,, ,, "
            ",, ,, ,, {1 ,, ,, {{ rr rr rr {{ {{ {{ {% ,, ,, ,, {% ,, "
            "{{ {{ {{ {a bb .. {5 {{ {{ {{ {{ {{ {7 {{ {{ {{ {{ {{ {{ "
            "rr {{ {{ {{ {2 {8 bb {5 {{ {{ {{ {3 bb {d {{ {{ {{ rr rr "
            "rr rr {{ rr rr {{ {a .. {5 {{ {{ {2 {c {{ {{ {{ rr rr rr "
            "rr rr rr rr {{ {{ {{ {a {8 {1 {1 {4 {{ {{ {{ {{ rr rr rr "
            "rr rr rr rr {{ {{ {{ {{ {{ {2 bb {4 {{ {{ rr rr rr rr rr "
            "rr rr rr rr rr {{ {{ {{ {{ ,, .. {c {{ {{ {{ rr rr rr rr "
            "rr rr rr rr rr rr rr {{ {{ {2 {4 {{ rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr {{ ,, .. .. rr rr rr rr rr rr rr rr "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        (list
          (list
            (kern-mk-place 'p_kurpolis_entrance "Entrance to Kurpolis"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "rr rr rr rr xx xx x! xx xx && xx xx x! xx xx rr rr rr rr "
                  "rr .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! rr rr rr rr "
                  "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx ,, xx xx xx xx xx xx x! xx xx xx xx xx rr bb ,, rr "
                  "xx xx ,, xx xx .K .U .R .P .O .L .I .S xx ,, bb bb bb ,, "
                  "xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb ,, "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, bb ,, ,, ,, "
                  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb ,, "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb bb bb bb ,, "
                  "xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb rr "
                  "xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx ,, bb rr rr "
                  "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
                  "rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              nil ;; neighbors
              (list ;; objects in p_kurpolis_entrance
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                7 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                9 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                11 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                13 17)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                18 7)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                18 11)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                15 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'crossbowman
                    )
                  ) ;; bind
                12 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'crossbowman
                    )
                  ) ;; bind
                12 11)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'halberdier
                    )
                  ) ;; bind
                10 10)
                (list
                  (bind
                    (kern-mk-char
                      'ch_douglas
                      "Douglas"
                      sp_human
                      oc_warrior
                      s_companion_paladin
                      2
                      0 0 0
                      0 0
                      0 0
                      14 0
                      0 4
                      #f ;; dead?
                      'doug-conv
                      sch_doug
                      nil
                      nil ;; inventory
                      (list
                        t_chain_coif
                        t_sword
                        t_armor_chain
                      )
                      ;; hooks
                      (list
                      )
                    )
                    '()
                  ) ;; bind
                5 1)
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_ladder_up 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_shard
                      53
                      18
                    )
                  ) ;; bind
                9 10)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                2 8)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                2 12)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                4 15)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                4 3)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #t
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                4 1)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_windowed_wood_door_in_rock
                          #f
                          5
                        )
                        (list
                          's_open_door_in_rock
                          #f
                          0
                        )
                        (list
                          's_locked_windowed_wood_door_in_rock
                          #f
                          5
                        )
                        (list
                          's_magically_locked_windowed_wood_door_in_rock
                          #f
                          5
                        )
                      )
                    )
                  ) ;; bind
                13 10)
                (list
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 10 t_food)
                    )
                    ;; hooks
                    (list
                    )
                  )
                1 1)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                5 17)
              ) ;; end of objects in p_kurpolis_entrance
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_kurpolis_entrance

          3)
          (list
            (kern-mk-place 'p_cave_goblin_village "Cave Goblin Village"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                  "rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr rr "
                  "rr rr rr rr rr {{ ,, ,, ,, ,, {A {{ {{ rr {{ {{ {{ rr rr "
                  "rr rr rr rr rr {{ ,, ,, && ,, ,, bb {{ rr {{ ,, {A {{ rr "
                  "rr rr rr rr rr {{ ,, ,, ,, ,, ,, ,, {1 .. {9 ,, ,, {{ rr "
                  "rr rr rr rr rr {{ {a {8 ,, ,, {# {{ ,, rr {{ ,, ,, {{ rr "
                  "rr rr rr rr rr {{ {{ {{ {6 {{ bb {C ,, rr {{ {{ {{ {{ rr "
                  "{{ {{ {{ {{ bb {{ bb {{ {2 {5 {C ,, ,, rr rr {{ {{ rr rr "
                  "{{ {{ {{ {{ bb {{ {{ {3 ,, ,, ,, ,, {# rr rr rr rr rr rr "
                  ",, {A ,, {9 .. {9 {1 ,, .. ,, ,, ,, {{ {{ rr rr rr rr rr "
                  ",, ,, {c {{ bb {{ {a ,, ,, ,, ,, {4 {{ {{ {{ rr rr rr rr "
                  "{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, {1 {5 {{ rr rr rr rr "
                  "rr rr rr rr rr {{ bb {% ,, ,, ,, {# ,, .. ~C ~~ ~~ rr rr "
                  "rr rr rr rr rr {{ {{ {{ {D ,, {B {{ {{ {a ~3 -- -- ~~ rr "
                  "rr rr rr rr rr rr rr bb bb .. bb bb rr {{ ~a -- -- ~~ rr "
                  "rr rr rr rr rr rr rr {{ {{ {6 {{ {{ rr {{ {H ~a ~c {G rr "
                  "rr rr rr rr rr rr rr {{ {{ {a {5 {{ rr rr {{ {{ {{ {{ rr "
                  "rr rr rr rr rr rr rr {{ {{ {{ {6 {{ rr rr {{ {{ {{ rr rr "
                  "rr rr rr rr rr rr rr {{ {{ {3 {4 {{ rr rr rr rr rr rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              (list
                (list
                  (kern-mk-place 'p_trolls_den "Troll's Den"
                    nil ;; sprite
                    (kern-mk-map
                      nil                       19 19 pal_expanded
                      (list
                        "rr rr rr rr rr rr rr {{ {2 ,, ,, {{ rr rr rr rr rr rr rr "
                        "rr {{ {{ {{ rr rr rr {{ {2 ,, {B {{ rr rr rr rr rr rr rr "
                        "rr {{ {{ {{ {{ rr rr {{ {a .. ,, {A rr rr rr rr rr rr rr "
                        "rr {{ {{ {{ {{ {{ rr rr {{ {2 ,, bb rr rr rr rr rr rr rr "
                        "rr rr {{ {{ rr {{ {{ rr {{ ,, {c {{ rr rr rr rr rr rr rr "
                        "rr rr {{ rr rr rr {{ {{ {{ {6 {{ {{ rr rr rr rr rr rr rr "
                        "rr rr {{ {{ rr {{ {{ rr {{ {a {5 {{ rr {{ {{ rr rr rr rr "
                        "rr {{ {{ {{ {6 {{ rr rr rr {{ {2 {1 {8 {5 {{ {{ {{ rr rr "
                        "rr {{ {{ {b bb {d {{ rr {{ {3 .. rr {{ {a {5 {{ {3 .. rr "
                        "rr {{ {{ {{ {e {{ {{ {{ {{ {2 rr rr rr {{ {2 {1 .. .. rr "
                        "rr rr {{ {{ {{ {{ rr {{ {3 {c {{ rr {{ {{ {2 .. .. && rr "
                        "rr rr rr {{ {{ rr rr rr {4 {{ {{ {{ {3 {1 .. {8 .. .. rr "
                        "rr rr rr {{ {7 {{ rr {{ {2 rr rr {1 bb {8 {c {{ {a .. rr "
                        "rr rr {{ bb .. {5 {{ {3 {8 rr rr rr {4 {{ {{ {{ {{ rr rr "
                        "rr {{ {3 .. .. .. {9 {c {{ {{ rr rr rr rr {{ {{ {{ rr rr "
                        "rr {{ {a .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr "
                        "rr {{ {{ {a {8 bb {{ {{ rr rr rr rr rr rr rr rr rr rr rr "
                        "rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                        "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                      )
                    )
                    #f #t #f #f
                    ;; subplaces
                    nil
                    (list
                      (list
                        (kern-mk-place 'p_watchpoint "Watchpoint"
                          nil ;; sprite
                          (kern-mk-map
                            nil                             19 19 pal_expanded
                            (list
                              "rr rr rr rr rr rr rr {{ {C ,, ,, {{ rr rr rr rr rr rr rr "
                              "rr rr rr rr rr rr rr {{ ,, ,, {4 {{ rr rr rr rr rr rr rr "
                              "rr rr rr rr rr rr bb {{ ,, .. ,, {5 {{ rr rr rr rr rr rr "
                              "rr rr rr rr rr rr {{ {{ ,, .. ,, bb {{ rr rr rr rr rr rr "
                              "rr rr rr rr rr bb {{ {{ {2 ,, ,, {c {{ rr rr rr rr rr rr "
                              "rr rr rr rr rr {{ {{ {{ ,, .. ,, {{ {{ {{ rr rr rr rr rr "
                              "rr rr rr rr bb {A {{ {C oo ee oo {{ ~~ -- rr rr rr rr rr "
                              "rr rr rr rr bb bb ~1 ~9 ~~ ee ee ~9 ~~ _! rr rr rr rr rr "
                              "rr rr rr -- -- ~~ ~c {& oo ee ~c {{ ~~ -- rr rr rr rr rr "
                              "rr rr rr _! -- -c {G {{ {{ ,, {{ {{ {{ {{ {{ rr rr rr rr "
                              "rr rr rr -- -- {G {{ bb ,, ,, ,, bb {{ {{ {{ rr rr rr rr "
                              "rr rr rr rr {{ {{ x. x. w+ d, w+ x. x. {{ {{ rr rr rr rr "
                              "rr rr rr rr {{ {{ x. .. ,, ,, ,, ,, rr {d {{ rr rr rr rr "
                              "rr rr rr rr rr {{ x. ,, ,, ,, ,, ,, x. {{ {{ rr rr rr rr "
                              "rr rr rr rr {{ {{ x. ,, ,, ,, ,, ,, x. {{ {{ rr rr rr rr "
                              "rr rr rr rr {{ {{ bb ,, ,, ,, ,, .. x. {{ rr rr rr rr rr "
                              "rr rr rr rr rr {{ x. ,, ,, ,, .. bb x. rr rr rr rr rr rr "
                              "rr rr rr rr rr rr rr x. x. && x. x. x. rr rr rr rr rr rr "
                              "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                            )
                          )
                          #f #t #f #f
                          ;; subplaces
                          nil
                          nil ;; neighbors
                          (list ;; objects in p_watchpoint
                            (list
                              (bind
                                (kern-mk-obj t_guard_pt 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'spawn-pt
                                  'halberdier
                                )
                              ) ;; bind
                            8 12)
                            (list
                              (bind
                                (kern-mk-obj t_guard_pt 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'spawn-pt
                                  'crossbowman
                                )
                              ) ;; bind
                            10 12)
                            (list
                              (bind
                                (kern-mk-obj t_monman 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'monman
                                  (list
                                    0
                                    0
                                    0
                                    -1
                                    6
                                    -1
                                  )
                                )
                              ) ;; bind
                            0 0)
                            (list
                              (bind
                                (kern-mk-obj t_ladder_down 1
                                  ;; hooks
                                  (list
                                  )
                                )
                                (list
                                  'p_paladins_hold
                                  3
                                  9
                                )
                              ) ;; bind
                            9 14)
                          ) ;; end of objects in p_watchpoint
                          (list ;; on-entry-hooks
                            'on-entry-to-dungeon-room
                          )
                          (list ;; edge entrances
                            (list 0 18 18) ;; Northwest
                            (list 1 9 18) ;; North
                            (list 2 0 18) ;; Northeast
                            (list 3 18 9) ;; West
                            (list 4 9 9) ;; Here
                            (list 5 0 9) ;; East
                            (list 6 18 0) ;; Southwest
                            (list 7 9 0) ;; South
                            (list 8 0 0) ;; SoutheastUp
                          )
                        ) ;; end of place p_watchpoint

                      3)
                    ) ;; end neighbors of p_trolls_den
                    (list ;; objects in p_trolls_den
                      (list
                        (bind
                          (kern-mk-obj t_ladder_up 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'p_old_mine
                            17
                            17
                          )
                        ) ;; bind
                      3 15)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'troll
                          )
                        ) ;; bind
                      16 10)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'troll-geomancer
                          )
                        ) ;; bind
                      17 9)
                      (list
                        (bind
                          (kern-mk-obj t_spawn_pt 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'spawn-pt
                            'troll
                          )
                        ) ;; bind
                      17 11)
                      (list
                        (kern-mk-obj t_food 1
                          ;; hooks
                          (list
                          )
                        )
                      17 8)
                      (list
                        (kern-mk-obj t_beer 1
                          ;; hooks
                          (list
                          )
                        )
                      16 8)
                      (list
                        (kern-mk-obj t_food 1
                          ;; hooks
                          (list
                          )
                        )
                      16 12)
                      (list
                        (kern-mk-obj t_beer 1
                          ;; hooks
                          (list
                          )
                        )
                      15 11)
                      (list
                        (bind
                          (kern-mk-obj t_monman 1
                            ;; hooks
                            (list
                            )
                          )
                          (list
                            'monman
                            (list
                              0
                              0
                              0
                              -1
                              6
                              -1
                            )
                          )
                        ) ;; bind
                      0 0)
                    ) ;; end of objects in p_trolls_den
                    (list ;; on-entry-hooks
                      'on-entry-to-dungeon-room
                    )
                    (list ;; edge entrances
                      (list 0 18 18) ;; Northwest
                      (list 1 9 18) ;; North
                      (list 2 0 18) ;; Northeast
                      (list 3 18 9) ;; West
                      (list 4 9 9) ;; Here
                      (list 5 0 9) ;; East
                      (list 6 18 0) ;; Southwest
                      (list 7 9 0) ;; South
                      (list 8 0 0) ;; SoutheastUp
                    )
                  ) ;; end of place p_trolls_den

                7)
              ) ;; end neighbors of p_cave_goblin_village
              (list ;; objects in p_cave_goblin_village
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-priest
                    )
                  ) ;; bind
                9 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                14 4)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-priest
                    )
                  ) ;; bind
                16 4)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #t
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                13 4)
                (list
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 1 t_bolt)
                      (list 2 t_food)
                      (list 5 t_gold_coins)
                      (list 1 t_mana_potion)
                      (list 9 t_arrow)
                    )
                    ;; hooks
                    (list
                    )
                  )
                16 4)
                (list
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 1 t_heal_potion)
                      (list 2 t_food)
                      (list 2 t_cure_potion)
                      (list 4 t_gold_coins)
                      (list 1 t_bolt)
                    )
                    ;; hooks
                    (list
                    )
                  )
                16 5)
                (list
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 5 t_gold_coins)
                    )
                    ;; hooks
                    (list
                    )
                  )
                15 3)
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                5 8)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                8 2)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                9 3)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-slinger
                    )
                  ) ;; bind
                10 13)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                6 10)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                8 4)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                7 3)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-berserker
                    )
                  ) ;; bind
                8 13)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'cave-goblin-priest
                    )
                  ) ;; bind
                15 4)
              ) ;; end of objects in p_cave_goblin_village
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_cave_goblin_village

          5)
          (list
            p_watchpoint
          7)
        ) ;; end neighbors of p_goblin_crossroads
        (list ;; objects in p_goblin_crossroads
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'cave-goblin-slinger
              )
            ) ;; bind
          14 11)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'cave-goblin-berserker
              )
            ) ;; bind
          15 9)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'forest-goblin-hunter
              )
            ) ;; bind
          15 5)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'forest-goblin-hunter
              )
            ) ;; bind
          12 7)
        ) ;; end of objects in p_goblin_crossroads
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_goblin_crossroads

    7)
  ) ;; end neighbors of p_shamans_grove
  (list ;; objects in p_shamans_grove
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-hunter
        )
      ) ;; bind
    4 10)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-stalker
        )
      ) ;; bind
    5 11)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_dank_cave
          9
          1
        )
      ) ;; bind
    14 11)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-shaman
        )
      ) ;; bind
    9 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-hunter
        )
      ) ;; bind
    3 11)
  ) ;; end of objects in p_shamans_grove
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_shamans_grove

p_watchpoint
p_trolls_den
p_cave_goblin_village
p_goblin_crossroads
p_kurpolis_entrance
(kern-mk-place 'p_treasury2 "Lost Treasury of Luximene"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx ,T ,R ,E ,A ,S ,U ,R ,Y @@ xx xx xx xx xx "
      "xx xx xx xx xx @@ @@ @@ ,O @@ ,F @@ @@ @@ xx xx xx xx xx "
      "xx xx xx xx xx @@ ,L ,U ,X ,I ,M ,E ,N ,E xx xx xx xx xx "
      "xx xx xx xx xx cc cc cc ,, cc ,, cc cc cc xx xx xx xx xx "
      "xx xx xx xx xx cc pp ,, ,, ,, ,, ,, pp cc xx xx xx xx xx "
      "xx xx xx xx xx cc ,, ,, ,, cc ,, ,, ,, cc xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, cc cc cc ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, cc cc ,, cc cc ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, cc cc cc ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx ,, ,, ,, cc ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_treasury2
    (list
      (kern-mk-obj t_gold_coins 63
        ;; hooks
        (list
        )
      )
    9 13)
    (list
      (kern-mk-obj t_gold_coins 45
        ;; hooks
        (list
        )
      )
    13 5)
    (list
      (kern-mk-obj t_gold_coins 62
        ;; hooks
        (list
        )
      )
    12 11)
    (list
      (kern-mk-obj t_gold_coins 27
        ;; hooks
        (list
        )
      )
    10 8)
    (list
      (kern-mk-obj t_gold_coins 54
        ;; hooks
        (list
        )
      )
    7 11)
    (list
      (kern-mk-obj t_gold_coins 48
        ;; hooks
        (list
        )
      )
    5 8)
    (list
      (kern-mk-obj t_gold_coins 47
        ;; hooks
        (list
        )
      )
    13 8)
    (list
      (kern-mk-obj t_gold_coins 64
        ;; hooks
        (list
        )
      )
    11 11)
    (list
      (kern-mk-obj t_gold_coins 45
        ;; hooks
        (list
        )
      )
    8 13)
    (list
      (kern-mk-obj t_gold_coins 60
        ;; hooks
        (list
        )
      )
    8 9)
    (list
      (kern-mk-obj t_gold_coins 62
        ;; hooks
        (list
        )
      )
    13 10)
    (list
      (kern-mk-obj t_gold_coins 81
        ;; hooks
        (list
        )
      )
    8 10)
    (list
      (kern-mk-obj t_gold_coins 49
        ;; hooks
        (list
        )
      )
    7 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_torch)
        )
        ;; hooks
        (list
        )
      )
    8 11)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_food)
        )
        ;; hooks
        (list
        )
      )
    10 13)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 9 t_gold_coins)
          (list 1 t_mana_potion)
          (list 3 t_arrow)
        )
        ;; hooks
        (list
        )
      )
    10 10)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_torch)
          (list 5 t_bolt)
          (list 1 t_gold_coins)
        )
        ;; hooks
        (list
        )
      )
    11 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 4 t_food)
          (list 4 t_gold_coins)
          (list 4 t_arrow)
          (list 3 t_bolt)
        )
        ;; hooks
        (list
        )
      )
    6 9)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    9 8)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    10 11)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    10 5)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    10 6)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    12 7)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    10 9)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    5 10)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    12 9)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    11 6)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    7 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_arrow
            )
            (list
              2
              't_arrow
            )
          )
        )
      ) ;; bind
    11 7)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              5
              't_bolt
            )
            (list
              5
              't_bolt
            )
          )
        )
      ) ;; bind
    9 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_bolt
            )
          )
        )
      ) ;; bind
    5 7)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_treasury
          9
          9
        )
      ) ;; bind
    9 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 1 t_eldritch_blade)
          (list 1 t_armor_plate_4)
          (list 1 t_iron_helm_4)
          (list 1 t_doom_staff)
        )
        ;; hooks
        (list
        )
      )
    9 5)
    (list
      (kern-mk-obj t_gold_coins 44
        ;; hooks
        (list
        )
      )
    11 5)
    (list
      (kern-mk-obj t_gold_coins 32
        ;; hooks
        (list
        )
      )
    9 11)
    (list
      (kern-mk-obj t_gold_coins 56
        ;; hooks
        (list
        )
      )
    12 8)
    (list
      (kern-mk-obj t_gold_coins 29
        ;; hooks
        (list
        )
      )
    11 12)
    (list
      (kern-mk-obj t_gold_coins 51
        ;; hooks
        (list
        )
      )
    9 10)
    (list
      (kern-mk-obj t_gold_coins 61
        ;; hooks
        (list
        )
      )
    7 10)
    (list
      (kern-mk-obj t_gold_coins 49
        ;; hooks
        (list
        )
      )
    7 6)
  ) ;; end of objects in p_treasury2
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_treasury2

(kern-load "may.scm")

(kern-load "kathryn.scm")

(kern-load "thud.scm")

(kern-load "bill.scm")

(kern-load "melvin.scm")

(kern-load "hackle.scm")

(kern-mk-place 'p_bole "Bole"
  s_hamlet ;; sprite
  (kern-mk-map
    nil     48 39 pal_expanded
    (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~6 ^a ^^ ^c ~6 ^a ^^ ^^ ~6 || || || || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^c t7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~6 |B || |% ~6 |# |% ^e ~6 || || || || || || || || || ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^c t3 tt tc {5 ^^ ^^ ^^ ^^ ^^ ^^ ~a ~5 |A |C ~6 |A |C ~3 ~c || || || || || || || || || |% ^a ^^ ^^ ^^ ^^ ^c |& ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ t3 tt tc t# .. {1 {5 ^^ ^^ ^^ ^c |& ~a ~1 ~~ ~~ ~~ ~1 ~c |# || || || || || || || || || || |% ^^ ^^ ^c |# || || |% ^e t3 tt tt tt t5 ^^ ^^ "
      "^^ ^c tt t# .. .. .. .. .. {5 ^c tb tt || |% ~a ~~ bb ~~ ~c |# || || || tt tt tt tt tt tt tt tt || ^a ^c |# || || || || tt tc bb .. bb te ^a ^^ "
      "^^ t3 tc .. .. .. .. .. .. .. bb .. t% tt || tH ~a ~~ ~c tG || || || tc t# .. .. .. t% ta tt tt || || || || || || |C ^7 tt bb .. .. .. bb t7 ^^ "
      "^^ tt t# .. .. .. .. .. .. bb .. .. .. ta tt tt td ~6 tb tt || || tt t# tC t3 tt t5 tA .. t% ta tt tt tt || || || ^3 ^^ tt .. .. .. .. tb tt ^^ "
      "^^ tt .. .. rr rr rr rr rr rr rr .. .. .. .. .. .. == .. t% ta tt tc .. t3 || || || || t5 .. .. .. t% tt || || |C ^^ ^^ tt bb .. .. .. bb tt ^^ "
      "^^ tt .. .. rr .. .. .. .. .. rr .. .. tC t3 tt td ~6 t7 tA .. .. .. tC tt || || || || tt tA .. .. tC tt || |C ^3 ^^ ^^ ta t5 bb .. bb t3 tc ^^ "
      "^^ tt .. .. rr .. .. .. .. .. rr .. .. t3 || || ~3 ~c || tt tt tt tt tt || || || || || tt t5 .. .. t3 tt || ^3 ^^ ^^ ^^ ^5 ta tt tt tt tc ^3 ^^ "
      "^^ tt .. .. rr .. .. && .. .. .. .. .. tt || || ~6 |# || || || || || || || || || || || || tt .. tC tt || |C ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || |C ~6 || || || || || || || || || || tt tt tt tc .. t3 || |C ^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ tt .. .. rr .. .. .. .. .. rr .. .. tt || ~3 ~c tt tt tt tt tt tt tt tt || || tt t# .. .. tC tt || ^b ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ tt .. .. rr rr rr .. rr rr rr .. .. || |C ~6 t3 tt tt tt tt tt tt tt tt tt || tt .. t3 tt tt || || |% ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ tt tA .. .. .. .. .. .. bb .. .. tC || ~3 ~c tt xx xx xx xx xx xx xx xx tt tt tc .. tt tt tt tt tt tt t5 ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ tt t5 tA .. .. .. .. bb {8 tC t3 tt || ~6 t3 tt xx cc cc cc cc cc cc xx te bb .. tC tt tt tt tt tt tt tt tt td ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ta tt t5 .. .. .. {c ^^ ^c t3 tt || || ~6 tt tt xx cc xx cc cc cc cc xx .. .. .. t3 tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
      "^^ ^5 tt tt tA .. {c ^^ ^^ t3 tt || tt tc ~6 tt tt xx xx xx cc cc cc cc cc .. .. bb tt tt xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
      "^^ ^^ ta tt tt td ^^ ^^ ^c tt tt || tt ~3 ~c tt tt tt t5 xx cc cc cc cc xx .. .. tb tt tt xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
      "^^ ^^ ^5 ta tc ^3 ^^ ^^ t3 tt tt || tt ~6 t3 tt tt tt tt xx cc cc cc cc xx .. .. bb tt tt xx xx xx xx cc cc xx xx xx xx cc cc cc cc cc xx ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^c tt || || tt tc ~6 tt || || tt tt xx xx xx && xx xx .. .. .. ta tt xx cc cc cc cc cc cc cc cc xx cc cc cc cc cc xx ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^c t3 tt || tt tc ~3 ~c tt || || tt tt tt t5 xx xx xx t7 bb .. .. bb tt xx cc cc x! cc cc x! cc cc xx cc cc cc cc cc xx ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ t3 tt tt tt tt tc ~3 ~c t3 tt || || || tt tt tt tt tt tt tt td .. .. tb tt xx xx xx xx cc cc xx xx xx xx xx xx xx cc cc xx ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ tt || || || tt ~3 ~c t3 tt || || || || || || tt tt tt tt tt bb .. .. bb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ tt || || tt tc ~6 t3 tt || || || || || || || || || tt tt tt td .. .. tb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
      "^^ |& ^a ^^ ^^ ta tt tt tc ~3 ~c tt || || || || || || || || || || || tt bb .. .. .. bb te xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
      "^^ || |% ^a ^^ ^5 ta tL ~3 ~4 t3 tt || || || || || || || || || || || tc .. .. .. .. .. .. sI cc cc 00 cc cc && xx && cc cc cc xx cc cc xx ^^ ^^ "
      "^^ || tt td ^a ^L ~3 ~~ ~~ ~4 tt || || || || || || || || || || || || bb .. .. .. .. .. .. cc cc cc 00 cc cc && xx && cc cc cc xx xx xx xx ^^ ^^ "
      "^^ tt tL ~3 ~1 ~~ ~~ ~~ ~~ ~4 tt || || || || || || tt tt tt || || || t5 .. .. .. .. .. .. xx cc cc 00 cc cc xx xx xx cc cc cc cc cc cc xx ^^ ^^ "
      "^c tL ~3 ~~ ~~ bb ~~ ~~ ~~ ~c tt || || || || || tt tc ^7 ta tt || || tt bb .. .. .. bb t7 xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
      "~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ tG tt || || || || tt tc ^3 ^^ ^5 ta tt || || tt td .. .. tb tt xx cc cc cc cc cc cc cc cc cc cc 00 xx cc cc xx ^^ ^^ "
      "bb ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~c tt || || || || || tt ^b ^^ ^^ ^^ ^d tt || || || bb .. .. bb tt xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ^^ ^^ "
      "~~ ~~ ~~ bb ~~ ~~ ~8 ~c tG tt || || || || || tt t5 ^a ^^ ^c t3 tt || || || td .. .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 ^^ ^^ ^^ "
      "~~ ~~ ~~ ~~ ~c tG tt tt tt || || || || || || || tt tt tt tt tt || || || tt bb .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt t5 ^a ^^ "
      "~~ bb ~c tG tt tt tt tt || || || || || || || || || || || || || || || || tc .. .. .. tb tt tt || || || || || || || || || || || tt tt tt tt td ^^ "
      "~~ ~~ || tt tt tt tt tt tt || || || || || || || || || || || || || || || bb .. .. .. bb || || || || || || || || || || || || || || || tt tc ^3 ^^ "
      "~~ ~c || || tt tt tt tt tt || || || || || || || || || || || || || || || td .. .. tb tt || || || || || || || || || || || || || || || tt ^b ^^ ^^ "
      "~c tG || || tt tt || || || || || || || || || || || || || || || || || || bb .. .. bb || || || || || || || || || || || || || || || || tt t5 ^a ^^ "
      "|| || || || || || || || || || || || || || || || || || || || || || || || t5 .. .. t3 || || || || || || || || || || || || || || || || || tt t5 ^^ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_bole
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    38 21)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    40 17)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    44 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    24 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    19 15)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    23 19)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    5 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    7 13)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    10 10)
    (list
      (bind
        (kern-mk-obj t_thief_door 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'p_traps_1
          4
          16
        )
      ) ;; bind
    43 6)
    (list
      (bind
        (kern-mk-char
          'ch_may
          "May"
          sp_human
          nil
          s_townswoman
          2
          0 0 0
          0 0
          0 0
          16 0
          8 6
          #f ;; dead?
          'may-conv
          sch_may
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    31 23)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_kathryn
            "Kathryn"
            sp_human
            oc_wizard
            s_wizard
            2
            0 4 0
            2 1
            4 2
            16 0
            8 6
            #f ;; dead?
            'kathryn-conv
            sch_kathryn
            'spell-sword-ai
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              (list
                (list 1 t_kathryns_letter)
                (list 100 t_gold_coins)
                (list 5 sulphorous_ash)
                (list 5 ginseng)
                (list 5 garlic)
                (list 3 spider_silk)
                (list 3 blood_moss)
                (list 2 black_pearl)
                (list 1 nightshade)
                (list 1 mandrake)
                (list 1 t_wis_quas_scroll)
              )
              ;; hooks
              (list
              )
            )
            (list
              t_staff
            )
            ;; hooks
            (list
            )
          )
        #t) ;; kern-char-force-drop
        '()
      ) ;; bind
    31 23)
    (list
      (bind
        (kern-mk-char
          'ch_thud
          "Thud"
          sp_troll
          oc_warrior
          s_troll
          2
          4 0 2
          2 1
          0 0
          40 4096
          8 6
          #f ;; dead?
          'thud-conv
          sch_thud
          nil
          nil ;; inventory
          (list
            t_iron_helm
            t_2h_axe
            t_armor_plate
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    31 23)
    (list
      (bind
        (kern-mk-char
          'ch_bill
          "Bill"
          sp_human
          nil
          s_townsman
          2
          0 0 0
          0 0
          0 0
          13 0
          5 3
          #f ;; dead?
          'bill-conv
          sch_bill
          nil
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 10 t_torch)
              (list 100 t_arrow)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_2h_axe
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    20 15)
    (list
      (bind
        (kern-mk-char
          'ch_melvin
          "Melvin"
          sp_human
          nil
          s_townsman
          2
          10 10 10
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'melvin-conv
          sch_melvin
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    40 18)
    (list
      (bind
        (kern-mk-char
          'ch_hackle
          "Hackle"
          sp_human
          nil
          s_beggar
          2
          10 10 10
          0 0
          0 0
          16 0
          0 6
          #f ;; dead?
          'hackle-conv
          sch_hackle
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    2 3)
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          22 128
          1 1
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    6 4)
    (list
      (bind
        (kern-mk-char
          nil
          "dryad"
          sp_dryad
          nil
          s_reaper
          5
          0 0 0
          0 0
          0 0
          20 16384
          30 8
          #f ;; dead?
          nil
          nil
          'dryad-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d5"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'dryad
          #f
          #f
          '()
        )
      ) ;; bind
    6 35)
    (list
      (kern-tag 'bole-inn-room-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    33 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    36 17)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    33 20)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    36 20)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    42 25)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    42 28)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    30 27)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    31 18)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    38 18)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    31 21)
  ) ;; end of objects in p_bole
  (list ;; on-entry-hooks
    'lock-inn-room-doors
  )
  (list ;; edge entrances
    (list 0 47 38) ;; Northwest
    (list 1 26 38) ;; North
    (list 2 0 38) ;; Northeast
    (list 3 47 19) ;; West
    (list 4 24 19) ;; Here
    (list 5 0 19) ;; East
    (list 6 47 0) ;; Southwest
    (list 7 24 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_bole

(kern-load "patch.scm")

(kern-load "angela.scm")

(kern-load "jess.scm")

(kern-load "chester.scm")

(kern-load "steward.scm")

(kern-load "ini.scm")

(kern-load "jeffreys.scm")

(kern-mk-place 'p_glasdrin "Glasdrin"
  s_keep ;; sprite
  (kern-mk-map
    nil     31 31 pal_expanded
    (list
      "xx xx xx xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx xx xx xx "
      "xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx cc ++ cc xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc xx "
      "xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx "
      "xx xx ,, xx xx xx .A .R .M .S ]] xx ,, ,, ,, ,, ,, ,, ,, xx .M .E .D .I .C .K xx xx ,, xx xx "
      ".. xx ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, pp ,, ,, ,, pp ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
      ".. xx ,, xx ,, xx @@ @@ @@ @@ @@ xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
      ".. xx ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, pp ,, ,, ,, pp ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
      ".. xx ,, xx xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx xx ,, xx .. "
      ".. xx ,, xx xx xx xx xx ,, sA xx xx ,, pp ,, ,, ,, pp ,, xx xx xx ,, sH xx xx xx xx ,, xx .. "
      ".. xx ,, xx ,, ,, xx .. cc .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx cc .. xx ,, ,, xx ,, xx .. "
      ".. xx ,, xx ,, ,, ,, cc cc xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx cc cc ,, ,, ,, xx ,, xx .. "
      ".. xx ,, xx xx xx xx .. cc xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx cc .. xx xx xx xx ,, xx .. "
      ".. xx ,, xx ,, ,, xx .. cc xx ,, ,, ,, xx w+ ,, w+ xx ,, ,, ,, xx cc .. xx ,, ,, xx ,, xx .. "
      ".. xx ,, xx ,, ,, ,, cc cc xx ,, ,, ,, xx cc cc cc xx ,, ,, ,, xx cc cc ,, ,, ,, xx ,, xx .. "
      ".. xx ,, xx xx xx xx .. cc xx xx xx xx x! cc cc cc x! xx xx xx xx cc .. xx xx xx xx ,, xx .. "
      ".. xx ,, ,, cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ,, ,, xx .. "
      ".. xx ,, xx xx xx xx xx xx xx .. .. .. .. cc cc cc t3 tt tt t5 xx xx xx xx xx xx xx ,, xx .. "
      ".. xx ,, xx .H .O .T .E .L xx .. .. .. .. cc cc cc tt ~3 ~5 tt xx .H .O .L .Y ]] xx ,, xx .. "
      ".. xx ,, xx ,, ,, ,, ,, ,, ,, .. .. .. .. cc cc cc tt ~a ~c tt xx .G .R .A .I .L xx ,, xx .. "
      ".. xx ,, xx @@ @@ @@ @@ @@ sI .. .. .. .. cc cc cc ta tt tt tc ws ,, ,, ,, ,, ,, xx ,, xx .. "
      ".. xx ,, xx ,, ,, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc cc cc ,, ,, ,, ,, ,, ,, xx ,, xx .. "
      ".. xx ,, xx x! ,, ,, ,, ,, ws bb .. .. bb cc cc cc bb bb bb bb ws ,, ,, 00 ,, && xx ,, xx .. "
      ".. xx ,, xx xx ,, xx ,, xx xx .. .. .. .. cc cc cc bb .. .. .. sT ,, ,, 00 ,, ,, xx ,, xx .. "
      ".. xx ,, xx ,, ,, xx ,, ,, xx .. .. .. .. cc cc cc bb .. tC t7 xx ,, ,, ,, ,, ,, xx ,, xx .. "
      ".. xx ,, xx ,, ,, xx ,, ,, xx .. .. .. bb cc cc cc bb .. tb tc xx xx [[ @@ ]] xx xx ,, xx .. "
      "xx xx ,, xx xx xx xx xx xx xx xx xx xx x! cc w+ cc x! xx xx xx xx xx ,, ,, ,, xx xx ,, xx xx "
      "xx ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, xx xx xx ,, xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, w+ cc cc cc w+ ,, ,, ,, xx xx xx xx xx xx ,, ,, ,, xx "
      "xx xx xx xx xx .. .. .. .. xx xx xx xx xx cc cc cc xx xx xx xx xx .. .. .. .. xx xx xx xx xx "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_glasdrin
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'halberdier
        )
      ) ;; bind
    14 12)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'halberdier
        )
      ) ;; bind
    16 12)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'crossbowman
        )
      ) ;; bind
    12 28)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'crossbowman
        )
      ) ;; bind
    18 28)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    21 21)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    24 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    24 14)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 14)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    11 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    19 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    8 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    5 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    5 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    15 13)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    24 27)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    22 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    27 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    2 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 2)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    26 28)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    28 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    9 21)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 19)
    (list
      (kern-tag 'glasdrin-inn-room-1-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    5 23)
    (list
      (kern-tag 'glasdrin-inn-room-2-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    7 23)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 25)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 25)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    26 10)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 13)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    26 13)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 10)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    11 14)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    29 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    19 14)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_prison
          6
          4
        )
      ) ;; bind
    2 2)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-char
          'ch_patch
          "Patch"
          sp_human
          oc_wizard
          s_companion_wizard
          2
          0 0 0
          0 0
          0 0
          16 0
          0 6
          #f ;; dead?
          'patch-conv
          sch_patch
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    25 10)
    (list
      (bind
        (kern-mk-char
          'ch_angela
          "Angela"
          sp_human
          nil
          s_townswoman
          2
          0 0 0
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'ange-conv
          sch_ange
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    4 19)
    (list
      (bind
        (kern-mk-char
          'ch_jess
          "Jess"
          sp_human
          nil
          s_townswoman
          2
          0 0 0
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'jess-conv
          sch_jess
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    23 26)
    (list
      (bind
        (kern-mk-char
          'ch_chester
          "Chester"
          sp_human
          oc_warrior
          s_townsman
          2
          0 0 0
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'ches-conv
          sch_ches
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    4 10)
    (list
      (bind
        (kern-mk-char
          'ch_steward
          "Victoria"
          sp_human
          nil
          s_lady
          2
          0 0 0
          0 0
          0 0
          16 0
          0 6
          #f ;; dead?
          'stew-conv
          sch_stew
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
        )
      ) ;; bind
    11 14)
    (list
      (bind
        (kern-mk-char
          'ch_ini
          "Ini"
          sp_human
          oc_warrior
          s_companion_paladin
          2
          5 0 5
          25 5
          0 0
          30 612
          0 3
          #f ;; dead?
          'ini-conv
          sch_ini
          nil
          nil ;; inventory
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
          )
        )
        (list
          #f
        )
      ) ;; bind
    29 28)
    (list
      (bind
        (kern-mk-char
          'ch_jeffreys
          "Jeff"
          sp_human
          oc_warrior
          s_companion_paladin
          2
          0 0 0
          0 0
          0 0
          15 0
          0 5
          #f ;; dead?
          'jeff-conv
          sch_jeff
          nil
          nil ;; inventory
          (list
            t_chain_coif
            t_sword
            t_armor_chain
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    19 14)
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          36 16384
          8 8
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    19 24)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'halberdier
        )
      ) ;; bind
    15 25)
  ) ;; end of objects in p_glasdrin
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
    'lock-inn-room-doors
  )
  (list ;; edge entrances
    (list 0 30 30) ;; Northwest
    (list 1 15 30) ;; North
    (list 2 0 30) ;; Northeast
    (list 3 30 15) ;; West
    (list 4 15 15) ;; Here
    (list 5 0 15) ;; East
    (list 6 30 0) ;; Southwest
    (list 7 15 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_glasdrin

(kern-load "alchemist.scm")

(kern-load "oscar.scm")

(kern-load "henry.scm")

(kern-load "bart.scm")

(kern-load "lia.scm")

(kern-load "fing.scm")

(kern-load "ghertie.scm")

(kern-mk-place 'p_oparine "Oparine"
  s_town ;; sprite
  (kern-mk-map
    nil     31 55 pal_expanded
    (list
      ".. .. .. .. .. .. .. .. .. .. .. .. cc cc cc cc cc cc .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. xx xx xx xx xx xx xx xx xx xx cc cc cc cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx .. "
      ".. .. xx .C .H .E .E .R .F .U .L xx cc cc cc cc cc cc xx ,, xx .B .I .L .G .E xx ,, ,, xx .. "
      ".. .. xx .B .U .N .K .M .A .T .E xx cc cc cc cc cc cc xx ,, xx .W .A .T .E .R xx ,, ,, xx .. "
      ".. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, xx cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. "
      ".. .. xx [[ @@ @@ @@ @@ @@ @@ ]] xx cc cc cc cc cc cc xx ,, xx [[ @@ @@ @@ ]] xx xx xx xx .. "
      ".. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ws cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
      ".. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc cc cc cc ws ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
      ".. .. xx xx xx xx ,, ,, xx xx xx sI cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
      ".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
      ".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
      ".. .. xx xx xx xx ,, ,, xx xx xx xx cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
      ".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc cc ws ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
      ".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
      ".. .. xx xx xx xx xx ,, xx xx xx xx cc cc cc cc cc cc xx xx xx xx sT ,, xx xx xx xx xx .. .. "
      ".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
      ".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
      ".. .. xx ,, ws xx xx xx xx cc cc cc cc cc cc ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ## _3 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 __ "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc oo __ __ __ __ __ oo __ __ __ __ __ oo __ __ "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
      ".. .. .. .. xx xx xx xx xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
      ".. .. .. .. xx ,, ,, ,, xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
      ".. .. .. .. xx ,, ,, ,, ,, cc cc cc cc cc cc cc oo __ __ ee ee ee oo __ __ __ __ __ oo __ __ "
      ".. .. .. .. xx ,, ,, ,, xx cc cc cc ## ## ## ## __ __ __ ee ee ee __ __ __ __ _8 __ __ __ __ "
      ".. xx xx xx xx xx xx xx xx cc cc cc ## _3 _1 __ __ __ __ ee ee ee __ __ _c ## ## ## _a __ __ "
      ".. xx ,A ,L ,K ,E ,M ,Y xx cc cc cc ## _2 __ __ __ __ __ ee ee ee __ __ ## ## ee ## ## __ __ "
      ".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _2 xx xx xx xx oo ee ee ee oo _c ## ee ee ee ## _a __ "
      ".. xx ,, ,, ,, ,, ,, ,, sP cc cc cc ## _2 xx ee ee xx ~~ ee ee ee __ ## ## ee ee ee ## ## __ "
      ".. xx ,, ,, aa && ,, ,, ,, cc cc cc ## _2 xx ee ee ee ee ee ee ee __ ## ee ee ee ee ee ## _2 "
      ".. xx ,, ,, ,, ,, ,, ,, xx cc cc cc ## _2 xx ee ee xx __ ee ee ee ee ee ee ee ee ee ee ## _2 "
      ".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _2 xx xx xx xx __ ee ee ee __ ## vv vv ee ee vv ## _2 "
      ".. xx [[ @@ @@ @@ @@ ]] xx cc cc cc ## _2 __ __ __ __ __ ee ee ee _c ## ee ee ee ee ee ## _2 "
      ".. xx xx xx xx xx xx xx xx cc cc cc ## ~a ~~ ~~ __ __ oo ee ee ee oo ## vv vv vv vv vv ## _2 "
      ".. .. .. .. .. .. .. .. bb .. .. .. bb .. ~% ~~ -- __ __ ee ee ee _5 ## ee ee ee ee ee ## _2 "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~a ~~ -- __ __ __ __ __ ## ## vv vv vv ## ## __ "
      ".. .. .. .. .. .. .. .. bb .. .. .. bb .. .. ~% ~~ ~~ -- __ __ __ __ _5 ## ee ee ee ## _3 __ "
      ".. xx xx xx xx xx xx xx .. .. .. .. .. .. .. .. ~a ~~ -- -- __ __ __ __ ## ## vv ## ## __ __ "
      ".. xx [[ .S .E .A ]] xx bb .. .. .. bb .. .. .. ~% ~~ ~~ -- __ __ __ __ _5 ## ## ## _3 __ __ "
      ".. xx .W .I .T .C .H sR .. .. .. .. .. .. .. .. ~C ~~ ~~ -- -- __ __ __ __ __ _1 __ __ __ __ "
      ".. xx ,, ,, ,, ,, ,, ws bb .. .. .. bb .. .. .. ~3 ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ "
      ".. xx ,, ,, ,, ,, ,, ,, .. .. .. .. .. .. .. ~C ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ "
      ".. xx ,, [[ @@ ]] ,, ws bb .. .. .. bb .. .. ~3 ~~ ~~ -- -- -- -- __ __ __ __ __ __ __ __ __ "
      ".. xx xx ,, ,, ,, xx xx .. .. .. .. .. .. .. ~2 ~~ ~~ ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ "
      ".. .. xx xx ,, xx xx .. bb .. .. .. oo .. oo ~8 ~8 ~~ ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ "
      ".. .. xx ,, ,, ,, xx .. .. .. .. .. ee ee ee .. .. bb ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
      ".. .. xx ,, ,, ,, ws .. bb .. .. .. ee ee ee .. .. .. ~a ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
      ".. .. xx xx ,, xx xx .. .. .. .. .. ee ee ee .. .. .. bb ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
      ".. .. .. bb .. bb .. .. .. .. bb .. oo ~~ oo .. .. .. ~3 ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. ~6 .. .. .. .. ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
      ".. .. bb ~3 ~1 ~5 bb .. .. .. .. ~C ~3 ~~ oo ee ee ee oo ~~ ~~ -- -- -- __ __ __ __ __ __ __ "
      "~C ~3 ~~ ~~ ~~ ~~ ~~ ~5 ~A .. ~C ~3 ~~ ~~ ~~ ee ee ee ~~ ~~ ~~ -- -- -- -- __ __ __ __ __ __ "
      "~~ ~~ -- -- -- -- -- ~~ ~~ ~1 ~~ ~~ ~~ ~c oo ee ee ee oo ~a ~~ ~~ -- -- -- __ __ __ __ __ __ "
      "-- -- -- __ __ __ -- -- -- -- -- ~~ ~c ~# .. .. .. .. .. ~% ~a ~~ ~~ -- -- __ __ __ __ __ __ "
      "-- -- -- __ __ __ -- -- -- -- -- ~~ ~# .. .. .. .. .. .. .. ~% ~~ ~~ -- -- -- __ __ __ __ __ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_oparine
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    3 9)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    3 12)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    6 15)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    10 12)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    10 9)
    (list
      (kern-tag 'oparine-inn-room-1-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    5 10)
    (list
      (kern-tag 'oparine-inn-room-2-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    5 13)
    (list
      (kern-tag 'oparine-inn-room-3-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #t
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    7 14)
    (list
      (kern-tag 'oparine-inn-room-4-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    8 12)
    (list
      (kern-tag 'oparine-inn-room-5-door
        (bind
          (kern-mk-obj t_door 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            0
            '()
            #f
            #t
            #f
            (list
              (list
                's_closed_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_open_door_in_stone
                #f
                0
              )
              (list
                's_locked_solid_wood_door_in_stone
                #t
                5
              )
              (list
                's_magically_locked_solid_wood_door_in_stone
                #t
                5
              )
            )
          )
        ) ;; bind
      ) ;; kern-tag
    8 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 17)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    17 29)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    7 41)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 47)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 44)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    3 45)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    8 29)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    8 23)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    5 22)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    19 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    23 14)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    20 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    26 4)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    27 2)
    (list
      (bind
        (kern-mk-char
          'ch_alchemist
          "Alchemist"
          sp_human
          oc_wright
          s_companion_tinker
          2
          0 0 0
          0 0
          0 0
          18 0
          10 8
          #f ;; dead?
          'alch-conv
          sch_alch
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          (list
            #f
            #f
            #f
          )
          #f
          #f
        )
      ) ;; bind
    3 28)
    (list
      (bind
        (kern-mk-char
          'ch_oscar
          "Oscar"
          sp_human
          nil
          s_townsman
          2
          0 0 0
          0 0
          0 0
          11 0
          0 1
          #f ;; dead?
          'oscar-conv
          sch_oscar
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    21 7)
    (list
      (bind
        (kern-mk-char
          'ch_henry
          "Henry"
          sp_human
          nil
          s_townsman
          2
          0 0 0
          0 0
          0 0
          16 0
          0 6
          #f ;; dead?
          'henry-conv
          sch_henry
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    21 4)
    (list
      (bind
        (kern-mk-char
          'ch_bart
          "Bart"
          sp_forest_goblin
          nil
          s_orc
          2
          0 0 0
          0 0
          0 0
          9 0
          0 1
          #f ;; dead?
          'bart-conv
          sch_bart
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    22 6)
    (list
      (bind
        (kern-mk-char
          'ch_lia
          "Lia"
          sp_human
          oc_wizard
          s_townswoman
          2
          0 0 0
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'lia-conv
          sch_lia
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    3 49)
    (list
      (bind
        (kern-mk-char
          'ch_fing
          "Fing"
          sp_nixie
          oc_warrior
          s_nixie
          2
          0 0 0
          0 0
          0 0
          13 0
          0 3
          #f ;; dead?
          'fing-conv
          sch_fing
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    3 50)
    (list
      (bind
        (kern-mk-char
          'ch_ghertie
          "Ghertie"
          sp_ghast
          oc_warrior
          s_ghost
          2
          0 0 0
          0 0
          0 0
          16 0
          0 6
          #f ;; dead?
          'ghertie-conv
          sch_ghertie
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          (list
            #f
            #f
            #f
          )
        )
      ) ;; bind
    6 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    2 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    11 7)
  ) ;; end of objects in p_oparine
  (list ;; on-entry-hooks
    'lock-inn-room-doors
  )
  (list ;; edge entrances
    (list 0 30 54) ;; Northwest
    (list 1 16 54) ;; North
    (list 2 0 54) ;; Northeast
    (list 3 30 21) ;; West
    (list 4 15 27) ;; Here
    (list 5 0 19) ;; East
    (list 6 30 0) ;; Southwest
    (list 7 15 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_oparine

(kern-mk-place 'p_traps_1 "Riddles"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx rr rr "
      "rr rr rr xx xx xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, x! rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx rr rr "
      "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
      "rr rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr x! ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx rr rr rr "
      "rr rr xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_traps_1
    (list
      (bind
        (kern-mk-obj t_step_riddle 1
          ;; hooks
          (list
          )
        )
        (list
          'egg
          't_lava
          3
          5
          3
          9
          (list
            "All who would pass must answer the riddle:

"
            "  In a marble hall white as milk
"
            "  Lined with skin soft as silk
"
            "  Within a fountain crystal clear
"
            "  A golden apple doth appear
"
            "  No doors there are to this stronghold
"
            "  Yet thieves break in to steal its gold.
"
          )
        )
      ) ;; bind
    4 14)
    (list
      (bind
        (kern-mk-obj t_step_riddle 1
          ;; hooks
          (list
          )
        )
        (list
          'few
          't_lava
          8
          5
          3
          9
          (list
            "All who would pass must answer the riddle:

"
            "  I know a word of letters three.
"
            "  Add two, and fewer there will be."
          )
        )
      ) ;; bind
    9 4)
    (list
      (bind
        (kern-mk-obj t_step_riddle 1
          ;; hooks
          (list
          )
        )
        (list
          'eye
          't_lava
          13
          5
          3
          9
          (list
            "All who would pass must answer the riddle:

"
            "  Pronounced as one letter,
"
            "  but written with three.
"
            "  Two letters there are
"
            "  and two only in me.
"
            "  I'm double, and single,
"
            "  and black, blue and gray.
"
            "  When read from both ends
"
            "  I'm the same either way."
          )
        )
      ) ;; bind
    14 14)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_2
          9
          15
        )
      ) ;; bind
    14 2)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_bole
          43
          6
        )
      ) ;; bind
    4 16)
  ) ;; end of objects in p_traps_1
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_traps_1

(kern-mk-place 'p_traps_2 "The Choice"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, xx xx "
      "xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx "
      "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, xx xx ,, xx xx xx xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
      "xx xx xx xx ,, xx xx xx xx ~x xx xx xx xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx .C .H ~O .O .S .E xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx !! !! ~! !! !! !! xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, ,, xx ,, xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx !! !! !! !! !! !! xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx .W .I .S .E .L .Y xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_traps_2
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'mk-bandit
        )
      ) ;; bind
    4 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    14 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    14 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 8)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_3
          9
          9
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_1
          14
          2
        )
      ) ;; bind
    9 15)
    (list
      (bind
        (kern-mk-obj t_step_clue 1
          ;; hooks
          (list
          )
        )
        (list
          "Doesn't the other way look better?"
        )
      ) ;; bind
    13 15)
    (list
      (bind
        (kern-mk-obj t_step_clue 1
          ;; hooks
          (list
          )
        )
        (list
          "Doesn't the other way look better?"
        )
      ) ;; bind
    6 15)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 14)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 13)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 12)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 11)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 10)
    (list
      (kern-mk-obj t_secret_path 1
        ;; hooks
        (list
          (list
            ef_permanent_invisibility
            '()
            2
            0
          )
        )
      )
    9 9)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_2
          14
          3
        )
      ) ;; bind
    4 3)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_2
          4
          3
        )
      ) ;; bind
    14 3)
  ) ;; end of objects in p_traps_2
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_traps_2

(kern-load "traps_3_mechs.scm")

(kern-mk-place 'p_traps_3 "Fun with Levers"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, x! xx xx xx x! ,, x! xx xx xx x! ,, xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, x! xx xx xx x! ,, x! xx xx xx x! ,, xx xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_traps_3
    (list
      (kern-tag 't3_pc6
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    12 15)
    (list
      (kern-tag 't3_pc7
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    3 6)
    (list
      (kern-tag 't3_pc8
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    9 6)
    (list
      (kern-tag 't3_pc9
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    15 6)
    (list
      (kern-tag 't3_pc10
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    3 12)
    (list
      (kern-tag 't3_pc11
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    9 12)
    (list
      (kern-tag 't3_pc12
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    15 12)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          0
        )
      ) ;; bind
    3 3)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          1
        )
      ) ;; bind
    15 3)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          2
        )
      ) ;; bind
    3 9)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          3
        )
      ) ;; bind
    15 9)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          4
        )
      ) ;; bind
    3 15)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          5
        )
      ) ;; bind
    9 15)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          't3_ctrl
          #f
          6
        )
      ) ;; bind
    15 15)
    (list
      (kern-tag 't3_ctrl
        (bind
          (kern-mk-obj t_t3_ctrl 1
            ;; hooks
            (list
            )
          )
          (list
            (list
              't3_pc1
              't3_pc2
              't3_pc3
              't3_pc4
              't3_pc5
              't3_pc6
              't3_pc7
              't3_pc8
              't3_pc9
              't3_pc10
              't3_pc11
              't3_pc12
            )
            (list
              #f
              #t
              #f
              #t
              #f
              #t
              #t
              #f
              #t
              #f
              #t
              #f
            )
            (list
              #t
              #t
              #t
              #t
              #t
              #t
              #f
              #f
              #f
              #f
              #f
              #f
            )
            (list
              #t
              #f
              #t
              #f
              #t
              #f
              #f
              #t
              #f
              #t
              #f
              #t
            )
            (list
              #f
              #t
              #f
              #t
              #f
              #t
              #f
              #t
              #f
              #t
              #f
              #t
            )
            (list
              #f
              #f
              #f
              #f
              #f
              #f
              #t
              #t
              #t
              #t
              #t
              #t
            )
            (list
              #t
              #f
              #t
              #f
              #t
              #f
              #t
              #f
              #t
              #f
              #t
              #f
            )
            (list
              #f
              #f
              #t
              #t
              #t
              #t
              #t
              #t
              #t
              #t
              #f
              #f
            )
          )
        ) ;; bind
      ) ;; kern-tag
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_2
          9
          9
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_4
          9
          9
        )
      ) ;; bind
    9 3)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'queen-spider
        )
      ) ;; bind
    6 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    6 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'yellow-slime
        )
      ) ;; bind
    15 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bandit
        )
      ) ;; bind
    15 3)
    (list
      (kern-tag 't3_pc1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    6 3)
    (list
      (kern-tag 't3_pc2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    6 9)
    (list
      (kern-tag 't3_pc3
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    6 15)
    (list
      (kern-tag 't3_pc4
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    12 3)
    (list
      (kern-tag 't3_pc5
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    12 9)
  ) ;; end of objects in p_traps_3
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_traps_3

(kern-mk-place 'p_traps_4 "Labyrinth of Burning Glass"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ,, pp ~, ~p ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ~, ,, xx "
      "xx ,, pp ~, pp ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ~, ,, ,, ,, ,, ,, ,, ,, ~, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ,, pp ,, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_traps_4
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_3
          9
          3
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_thiefs_den
          9
          9
        )
      ) ;; bind
    9 7)
  ) ;; end of objects in p_traps_4
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_traps_4

(kern-load "mouse.scm")

(kern-mk-place 'p_thiefs_den "Thief's Den"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_thiefs_den
    (list
      (kern-mk-obj t_picklock 1
        ;; hooks
        (list
        )
      )
    8 6)
    (list
      (kern-mk-obj t_gem 1
        ;; hooks
        (list
        )
      )
    8 7)
    (list
      (kern-mk-obj t_an_tym_scroll 1
        ;; hooks
        (list
        )
      )
    9 7)
    (list
      (kern-mk-obj t_in_mani_corp_scroll 1
        ;; hooks
        (list
        )
      )
    10 7)
    (list
      (kern-mk-obj t_vas_rel_por_scroll 1
        ;; hooks
        (list
        )
      )
    10 8)
    (list
      (kern-mk-obj t_sanct_lor_scroll 1
        ;; hooks
        (list
        )
      )
    10 10)
    (list
      (kern-mk-obj t_in_quas_xen_scroll 1
        ;; hooks
        (list
        )
      )
    9 10)
    (list
      (kern-mk-obj t_an_xen_exe_scroll 1
        ;; hooks
        (list
        )
      )
    8 9)
    (list
      (kern-mk-obj t_in_an_scroll 1
        ;; hooks
        (list
        )
      )
    7 9)
    (list
      (kern-mk-obj t_in_ex_por_scroll 1
        ;; hooks
        (list
        )
      )
    6 9)
    (list
      (kern-mk-obj t_vas_mani_scroll 1
        ;; hooks
        (list
        )
      )
    6 8)
    (list
      (kern-mk-obj t_wis_quas_scroll 1
        ;; hooks
        (list
        )
      )
    6 7)
    (list
      (kern-mk-obj t_gold_coins 74
        ;; hooks
        (list
        )
      )
    6 2)
    (list
      (kern-mk-obj t_gold_coins 112
        ;; hooks
        (list
        )
      )
    2 9)
    (list
      (kern-mk-obj t_gold_coins 243
        ;; hooks
        (list
        )
      )
    1 17)
    (list
      (kern-mk-obj t_gold_coins 30
        ;; hooks
        (list
        )
      )
    7 3)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    3 1)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_mouse
            "Mouse"
            sp_human
            nil
            s_brigand
            2
            0 0 10
            2 2
            1 1
            18 0
            10 8
            #f ;; dead?
            'mouse-conv
            nil
            nil
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              (list
                (list 1 t_rune_k)
                (list 1 t_sword)
                (list 50 t_arrow)
              )
              ;; hooks
              (list
              )
            )
            (list
              t_leather_helm
              t_bow
              t_armor_leather
            )
            ;; hooks
            (list
            )
          )
        #t) ;; kern-char-force-drop
        (list
          #t
        )
      ) ;; bind
    3 3)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_traps_4
          9
          7
        )
      ) ;; bind
    9 9)
    (list
      (kern-mk-obj t_food 10
        ;; hooks
        (list
        )
      )
    4 1)
    (list
      (kern-mk-obj t_wine 1
        ;; hooks
        (list
        )
      )
    4 2)
    (list
      (kern-mk-obj t_cure_potion 1
        ;; hooks
        (list
        )
      )
    3 4)
    (list
      (kern-mk-obj t_mana_potion 1
        ;; hooks
        (list
        )
      )
    4 5)
    (list
      (kern-mk-obj t_heal_potion 1
        ;; hooks
        (list
        )
      )
    5 5)
    (list
      (kern-mk-obj t_poison_immunity_potion 1
        ;; hooks
        (list
        )
      )
    6 5)
    (list
      (kern-mk-obj t_inv_potion 1
        ;; hooks
        (list
        )
      )
    7 5)
    (list
      (kern-mk-obj t_torch 1
        ;; hooks
        (list
        )
      )
    8 5)
  ) ;; end of objects in p_thiefs_den
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_thiefs_den

(kern-load "anne.scm")

(kern-load "jones.scm")

(kern-load "alex.scm")

(kern-mk-place 'p_treasury "Treasury"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
      "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx xx "
      "xx xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, xx xx xx xx ,, xx xx xx xx ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx "
      "xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx "
      "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_great_hall "Great Hall"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "xx xx xx xx xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, xx xx ,, cc cc cc ,, xx xx ,, ,, ,, ,, xx "
            "x! ,, ,, ,, ,, xx x! ,, cc cc cc ,, x! xx ,, ,, ,, ,, x! "
            ",, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, "
            "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
            "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
            "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
            ",, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, "
            "x! ,, ,, ,, ,, xx x! ,, cc cc cc ,, x! xx ,, ,, ,, ,, x! "
            "xx ,, ,, ,, ,, xx xx ,, cc cc cc ,, xx xx ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
            "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        (list
          (list
            (kern-mk-place 'p_paladins_hold "Paladin's Hold"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "xx xx xx xx xx xx xx xx xx xx xx xx xx && xx xx xx xx xx "
                  "xx xx xx xx xx xx xx xx xx x! ,, ,, ,, ,, ,, ,, ,, x! xx "
                  "xx xx ,, ,, ,, xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
                  "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, xx "
                  "xx xx ,, ,, ,, xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
                  "xx xx xx xx xx xx ,, xx xx x! ,, ,, ,, ,, ,, ,, ,, x! xx "
                  "xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx "
                  "xx xx xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, "
                  "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
                  "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
                  "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
                  "xx xx xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! .. ,, ,, ,, "
                  "xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx "
                  "xx .A .R .M .S xx xx xx ,, xx xx xx xx .M .E .D .I .K xx "
                  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, sH ,, ,, ,, ,, ,, xx "
                  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
                  "xx ,, ,, ,, ,, sA ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
                  "xx ,, ,, ,, ,, xx xx xx ,, xx xx xx xx ,, ,, ,, ,, ,, xx "
                  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              nil ;; neighbors
              (list ;; objects in p_paladins_hold
                (list
                  (bind
                    (kern-mk-char
                      'ch_anne
                      "Anne"
                      sp_human
                      oc_wizard
                      s_companion_wizard
                      2
                      0 0 0
                      0 0
                      0 0
                      14 0
                      6 4
                      #f ;; dead?
                      'anne-conv
                      sch_anne
                      nil
                      nil ;; inventory
                      nil
                      ;; hooks
                      (list
                      )
                    )
                    '()
                  ) ;; bind
                10 1)
                (list
                  (bind
                    (kern-mk-char
                      'ch_jones
                      "Jones"
                      sp_human
                      nil
                      s_townsman
                      2
                      0 0 0
                      0 0
                      0 0
                      16 0
                      8 6
                      #f ;; dead?
                      'jones-conv
                      sch_jones
                      nil
                      nil ;; inventory
                      nil
                      ;; hooks
                      (list
                      )
                    )
                    '()
                  ) ;; bind
                4 7)
                (list
                  (bind
                    (kern-mk-char
                      'ch_alex
                      "Alex"
                      sp_human
                      oc_wizard
                      s_companion_wizard
                      2
                      0 0 0
                      0 0
                      0 0
                      18 0
                      10 8
                      #f ;; dead?
                      'alex-conv
                      sch_alex
                      nil
                      nil ;; inventory
                      nil
                      ;; hooks
                      (list
                      )
                    )
                    '()
                  ) ;; bind
                10 1)
                (list
                  (bind
                    (kern-mk-obj t_custom_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'put-gate-guard
                      'php1
                      'deep
                    )
                  ) ;; bind
                11 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'crossbowman
                    )
                  ) ;; bind
                13 8)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'crossbowman
                    )
                  ) ;; bind
                13 10)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'halberdier
                    )
                  ) ;; bind
                15 10)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'halberdier
                    )
                  ) ;; bind
                15 8)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                17 9)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'skeletal-spear-thrower
                    )
                  ) ;; bind
                18 7)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'skeletal-spear-thrower
                    )
                  ) ;; bind
                18 11)
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (kern-tag 'php1
                    (bind
                      (kern-mk-obj t_portcullis 1
                        ;; hooks
                        (list
                        )
                      )
                      (list
                        #f
                        '()
                        #f
                        '()
                      )
                    ) ;; bind
                  ) ;; kern-tag
                14 9)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                5 15)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                12 15)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                13 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                15 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                17 17)
                (list
                  (kern-mk-obj t_bed 1
                    ;; hooks
                    (list
                    )
                  )
                17 15)
                (list
                  (bind
                    (kern-mk-obj t_ladder_down 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_forsaken_prison
                      9
                      9
                    )
                  ) ;; bind
                8 17)
                (list
                  (bind
                    (kern-mk-obj t_ladder_up 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_watchpoint
                      9
                      14
                    )
                  ) ;; bind
                3 9)
              ) ;; end of objects in p_paladins_hold
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_paladins_hold

          3)
          (list
            (kern-mk-place 'p_death_knights_hold "Death Knight's Hold"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb rr rr rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb rr rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb rr "
                  "xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. rr "
                  ",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
                  ",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
                  ",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
                  "xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb rr rr "
                  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb .. bb rr "
                  "xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              nil ;; neighbors
              (list ;; objects in p_death_knights_hold
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                14 16)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                7 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                9 2)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                9 16)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                14 6)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                14 12)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'warlock
                    )
                  ) ;; bind
                14 9)
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #t
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                10 2)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #t
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                14 5)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #f
                      #t
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                14 13)
                (list
                  (bind
                    (kern-mk-obj t_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      #f
                      0
                      '()
                      #f
                      #t
                      #f
                      (list
                        (list
                          's_closed_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_open_door_in_stone
                          #f
                          0
                        )
                        (list
                          's_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                        (list
                          's_magically_locked_solid_wood_door_in_stone
                          #t
                          5
                        )
                      )
                    )
                  ) ;; bind
                10 16)
                (list
                  (bind
                    (kern-mk-obj t_ladder_down 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_altar_room
                      17
                      17
                    )
                  ) ;; bind
                17 9)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'craven-archer
                    )
                  ) ;; bind
                5 5)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'craven-archer
                    )
                  ) ;; bind
                5 13)
                (list
                  (bind
                    (kern-mk-obj t_guard_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'craven-archer
                    )
                  ) ;; bind
                11 9)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'death-knight
                    )
                  ) ;; bind
                14 2)
              ) ;; end of objects in p_death_knights_hold
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_death_knights_hold

          5)
        ) ;; end neighbors of p_great_hall
        (list ;; objects in p_great_hall
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'death-knight
              )
            ) ;; bind
          10 8)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'death-knight
              )
            ) ;; bind
          10 9)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'death-knight
              )
            ) ;; bind
          10 10)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'warlock
              )
            ) ;; bind
          12 9)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'skeletal-warrior
              )
            ) ;; bind
          9 13)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'skeletal-spear-thrower
              )
            ) ;; bind
          10 14)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-mk-obj t_ladder_down 1
                ;; hooks
                (list
                )
              )
              (list
                'p_pools
                9
                9
              )
            ) ;; bind
          9 6)
          (list
            (bind
              (kern-mk-obj t_hidden 1
                ;; hooks
                (list
                )
              )
              (list
                't_hidden_mech
                1
              )
            ) ;; bind
          13 0)
          (list
            (bind
              (kern-mk-obj t_disg_lvr 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                'gh_wall
                #f
                's_wall_torch
              )
            ) ;; bind
          13 0)
          (list
            (kern-tag 'gh_wall
              (bind
                (kern-mk-obj t_terrain_blitter 1
                  ;; hooks
                  (list
                  )
                )
                (list
                  'p_great_hall
                  8
                  0
                  3
                  1
                  'm_hall_section
                )
              ) ;; bind
            ) ;; kern-tag
          0 0)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'halberdier
              )
            ) ;; bind
          8 8)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'halberdier
              )
            ) ;; bind
          8 10)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'crossbowman
              )
            ) ;; bind
          6 8)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'medik
              )
            ) ;; bind
          7 9)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'crossbowman
              )
            ) ;; bind
          6 10)
        ) ;; end of objects in p_great_hall
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_great_hall

    7)
  ) ;; end neighbors of p_treasury
  (list ;; objects in p_treasury
    (list
      (kern-tag 'tr-be2
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            9
            6
            1
            1
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    9 6)
    (list
      (kern-tag 'tr-be3
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            9
            7
            1
            1
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    9 7)
    (list
      (kern-tag 'tr-be4
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            9
            8
            1
            1
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    9 8)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-be1
          #f
          '()
        )
      ) ;; bind
    1 1)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-be2
          #f
          '()
        )
      ) ;; bind
    17 1)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-be3
          #f
          '()
        )
      ) ;; bind
    17 17)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-be4
          #f
          '()
        )
      ) ;; bind
    1 17)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_treasury2
          9
          9
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    15 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    16 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    17 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    15 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    16 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    17 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    15 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    16 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'green-slime
        )
      ) ;; bind
    17 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    15 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    16 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    17 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    15 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    16 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    17 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    15 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    16 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    17 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    1 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    2 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    3 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    1 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    2 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    3 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    1 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    2 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    3 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    1 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    2 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    3 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    1 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    2 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    3 16)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    1 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    2 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    3 17)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (kern-tag 'tr-ba
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            2
            2
            3
            3
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    2 2)
    (list
      (kern-tag 'tr-bb
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            14
            2
            3
            3
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    14 2)
    (list
      (kern-tag 'tr-bc
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            14
            14
            3
            3
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    14 14)
    (list
      (kern-tag 'tr-bd
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            2
            14
            3
            3
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    2 14)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-ba
          #f
          '()
        )
      ) ;; bind
    11 11)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-bb
          #f
          '()
        )
      ) ;; bind
    7 11)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-bc
          #f
          '()
        )
      ) ;; bind
    7 7)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'tr-bd
          #f
          '()
        )
      ) ;; bind
    11 7)
    (list
      (kern-tag 'tr-be1
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_treasury
            9
            5
            1
            1
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    9 5)
  ) ;; end of objects in p_treasury
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_treasury

p_death_knights_hold
p_great_hall
p_paladins_hold
(kern-mk-place 'p_absalot "Absalot"
  s_ruin ;; sprite
  (kern-mk-map
    nil     41 36 pal_expanded
    (list
      "tt tt tt tt tc .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc .. .. .. .. .. .. .. .. .. .. .. .. .. ta tt tt tt tt tt "
      "tt xx xx xx xx xx xx xx xx xx xx xx xx .. .. .. xx xx xx xx .. cc .. xx xx xx xx .. .. %7 .. .. .. .. .. t% ta tt tt tt tt "
      "tt xx ,, xx xx xx xx xx xx xx xx xx xx .. .. .. xx ,, ,, xx .. cc .. xx ,, ,, xx .. %b %% %d .. .. .. .. .. .. .. t% ta tt "
      "tt xx ?? xx .L .I .B .R .A .R .Y xx xx xx .. .. xx ,, .. .. .. cc .. ,, ,, ,, xx .. .. %e .. .. .. .. .. .. .. .. .. t% ta "
      "tc rr ,, ,, ,, ,, tt ,, ,, .. ,, ,, ,, xx .. .. xx xx xx .. .. cc .. xx xx xx xx .. .. .. xx xx xx xx xx xx xx xx xx .. .. "
      ".. xx ,, ,, ,, t3 tt td ,, ,, ,, ,, ,, rr .. .. .. .. .. .. .. cc .. .. .. .. .. .. .. xx bb .A .C .A .D .E .M .Y xx xx .. "
      ".. xx .. ,, 00 te ,, 00 ,, .. 00 ,, ,, xx .. .. xx xx rr rr .. cc .. xx xx xx .. .. .. bb ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. "
      ".. xx ,, ,, 00 ,, ,, 00 ,, ,, 00 ,, ,, xx .. .. xx ,, ,, rr .. cc .. ,, ,, ,, xx .. .. xx rr ,, ,, ,, ,, .. ,, ,, ,, xx .. "
      ".. xx ,, ,, 00 ,, ,, 00 ,, .. 00 ,, ,, xx .. .. xx ,, ,, ,, .. cc .. xx ,, ,, xx .. .. xx ,, ,, 00 ,, ,, ,, 00 ,, ,, xx .. "
      ".. rr ,, ,, .. ,, ,, 00 ,, ,, 00 ,, ,, rr .. .. xx xx .. xx .. cc .. bb xx xx xx .. .. xx ,, .. .. ,, ,, ,, 00 ,, ,, xx .. "
      ".. xx ,, ,, 00 ,, ,, 00 ,, ,, 00 ,, ,, rr .. .. .. .. .. .. .. cc .. .. .. .. .. %7 .. xx ,, ,, 00 00 00 00 00 ,, ,, xx .. "
      ".. tC t7 ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tC t7 pp ,, pp ,, ,, cc ,, ,, pp .. %3 %% %% rr ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. "
      ".. tb tt bb ,, ,, ,, ,, ,, ,, ,, .. ,, xx tb tt td ,, .. ,, ,, cc ,, ,, ,, .. %a %% .. rr .. ,, ,, .. ,, ,, rr ,, ,, xx .. "
      ".. t% ta td xx xx xx ,, .. xx xx xx rr xx t% te pp ,, ,, ,, ,, cc ,, ,, ,, ,, .. %e .. rr rr xx xx xx ,, rr xx xx xx xx .. "
      ".. .. .. .. .. .. .. cc .. .. .. .. .. .. .. .. .. .. ,, ,, .. cc cc ,, ,, ,, ,, .. .. .. .. .. .. .. cc .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ,, ,, cc cc cc cc cc cc cc ,, ,, .. .. .. .. .. .. .. cc .. .. .. .. .. .. "
      "cc cc cc cc cc .. cc cc cc cc cc cc .. cc cc cc cc cc cc .F .O .R .U .M cc cc cc cc cc cc cc .. cc cc .. cc cc cc cc cc cc "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ,, ,, cc cc .. cc cc cc cc ,, ,, .. .. .. .. cc .. .. .. .. .. bb .. .. bb "
      "%5 .. .. .. xx xx ,, xx .. .. xx xx ,, xx .. .. ,, ,, ,, .. cc cc cc ,, ,, ,, ,, .. .. .. xx ,, rr xx ,, xx .. .. .. .. .. "
      "%% %5 .. .. xx ,, ,, xx .. .. xx .. ,, xx .. .. pp ,, ,, .. .. cc ,, .. ,, ,, pp .. .. xx ,, ,, xx ,, .. xx tC t7 tA .. bb "
      "%% %% .. .. xx ,, ,, rr %5 .. xx ,, ,, xx .. .. ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, .. .. xx .. ,, xx ,, .. .. tb tt td .. .. "
      "%% %% .. .. xx xx rr rr %% .. xx xx xx xx t7 tA .. ,, pp ,, ,, cc ,, ,, pp ,, pp .. .. rr xx xx .. .. .. xx .. bb .. .. bb "
      "%% %% %5 .. .. .. .. %% %c .. .. .. .. tb tt td .. .. .. .. .. cc .. .. .. .. .. tC t7 tA .. .. .. .. .. .. .. .. .. .. .. "
      "%% %% %c .. %3 %% %% %% .. .. .. .. .. t% te t# .. .. .. .. .. cc tC t7 tA .. .. tb tt td .. .. .. .. .. .. .. bb .. .. bb "
      "%% %c .. .. %a %% %% %% .. .. .. xx xx xx xx bb xx rr .. .. .. cc tb tt td tA .. bb tt bb .. bb .. bb .. .. .. .. .. .. .. "
      "%% .. .. .. .. %a %% %c .. .. xx .. ,, ,, ,, ,, ,, xx xx .. .. cc t% te tF t7 bb .G .A .R .. .E .N .S bb .. .. bb .. .. bb "
      "%% .. %3 %5 .. .. %% .. .. .. xx .M .. .G .I .C .K .S xx .. .. cc .. t% tb tt td .. .. tt tt tt .. .. .. .. .. .. .. .. .. "
      "%% %% %% %% %5 .. %% %5 .. .. xx ,, ,, ,, ,, ,, ,, ,, sA .. .. bb .. .. bb te bb .. t3 tt tt tt t5 .. bb .. bb .. .. .. bb "
      "%% %% %% bb %% %% %% %% %5 .. xx ,, ,, ,, .. ,, ,, ,, ,, cc cc .. .. .. .. .. .. .. ta tt tt tt tt tA .. .. .. .. .. .. .. "
      "%% %% %% %% ~5 %% %% %% %c .. xx ,, ,, ,, ,, ,, .. .. xx .. .. cc .. .. .. .. .. .. t% tt tt tt tt tt tt t5 tA .. .. bb .. "
      "%% bb %% ~3 ~~ %% %% bb .. .. .. .F .U .T .U .R .E .S xx .. .. cc .. .. .. %3 %% %5 .. ta tt tt tt tt tt tt tt t5 .. .. .. "
      "%% .. %% ~2 ~~ ~5 %% %% .. .. xx xx ,, ,, ,, ,, ,, xx xx .. .. cc bb .. bb %% bb %% %5 .. t% ta tt tt bb tt bb tt bb tC t3 "
      "%% bb %% ~a ~8 ~c %% bb .. .. %a xx xx .. aa ,, xx xx .. %7 .. cc .. %3 %% %% %% %% %% %5 .. t% tt tt tt tt tt tt tt tt tt "
      "%% %% %% %% %% %% %% %c .. %7 .. %% xx ,, ,, ,, xx %% %% %% .. .. .. %% %% %% %% %% %% %% %5 .. tt bb tt tt tt tt || || || "
      "%% %% %% bb %% bb %% .. .. %% .. %% xx xx xx xx xx %% %% %c .. cc .. %a %% %% %% %% %% %% %% bb ta tt tt tt tt || || || || "
      "%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. cc .. .. %% %% %% %% %% %% %% .. .. t% tt || || || || || || "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_absalot
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    23 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    12 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    31 20)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    35 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    3 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    23 19)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    28 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    36 30)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    12 19)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_old_absalot
          11
          27
        )
      ) ;; bind
    2 2)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    38 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 32)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    21 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    39 25)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    25 26)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    34 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    24 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    37 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 31)
  ) ;; end of objects in p_absalot
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 40 35) ;; Northwest
    (list 1 21 35) ;; North
    (list 2 0 35) ;; Northeast
    (list 3 40 16) ;; West
    (list 4 20 18) ;; Here
    (list 5 0 16) ;; East
    (list 6 40 0) ;; Southwest
    (list 7 21 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_absalot

(kern-load "silas.scm")

(kern-load "dennis.scm")

(kern-load "selene.scm")

(kern-mk-place 'p_old_absalot "Old Absalot"
  nil ;; sprite
  (kern-mk-map
    nil     31 31 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr xx xx ,T ,E ,M ,P ,L ,E xx xx rr rr rr ,, ,, ,, rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr xx !! cc cc cc cc cc cc !! xx xx rr rr ,, ,, ,, rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr !! !! cc aa cc cc aa cc !! !! xx xx rr ,, ,, ,, rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx !! !! !! cc cc cc cc cc cc !! !! !! xx rr ,, ,, ,, rr rr rr rr rr rr rr "
      "rr rr rr rr ,, rr xx ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, xx rr ,, ,, ,, rr rr rr rr rr rr rr "
      "rr rr rr rr ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr ,, rr rr rr rr rr rr rr rr rr "
      "rr rr rr ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr ,, rr ,M ,E ,A ,T rr rr rr rr "
      "rr rr rr ,, ,, ,, ,, ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, ,, ,, ,, rr ,, ,, ,, ,, rr rr rr rr "
      "rr rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr ,, ,, ,, ,, rr rr rr rr "
      "xx xx xx rr xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr ,, ,, ,, ,, ,, ,, rr rr rr rr "
      "rr ,S ,L ,A ,V ,E ,S xx rr xx ,, pp ,, ,, pp ,, rr xx xx rr rr ,, xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr ,, xx ,F ,L ,E ,S ,H ,L ,Y xx "
      "xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr ,, rr ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, xx rr xx ,, pp ,, ,, pp ,, xx rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx rr ,, ,, xx xx xx rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr rr xx ,, ,, ,, aa ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, rr rr xx xx xx ,, ,, xx xx xx rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, ,, xx rr rr rr rr rr xx ,, ,, rr rr rr rr rr xx xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, xx rr rr rr xx ,, ,, xx rr rr rr xx ,, ,, ,, xx ,, ,, [[ @@ ]] ,, ,, xx "
      "xx xx xx ,, ,, xx xx xx xx xx xx rr ,, ,, xx xx rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx ,F ,E ,A ,S ,T xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr xx xx xx xx rr rr xx "
      "xx xx xx ,, xx xx xx xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx xx rr cc cc cc cc cc rr "
      "xx xx xx ,, xx xx xx xx rr xx ,, ,, ,, xx xx ,, ,, ,, xx ,B ,L ,O ,O ,D xx cc cc ~~ ~~ cc rr "
      "xx ,, xx ,, xx xx xx xx rr xx ,, ,, ,, rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc rr "
      "xx xx xx ,, xx xx xx xx rr ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ~~ aa cc cc rr "
      "xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc xx "
      "xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx rr rr xx ,B ,A ,T ,H ,S xx cc cc ~~ ~~ cc xx "
      "xx ,, ,, ,, ,, ,, xx xx xx xx ,, ,, ,, xx rr rr rr rr xx xx xx xx xx xx xx cc cc cc cc cc xx "
      "xx xx xx rr rr rr xx xx rr rr xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_old_absalot
    (list
      (kern-tag 'oa-ps2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            'oa-ps1
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    4 19)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'oa-ps2
          #f
          '()
        )
      ) ;; bind
    2 20)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'oa-cp1
          #f
          '()
        )
      ) ;; bind
    1 14)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'oa-cp2
          #f
          '()
        )
      ) ;; bind
    2 14)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'oa-cp3
          #f
          '()
        )
      ) ;; bind
    5 14)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'oa-cp4
          #f
          '()
        )
      ) ;; bind
    6 14)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 23)
    (list
      (kern-mk-field f_poison_perm -1)    3 24)
    (list
      (kern-mk-field f_sleep_perm -1)    3 25)
    (list
      (kern-mk-field f_energy_perm -1)    3 26)
    (list
      (kern-mk-field f_fire_perm -1)    3 27)
    (list
      (kern-mk-field f_poison_perm -1)    3 28)
    (list
      (kern-mk-field f_poison_perm -1)    2 27)
    (list
      (kern-mk-field f_poison_perm -1)    4 27)
    (list
      (kern-mk-field f_sleep_perm -1)    1 27)
    (list
      (kern-mk-field f_sleep_perm -1)    2 28)
    (list
      (kern-mk-field f_sleep_perm -1)    3 29)
    (list
      (kern-mk-field f_sleep_perm -1)    4 28)
    (list
      (kern-mk-field f_sleep_perm -1)    5 27)
    (list
      (kern-mk-field f_energy_perm -1)    1 28)
    (list
      (kern-mk-field f_energy_perm -1)    2 29)
    (list
      (kern-mk-field f_energy_perm -1)    4 29)
    (list
      (kern-mk-field f_energy_perm -1)    5 28)
    (list
      (kern-mk-field f_fire_perm -1)    1 29)
    (list
      (kern-mk-field f_fire_perm -1)    5 29)
    (list
      (bind
        (kern-mk-obj t_hidden 1
          ;; hooks
          (list
          )
        )
        (list
          't_rune_s
          1
        )
      ) ;; bind
    4 29)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    22 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    26 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    26 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    26 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    25 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    25 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    24 8)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    4 6)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 1 t_demon_gate_book)
        )
        ;; hooks
        (list
        )
      )
    4 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    19 8)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    21 6)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    21 1)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    23 1)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    23 3)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    23 5)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_absalot
          2
          2
        )
      ) ;; bind
    11 27)
    (list
      (bind
        (kern-mk-char
          'ch_silas
          "Silas"
          sp_human
          oc_wizard
          s_companion_shepherd
          2
          0 5 0
          2 1
          2 1
          28 16384
          20 8
          #f ;; dead?
          'silas-conv
          sch_silas
          'spell-sword-ai
          nil ;; inventory
          (list
            t_stun_wand
          )
          ;; hooks
          (list
          )
        )
        (list
          #f
          (list
            #f
            #f
            #f
          )
          #f
        )
      ) ;; bind
    25 23)
    (list
      (bind
        (kern-mk-char
          'ch_dennis
          "Dennis"
          sp_human
          oc_wright
          s_townsman
          2
          0 0 0
          0 0
          0 0
          13 512
          5 3
          #f ;; dead?
          'dennis-conv
          sch_dennis
          'spell-sword-ai
          nil ;; inventory
          (list
            t_staff
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    23 13)
    (list
      (bind
        (kern-mk-char
          'ch_selene
          "Selene"
          sp_human
          nil
          s_townswoman
          2
          0 0 0
          0 0
          0 0
          15 2048
          7 5
          #f ;; dead?
          'selene-conv
          sch_selene
          nil
          nil ;; inventory
          (list
            t_dagger
          )
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    10 5)
    (list
      (kern-tag 'oa-cp1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    2 16)
    (list
      (kern-tag 'oa-cp2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    2 18)
    (list
      (kern-tag 'oa-cp3
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    5 16)
    (list
      (kern-tag 'oa-cp4
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    5 18)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 16)
    (list
      (kern-tag 'oa-ps1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    3 19)
  ) ;; end of objects in p_old_absalot
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 30 30) ;; Northwest
    (list 1 15 30) ;; North
    (list 2 0 30) ;; Northeast
    (list 3 30 15) ;; West
    (list 4 15 15) ;; Here
    (list 5 0 15) ;; East
    (list 6 30 0) ;; Southwest
    (list 7 15 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_old_absalot

(kern-load "engineer.scm")

(kern-mk-place 'p_engineers_hut "Engineers Hut"
  s_hamlet ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "** *. *8 *8 *8 *8 *8 *8 *8 ** ** ee ee *. ** *8 ** ** ** "
      "*c rr rr rr rr ws rr rr rr rr *4 ee ee *2 rr rr rr ** ** "
      ".. rr rr [[ @@ @@ @@ ]] rr rr *6 ee ee *e rr ,, *b rr *. "
      "|| rr 00 ,, ,, ,, ,, ,, ,, rr *e /0 /d /d ,, ,, ,, *a ** "
      "*d rr 00 ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr ,, ,, rr *a "
      ".. rr 00 ,, ,, [[ ]] ,, ,, ,, /d /6 .. .. rr ,, bb rr .. "
      "~~ rr ,, ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr .. ,, rr .. "
      "~~ bb ~~ ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr ,, ,, rr .. "
      "bb ~~ ~~ ,, ,, ,, [[ @@ ]] rr .. /7 .. .. rr rr rr rr .. "
      "~~ rr ,, ,, ,, ,, rr rr rr rr .. /7 .. bb .. .. .. tC t3 "
      ".. rr ,, ,, ,, ,, 00 rr .. .. .. /7 .. .. .. .. .. t3 tt "
      ".. rr ,, ,, ,, ,, 00 rr .. /0 /d /9 /d /d /d /2 .. ta tt "
      ".. rr ,, rr && rr rr rr .. /7 .. .. .. .. .. /7 .. t% ta "
      "bb .. .. rr rr rr ~~ .. .. /7 .. .. .. rr rr ,, rr rr .. "
      "bb .. .. .. .. .. ~2 bb .. /7 .. .. rr rr ,, ,, ,, rr .. "
      "bb .. .. .. .. .. ~a ~~ ~9 =| ~9 ~~ rr && ,, ,, ,, rr .. "
      "bb .. .. .. .. .. .. bb .. /7 .. ~~ rr rr ,, ,, ,, rr .. "
      "bb .. .. .. .. .. bb .. .. /7 .. ~a ~~ rr rr rr rr rr .. "
      "t5 bb bb bb bb bb t7 .. .. /7 .. .. ~6 .. .. .. .. .. .. "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_engineers_hut
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          36 16384
          8 8
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    3 15)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 13)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    16 15)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 5 t_food)
        )
        ;; hooks
        (list
        )
      )
    14 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    14 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    2 12)
    (list
      (kern-mk-obj t_voidship_plans 1
        ;; hooks
        (list
        )
      )
    16 7)
    (list
      (bind
        (kern-mk-char
          'ch_engineer
          "Engineer"
          sp_human
          oc_wright
          s_companion_tinker
          2
          2 10 2
          10 2
          20 5
          18 0
          10 8
          #f ;; dead?
          'engineer-conv
          sch_engineer
          nil
          (kern-mk-container
            t_chest
            ;; trap
            'bomb-trap
            ;; contents
            (list
              (list 1 t_dagger)
              (list 5 t_cure_potion)
              (list 5 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_doom_staff
          )
          ;; hooks
          (list
          )
        )
        (list
          #f
          (list
            #f
            #f
            #f
          )
        )
      ) ;; bind
    3 3)
  ) ;; end of objects in p_engineers_hut
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 11 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_engineers_hut

(kern-load "the-man.scm")

(kern-mk-place 'p_mans_hideout "The MAN's Hideout"
  s_mountains ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx x! xx xx xx rr rr rr rr rr xx xx xx xx xx xx xx "
      "xx pp cc cc cc pp xx xx xx rr rr rr xx .T .O .O .L .S xx "
      "xx cc ,, ,, ,, cc ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, xx "
      "x! cc ,, ,, ,, cc x! ,, ?? ,, ?? ?? ?? ,, ,, ,, ,, ,, xx "
      "xx cc ,, ,, ,, cc xx xx xx x! ?? xx xx ,, ,, ,, ,, ,, xx "
      "xx pp cc cc cc pp xx xx ,, ,, ,, xx xx xx x! ,, x! xx xx "
      "xx xx xx ,, xx xx x! cc ,, ,, ,, cc x! xx xx ,, xx rr rr "
      "rr rr xx ,, xx xx cc pp cc ,, cc pp cc xx xx ,, xx rr rr "
      "rr rr xx ,, xx ,, ,, cc ,, ,, ,, cc ,, ,, xx ,, xx rr rr "
      "rr rr x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rr rr "
      "rr rr xx ,, xx ,, ,, cc ,, ,, ,, cc ,, ,, xx ,, xx ~~ ~~ "
      "rr rr xx ,, xx xx cc pp cc ,, cc pp cc xx xx ,, xx ~~ rr "
      "rr xx x! ,, x! xx x! cc ,, ,, ,, cc x! xx ~~ ,, ~~ ~c rr "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ~~ ee ~~ rr rr "
      "xx ,, ,, ,, ,, ,, rr rr rr rr rr rr rr ~~ ~~ ~8 ~c rr rr "
      "x! ,, ,, 00 ,, ,, rr rr .. .. .. rr rr ~2 rr rr rr rr rr "
      "xx ,, ,, ,, ,, ,, rr .. .. .. .. .. rr ~4 rr ,, ,, rr rr "
      "xx xx xx && xx xx rr .. .. .. .. ~3 ~~ ~c rr ,, ,, rr rr "
      "rr rr xx xx xx rr rr rr .. .. .. ~~ rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_mans_hideout
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 1 t_sling_4)
          (list 1 t_dagger_4)
          (list 1 t_leather_helm_4)
          (list 1 t_armor_leather_4)
        )
        ;; hooks
        (list
        )
      )
    13 3)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 10 t_cure_potion)
          (list 10 t_poison_immunity_potion)
          (list 10 t_inv_potion)
        )
        ;; hooks
        (list
        )
      )
    13 4)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 10 t_heal_potion)
          (list 10 t_mana_potion)
        )
        ;; hooks
        (list
        )
      )
    17 2)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 10 t_in_ex_por_scroll)
          (list 10 t_wis_quas_scroll)
          (list 10 t_vas_mani_scroll)
        )
        ;; hooks
        (list
        )
      )
    17 3)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 10 t_an_tym_scroll)
          (list 10 t_sanct_lor_scroll)
          (list 10 t_in_quas_xen_scroll)
          (list 10 t_an_xen_exe_scroll)
        )
        ;; hooks
        (list
        )
      )
    17 4)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_forsaken_prison
          2
          16
        )
      ) ;; bind
    9 3)
    (list
      (bind
        (kern-mk-char
          'ch_man
          "The MAN"
          sp_human
          oc_wrogue
          s_brigandess
          2
          0 0 0
          0 0
          0 0
          19 0
          11 9
          #f ;; dead?
          'man-conv
          sch_man
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    15 13)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 10 t_food)
        )
        ;; hooks
        (list
        )
      )
    5 16)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    3 3)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 100 t_gold_coins)
        )
        ;; hooks
        (list
        )
      )
    7 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 2)
    (list
      (bind
        (kern-mk-obj t_disg_lvr 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'mh-b1
          #f
          's_wall_rock
        )
      ) ;; bind
    7 18)
    (list
      (bind
        (kern-mk-obj t_hidden 1
          ;; hooks
          (list
          )
        )
        (list
          't_hidden_mech
          1
        )
      ) ;; bind
    7 18)
    (list
      (kern-tag 'mh-b1
        (bind
          (kern-mk-obj t_terrain_blitter 1
            ;; hooks
            (list
            )
          )
          (list
            'p_mans_hideout
            8
            14
            3
            1
            'm_hall_section
          )
        ) ;; bind
      ) ;; kern-tag
    0 0)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 5)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'bomb-trap
        ;; contents
        (list
          (list 10 t_gem)
          (list 10 t_torch)
          (list 10 t_picklock)
        )
        ;; hooks
        (list
        )
      )
    13 2)
  ) ;; end of objects in p_mans_hideout
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_mans_hideout

(kern-load "luximene.scm")

(kern-load "necromancer.scm")

(kern-mk-place 'p_necromancers_lair "Necromancers Lair"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx && xx xx xx xx xx x! xx xx xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, x! ,, ,, ,, ,, [[ @@ ]] ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx xx xx xx xx xx xx xx x! ,, x! xx xx xx x! ,, x! xx xx "
      "xx xx .. .. .. xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx .. .. .. ,, .. xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx .. ,, ,, .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx xx .. .. .. xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx xx xx ,, xx xx xx xx x! ,, x! xx xx xx x! ,, x! xx xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx ,, ,, ,, ,, ,, x! ,, ~~ ee ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ee ee ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx ,, ,, ,, ,, ,, x! ,, ~~ ~~ ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx x! [[ @@ ]] x! xx xx ~~ ~~ ~~ xx xx x! 00 00 00 x! xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_necromancers_lair
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 7)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    3 7)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 7)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 8)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 3)
    (list
      (kern-mk-obj mandrake 6
        ;; hooks
        (list
        )
      )
    2 1)
    (list
      (kern-mk-obj nightshade 4
        ;; hooks
        (list
        )
      )
    3 1)
    (list
      (kern-mk-obj blood_moss 12
        ;; hooks
        (list
        )
      )
    4 1)
    (list
      (kern-mk-obj black_pearl 14
        ;; hooks
        (list
        )
      )
    2 5)
    (list
      (kern-mk-obj spider_silk 21
        ;; hooks
        (list
        )
      )
    3 5)
    (list
      (kern-mk-obj garlic 18
        ;; hooks
        (list
        )
      )
    4 5)
    (list
      (kern-mk-obj ginseng 17
        ;; hooks
        (list
        )
      )
    1 4)
    (list
      (kern-mk-obj sulphorous_ash 26
        ;; hooks
        (list
        )
      )
    1 3)
    (list
      (kern-mk-obj t_mana_potion 8
        ;; hooks
        (list
        )
      )
    1 2)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_shard
          40
          70
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-char
          'ch_necr
          "Necromancer"
          sp_human
          oc_wizard
          s_companion_wizard
          2
          0 0 0
          0 0
          0 0
          18 0
          10 8
          #f ;; dead?
          'necr-conv
          sch_necr
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
          #f
          #f
        )
      ) ;; bind
    15 13)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    15 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 6)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 5 t_food)
        )
        ;; hooks
        (list
        )
      )
    10 1)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 12)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 12)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 6)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    12 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 15)
  ) ;; end of objects in p_necromancers_lair
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_necromancers_lair

(kern-mk-place 'p_smoldering_cave "Smoldering Cave"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr {8 .. {8 rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr !_ !_ rr {{ {6 {{ rr !_ !_ rr rr rr rr rr "
      "rr rr rr rr !_ !_ !_ !! {{ {6 {{ !! !_ !_ !_ rr rr rr rr "
      "rr rr rr !_ !_ !_ !_ !_ !! =| !! !_ !_ !_ !_ !_ rr rr rr "
      "rr rr rr !_ !_ !_ !_ !c {{ {6 {{ !a !_ !_ !_ !_ rr rr rr "
      "rr rr !_ !_ !_ !_ !c bb {{ {6 {{ bb !a !_ !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !! bb {{ {{ {6 {{ {{ bb !! !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !! {{ {{ {3 .. {5 {{ {{ !! !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !! {{ {{ {2 .. {4 {{ {{ !! !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !! {{ {{ {a {8 {c {{ {{ !! !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !! bb {{ {{ {{ {{ {{ bb !! !_ !_ !_ rr rr "
      "rr rr !_ !_ !_ !_ !5 bb {{ {{ {{ bb !3 !_ !_ !_ !_ rr rr "
      "rr rr rr !_ !_ !_ !_ !! !! !! !! !! !_ !_ !_ !_ rr rr rr "
      "rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr "
      "rr rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr rr "
      "rr rr rr rr rr rr !_ !_ !_ !_ !_ !_ !_ rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_fire_sea "Fire Sea"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
            "rr !_ !_ !! {A {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr "
            "rr !_ !_ !_ !5 {A {{ {{ {3 {9 {9 {9 {9 {5 {{ {C ~! rr rr "
            "rr !! !_ !! !! !5 {{ {3 {c {{ {{ {{ {{ {6 {{ ~! !_ ~! rr "
            "rr {% !e {# {{ !! {{ {6 {{ {C !3 !5 {{ {6 {{ {% ~! {# rr "
            "rr {{ {{ {3 {9 == {9 {c {C !3 !_ !! {{ {6 {{ !7 {{ {{ rr "
            "rr {{ {3 {c {{ !! {{ {{ !_ !_ !_ !_ {{ {a {9 == {9 {1 rr "
            "rr {{ {6 {{ {{ !_ !_ !_ rr rr rr rr !5 {{ {{ !! {{ {2 rr "
            "rr {{ {6 {{ {{ {% !a !_ rr .. .. rr rr !! !! !c {{ {2 rr "
            "rr {{ {a {9 {9 {5 {{ rr rr .. .. .. rr {{ !! {{ {{ {2 rr "
            "rr {{ {{ {{ {{ {6 {{ rr rr .. bb .. .. {9 == {9 {9 {8 rr "
            "rr {C !3 !! !! =| !! !_ rr .. .. .. rr {{ !! {{ {{ {{ rr "
            "rr !! !_ !c {{ {6 {{ !_ rr rr rr .. rr rr !_ !! !5 {A rr "
            "rr {% !e {# {{ {6 {{ !! !_ !_ rr rr rr rr !_ !_ !_ !! rr "
            "rr rr {{ {{ {{ {6 {{ !a !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr "
            "rr rr rr {{ {{ {6 {{ {% !a !! !! !_ !_ !_ !_ !_ !_ !_ rr "
            "rr rr rr bb {{ {a {5 {{ {{ {{ {% !a !! !! !_ !_ !_ !_ rr "
            "rr rr rr rr bb {{ {2 {1 {1 {5 {{ {{ {{ {{ !! !_ !_ !_ rr "
            "rr rr rr rr rr rr rr rr .. .. {5 {{ {{ {3 bb !! !! ~! rr "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        nil ;; neighbors
        (list ;; objects in p_fire_sea
          (list
            (kern-mk-obj t_gold_coins 140
              ;; hooks
              (list
              )
            )
          11 10)
          (list
            (kern-mk-obj t_armor_plate_4 1
              ;; hooks
              (list
              )
            )
          11 11)
          (list
            (kern-mk-obj t_gem 14
              ;; hooks
              (list
              )
            )
          9 11)
          (list
            (kern-mk-obj t_gold_coins 104
              ;; hooks
              (list
              )
            )
          10 11)
          (list
            (kern-mk-obj t_shield_4 1
              ;; hooks
              (list
              )
            )
          10 11)
          (list
            (kern-mk-obj t_gem 10
              ;; hooks
              (list
              )
            )
          11 11)
          (list
            (kern-mk-obj t_gold_coins 96
              ;; hooks
              (list
              )
            )
          11 12)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'dragon
              )
            ) ;; bind
          11 10)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (kern-mk-obj t_rune_w 1
              ;; hooks
              (list
              )
            )
          11 12)
          (list
            (kern-mk-obj t_gold_coins 181
              ;; hooks
              (list
              )
            )
          9 8)
          (list
            (kern-mk-obj t_gold_coins 157
              ;; hooks
              (list
              )
            )
          10 8)
          (list
            (kern-mk-obj t_iron_helm_4 1
              ;; hooks
              (list
              )
            )
          10 8)
          (list
            (kern-mk-obj t_gold_coins 94
              ;; hooks
              (list
              )
            )
          9 9)
          (list
            (kern-mk-obj t_gem 13
              ;; hooks
              (list
              )
            )
          10 9)
          (list
            (kern-mk-obj t_gold_coins 135
              ;; hooks
              (list
              )
            )
          11 9)
          (list
            (kern-mk-obj t_gold_coins 113
              ;; hooks
              (list
              )
            )
          9 10)
          (list
            (kern-mk-obj t_sword_4 1
              ;; hooks
              (list
              )
            )
          9 10)
        ) ;; end of objects in p_fire_sea
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_fire_sea

    1)
  ) ;; end neighbors of p_smoldering_cave
  (list ;; objects in p_smoldering_cave
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_shard
          118
          46
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'fire-slime
        )
      ) ;; bind
    9 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'fire-slime
        )
      ) ;; bind
    7 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'fire-slime
        )
      ) ;; bind
    10 4)
  ) ;; end of objects in p_smoldering_cave
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_smoldering_cave

p_fire_sea
(kern-mk-place 'p_void_temple "Void Temple"
  s_shrine ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
      "cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
      "cc cc t3 tt tt *3 *1 *5 bb .. bb *3 *1 *5 tt tt t5 cc cc "
      "cc cc tt tt *3 ** *. ** *5 cc *3 ** ** ** *5 tt tt cc cc "
      "cc cc tt *3 *. ** ** ** *4 cc *2 ** ** *. *. *5 tt cc cc "
      "cc cc *3 ** ** *. ** ** *c cc *a ** ** ** ** *. *5 cc cc "
      "cc cc *2 *. *. ** *. *c bb .. bb *a *. ** *. ** *4 cc cc "
      ".. cc *a ** *. ** *c .g .. .. .. .h *a ** ** ** *c cc .. "
      ".. .. bb *a *8 *c bb .. .. .. .. .. bb *a *8 *c bb .. .. "
      ".. .. .. cc cc cc .. .. .. .. .. .. .. cc cc cc .. .. .. "
      ".. .. bb *3 *1 *5 bb .. .. .. .. .. bb *3 *1 *5 bb .. .. "
      ".. cc *3 ** ** ** *5 .j .. .. .. .l *3 ** *. ** *5 cc .. "
      "cc cc *2 *. *. ** ** *5 bb .. bb *3 ** *. *. *. *4 cc cc "
      "cc cc *a *. *. ** ** *. *5 cc *3 ** ** *. ** ** *c cc cc "
      "cc cc tt *a ** *. ** *. *4 cc *2 *. *. *. *. *c tt cc cc "
      "cc cc tt tt *a *. *. ** *c cc *a ** *. *. *c tt tt cc cc "
      "cc cc ta tt tt *a *8 *c bb .. bb *a *8 *c tt tt tc cc cc "
      "cc cc cc cc cc cc cc cc .. .. .. cc cc cc cc cc cc cc cc "
      "cc cc cc cc cc cc cc .. .. .. .. .. cc cc cc cc cc cc cc "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_void_temple
    (list
      (kern-mk-field f_energy_perm -1)    9 5)
    (list
      (kern-mk-field f_sleep_perm -1)    9 15)
    (list
      (kern-mk-field f_fire_perm -1)    9 14)
    (list
      (kern-mk-field f_energy_perm -1)    9 13)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    7 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    11 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    9 11)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (kern-mk-obj t_rune_d 1
        ;; hooks
        (list
        )
      )
    9 9)
    (list
      (kern-mk-field f_sleep_perm -1)    15 9)
    (list
      (kern-mk-field f_fire_perm -1)    14 9)
    (list
      (kern-mk-field f_energy_perm -1)    13 9)
    (list
      (kern-mk-field f_sleep_perm -1)    3 9)
    (list
      (kern-mk-field f_fire_perm -1)    4 9)
    (list
      (kern-mk-field f_energy_perm -1)    5 9)
    (list
      (kern-mk-field f_sleep_perm -1)    9 3)
    (list
      (kern-mk-field f_fire_perm -1)    9 4)
  ) ;; end of objects in p_void_temple
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_void_temple

(kern-mk-place 'p_merciful_death "Wreck of the Merciful Death (abovedecks)"
  s_ship ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. ~C ~3 ~1 ~1 ~1 ~5 ~A .. .. .. ~C ~3 ~1 ~1 ~1 ~5 ~A .. "
      "~~ ~~ ~~ bb -- -- ~~ ~~ ~1 ee ~1 ~~ ~~ -- -- -- ~~ ~5 ~A "
      "-- -- -- ~~ -- -- -- -- -- ee -- -- -- -- -- bb -- ~~ ~~ "
      "-- ## ## ## ## ## ## ## ## ee ## ## ## -- -- ~~ -- -- -- "
      "-- ## ee ee ee ee ee ee ee ee ee ee ## ## ## ## -- -- -- "
      "-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- -- "
      "-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- "
      "-- ## ee ee oo ee ee ee ee oo ee ee ee WW ee ee ee ## -- "
      "-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- "
      "-- ## ee ee ee ee ee ee ee ee ee ee ee ee ~~ ~~ ~~ ~~ -- "
      "-- ## ee ~~ ~~ ~~ ee ee ~~ ~~ ee ee ## ## ## ~~ ~~ ~~ -- "
      "-- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb -- "
      "-- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ ~~ ~~ ~~ -- -- bb ~~ -- "
      "-- -- bb -- -- -- -- -- -- ~~ -- -- -- -- -- -- ~~ -- -- "
      "-- -- ~~ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- -- -- __ __ __ __ -- -- -- -- __ __ __ -- -- -- -- "
      "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_merciful_death
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    7 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-spear-thrower
        )
      ) ;; bind
    10 9)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_merciful_death_l2
          6
          9
        )
      ) ;; bind
    6 9)
  ) ;; end of objects in p_merciful_death
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_merciful_death

(kern-mk-place 'p_merciful_death_l2 "Wreck of the Merciful Death (belowdecks)"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr -- -- -- -- -- rr rr rr rr rr -- -- -- -- -- rr rr "
      "-- -- -- bb -- -- -- -- -- -- -- -- -- -- -- -- -- -- rr "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- bb -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- #> #> #> #> #> #> #> #> #> #> -- -- -- -- -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee #> #> #> #> -- -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee #> #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee ee #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee #> #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee #> #> #> #> -- -- -- -- "
      "-- -- #> #> #> #> #> #> #> #> #> #> -- -- -- -- -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- bb -- "
      "-- -- -- -- -- -- -- -- -- bb -- -- -- -- -- -- bb -- -- "
      "-- -- bb -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- -- -- __ __ __ __ -- -- -- -- __ __ __ -- -- -- -- "
      "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_merciful_death_l2
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_merciful_death
          6
          9
        )
      ) ;; bind
    6 9)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'poison-trap
        ;; contents
        (list
          (list 1 t_rune_c)
          (list 342 t_gold_coins)
          (list 4 t_gem)
          (list 1 t_sextant)
        )
        ;; hooks
        (list
        )
      )
    14 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    7 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-spear-thrower
        )
      ) ;; bind
    10 9)
  ) ;; end of objects in p_merciful_death_l2
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_merciful_death_l2

(kern-mk-place 'p_angriss_lair "Entrance to Angriss's Lair"
  s_spider_web ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "|| || || || || || || || tt tt tt || || || tt || || || || "
      "|| || || || || || || || || tt || || || tt tt tt || || || "
      "|| || || || || || bb bb bb bb || || || bb .. bb || || || "
      "|| || || || bb bb bb t3 t5 bb bb || bb .. .. .. bb || || "
      "|| || || bb .. .. t3 || || t5 bb .. .. .. .. .. bb || || "
      "|| || || bb .. .. ta || || tc .. .. .. .. .. bb || || || "
      "|| || || bb bb bb .. .. .. .. .. .. .. bb bb || || || || "
      "|| || || || bb bb .. bb .. .. tC t3 tt t5 bb || || || || "
      "tt || || || bb || || || bb .. tb || || tt td bb || || tt "
      "tt tt || || bb || tF || bb .. t% |A || tc t# bb || tt tt "
      "tt || || || bb || || || bb bb .. .. .. .. bb || || || tt "
      "|| || || || bb bb bb bb .. .. .. bb .. bb bb bb || || || "
      "|| || || bb .. tC t3 t5 tA .. .. bb .. || || bb || || || "
      "|| || || bb .. t3 || || td .. bb .. .. t% || bb || || || "
      "|| || || bb .. ta || |C t# .. bb .. .. .. .. bb || || || "
      "|| || || || bb .. .. .. .. bb || bb bb .. bb || || || || "
      "|| || || || || bb .. bb bb || || || tt tt tt || || || || "
      "|| || || || || tt tt tt || tt || || || tt || || || || || "
      "|| || || || || || tt || tt tt tt || || || || || || || || "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_angriss_lair
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_spider_cave
          15
          4
        )
      ) ;; bind
    15 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    6 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    15 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    7 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    6 13)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    14 12)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_spider_cave
          6
          9
        )
      ) ;; bind
    6 9)
  ) ;; end of objects in p_angriss_lair
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_angriss_lair

(kern-load "angriss.scm")

(kern-mk-place 'p_spider_cave "Spider Cave"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. rr rr bb .. bb bb rr "
      "rr rr .. .. .. .. .. .. .. .. .. rr rr bb .. .. .. bb rr "
      "rr rr rr .. rr .. .. .. tC t7 tA rr .. .. .. .! .. bb rr "
      "rr rr rr rr rr rr .. tC t3 tt t5 rr .. .. .! .! .! .. rr "
      "rr rr rr bb rr t3 tt tt tt tt tc rr bb .. .. .! .. .. rr "
      "rr rr .. tC t3 tt tt tt tt tt t# rr rr bb bb .. .. bb rr "
      "rr .. tC t3 tt tt tt tt tt tt tA .. rr rr rr rr rr rr rr "
      "rr .. t3 tt tt .! .! .! tt tt t5 tA .. .. bb .. .. .. rr "
      "rr .. tt tt tt .! .! .! tt tt tt td .. .. .. .. .. rr rr "
      "rr .. tt tt tt .! .! .! tt tt tc t# .. .. bb .. .. .. rr "
      "rr .. ta tt tt tt tt tt tt tc rr .. .. .. .. bb .. .. rr "
      "rr .. t% ta tt tc bb ta tc rr rr bb .. rr rr .. .. rr rr "
      "rr .. .. t% te rr rr rr rr rr rr rr rr rr bb rr .. .. rr "
      "rr bb .. rr .. .. rr rr .. .. bb .. .. bb .. .. .. .. rr "
      "rr rr bb .. .. .. .. .. .. .. .. bb .. .. .. .. .. .. rr "
      "rr rr .. .. .. bb .. .. .. .. bb .. .. .. .. bb .. bb rr "
      "rr .. .. .. .. .. .. .. rr rr rr rr rr .. .. .. .. rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_angriss_throne_room "Angriss's Throne Room"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
            "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
            "rr rr .. .. .. .. bb .. .. .. .. .. .. .. .. .. .. rr rr "
            "rr .. .. .. .. .. bb bb .. bb bb bb .. .. .. .. .. .. rr "
            "rr .. .. .. .. bb .. bb bb bb .. bb bb .. .. .. .. .. rr "
            "rr .. .. .. bb bb .. .. .. .. bb bb bb bb .. .. .. .. rr "
            "rr .. .. .. bb .. .. bb bb .. .. bb bb .. bb .. .. .. rr "
            "rr .. .. bb .. .. bb .. .. .. .. .. bb .. bb bb .. .. rr "
            "rr .. .. bb .. bb .. .. .. .. .. bb .. .. bb .. .. .. rr "
            "rr .. .. bb .. .. bb .. .. .. .. .. bb .. bb .. .. .. rr "
            "rr .. .. bb bb .. bb .. .. .. .. .. bb bb .. bb .. .. rr "
            "rr .. .. .. .. .. bb bb .. .. .. bb bb .. .. bb .. .. rr "
            "rr .. .. .. bb bb bb .. bb .. bb .. .. bb bb .. .. .. rr "
            "rr .. .. .. bb bb bb bb bb bb bb .. bb bb bb .. .. .. rr "
            "rr bb .. .. .. bb .. bb bb .. bb .. .. bb .. .. .. .. rr "
            "rr bb bb .. .. .. bb bb bb bb .. bb .. .. .. .. .. .. rr "
            "rr bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb rr "
            "rr bb bb .. bb .. .. .. .. .. .. .. .. .. .. .. bb bb rr "
            "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        nil ;; neighbors
        (list ;; objects in p_angriss_throne_room
          (list
            (kern-mk-field f_web_perm -1)          7 8)
          (list
            (kern-mk-field f_web_perm -1)          8 8)
          (list
            (kern-mk-field f_web_perm -1)          9 8)
          (list
            (kern-mk-field f_web_perm -1)          10 8)
          (list
            (kern-mk-field f_web_perm -1)          7 9)
          (list
            (kern-mk-field f_web_perm -1)          8 9)
          (list
            (kern-mk-field f_web_perm -1)          9 9)
          (list
            (kern-mk-field f_web_perm -1)          10 9)
          (list
            (kern-mk-field f_web_perm -1)          11 9)
          (list
            (kern-mk-field f_web_perm -1)          7 10)
          (list
            (kern-mk-field f_web_perm -1)          8 10)
          (list
            (kern-mk-field f_web_perm -1)          9 10)
          (list
            (kern-mk-field f_web_perm -1)          10 10)
          (list
            (kern-mk-field f_web_perm -1)          11 10)
          (list
            (kern-mk-field f_web_perm -1)          8 11)
          (list
            (kern-mk-field f_web_perm -1)          9 11)
          (list
            (kern-mk-field f_web_perm -1)          10 11)
          (list
            (kern-mk-field f_web_perm -1)          9 12)
          (list
            (kern-mk-field f_web_perm -1)          11 12)
          (list
            (kern-mk-field f_web_perm -1)          11 13)
          (list
            (kern-mk-field f_web_perm -1)          11 14)
          (list
            (kern-mk-field f_web_perm -1)          12 14)
          (list
            (kern-mk-field f_web_perm -1)          5 6)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_bolt
                  )
                )
              )
            ) ;; bind
          5 3)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_torch
                  )
                  (list
                    5
                    't_gold_coins
                  )
                  (list
                    2
                    't_arrow
                  )
                )
              )
            ) ;; bind
          17 5)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_mana_potion
                  )
                  (list
                    1
                    't_mana_potion
                  )
                  (list
                    1
                    't_bolt
                  )
                )
              )
            ) ;; bind
          3 16)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    5
                    't_gold_coins
                  )
                  (list
                    2
                    't_arrow
                  )
                )
              )
            ) ;; bind
          17 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_arrow
                  )
                  (list
                    3
                    't_gold_coins
                  )
                  (list
                    3
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          3 4)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          9 17)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    2
                    't_gold_coins
                  )
                  (list
                    4
                    't_bolt
                  )
                  (list
                    5
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          13 8)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_arrow
                  )
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_arrow
                  )
                )
              )
            ) ;; bind
          16 15)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_bolt
                  )
                  (list
                    2
                    't_bolt
                  )
                  (list
                    2
                    't_arrow
                  )
                )
              )
            ) ;; bind
          16 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    2
                    't_arrow
                  )
                  (list
                    1
                    't_heal_potion
                  )
                  (list
                    1
                    't_mana_potion
                  )
                )
              )
            ) ;; bind
          3 12)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_heal_potion
                  )
                  (list
                    5
                    't_bolt
                  )
                  (list
                    5
                    't_arrow
                  )
                )
              )
            ) ;; bind
          8 3)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    5
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          9 12)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    3
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          13 3)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_arrow
                  )
                  (list
                    3
                    't_arrow
                  )
                )
              )
            ) ;; bind
          3 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_gold_coins
                  )
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_heal_potion
                  )
                )
              )
            ) ;; bind
          3 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_mana_potion
                  )
                )
              )
            ) ;; bind
          4 4)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_cure_potion
                  )
                  (list
                    3
                    't_arrow
                  )
                  (list
                    1
                    't_torch
                  )
                )
              )
            ) ;; bind
          10 9)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_bolt
                  )
                  (list
                    3
                    't_bolt
                  )
                  (list
                    4
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          13 9)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_arrow
                  )
                )
              )
            ) ;; bind
          5 3)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_cure_potion
                  )
                )
              )
            ) ;; bind
          1 9)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_torch
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          9 7)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    2
                    't_arrow
                  )
                  (list
                    4
                    't_bolt
                  )
                )
              )
            ) ;; bind
          2 12)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    2
                    't_gold_coins
                  )
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          1 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_bolt
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          16 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          3 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_arrow
                  )
                )
              )
            ) ;; bind
          1 4)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          15 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_mana_potion
                  )
                  (list
                    5
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          8 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_heal_potion
                  )
                  (list
                    2
                    't_bolt
                  )
                )
              )
            ) ;; bind
          1 8)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_arrow
                  )
                  (list
                    3
                    't_bolt
                  )
                )
              )
            ) ;; bind
          16 8)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_bolt
                  )
                  (list
                    1
                    't_cure_potion
                  )
                  (list
                    1
                    't_mana_potion
                  )
                )
              )
            ) ;; bind
          7 7)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_heal_potion
                  )
                )
              )
            ) ;; bind
          9 2)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_cure_potion
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          14 17)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                  (list
                    2
                    't_gold_coins
                  )
                  (list
                    4
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          14 15)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_gold_coins
                  )
                  (list
                    3
                    't_gold_coins
                  )
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          16 13)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    2
                    't_bolt
                  )
                  (list
                    1
                    't_food
                  )
                  (list
                    1
                    't_cure_potion
                  )
                )
              )
            ) ;; bind
          5 1)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    5
                    't_arrow
                  )
                )
              )
            ) ;; bind
          5 1)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                  (list
                    2
                    't_bolt
                  )
                  (list
                    1
                    't_heal_potion
                  )
                )
              )
            ) ;; bind
          8 5)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_bolt
                  )
                  (list
                    3
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          7 18)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    3
                    't_arrow
                  )
                  (list
                    1
                    't_mana_potion
                  )
                  (list
                    2
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          14 4)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_heal_potion
                  )
                )
              )
            ) ;; bind
          3 17)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_picklock
                  )
                )
              )
            ) ;; bind
          9 10)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          10 8)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_bolt
                  )
                )
              )
            ) ;; bind
          12 16)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    5
                    't_gold_coins
                  )
                  (list
                    1
                    't_bolt
                  )
                )
              )
            ) ;; bind
          1 9)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    4
                    't_bolt
                  )
                )
              )
            ) ;; bind
          5 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_food
                  )
                )
              )
            ) ;; bind
          5 11)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_gold_coins
                  )
                )
              )
            ) ;; bind
          15 9)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_arrow
                  )
                )
              )
            ) ;; bind
          1 3)
          (list
            (bind
              (kern-mk-obj t_corpse 1
                ;; hooks
                (list
                )
              )
              (list
                (list
                  (list
                    1
                    't_bolt
                  )
                )
              )
            ) ;; bind
          3 17)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          6 12)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          12 6)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          10 10)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          7 10)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          10 11)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          9 6)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          6 8)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          12 10)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          11 9)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          7 8)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          7 7)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          12 11)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          11 6)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          6 7)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          8 10)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          6 8)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          8 8)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          6 10)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          9 9)
          (list
            (bind
              (kern-mk-obj spider-egg-type 1
                ;; hooks
                (list
                )
              )
              (list
                10
              )
            ) ;; bind
          10 6)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-char-force-drop                (kern-mk-char
                  'ch_angriss
                  "Angriss"
                  sp_queen_spider
                  nil
                  s_purple_spider
                  7
                  20 0 20
                  10 5
                  10 5
                  138 0
                  132 20
                  #f ;; dead?
                  'angriss-conv
                  nil
                  'angriss-ai
                  (kern-mk-container
                    t_chest
                    ;; trap
                    nil
                    ;; contents
                    (list
                      (list 1 t_rune_f)
                    )
                    ;; hooks
                    (list
                    )
                  )
                  nil
                  ;; hooks
                  (list
                  )
                )
              #t) ;; kern-char-force-drop
              (list
                #f
                (list
                  #f
                  #f
                  #f
                )
              )
            ) ;; bind
          9 9)
          (list
            (kern-mk-field f_web_perm -1)          9 6)
          (list
            (kern-mk-field f_web_perm -1)          10 6)
          (list
            (kern-mk-field f_web_perm -1)          7 7)
          (list
            (kern-mk-field f_web_perm -1)          8 7)
          (list
            (kern-mk-field f_web_perm -1)          9 7)
          (list
            (kern-mk-field f_web_perm -1)          10 7)
          (list
            (kern-mk-field f_web_perm -1)          11 7)
          (list
            (kern-mk-field f_web_perm -1)          6 8)
        ) ;; end of objects in p_angriss_throne_room
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_angriss_throne_room

    1)
  ) ;; end neighbors of p_spider_cave
  (list ;; objects in p_spider_cave
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 4)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    15 1)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    12 3)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    12 4)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    16 6)
    (list
      (bind
        (kern-mk-obj spider-egg-type 1
          ;; hooks
          (list
          )
        )
        (list
          10
        )
      ) ;; bind
    17 4)
    (list
      (kern-mk-field web-type 20)    15 1)
    (list
      (kern-mk-field web-type 20)    12 3)
    (list
      (kern-mk-field web-type 20)    12 4)
    (list
      (kern-mk-field web-type 20)    16 6)
    (list
      (kern-mk-field web-type 20)    17 4)
    (list
      (kern-mk-field web-type 20)    17 5)
    (list
      (kern-mk-field web-type 20)    15 6)
    (list
      (kern-mk-field web-type 20)    13 5)
    (list
      (kern-mk-obj t_gold_coins 52
        ;; hooks
        (list
        )
      )
    14 2)
    (list
      (kern-mk-obj t_gold_coins 34
        ;; hooks
        (list
        )
      )
    17 5)
    (list
      (kern-mk-obj t_bow 1
        ;; hooks
        (list
        )
      )
    15 6)
    (list
      (kern-mk-obj t_arrow 34
        ;; hooks
        (list
        )
      )
    13 5)
    (list
      (kern-mk-obj t_leather_helm 1
        ;; hooks
        (list
        )
      )
    16 2)
    (list
      (kern-mk-obj t_halberd 1
        ;; hooks
        (list
        )
      )
    12 3)
    (list
      (kern-mk-obj t_heal_potion 3
        ;; hooks
        (list
        )
      )
    15 1)
    (list
      (kern-mk-obj t_mana_potion 5
        ;; hooks
        (list
        )
      )
    16 6)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_angriss_lair
          6
          9
        )
      ) ;; bind
    6 9)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_angriss_lair
          15
          4
        )
      ) ;; bind
    15 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'queen-spider
        )
      ) ;; bind
    9 0)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'queen-spider
        )
      ) ;; bind
    15 15)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'queen-spider
        )
      ) ;; bind
    1 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    15 1)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    12 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    12 4)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 6)
  ) ;; end of objects in p_spider_cave
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_spider_cave

p_angriss_throne_room
(kern-load "meaney.scm")

(kern-load "amy.scm")

(kern-mk-place 'p_poor_house "The Poor House"
  s_hamlet ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      ".. .. .. .. .. .. .. .. .. /7 .. bb bb bb bb bb bb bb ta "
      ".. /0 /d /d /d /d /d /d /d /a bb .. .. .. .. t3 tt t5 bb "
      ".. /7 .. .. .. .. .. .. .. .. bb .. .. .. .. te bb tt bb "
      ".. /7 .. rr rr rr rr rr rr bb rr .. .. .. .. t% tb tc bb "
      ".. /7 .. rr .P .O .O .R ]] rr .. .. .. .. .. .. .. .. bb "
      ".. /7 .. rr .H .O .U .S .E rr .. .. .. .. .. .. .. .. bb "
      ".. /7 .. ,, ,, ,, ,, ,, ,, rr rr rr ,, rr bb rr .. .. bb "
      ".. /7 .. bb ,, ,, ,, ,, ,, rr ,, ,, ,, ,, .. rr .. .. bb "
      ".. /7 .. rr ,, ,, ,, ,, ,, d, ,, ,, ,, ,, ,, rr bb bb .. "
      "/d /6 .. rr rr rr d, rr rr bb ,, ,, 00 ,, ,, ws .. /0 /d "
      ".. /7 .. rr ,, ,, ,, ,, ,, rr ,, ,, 00 ,, ,, rr .. /7 .. "
      ".. /7 .. rr ,, ,, ,, ,, ,, rr ,, ,, 00 ,, ,, rr .. /7 .. "
      ".. /7 .. ws ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, rr .. /7 .. "
      ".. /7 .. rr ,, .. ,, ,, ,, rr ,, ,, ,, ,, ,, ws .. /7 .. "
      ".. /7 .. rr ,, ,, .. ,, ,, rr ,, bb && bb ,, rr .. /7 .. "
      ".. /7 .. rr rr rr bb rr rr rr rr rr rr rr rr rr .. /7 .. "
      ".. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. "
      ".. /8 /d /d /d /d /d /d /d /1 /d /d /d /d /d /d /d /a .. "
      ".. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_poor_house
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          4
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 6)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          4
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    12 6)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_food)
        )
        ;; hooks
        (list
        )
      )
    10 14)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        nil
        ;; contents
        (list
          (list 1 t_gold_coins)
        )
        ;; hooks
        (list
        )
      )
    14 14)
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          32 4096
          6 6
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    16 3)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_meaney
            "Meaney"
            sp_human
            nil
            s_companion_shepherd
            2
            0 0 0
            0 0
            0 0
            16 0
            8 6
            #f ;; dead?
            'meaney-conv
            sch_meaney
            nil
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              (list
                (list 1 t_skull_ring)
                (list 1 t_dagger)
              )
              ;; hooks
              (list
              )
            )
            nil
            ;; hooks
            (list
            )
          )
        #t) ;; kern-char-force-drop
        (list
          0
        )
      ) ;; bind
    4 10)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 10)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 12)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 14)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 10)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 12)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 14)
  ) ;; end of objects in p_poor_house
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_poor_house

(kern-load "gholet.scm")

(kern-mk-place 'p_prison "Prison"
  s_hamlet ;; sprite
  (kern-mk-map
    nil     32 9 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, xx xx xx xx xx ,, xx ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx ,, ,, ,, x! ,, ,, ,, ee ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
      "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx ,, ,, ,, xx xx xx xx xx ,, xx ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_prison
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    1 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    2 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    3 7)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    1 6)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    4 2)
    (list
      (kern-tag 'pp1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    12 4)
    (list
      (kern-tag 'pp2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    14 4)
    (list
      (kern-mk-field f_energy_perm -1)    26 4)
    (list
      (kern-mk-field f_energy_perm -1)    28 3)
    (list
      (kern-mk-field f_energy_perm -1)    28 4)
    (list
      (kern-mk-field f_energy_perm -1)    28 5)
    (list
      (kern-mk-field f_energy_perm -1)    29 3)
    (list
      (kern-mk-field f_energy_perm -1)    29 5)
    (list
      (kern-mk-field f_energy_perm -1)    30 3)
    (list
      (kern-mk-field f_energy_perm -1)    30 4)
    (list
      (kern-mk-field f_energy_perm -1)    30 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    16 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    20 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    24 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    16 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    20 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    24 5)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    27 4)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'pp1
          #f
          '()
        )
      ) ;; bind
    11 1)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'pp2
          #f
          '()
        )
      ) ;; bind
    11 7)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_my
            "Gholet"
            sp_human
            nil
            s_brigand
            2
            0 0 0
            0 0
            0 0
            14 0
            6 4
            #f ;; dead?
            'gholet-conv
            nil
            nil
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              (list
                (list 1 t_skull_ring)
              )
              ;; hooks
              (list
              )
            )
            nil
            ;; hooks
            (list
            )
          )
        #t) ;; kern-char-force-drop
        '()
      ) ;; bind
    24 1)
    (list
      (bind
        (kern-mk-char
          nil
          "zorn"
          sp_zorn
          oc_wrogue
          s_zorn
          5
          0 0 0
          0 0
          0 0
          18 16384
          10 8
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'poison-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d20+9"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'zorn
          #f
          #f
          '()
        )
      ) ;; bind
    29 4)
    (list
      (bind
        (kern-mk-char
          nil
          "footpad"
          sp_human
          oc_wrogue
          s_brigand
          8
          0 0 0
          0 0
          0 0
          18 16384
          10 8
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'self-destruct-trap
            ;; contents
            (list
              (list 5 t_gold_coins)
              (list 2 t_food)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_dagger
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "2d6-2"
                      't_gold_coins
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                    (list
                      10
                      "1d3"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'footpad
          #f
          #f
          '()
        )
      ) ;; bind
    16 7)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'halberdier
        )
      ) ;; bind
    9 1)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'halberdier
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_glasdrin
          2
          2
        )
      ) ;; bind
    6 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 2)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 10 t_food)
        )
        ;; hooks
        (list
        )
      )
    1 1)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 1 t_cure_potion)
          (list 1 t_heal_potion)
        )
        ;; hooks
        (list
        )
      )
    2 1)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 10 t_arrow)
        )
        ;; hooks
        (list
        )
      )
    1 2)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 10 t_bolt)
        )
        ;; hooks
        (list
        )
      )
    2 7)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 1 t_vas_mani_scroll)
        )
        ;; hooks
        (list
        )
      )
    3 7)
    (list
      (kern-mk-container
        t_chest
        ;; trap
        'burn-trap
        ;; contents
        (list
          (list 10 sulphorous_ash)
          (list 5 garlic)
          (list 5 ginseng)
        )
        ;; hooks
        (list
        )
      )
    3 6)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    1 1)
  ) ;; end of objects in p_prison
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 31 8) ;; Northwest
    (list 1 16 8) ;; North
    (list 2 0 8) ;; Northeast
    (list 3 31 4) ;; West
    (list 4 16 4) ;; Here
    (list 5 0 4) ;; East
    (list 6 31 0) ;; Southwest
    (list 7 16 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_prison

(kern-load "talking-ankh.scm")

(kern-load "demon-gate.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-load "nossifer.scm")

(kern-mk-place 'p_ankh_shrine "Ankh Shrine"
  s_shrine ;; sprite
  (kern-mk-map
    nil     31 31 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr ** ** ** ** ** ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** ** ** ** ** ** ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx xx xx xx xx xx xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx pp ,, ,, ,, pp xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx ,, cc cc cc ,, xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx pp ,, cc ,, pp xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** xx xx ,, cc ,, xx xx ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr ** ** ** ** ,, cc ,, ** ** ** ** rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr ** ** ** ,, cc ,, ** ** ** rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr ** ** ,, cc ,, ** ** rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_ankh_shrine
    (list
      (bind
        (kern-mk-char
          'ch_ankh
          "Ankh"
          sp_statue
          nil
          s_ankh
          2
          0 0 0
          0 0
          0 0
          189 0
          9 9
          #f ;; dead?
          'ankh-conv
          nil
          'ankh-ai
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #f
        )
      ) ;; bind
    14 21)
    (list
      (kern-tag 'demon-gate
        (bind
          (kern-mk-obj t_demon_gate 1
            ;; hooks
            (list
            )
          )
          (list
            8
            #f
            #f
          )
        ) ;; bind
      ) ;; kern-tag
    0 0)
  ) ;; end of objects in p_ankh_shrine
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 30 30) ;; Northwest
    (list 1 14 30) ;; North
    (list 2 0 30) ;; Northeast
    (list 3 14 30) ;; West
    (list 4 15 15) ;; Here
    (list 5 14 30) ;; East
    (list 6 30 0) ;; Southwest
    (list 7 14 30) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_ankh_shrine

(kern-mk-place 'p_pools "Pools"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr -- -- rr rr .. .. .. .. .. .. .. %3 %% %% %5 .. .. rr "
      "rr -- -- rr rr .. .. .. .. .. .. %b %% ~3 ~5 %% %5 .. rr "
      "rr -- -- ~~ %5 .. .. .. .. .. .. ~C ~3 -- -- ~5 %% %5 rr "
      "rr rr ~~ ~c %% .. .. .. .. .. .. ~3 -- -- -- -- ~5 %% rr "
      "rr rr %a %% %c .. .. .. .. .. .. ~~ -- -- -- -- ~4 %% rr "
      "rr rr .. .. .. .. .. xx w+ d, ,, xx ~~ -- -- -- ~c %% rr "
      "rr .. .. .. .. .. .. rr ,, ,, ,, xx ~~ -- -- ~c %% %c rr "
      "rr .. %3 %% %% %5 .. w+ .. ,, ,, rr ~% ~a ~c %% %c rr rr "
      "rr %3 %% ~3 ~5 %% .. ,, ,, ,, ,, ,, .. %% %% %% .. .. rr "
      "rr %% ~3 -- ~c %% .. rr xx .. ,, rr .. %% ~7 %% %5 .. rr "
      "rr %% ~a ~c %% %c .. .. .. .. .. .. %3 ~3 -- ~5 %% .. rr "
      "rr %a %% %% %c .. .. .. .. .. %3 %% ~3 -- -- ~4 %% .. rr "
      "rr rr .. rr rr .. .. .. .. .. %% ~3 -- -- -- ~c %% .. rr "
      "rr rr rr rr rr .. .. .. .. .. %% ~a -- -- ~c %% %c .. rr "
      "rr rr rr rr .. .. .. .. .. .. %a %% ~a ~c %% %c .. rr rr "
      "rr rr rr rr rr .. .. rr rr .. .. %a %% %% %c .. .. rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_deepness "The Deepness"
        nil ;; sprite
        (kern-mk-map
          nil           38 38 pal_expanded
          (list
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr bb .. .. .. bb rr rr rr rr rr rr "
            "rr rr _3 _1 _1 _5 {{ {{ {{ {{ {{ {{ {{ {{ rr {{ rr rr rr bb %3 %% %% ~7 %% %% %5 .. .. .. .. .. %3 %% %% %% %5 rr "
            "rr _3 __ __ __ __ =| _1 _1 _5 rr rr rr {{ {{ {{ {{ {{ {{ {% %e bb %a ~e %% %% %% bb .. .. .. bb %% %% %% %% %% rr "
            "rr _a _8 _c rr _e {{ __ __ __ __ _5 rr rr rr {{ rr rr rr {{ {2 == .. bb %e bb %% %5 .. .. .. %3 %% bb %% bb %% rr "
            "rr {{ {{ rr rr rr {{ _2 __ __ __ __ ~~ ~1 ~1 ~1 ~5 rr rr ~7 .. ~6 .. .. .. .. ~7 %c .. .. .. %a %% %% %% %% %% rr "
            "rr {{ {{ {{ rr {{ {L __ __ __ __ ~8 ~8 ~8 ~~ ~~ ~~ ~~ ~~ b~ ~9 ~~ ~~ ~d .. .. == .. .. .. .. .. .. .. %e bb %% rr "
            "rr {{ rr {{ {{ {L _3 __ __ _c {G {{ {{ {{ {H -a ~~ ~~ ~~ ~~ %% b~ ~~ bb %7 bb ~6 %5 .. .. .. %3 %5 .. .. %b %% rr "
            "rr {{ {L _3 _1 __ __ __ __ {G {{ rr rr rr {{ {H ~~ ~~ ~~ ~~ ~~ ~~ ~~ %% %% %% ~6 %% %5 .. ~3 ~9 ~9 ~d .. bb %% rr "
            "rr {{ _3 __ __ __ __ __ _4 {{ rr rr {{ rr rr {{ ~2 ~~ ~~ ~~ ~~ ~~ ~~ ~9 ~1 ~9 ~c %% %% %% ~6 %% %% .. .. %b %% rr "
            "rr {{ _a __ __ __ __ __ _4 {{ rr {{ {{ {{ rr {{ ~2 ~~ -- ~~ ~~ b~ %% %% ~6 %% %% %% %% bb ~~ bb %% .. %7 bb %% rr "
            "rr {{ {H _a __ __ __ __ _c {{ rr {{ {{ {{ rr {{ ~2 -- __ -- ~~ ~~ ~5 %% ~6 %c bb ~3 ~1 ~9 ~8 ~9 ~9 =| ~9 ~9 ~9 ~~ "
            "rr {{ {{ {H _a _8 _8 _c {G {{ rr rr {{ rr rr {{ ~2 __ __ __ ~~ ~~ ~~ b~ ~~ bb %3 ~~ bb bb %% %% %% .. .. bb %e rr "
            "rr rr {{ {{ {{ {{ {{ {{ bb {{ {{ rr {{ {{ {{ {L ~~ -- __ -- ~~ ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~5 %% %% %% %5 .. .. .. rr "
            "rr rr rr rr rr {1 rr {{ {{ {{ rr rr b~ ~1 ~1 ~~ ~~ ~~ -- ~~ ~~ ~~ ~~ ~~ __ __ ~~ b~ %% ~~ ~9 b~ ~9 ~d .. bb .. rr "
            "rr rr rr rr rr .. rr rr rr rr rr bb ~~ ~~ b~ ~~ ~~ ~8 ~8 ~8 ~8 ~8 ~~ ~~ __ __ ~~ ~~ ~~ %% %% %% %c .. .. .. .. rr "
            "rr rr rr rr {{ {2 .. rr rr rr bb ~3 ~~ b~ ~~ ~c {G {{ {{ {{ {{ {{ {H ~a ~~ ~~ ~~ ~~ ~~ ~d bb .. .. .. bb %7 .. rr "
            "rr rr rr rr {{ {2 .. rr rr rr b~ ~~ ~~ ~~ ~c {G {{ rr rr rr rr rr bb bb {H ~a ~~ -- ~~ %c .. .. .. .. %3 %% %5 rr "
            "rr rr {{ bb {1 .. .. bb rr bb ~~ ~~ ~~ ~~ {G {{ {{ rr .. .. .. rr .. .. bb {H ~~ -- ~4 bb .. .. .. bb %% ~7 %% rr "
            "rr {{ {{ {2 .. .. .. .. ~C ~3 ~~ -- -- ~4 {A {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %d .. .. .. %b ~b ~~ ~d rr "
            "rr {{ bb .. .. .. bb ~C ~3 ~~ -- -- -- ~4 bb {{ rr && .. .. .. .. .. .. .. bb ~2 -- ~4 bb .. .. .. bb %% ~e %% rr "
            "rr {{ {2 .. .. .. .. ~b ~~ -- -- __ -- ~4 {# {{ rr rr .. .. .. rr .. .. .. bb ~2 -- ~4 %5 .. .. .. .. %a %% %c rr "
            "rr {{ bb .. .. .. bb ~% ~~ -- __ __ __ ~~ {J {{ {{ rr .. .. .. rr .. .. bb {L ~~ -- ~~ %% bb .. .. .. bb %e .. rr "
            "rr rr {{ {2 .. .. .. .. ~a -- -- __ -- -- ~5 {J {{ rr rr .. rr rr bb bb {L ~3 ~~ -- ~~ ~d .. .. .. .. .. .. .. rr "
            "rr rr rr bb .. .. .. bb {H ~~ -- __ __ -- ~~ b~ {J {{ {{ {6 {{ {{ {{ {L ~3 ~~ -- -- ~~ bb .. bb .. .. .. bb .. rr "
            "rr rr rr {{ {2 .. {4 {{ rr ~a -- -- __ -- -- ~~ ~~ ~1 ~1 ~1 ~5 {N ~3 ~~ ~~ -- -- -- ~~ b~ rr rr .. .. .. .. rr rr "
            "rr rr rr bb .. .. .. bb rr rr ~~ -- __ __ -- -- ~~ -- -- -- ~~ ~~ ~~ -- -- -- ~~ ~~ ~~ ~~ b~ rr rr rr .. .. rr rr "
            "rr rr rr {{ {2 .. {4 {{ rr ~b ~~ -- __ __ __ -- -- __ __ __ -- -- -- -- -- ~~ ~~ b~ ~~ ~~ ~~ ~~ ~5 rr .. .. rr rr "
            "rr rr {{ bb .. .. .. bb {{ {H ~~ -- -- -- __ __ __ __ __ __ __ -- -- ~~ ~~ ~~ b~ bb b~ ~~ ~~ b~ ~~ rr .. .. .. rr "
            "rr rr {{ {2 .. .. {4 {{ {{ {{ ~a ~8 ~8 ~~ -- -- -- __ __ __ -- -- ~~ ~~ b~ rr rr rr rr xx xx ~~ ~~ xx xx .. .. rr "
            "rr {{ bb .. .. .. bb {A {{ {{ {{ {{ {{ {H ~a ~8 ~~ -- -- -- ~~ ~8 ~c rr rr rr xx xx xx xx __ __ __ __ xx ,, xx xx "
            "rr {{ {2 .. .. .. .. bb {1 bb {{ {{ {{ {{ {{ {{ {H ~a ~8 ~c {G {{ {{ bb .. bb xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
            "rr {{ bb .. .. .. .. .. .. .. {1 bb {1 bb {1 bb {1 bb .. bb {1 bb {1 .. .. .. ,, ,, ,, ,, ee ee ee ee ,, ,, ,, xx "
            "rr rr {{ {2 {8 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx ,, ,, w+ __ __ __ __ w+ ,, ,, xx "
            "rr rr {{ bb {{ bb {8 bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. {8 bb xx xx xx xx __ __ __ __ xx xx xx xx "
            "rr rr rr rr rr {{ {{ {2 .. .. .. .. .. .. .. .. {8 .. {8 .. .. .. {8 bb {{ {{ xx xx xx __ __ __ __ __ __ xx xx xx "
            "rr rr rr rr rr {{ {{ bb .. .. .. bb {8 bb {8 bb {{ bb {{ bb {8 bb {{ {{ {{ rr xx xx xx __ __ __ __ __ __ xx xx xx "
            "rr rr rr rr {{ {{ {{ {2 .. .. .. {4 {{ {{ {{ rr rr rr rr {{ {{ {{ {{ rr rr rr xx xx xx xx __ __ __ __ xx xx xx xx "
            "rr rr {{ {{ {{ {{ {{ bb .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        (list
          (list
            (kern-mk-place 'p_hydra_fen "Hydra Fen"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                  "rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr "
                  "rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr "
                  "rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr "
                  "rr rr rr {{ {{ {{ {{ {C %3 %% %5 {A {{ {{ {{ {{ rr rr rr "
                  "rr rr {{ {{ {{ {C %3 %% %% %% %% %% %5 {A {{ {{ {{ rr rr "
                  "rr {{ {{ {{ {C %3 %% %% %% %% %% %% %% %5 {A {{ {{ {{ rr "
                  "rr {{ {{ {{ %3 %% %% %% %% %% %% %% %% %% %5 {{ {{ {{ rr "
                  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
                  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
                  "rr {{ {{ {{ %% %% %% %% %% %% %% %% %% %% %% {{ {{ {{ rr "
                  "rr {{ {{ {{ %% %% %% %% ee ee ee %% %% %% %c {{ {{ {{ rr "
                  "rr {{ {{ {{ %a %% %% oo ee ee ee oo %% %% {# {{ {{ {{ rr "
                  "rr rr {{ {{ {% %a %% %% ee ee ee %% %% %c {{ {{ {{ rr rr "
                  "rr rr rr {{ {{ {% %e oo ee ee ee oo %e {# {{ {{ rr rr rr "
                  "rr rr rr rr {{ {{ {{ {{ ee ee ee {{ {{ {{ {{ rr rr rr rr "
                  "rr rr rr rr rr {{ {{ {{ {2 .. {4 {{ {{ {{ rr rr rr rr rr "
                  "rr rr rr rr rr rr {{ {{ {2 .. {4 {{ {{ rr rr rr rr rr rr "
                  "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              nil ;; neighbors
              (list ;; objects in p_hydra_fen
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'hydra
                    )
                  ) ;; bind
                9 9)
              ) ;; end of objects in p_hydra_fen
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_hydra_fen

          1)
        ) ;; end neighbors of p_deepness
        (list ;; objects in p_deepness
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'headless
              )
            ) ;; bind
          5 21)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'headless
              )
            ) ;; bind
          3 20)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'kraken
              )
            ) ;; bind
          5 9)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'sea-serpent
              )
            ) ;; bind
          18 11)
          (list
            (bind
              (kern-mk-obj t_step_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'step-pt
                "A yellow slime oozes up!"
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
                (list
                  (list
                    'yellow-slime
                    32
                    11
                  )
                )
              )
            ) ;; bind
          33 10)
          (list
            (bind
              (kern-mk-obj t_step_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'step-pt
                "A yellow slime oozes up!"
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
                (list
                  (list
                    'yellow-slime
                    26
                    3
                  )
                )
              )
            ) ;; bind
          26 5)
          (list
            (bind
              (kern-mk-obj t_step_trig 1
                ;; hooks
                (list
                  (list
                    ef_permanent_invisibility
                    '()
                    2
                    0
                  )
                )
              )
              (list
                'spawn-kraken-lakes-sea-serpent
                '()
              )
            ) ;; bind
          31 31)
          (list
            (bind
              (kern-mk-obj t_portcullis 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                '()
                #f
                '()
              )
            ) ;; bind
          31 28)
          (list
            (bind
              (kern-mk-obj t_portcullis 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                '()
                #f
                '()
              )
            ) ;; bind
          32 28)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          26 31)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_open_door_in_stone
                    #f
                    0
                  )
                  (list
                    's_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                  (list
                    's_magically_locked_solid_wood_door_in_stone
                    #t
                    5
                  )
                )
              )
            ) ;; bind
          35 29)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_windowed_wood_door_in_rock
                    #f
                    5
                  )
                  (list
                    's_open_door_in_rock
                    #f
                    0
                  )
                  (list
                    's_locked_windowed_wood_door_in_rock
                    #f
                    5
                  )
                  (list
                    's_magically_locked_windowed_wood_door_in_rock
                    #f
                    5
                  )
                )
              )
            ) ;; bind
          34 31)
          (list
            (bind
              (kern-mk-obj t_door 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                0
                '()
                #f
                #f
                #f
                (list
                  (list
                    's_closed_windowed_wood_door_in_rock
                    #f
                    5
                  )
                  (list
                    's_open_door_in_rock
                    #f
                    0
                  )
                  (list
                    's_locked_windowed_wood_door_in_rock
                    #f
                    5
                  )
                  (list
                    's_magically_locked_windowed_wood_door_in_rock
                    #f
                    5
                  )
                )
              )
            ) ;; bind
          29 31)
          (list
            (bind
              (kern-mk-obj t_ladder_down 1
                ;; hooks
                (list
                )
              )
              (list
                'p_lost_garrison
                11
                11
              )
            ) ;; bind
          12 9)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'headless
              )
            ) ;; bind
          3 22)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'headless
              )
            ) ;; bind
          7 22)
        ) ;; end of objects in p_deepness
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 37 37) ;; Northwest
          (list 1 9 37) ;; North
          (list 2 0 37) ;; Northeast
          (list 3 37 19) ;; West
          (list 4 19 19) ;; Here
          (list 5 0 19) ;; East
          (list 6 37 0) ;; Southwest
          (list 7 29 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_deepness

    1)
  ) ;; end neighbors of p_pools
  (list ;; objects in p_pools
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'yellow-slime
        )
      ) ;; bind
    4 6)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'yellow-slime
        )
      ) ;; bind
    10 13)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_great_hall
          9
          6
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'yellow-slime
        )
      ) ;; bind
    11 3)
  ) ;; end of objects in p_pools
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_pools

p_deepness
p_hydra_fen
(kern-mk-place 'p_lost_garrison "Deep Fortress"
  nil ;; sprite
  (kern-mk-map
    nil     38 38 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr xx xx xx rr rr rr xx xx xx rr rr rr xx xx rr rr rr xx xx rr rr rr "
      "rr rr rr xx xx xx w+ xx ,, x! ,, xx w+ xx xx xx xx xx rr x! xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr "
      "rr .. rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      ".. .. .. rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, .. ,, ,, xx ,, ,, ,, xx ,, ,, xx ,, xx ,, ,, xx xx rr rr "
      ".. .. .. w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, rr xx xx x! ,, x! xx xx xx xx rr rr "
      ".. .. .. xx ,, ,, .. ,, ,, ,, .. bb ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      ".. .. .. ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx ,, xx ,, ,, xx rr rr rr "
      ".. .. .. x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, xx xx xx xx ,, xx xx xx xx rr rr rr "
      ".. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rr rr "
      ".. .. .. xx ,, ,, bb ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rr rr "
      ".. .. .. w+ ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, w+ ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rr rr "
      ".. .. .. xx ,, ,, ,, ,, ,, .. ,, ,, bb ,, ,, xx ,, ,, ,, ,, ,, .. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, .. .. .. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr xx xx xx xx w+ w+ x! w+ x! w+ w+ xx xx xx xx xx xx x! xx xx xx ,, ,, ,, x! ,, [[ @@ @@ @@ ]] ,, x! xx rr rr "
      "rr rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rr xx xx rr xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr rr rr rr rr rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr rr x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rr rr rr rr rr rr xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx rr rr "
      "rr rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr rr rr xx ,, ,, ,, xx [[ .M .E .D .I .K ]] xx xx rr rr "
      "rr rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr rr rr rr ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr rr xx xx rr xx rr xx xx xx xx ,, ,, ,, xx xx rr rr xx xx xx rr ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, x! rr rr rr "
      "rr rr rr xx ,, ,, ,, [[ @@ @@ ]] xx ,, ,, ,, x! xx xx x! xx xx xx x! ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, bb rr rr rr "
      "rr rr rr xx 00 ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rr rr "
      "rr rr xx xx xx xx xx xx x! xx xx xx xx xx xx ,, ,, xx xx xx xx ,, ,, xx xx xx xx xx xx xx xx xx rr xx xx rr rr rr "
      "rr rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx rr rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx xx rr rr "
      "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, [[ @@ @@ @@ @@ @@ @@ @@ @@ ]] ,, ,, ,, 00 xx xx rr rr "
      "rr rr rr rr .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx rr rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx rr rr rr "
      "rr rr xx rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, [[ @@ @@ @@ @@ @@ @@ @@ @@ ]] ,, ,, ,, && xx xx rr rr "
      "rr rr xx bb ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx xx xx rr "
      "rr rr xx xx xx xx xx xx xx xx xx rr xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx xx xx rr rr "
      "rr rr rr xx rr xx xx xx rr rr xx xx xx xx rr rr rr xx xx xx xx xx rr xx rr xx rr xx xx rr xx xx rr rr xx rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_lost_garrison
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    29 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    29 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    31 4)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    31 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    26 12)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    26 24)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    21 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    22 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    16 26)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    11 24)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'edwp2
          #f
          '()
        )
      ) ;; bind
    4 17)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          'ednp2
          #f
          '()
        )
      ) ;; bind
    4 16)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          'ede1p2
          #f
          '()
        )
      ) ;; bind
    14 16)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          'ede2p2
          #f
          '()
        )
      ) ;; bind
    25 4)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    4 25)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    5 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    11 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    14 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    17 28)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    5 30)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 30)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    11 30)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    14 30)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    17 30)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    5 32)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    8 32)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    11 32)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    14 32)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    17 32)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    28 21)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    32 24)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    30 24)
    (list
      (kern-mk-obj t_bed 1
        ;; hooks
        (list
        )
      )
    32 22)
    (list
      (kern-mk-obj t_garrison_log 1
        ;; hooks
        (list
        )
      )
    4 23)
    (list
      (kern-mk-obj t_food 21
        ;; hooks
        (list
        )
      )
    33 27)
    (list
      (kern-mk-obj t_food 1
        ;; hooks
        (list
        )
      )
    22 29)
    (list
      (kern-mk-obj t_food 1
        ;; hooks
        (list
        )
      )
    27 32)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'troll-m
        )
      ) ;; bind
    6 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'troll-m
        )
      ) ;; bind
    11 19)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'cave-goblin-berserker-m
        )
      ) ;; bind
    29 11)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'cave-goblin-slinger-m
        )
      ) ;; bind
    32 13)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'cave-goblin-berserker-m
        )
      ) ;; bind
    32 23)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'gint-warrior-m
        )
      ) ;; bind
    23 28)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'gint-warrior-m
        )
      ) ;; bind
    28 33)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_armor_chain
            )
            (list
              1
              't_sword_2
            )
            (list
              1
              't_scratched_shield
            )
          )
        )
      ) ;; bind
    7 24)
    (list
      (bind
        (kern-mk-char
          nil
          "halberdier"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          14 1024
          6 4
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-halberdier
          #f
          #f
          '()
        )
      ) ;; bind
    18 12)
    (list
      (bind
        (kern-mk-char
          nil
          "halberdier"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          18 16384
          10 8
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-halberdier
          #f
          #f
          '()
        )
      ) ;; bind
    17 6)
    (list
      (bind
        (kern-mk-char
          nil
          "halberdier"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          16 4096
          8 6
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-halberdier
          #f
          #f
          '()
        )
      ) ;; bind
    24 12)
    (list
      (bind
        (kern-mk-char
          nil
          "crossbowman"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          15 2048
          7 5
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 10 t_bolt)
              (list 1 t_dagger)
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_bolt
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-crossbowman
          #f
          #f
          '()
        )
      ) ;; bind
    8 17)
    (list
      (bind
        (kern-mk-char
          nil
          "crossbowman"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          16 4096
          8 6
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 10 t_bolt)
              (list 1 t_dagger)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_bolt
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-crossbowman
          #f
          #f
          '()
        )
      ) ;; bind
    14 5)
    (list
      (bind
        (kern-mk-char
          nil
          "halberdier"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          17 8192
          9 7
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-halberdier
          #f
          #f
          '()
        )
      ) ;; bind
    24 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_heal_potion
            )
            (list
              1
              't_crossbow
            )
            (list
              13
              't_bolt
            )
          )
        )
      ) ;; bind
    13 6)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_gold_coins
            )
          )
        )
      ) ;; bind
    11 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_bolt
            )
            (list
              1
              't_bolt
            )
            (list
              1
              't_gold_coins
            )
          )
        )
      ) ;; bind
    17 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_mana_potion
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    7 29)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_gold_coins
            )
            (list
              4
              't_gold_coins
            )
          )
        )
      ) ;; bind
    30 30)
    (list
      (bind
        (kern-mk-char
          nil
          "gazer"
          sp_gazer
          oc_wizard
          s_gazer
          5
          0 0 0
          0 0
          0 0
          19 32768
          44 9
          #f ;; dead?
          nil
          nil
          'gazer-ai
          (kern-mk-container
            t_chest
            ;; trap
            'poison-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gazer
          #f
          #f
          '()
        )
      ) ;; bind
    25 8)
    (list
      (bind
        (kern-mk-char
          nil
          "gazer"
          sp_gazer
          oc_wizard
          s_gazer
          5
          0 0 0
          0 0
          0 0
          18 16384
          40 8
          #f ;; dead?
          nil
          nil
          'gazer-ai
          (kern-mk-container
            t_chest
            ;; trap
            'lightning-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gazer
          #f
          #f
          '()
        )
      ) ;; bind
    22 31)
    (list
      (bind
        (kern-mk-char
          nil
          "gazer"
          sp_gazer
          oc_wizard
          s_gazer
          5
          0 0 0
          0 0
          0 0
          17 8192
          36 7
          #f ;; dead?
          nil
          nil
          'gazer-ai
          (kern-mk-container
            t_chest
            ;; trap
            'poison-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gazer
          #f
          #f
          '()
        )
      ) ;; bind
    7 25)
    (list
      (bind
        (kern-mk-char
          nil
          "gazer"
          sp_gazer
          oc_wizard
          s_gazer
          5
          0 0 0
          0 0
          0 0
          16 4096
          32 6
          #f ;; dead?
          nil
          nil
          'gazer-ai
          (kern-mk-container
            t_chest
            ;; trap
            'lightning-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gazer
          #f
          #f
          '()
        )
      ) ;; bind
    32 11)
    (list
      (bind
        (kern-mk-char
          nil
          "gint warrior"
          sp_gint
          oc_warrior
          s_ettin
          5
          0 0 0
          0 0
          0 0
          90 16384
          10 8
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_2h_sword
            t_2h_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "4d25"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d5"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gint-warrior-m
          #f
          #f
          '()
        )
      ) ;; bind
    5 6)
    (list
      (bind
        (kern-mk-char
          nil
          "gint warrior"
          sp_gint
          oc_warrior
          s_ettin
          5
          0 0 0
          0 0
          0 0
          85 8192
          9 7
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_2h_sword
            t_2h_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "4d25"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d5"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gint-warrior-m
          #f
          #f
          '()
        )
      ) ;; bind
    12 32)
    (list
      (bind
        (kern-mk-char
          nil
          "gint warrior"
          sp_gint
          oc_warrior
          s_ettin
          5
          0 0 0
          0 0
          0 0
          80 4096
          8 6
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_2h_sword
            t_2h_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "4d25"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d5"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'gint-warrior-m
          #f
          #f
          '()
        )
      ) ;; bind
    30 27)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          5
          0 0 0
          0 0
          0 0
          34 8192
          9 7
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll-m
          #f
          #f
          '()
        )
      ) ;; bind
    7 19)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          5
          0 0 0
          0 0
          0 0
          32 4096
          8 6
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll-m
          #f
          #f
          '()
        )
      ) ;; bind
    29 17)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          5
          0 0 0
          0 0
          0 0
          30 2048
          7 5
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll-m
          #f
          #f
          '()
        )
      ) ;; bind
    10 29)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          5
          0 0 0
          0 0
          0 0
          30 2048
          7 5
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll-m
          #f
          #f
          '()
        )
      ) ;; bind
    31 21)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          5
          0 0 0
          0 0
          0 0
          28 1024
          6 4
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll-m
          #f
          #f
          '()
        )
      ) ;; bind
    13 16)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin berserker"
          sp_cave_goblin
          oc_warrior
          s_cgob_berserk
          5
          0 0 0
          0 0
          0 0
          18 4096
          8 6
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_axe
            t_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d15"
                      't_gold_coins
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-berserker-m
          #f
          #f
          '()
        )
      ) ;; bind
    12 31)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin berserker"
          sp_cave_goblin
          oc_warrior
          s_cgob_berserk
          5
          0 0 0
          0 0
          0 0
          17 2048
          7 5
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_axe
            t_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d15"
                      't_gold_coins
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-berserker-m
          #f
          #f
          '()
        )
      ) ;; bind
    9 32)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin berserker"
          sp_cave_goblin
          oc_warrior
          s_cgob_berserk
          5
          0 0 0
          0 0
          0 0
          16 1024
          6 4
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_axe
            t_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d15"
                      't_gold_coins
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-berserker-m
          #f
          #f
          '()
        )
      ) ;; bind
    18 8)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin slinger"
          sp_cave_goblin
          oc_warrior
          s_cgob_slinger
          5
          0 0 0
          0 0
          0 0
          18 4096
          8 6
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            'spike-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_sling
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      20
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-slinger-m
          #f
          #f
          '()
        )
      ) ;; bind
    7 20)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin slinger"
          sp_cave_goblin
          oc_warrior
          s_cgob_slinger
          5
          0 0 0
          0 0
          0 0
          17 2048
          7 5
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            'spike-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_sling
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      20
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-slinger-m
          #f
          #f
          '()
        )
      ) ;; bind
    12 16)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin slinger"
          sp_cave_goblin
          oc_warrior
          s_cgob_slinger
          5
          0 0 0
          0 0
          0 0
          16 1024
          6 4
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_sling
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      20
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-slinger-m
          #f
          #f
          '()
        )
      ) ;; bind
    10 32)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin slinger"
          sp_cave_goblin
          oc_warrior
          s_cgob_slinger
          5
          0 0 0
          0 0
          0 0
          15 512
          5 3
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_sling
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      20
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-slinger-m
          #f
          #f
          '()
        )
      ) ;; bind
    32 16)
    (list
      (bind
        (kern-mk-obj t_buried 1
          ;; hooks
          (list
          )
        )
        (list
          't_rune_p
          1
        )
      ) ;; bind
    3 33)
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          0
          #t
          #t
          (list
            'p_deeps_1
            'p_deeps_2
            'p_deeps_5
            'p_deeps_3
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_deepness
          12
          9
        )
      ) ;; bind
    11 11)
    (list
      (kern-tag 'ednp1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    8 3)
    (list
      (kern-tag 'ednp2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            'ednp1
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    10 3)
    (list
      (kern-tag 'edwp1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    3 8)
    (list
      (kern-tag 'edwp2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            'edwp1
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    3 10)
    (list
      (kern-tag 'ede1p1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    15 8)
    (list
      (kern-tag 'ede1p2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            'ede1p1
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    15 10)
    (list
      (kern-tag 'ede2p1
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    22 8)
    (list
      (kern-tag 'ede2p2
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #t
            'ede2p1
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    22 10)
  ) ;; end of objects in p_lost_garrison
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
    'deeps-room-handle-garrison
  )
  (list ;; edge entrances
    (list 0 37 37) ;; Northwest
    (list 1 19 37) ;; North
    (list 2 0 37) ;; Northeast
    (list 3 37 19) ;; West
    (list 4 19 19) ;; Here
    (list 5 0 9) ;; East
    (list 6 37 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_lost_garrison

(kern-mk-place 'p_deeps_1 "endless deepness"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      ".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_deeps_1
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          1
          #f
          #f
          (list
            'p_deeps_2
            'p_deeps_5
            'p_deeps_3
            'p_deeps_4
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_deeps_1
  (list ;; on-entry-hooks
    'deeps-room-handle-deeps
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_deeps_1

(kern-mk-place 'p_deeps_2 "endless deepness"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      ".. .. .. .. .. .. .B .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_deeps_2
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          2
          #f
          #f
          (list
            'p_deeps_5
            'p_deeps_3
            'p_deeps_4
            'p_deeps_1
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_deeps_2
  (list ;; on-entry-hooks
    'deeps-room-handle-deeps
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_deeps_2

(kern-mk-place 'p_deeps_3 "endless deepness"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .C .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_deeps_3
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          3
          #f
          #f
          (list
            'p_deeps_4
            'p_deeps_1
            'p_deeps_2
            'p_deeps_5
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_deeps_3
  (list ;; on-entry-hooks
    'deeps-room-handle-deeps
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_deeps_3

(kern-mk-place 'p_deeps_4 "endless deepness"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      ".. .. .. .. .. .. .D .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_deeps_4
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          4
          #f
          #f
          (list
            'p_deeps_1
            'p_deeps_2
            'p_deeps_5
            'p_deeps_3
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_deeps_4
  (list ;; on-entry-hooks
    'deeps-room-handle-deeps
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_deeps_4

(kern-mk-place 'p_deeps_5 "endless deepness"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      ".. .. .. .. .. .. .E .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
      "rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_deeps_5
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_roomdata 1
          ;; hooks
          (list
          )
        )
        (list
          0
          5
          #f
          #f
          (list
            'p_deeps_3
            'p_deeps_4
            'p_deeps_1
            'p_deeps_2
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_deeps_5
  (list ;; on-entry-hooks
    'deeps-room-handle-deeps
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_deeps_5

(kern-mk-place 'p_forsaken_prison "Forsaken Prison"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, xx ,, ,, ,, xx ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, xx ,, ,, ,, xx ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx xx xx xx "
      "xx ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx "
      "xx xx xx xx xx ,, xx xx ,, ,, ,, xx xx ,, xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx "
      "xx xx ?? ?? ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx "
      "xx xx ?? xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, xx ,, ,, xx ,, ,, ,, xx ,, ,, xx xx xx xx xx "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx ,, ,, ,, xx ,, ,, xx ,, ,, ,, xx ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_forsaken_prison
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    5 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    3 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    5 7)
    (list
      (bind
        (kern-mk-char
          nil
          "bandit"
          sp_human
          oc_wrogue
          s_brigand
          8
          0 0 0
          0 0
          0 0
          14 1024
          6 4
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            'self-destruct-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_sword
            t_armor_leather
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "2d6-2"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_picklock
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                    (list
                      10
                      "1d3"
                      't_torch
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'bandit
          #f
          #f
          '()
        )
      ) ;; bind
    5 2)
    (list
      (bind
        (kern-mk-char
          nil
          "cave goblin berserker"
          sp_cave_goblin
          oc_warrior
          s_cgob_berserk
          3
          0 0 0
          0 0
          0 0
          16 1024
          6 4
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            'spike-trap
            ;; contents
            (list
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_axe
            t_axe
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d15"
                      't_gold_coins
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'cave-goblin-berserker
          #f
          #f
          '()
        )
      ) ;; bind
    6 15)
    (list
      (bind
        (kern-mk-char
          nil
          "troll"
          sp_troll
          oc_warrior
          s_troll
          6
          0 0 0
          0 0
          0 0
          32 4096
          8 6
          #f ;; dead?
          nil
          nil
          'std-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_thrown_boulder)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_thrown_boulder
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_thrown_boulder
                    )
                    (list
                      25
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "2d10"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'troll
          #f
          #f
          '()
        )
      ) ;; bind
    2 9)
    (list
      (bind
        (kern-mk-char
          nil
          "warlock"
          sp_human
          oc_wizard
          s_wizard
          5
          0 0 0
          0 0
          0 0
          18 16384
          10 8
          #f ;; dead?
          nil
          nil
          'warlock-ai
          (kern-mk-container
            t_chest
            ;; trap
            'lightning-trap
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_dagger
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'warlock
          #f
          #f
          '()
        )
      ) ;; bind
    13 16)
    (list
      (bind
        (kern-mk-char
          nil
          "halberdier"
          sp_human
          oc_warrior
          s_guard
          5
          0 0 0
          0 0
          0 0
          13 512
          5 3
          #f ;; dead?
          nil
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_heal_potion)
              (list 1 t_in_an_scroll)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_halberd
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                    (list
                      10
                      "1"
                      't_vas_mani_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      50
                      "1d5"
                      't_food
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'corrupt-halberdier
          #f
          #f
          '()
        )
      ) ;; bind
    17 9)
    (list
      (bind
        (kern-mk-char
          nil
          "skeletal warrior"
          sp_skeleton
          oc_warrior
          s_skeleton
          5
          0 0 0
          0 0
          0 0
          17 2048
          7 5
          #f ;; dead?
          nil
          nil
          'generic-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_iron_helm
            t_shield
            t_sword
          )
          ;; hooks
          (list
            (list
              ef_poison_immunity
              '()
              2
              0
            )
            (list
              ef_disease_immunity
              '()
              2
              0
            )
            (list
              ef_fire_immunity
              '()
              2
              0
            )
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'skeletal-warrior
          #f
          #f
          '()
        )
      ) ;; bind
    13 1)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_bolt
            )
          )
        )
      ) ;; bind
    14 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_arrow
            )
          )
        )
      ) ;; bind
    12 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              5
              't_gold_coins
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    14 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              4
              't_bolt
            )
            (list
              4
              't_gold_coins
            )
          )
        )
      ) ;; bind
    5 6)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_paladins_hold
          8
          17
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_mans_hideout
          9
          3
        )
      ) ;; bind
    2 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    7 2)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    11 2)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    13 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    15 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    13 11)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    11 16)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    7 16)
  ) ;; end of objects in p_forsaken_prison
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_forsaken_prison

(kern-mk-place 'p_old_mine "Old Mine"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr bb bb rr rr rr rr rr bb bb bb .. .. .. .. bb bb rr "
      "rr bb .. .. bb rr rr rr bb .. .. .. .. .. .. .. .. bb rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. rr rr .. .. .. bb rr "
      "rr rr .. .. .. .. .. .. .. .. .. bb rr rr rr .. .. rr rr "
      "rr rr .. .. rr rr rr rr .. .. .. rr rr rr rr .. .. rr rr "
      "rr rr .. .. bb rr rr rr rr .. .. bb bb rr rr .. .. rr rr "
      "rr rr .. .. .. bb bb bb bb .. .. .. bb .. bb .. .. rr rr "
      "rr rr .. .. .. bb bb .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr rr .. .. bb rr .. .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr rr .. .. rr bb .. .. .. .. .. rr rr .. .. .. .. bb rr "
      "rr rr .. .. rr rr .. .. .. .. bb rr rr rr rr .. .. rr rr "
      "rr rr .. .. .. bb .. .. rr bb rr rr rr rr rr .. .. rr rr "
      "rr rr .. .. .. .. .. rr rr rr rr .. .. rr .. .. .. rr rr "
      "rr bb .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. rr rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. .. .. .. rr .. rr rr "
      "rr rr bb .. .. .. .. .. .. rr rr .. .. rr rr rr .. .. rr "
      "rr rr rr rr bb rr rr rr rr rr rr rr rr rr rr rr .. .. rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_old_mine
    (list
      (bind
        (kern-mk-obj t_step_trig 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'wind-trap
          '()
        )
      ) ;; bind
    15 14)
    (list
      (bind
        (kern-mk-obj t_step_trig 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'wind-trap
          '()
        )
      ) ;; bind
    16 12)
    (list
      (bind
        (kern-mk-obj t_step_trig 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'wind-trap
          '()
        )
      ) ;; bind
    2 8)
    (list
      (bind
        (kern-mk-obj t_step_trig 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'wind-trap
          '()
        )
      ) ;; bind
    10 3)
    (list
      (kern-mk-obj t_gem 8
        ;; hooks
        (list
        )
      )
    6 7)
    (list
      (kern-mk-obj t_gem 3
        ;; hooks
        (list
        )
      )
    1 14)
    (list
      (kern-mk-obj t_gem 7
        ;; hooks
        (list
        )
      )
    11 1)
    (list
      (kern-mk-obj t_gem 5
        ;; hooks
        (list
        )
      )
    1 2)
    (list
      (kern-mk-obj t_gem 5
        ;; hooks
        (list
        )
      )
    2 1)
    (list
      (kern-mk-obj t_gem 2
        ;; hooks
        (list
        )
      )
    9 1)
    (list
      (kern-mk-obj t_gem 4
        ;; hooks
        (list
        )
      )
    17 3)
    (list
      (kern-mk-obj t_gem 3
        ;; hooks
        (list
        )
      )
    4 9)
    (list
      (kern-mk-obj t_gem 8
        ;; hooks
        (list
        )
      )
    5 12)
    (list
      (kern-mk-obj t_gem 9
        ;; hooks
        (list
        )
      )
    5 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_gold_coins
            )
          )
        )
      ) ;; bind
    3 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_mana_potion
            )
            (list
              1
              't_food
            )
          )
        )
      ) ;; bind
    15 6)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              2
              't_bolt
            )
          )
        )
      ) ;; bind
    8 3)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_mana_potion
            )
            (list
              2
              't_arrow
            )
            (list
              1
              't_mana_potion
            )
          )
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_gold_coins
            )
          )
        )
      ) ;; bind
    8 14)
    (list
      (kern-mk-field f_web_perm -1)    2 1)
    (list
      (kern-mk-field f_web_perm -1)    3 1)
    (list
      (kern-mk-field f_web_perm -1)    1 2)
    (list
      (kern-mk-field f_web_perm -1)    2 2)
    (list
      (kern-mk-field f_web_perm -1)    3 2)
    (list
      (kern-mk-field f_web_perm -1)    4 2)
    (list
      (kern-mk-field f_web_perm -1)    1 3)
    (list
      (kern-mk-field f_web_perm -1)    2 3)
    (list
      (kern-mk-field f_web_perm -1)    3 3)
    (list
      (kern-mk-field f_web_perm -1)    4 3)
    (list
      (kern-mk-field f_web_perm -1)    5 3)
    (list
      (kern-mk-field f_web_perm -1)    2 4)
    (list
      (kern-mk-field f_web_perm -1)    3 4)
    (list
      (kern-mk-field f_web_perm -1)    4 4)
    (list
      (kern-mk-field f_web_perm -1)    5 4)
    (list
      (kern-mk-field f_web_perm -1)    2 5)
    (list
      (kern-mk-field f_web_perm -1)    3 5)
    (list
      (kern-mk-field f_web_perm -1)    2 6)
    (list
      (kern-mk-field f_web_perm -1)    3 6)
    (list
      (kern-mk-field f_web_perm -1)    4 6)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_trolls_den
          3
          15
        )
      ) ;; bind
    17 17)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    4 15)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-warrior
        )
      ) ;; bind
    15 2)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'skeletal-spear-thrower
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ghast
        )
      ) ;; bind
    15 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ghast
        )
      ) ;; bind
    11 14)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ghast
        )
      ) ;; bind
    9 4)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    3 3)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'giant-spider
        )
      ) ;; bind
    4 4)
  ) ;; end of objects in p_old_mine
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_old_mine

(kern-mk-place 'p_lich_tomb "Lich Tomb"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx %3 %% %% %5 pp ,, ,, ,, cc ,, ,, ,, pp %3 %% %% %5 xx "
      "xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
      "xx %% %% %% %% ,, cc cc cc cc cc cc cc ,, %% %% %% %% xx "
      "xx %% %% %% %% ,, ,, cc cc cc cc cc ,, ,, %% %% %% %% xx "
      "xx %% %% %% %% pp ,, ,, cc cc cc ,, ,, pp %% %% %% %% xx "
      "xx %a %% %% %% %% %% ,, cc cc cc ,, %% %% %% %% %% %c xx "
      "xx .. %a %% %% %% %% pp cc cc cc pp %% %% %% %% %c .. xx "
      "xx .. .. %a %% %% %% ,, cc cc cc ,, %% %% %% %c .. .. xx "
      "xx .. .. .. %a %% %c pp cc cc cc pp %a %% %c .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. ,, cc cc cc ,, .. .. .. .. .. .. xx "
      "xx .. .. .. .. .. .. pp cc cc cc pp .. .. .. .. .. .. xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_lich_tomb
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_crypt
          9
          10
        )
      ) ;; bind
    9 16)
    (list
      (kern-mk-field f_sleep_perm -1)    8 1)
    (list
      (kern-mk-field f_sleep_perm -1)    10 1)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_lich_king
            "Lich King"
            sp_lich
            oc_wizard
            s_lich
            5
            10 10 10
            10 1
            0 0
            54 0
            50 8
            #f ;; dead?
            nil
            nil
            'lich-ai
            (kern-mk-container
              t_chest
              ;; trap
              'lightning-trap
              ;; contents
              (list
                (list 1 t_morning_star)
                (list 1 t_armor_chain_4)
                (list 1 t_chain_coif_4)
                (list 100 t_gold_coins)
                (list 3 t_mana_potion)
                (list 3 t_heal_potion)
                (list 1 t_lich_skull)
              )
              ;; hooks
              (list
              )
            )
            nil
            ;; hooks
            (list
              (list
                ef_poison_immunity
                '()
                2
                0
              )
              (list
                ef_disease_immunity
                '()
                2
                0
              )
              (list
                ef_fire_immunity
                '()
                2
                0
              )
            )
          )
        #t) ;; kern-char-force-drop
        '()
      ) ;; bind
    9 2)
  ) ;; end of objects in p_lich_tomb
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_lich_tomb

(kern-mk-place 'p_altar_room "Altar Room"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx pp ,, ,, ,, pp xx xx xx xx xx xx xx "
      "xx xx ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, xx xx "
      "xx xx ,, cc cc cc cc cc cc aa cc cc cc cc cc cc ,, xx xx "
      "xx xx ,, cc ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx pp ,, cc ,, pp xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx "
      "xx pp ,, cc ,, pp xx xx vv vv vv xx xx pp ,, cc ,, pp xx "
      "xx ,, cc cc cc ,, ,, vv vv vv vv vv ,, ,, cc cc cc ,, xx "
      "xx ,, cc aa cc cc cc vv vv vv vv vv cc cc cc aa cc ,, xx "
      "xx ,, cc cc cc ,, ,, vv vv vv vv vv ,, ,, cc cc cc ,, xx "
      "xx pp ,, cc ,, pp xx xx vv vv vv xx xx pp ,, cc ,, pp xx "
      "xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx pp ,, cc ,, pp xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, cc ,, xx xx "
      "xx xx ,, cc cc cc cc cc cc aa cc cc cc cc cc cc ,, xx xx "
      "xx xx ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx pp ,, ,, ,, pp xx xx xx xx ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_altar_room
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'warlock
        )
      ) ;; bind
    6 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'warlock
        )
      ) ;; bind
    12 9)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_death_knights_hold
          17
          9
        )
      ) ;; bind
    17 17)
    (list
      (bind
        (kern-mk-char
          nil
          "warlock"
          sp_human
          oc_wizard
          s_wizard
          5
          0 0 0
          0 0
          0 0
          30 67108864
          22 20
          #f ;; dead?
          nil
          nil
          'warlock-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          (list
            t_dagger
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                    (list
                      100
                      "1d2+1"
                      't_mana_potion
                    )
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      10
                      "1d3"
                      't_food
                    )
                    (list
                      10
                      "1"
                      't_cure_potion
                    )
                    (list
                      10
                      "1"
                      't_poison_immunity_potion
                    )
                    (list
                      20
                      "1d5"
                      'sulphorous_ash
                    )
                    (list
                      20
                      "1d5"
                      'ginseng
                    )
                    (list
                      20
                      "1d5"
                      'garlic
                    )
                    (list
                      10
                      "1d3"
                      'spider_silk
                    )
                    (list
                      10
                      "1d3"
                      'blood_moss
                    )
                    (list
                      10
                      "1d3"
                      'black_pearl
                    )
                    (list
                      5
                      "1d2"
                      'nightshade
                    )
                    (list
                      5
                      "1d2"
                      'mandrake
                    )
                    (list
                      5
                      "1"
                      't_in_mani_corp_scroll
                    )
                    (list
                      5
                      "1"
                      't_xen_corp_scroll
                    )
                    (list
                      10
                      "1"
                      't_in_quas_xen_scroll
                    )
                    (list
                      10
                      "1"
                      't_an_xen_exe_scroll
                    )
                    (list
                      20
                      "1"
                      't_in_an_scroll
                    )
                    (list
                      20
                      "1"
                      't_vas_mani_scroll
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'warlock
          #f
          #f
          '()
        )
      ) ;; bind
    9 6)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'warlock
        )
      ) ;; bind
    9 12)
  ) ;; end of objects in p_altar_room
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_altar_room

(kern-mk-place 'p_dank_cave "Dank Cave"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
      "rr rr rr rr {{ rr rr rr {8 .. {8 rr rr rr {{ rr rr rr rr "
      "rr rr rr {{ {{ {{ rr {{ {{ {e {{ {{ rr {{ {{ {{ rr rr rr "
      "rr rr {{ {C %f {A {{ {{ {{ {{ {{ {{ {{ {C .! {A {{ rr rr "
      "rr {{ {{ %7 .! %3 %d {{ {{ rr {{ {{ %b %% %% %5 {{ {{ rr "
      "rr {{ {{ %a %% %c {# {{ rr rr rr {{ {% %a %% %c {{ {{ rr "
      "rr rr {{ {% %e {# {{ {{ {{ rr {{ {{ {{ {% %e {# {{ rr rr "
      "rr rr rr {{ {{ {{ rr {{ {{ {{ {{ {{ rr {{ {{ {{ rr rr rr "
      "rr rr rr rr {{ rr rr rr {{ {{ {{ rr rr rr {{ rr rr rr rr "
      "rr rr rr {{ {{ {{ rr {{ {{ {{ {{ {{ rr {{ {{ {{ rr rr rr "
      "rr rr {{ {C %7 {A {{ {{ {{ rr {{ {{ {{ {C %7 {A {{ rr rr "
      "rr {{ {{ %3 %% %d {A {{ rr rr rr {{ {C %3 %% %5 {{ {{ rr "
      "rr {{ {{ %a %% .! %7 {A {{ rr {{ {C %3 %% %% %c {{ {{ rr "
      "rr rr {{ {% %a %% %% %d {{ {{ {{ %b %% .! %e {# {{ rr rr "
      "rr rr rr {{ {% %a %c {# {{ rr {{ {% %a %d {# {{ rr rr rr "
      "rr rr rr rr {{ {{ {{ {{ rr rr rr {{ {{ {{ {{ rr rr rr rr "
      "rr rr rr rr rr {{ {{ rr rr rr rr rr {{ {{ rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_dank_cave
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    5 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'rat
        )
      ) ;; bind
    4 2)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'snake
        )
      ) ;; bind
    17 12)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'mandrake
          "1d5"
        )
      ) ;; bind
    14 14)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'nightshade
          "1d5"
        )
      ) ;; bind
    6 14)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'ginseng
          "1d5"
        )
      ) ;; bind
    3 12)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'ginseng
          "1d5"
        )
      ) ;; bind
    3 5)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'garlic
          "1d5"
        )
      ) ;; bind
    15 5)
    (list
      (bind
        (kern-mk-obj t_custom_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'grow-trig
          'garlic
          "1d5"
        )
      ) ;; bind
    3 13)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_shamans_grove
          14
          11
        )
      ) ;; bind
    9 1)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'gazer
        )
      ) ;; bind
    9 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'headless
        )
      ) ;; bind
    8 7)
  ) ;; end of objects in p_dank_cave
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_dank_cave

(kern-mk-place 'p_eastpass "Eastpass"
  s_keep ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      ".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. .. {5 {{ {{ {{ {{ xx ,, xx {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. .. {4 {{ {{ {{ {{ w+ ,, w+ {5 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. {1 {1 {1 {1 xx ,, xx .. {1 {5 {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. .. .. xx xx xx ,, xx xx xx {4 {{ {{ ^^ ^^ ^^ ^^ "
      ".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. {5 {{ {{ ^^ ^^ ^^ "
      ".. .. .. .. .. x! ,, ,, ,, ,, ,, x! .. .. {5 {{ {{ ^^ ^^ "
      ".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. .. {5 {{ {{ ^^ "
      ".. .. .. .. .. ,, ,, ,, ,, ,, ,, ,, .. .. .. .. {d {{ ^^ "
      ".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. .. {c {{ {{ ^^ "
      ".. .. .. .. .. x! ,, ,, ,, ,, ,, x! .. .. {4 {{ {{ ^^ ^^ "
      ".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. {c {{ ^^ ^^ ^^ "
      ".. .. .. .. .. xx xx xx ,, xx xx xx .. {c {{ {{ ^^ ^^ ^^ "
      ".. .. .. .. .. .. .. xx ,, xx .. .. {c {{ {{ ^^ ^^ ^^ ^^ "
      ".. .. .. {8 {8 {8 {8 w+ ,, w+ {8 {c {{ {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. {c {{ {{ {{ {{ xx ,, xx {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. {4 {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_eastpass
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bandit
        )
      ) ;; bind
    4 9)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            0
            7
            37
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-char
          nil
          "knight"
          sp_human
          oc_warrior
          s_human_knight
          2
          0 0 0
          0 0
          0 0
          11 140
          3 1
          #f ;; dead?
          'knight-conv
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_armor_plate)
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_iron_helm
            t_2h_sword
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'knight
          #f
          #t
          (list
            7
            8
          )
        )
      ) ;; bind
    7 8)
    (list
      (bind
        (kern-mk-char
          nil
          "squire"
          sp_human
          oc_warrior
          s_guard
          2
          0 0 0
          0 0
          0 0
          11 128
          3 1
          #f ;; dead?
          'knight-conv
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 2 t_bolt)
              (list 1 t_dagger)
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_bolt
                    )
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'squire
          #f
          #t
          (list
            9
            8
          )
        )
      ) ;; bind
    9 8)
    (list
      (bind
        (kern-mk-char
          nil
          "squire"
          sp_human
          oc_warrior
          s_guard
          2
          0 0 0
          0 0
          0 0
          11 128
          3 1
          #f ;; dead?
          'knight-conv
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 4 t_bolt)
              (list 1 t_dagger)
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_chain_coif
            t_crossbow
            t_armor_chain
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_bolt
                    )
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d2-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'squire
          #f
          #t
          (list
            7
            10
          )
        )
      ) ;; bind
    7 10)
    (list
      (bind
        (kern-mk-char
          nil
          "knight"
          sp_human
          oc_warrior
          s_human_knight
          2
          0 0 0
          0 0
          0 0
          11 128
          3 1
          #f ;; dead?
          'knight-conv
          nil
          'guard-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            (list
              (list 1 t_armor_plate)
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_iron_helm
            t_2h_sword
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d20"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'knight
          #f
          #t
          (list
            9
            10
          )
        )
      ) ;; bind
    9 10)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_westpass
          4
          14
        )
      ) ;; bind
    14 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    11 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #t
          4
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    5 9)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'knight
        )
      ) ;; bind
    7 8)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'knight
        )
      ) ;; bind
    9 10)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'squire
        )
      ) ;; bind
    7 10)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'squire
        )
      ) ;; bind
    9 8)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bandit
        )
      ) ;; bind
    3 8)
  ) ;; end of objects in p_eastpass
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_eastpass

(kern-mk-place 'p_westpass "Westpass"
  s_keep ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {a tt tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt || || || || || || "
      "^^ ^^ ^^ {{ bb && bb {{ ^^ ^^ {{ tt tt || || || || || || "
      "^^ ^^ {{ {3 .. .. .. {5 {{ ^^ {C ta tt tt || || || || || "
      "^^ ^^ {{ {2 .. .. .. .. {5 {{ bb t% tt tt tt || || || || "
      "^^ ^^ bb .. .. .. .. .. .. bb {A {2 ta tt tt tt || || || "
      "^^ ^^ ^5 bb {8 .. .. .. .. .. bb .. t% ta tt tt tt tt tt "
      "^^ ^^ ^^ ^^ {{ {a .. .. .. .. bb .. .. t% ta tt tt tt tt "
      "^^ ^^ ^^ ^^ bb {{ {2 .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ ^^ bb {A {2 .. .. /c /d /d /d /d /d /d /d /d /d "
      "^^ ^^ ^^ ^^ bb bb .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ {{ {{ {2 .. .. .. .. bb .. .. tC t3 tt tt tt tt "
      "^^ ^^ {{ {3 {1 .. .. .. {8 .. bb .. tC t3 tt tt tt tt tt "
      "^^ {{ {3 .. .. .. .. bb ^^ bb {& tC t3 tt tt tt || || || "
      "^^ {{ {2 .. .. .. {4 {{ ^^ {{ {{ t3 tt tt tt || || || || "
      "^^ {{ {a .. .. .. {c {{ ^^ ^^ {{ ta tt tt || || || || || "
      "^^ {{ {{ {a {8 {c {{ ^^ ^^ ^^ {{ {% tt || || || || || || "
      "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt || || || || || || "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_westpass
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            0
            7
            36
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-char
          nil
          "ranger"
          sp_human
          oc_warrior
          s_companion_ranger
          2
          0 0 0
          0 0
          0 0
          11 128
          3 1
          #f ;; dead?
          'ranger-conv
          nil
          'ranger-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 1 t_sword)
              (list 16 t_arrow)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d10"
                      't_arrow
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'ranger
          #f
          #t
          (list
            9
            11
          )
        )
      ) ;; bind
    9 11)
    (list
      (bind
        (kern-mk-char
          nil
          "ranger"
          sp_human
          oc_warrior
          s_companion_ranger
          2
          0 0 0
          0 0
          0 0
          13 512
          5 3
          #f ;; dead?
          'ranger-conv
          nil
          'ranger-ai
          (kern-mk-container
            t_chest
            ;; trap
            'burn
            ;; contents
            (list
              (list 1 t_sword)
              (list 18 t_arrow)
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d10"
                      't_arrow
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'ranger
          #f
          #t
          (list
            4
            12
          )
        )
      ) ;; bind
    4 12)
    (list
      (bind
        (kern-mk-char
          nil
          "ranger"
          sp_human
          oc_warrior
          s_companion_ranger
          2
          0 0 0
          0 0
          0 0
          10 152
          3 1
          #f ;; dead?
          'ranger-conv
          nil
          'ranger-ai
          (kern-mk-container
            t_chest
            ;; trap
            'spike-trap
            ;; contents
            (list
              (list 1 t_sword)
              (list 16 t_arrow)
              (list 1 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d10"
                      't_arrow
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'ranger
          #f
          #t
          (list
            9
            7
          )
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-char
          nil
          "ranger"
          sp_human
          oc_warrior
          s_companion_ranger
          2
          0 0 0
          0 0
          0 0
          11 128
          3 1
          #f ;; dead?
          'ranger-conv
          nil
          'ranger-ai
          (kern-mk-container
            t_chest
            ;; trap
            'spike-trap
            ;; contents
            (list
              (list 1 t_sword)
              (list 17 t_arrow)
              (list 2 t_heal_potion)
            )
            ;; hooks
            (list
            )
          )
          (list
            t_leather_helm
            t_bow
            t_armor_leather
          )
          ;; hooks
          (list
            (list
              ef_loot_drop
              (list
                'loot-drop-gob
                'drop-generic
                (list
                  (list
                    (list
                      100
                      "1d10"
                      't_gold_coins
                    )
                    (list
                      100
                      "1d10"
                      't_arrow
                    )
                    (list
                      30
                      "1d3"
                      't_food
                    )
                    (list
                      100
                      "1d3-1"
                      't_heal_potion
                    )
                  )
                )
              )
              2
              0
            )
          )
        )
        (list
          'npcg
          'ranger
          #f
          #t
          (list
            5
            4
          )
        )
      ) ;; bind
    5 4)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_eastpass
          14
          9
        )
      ) ;; bind
    4 14)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ranger
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ranger
        )
      ) ;; bind
    9 11)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ranger
        )
      ) ;; bind
    5 4)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ranger
        )
      ) ;; bind
    4 12)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-stalker
        )
      ) ;; bind
    11 8)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-hunter
        )
      ) ;; bind
    11 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'forest-goblin-stalker
        )
      ) ;; bind
    11 10)
  ) ;; end of objects in p_westpass
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_westpass

(kern-load "keep_crypt_mech.scm")

(kern-mk-place 'p_crypt "Crypt"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx ,, ,, ,, xx x! ,, ,, ,, x! xx ,, ,, ,, xx ,, xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, xx "
      "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx ?? xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx ,, xx xx xx .C .R .Y .P .T xx xx xx ,, xx xx xx "
      "xx .. .. ,, .. .. xx xx xx x! xx xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx xx ,, xx xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. x! xx xx ,, xx xx x! .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
      "xx .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. xx "
      "xx .. .. .. .. .. xx ,, ,, ,, ,, ,, xx .. .. .. .. .. xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_crypt
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    3 8)
    (list
      (kern-tag 'kc_pa
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            'kc_kcma
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    6 16)
    (list
      (kern-tag 'kc_pb
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            'kc_kcmb
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    12 16)
    (list
      (kern-tag 'kc_pc
        (bind
          (kern-mk-obj t_portcullis 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            '()
            #f
            '()
          )
        ) ;; bind
      ) ;; kern-tag
    9 13)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'kc_pb
          #f
          '()
        )
      ) ;; bind
    4 2)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'kc_pa
          #f
          '()
        )
      ) ;; bind
    14 2)
    (list
      (bind
        (kern-mk-obj t_lever 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          'kc_pc
          #f
          '()
        )
      ) ;; bind
    17 1)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 9)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 10)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 11)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 12)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 13)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 14)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 15)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 16)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 16)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 16)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 16)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    1 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    2 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    3 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    4 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    5 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    13 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    14 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    15 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    16 17)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          '()
        )
      ) ;; bind
    17 17)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (kern-tag 'kc_kcma
        (bind
          (kern-mk-obj t_kcm 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            (list
              1
              9
              5
              9
            )
          )
        ) ;; bind
      ) ;; kern-tag
    0 0)
    (list
      (kern-tag 'kc_kcmb
        (bind
          (kern-mk-obj t_kcm 1
            ;; hooks
            (list
            )
          )
          (list
            #f
            (list
              13
              9
              5
              9
            )
          )
        ) ;; bind
      ) ;; kern-tag
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_green_tower_lower
          17
          1
        )
      ) ;; bind
    9 3)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_lich_tomb
          9
          16
        )
      ) ;; bind
    9 10)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #t
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    12 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    6 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    12 7)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    15 8)
  ) ;; end of objects in p_crypt
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_crypt

(kern-mk-place 'p_ancient_derelict "Ancient Derelict"
  s_void_ship ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ "
      "^^ {{ {{ bb {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {3 {5 {{ {{ ^^ "
      "{{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {3 #> bb {5 {{ ^^ "
      "{{ {{ {{ {3 {1 {5 {{ {{ {{ ^^ ^^ ^^ {{ {2 ee .. {4 {{ ^^ "
      "{{ {{ {3 .. .. #> #> #> {{ ^^ ^^ #> {{ {2 .. ee #> {{ ^^ "
      "{1 {1 .. .. .. .. ee ee {{ {{ ^^ {{ {3 .. .. {4 {{ ^^ ^^ "
      ".. .. #> .. ee ee ee ee ee {{ {{ {{ ee ee .. #> ^^ ^^ ^^ "
      ".. .. bb ee ee ee ee ee ee ee {1 ee ee ee ee #> {1 {5 ^^ "
      ".. .. #> .. ee ee ee ee ee ee ee ee ee ee .. ee #> .. {1 "
      ".. .. .. .. .. ee ee ee ee ee ee .. .. #> #> .. #> .. .. "
      "{8 {8 bb .. #> #> #> #> #> .. #> #> .. .. .. .. .. bb .. "
      "{{ {{ {a .. .. .. .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "{{ {{ {{ {a {8 {8 .. .. .. .. .. .. .. {8 {8 {8 bb .. .. "
      "^^ {{ bb {{ {{ {{ {a {8 .. .. .. .. {c {{ {{ {{ {{ {a {8 "
      "^^ ^^ {{ {{ {{ {{ {{ {{ {2 .. .. {c {{ {{ {{ {{ {{ {{ {{ "
      "^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {2 .. {4 {{ ^^ ^^ ^^ {{ {{ {{ {{ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {2 .. {4 {{ ^^ ^^ ^^ ^^ ^^ {{ {{ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_ancient_derelict
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    7 9)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'wisp
        )
      ) ;; bind
    10 9)
    (list
      (kern-mk-obj t_power_core 1
        ;; hooks
        (list
        )
      )
    12 5)
    (list
      (bind
        (kern-mk-obj t_corpse 1
          ;; hooks
          (list
          )
        )
        (list
          (list
            (list
              1
              't_staff
            )
            (list
              1
              't_vas_rel_por_scroll
            )
          )
        )
      ) ;; bind
    9 10)
  ) ;; end of objects in p_ancient_derelict
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_ancient_derelict

(kern-load "joel.scm")

(kern-load "r2a_mech.scm")

(kern-mk-place 'p_road_to_absalot_3 "Passage to Absalot"
  nil ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr {C !! !! !c {{ {{ {{ {{ {{ !a !! !! {A rr rr rr "
      "rr rr {C !3 !! !c {# {{ {{ {{ {{ {{ {{ {% !! !5 {A rr rr "
      "rr {C !3 !! !c {# {{ {{ bb xx xx {{ {{ {{ !a !! !5 {A rr "
      "rr !! !! !c {# {{ {{ xx rr rr xx xx {{ {{ {% !a !! !! rr "
      "rr !! !! {# {{ {{ xx xx xx xx xx xx xx {{ {{ {% !! !! rr "
      "rr !! !! {{ {{ xx xx xx ,, ,, ,, bb rr xx {{ {{ !! !! rr "
      "rr !! !! {{ {{ rr xx xx ,, ,, ,, rr xx xx {{ {{ !! !! rr "
      "rr !! !! {{ {{ bb rr xx ,, ,, ,, xx xx xx {{ {{ !! !! rr "
      "rr !! !! {A {{ {{ xx xx xx ,, xx xx xx {{ {{ {C !! !! rr "
      "rr !! !! !5 {A {{ {{ xx xx ,, xx xx {{ {{ {C !3 !! !! rr "
      "rr !! !! !! !5 {A {{ {{ xx ,, xx {{ {{ {C !3 !! !! !! rr "
      "rr {% !! !! !! !5 {A {{ ,, ,, ,, {{ !3 !! !! !! !! {# ~r "
      "rr {{ !a !! !! !! !5 {{ {2 ,, ,, {{ !! !! !! !! !c {{ rr "
      "rr {{ {% !a !! !! !! {{ ,, ,, ,, {{ !! !! !! !c {# {{ rr "
      "rr {{ {{ {% !a !! xx rr ,, ,, .. xx xx !! !c {# {{ {{ rr "
      "rr rr {{ {{ {% !a xx ,, ,, ,, ,, ,, xx !c {# {{ {{ {{ rr "
      "rr rr rr {{ {{ {{ rr bb ,, ,, ,, ,, ?? {{ {{ {{ {{ rr rr "
      "rr rr rr rr rr xx xx xx ,, ,, ,, xx xx xx rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  (list
    (list
      (kern-mk-place 'p_fire_bridge "Fire Bridge"
        nil ;; sprite
        (kern-mk-map
          nil           19 19 pal_expanded
          (list
            "xx xx xx rr rr xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
            "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr "
            "xx ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr "
            "rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr "
            "rr ,, ,, ,, ,, xx rr xx ,, w+ ,, xx xx xx ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, xx !_ !_ ,, ,, !! !_ !_ xx ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, w+ !! !_ !_ !_ !_ !_ !! w+ ,, ,, ,, ,, xx "
            "rr ,, ,, ,, ,, xx !! !! !_ !_ !_ !! !! rr ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, xx xx !! !! !! !! !! xx xx ,, .. ,, ,, xx "
            "xx ,, ,, ,, ,, ,, w+ !! ,, ,, !! !! w+ ,, ,, ,, ,, ,, rr "
            "xx ,, ,, ,, ,, ,, w+ !! ,, ,, bb !! w+ ,, ,, ,, ,, ,, xx "
            "xx ,, ,, ,, ,, ,, w+ !! ,, ,, ,, !! w+ ,, ,, ,, ,, ,, xx "
            "xx xx ,, ,, ,, xx rr !! !! ,, ,, !! xx xx ,, ,, ,, xx xx "
            "!! xx bb w+ w+ xx !! !! ,, ,, ,, !! !! xx w+ w+ w+ xx !! "
            "!! !! ,, !! !! !! !! !! ,, ,, !! !! !! !! !! !! !! !! !! "
            "!! !! ,, !! !! !! !c {& ,, ,, {4 {% !a !! !! !! !! !! !! "
            "rr {{ {% !a !! !c {# {{ ,, ,, ,, {{ {% !a !! !c {# {{ rr "
            "rr {{ {{ {{ {{ {{ {{ {{ {2 ,, ,, {{ {{ {{ {{ {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr ,, ,, ,, rr rr rr rr rr rr rr rr "
          )
        )
        #f #t #f #f
        ;; subplaces
        nil
        (list
          (list
            (kern-mk-place 'p_road_to_absalot_1 "Passage to Absalot"
              nil ;; sprite
              (kern-mk-map
                nil                 19 19 pal_expanded
                (list
                  "rr rr rr rr !! rr rr rr ,, ,, ,, rr rr rr rr !! rr rr rr "
                  "rr rr rr rr !! rr rr {{ ,, .. ,, {{ {{ {{ rr !! rr rr rr "
                  "rr rr rr {{ !! {A {{ {C ,, ,, ,, {{ !3 !! !! !! {{ rr rr "
                  "rr {{ {{ {C !! !! !! !! ,, ,, ,, {{ !! bb {F !! {{ rr rr "
                  "rr {{ !3 !! !! {& bb !! ,, ,, {4 {{ !! {# bb !! {{ rr rr "
                  "rr {{ !! bb !! {{ {% !! ,, ,, ,, {{ !e {{ {% !! {{ rr rr "
                  "rr {{ !e {& !! {{ {{ !e .. ,, ,, {A {{ {{ {{ !! {{ {{ ~r "
                  "rr {{ {{ {{ !! {{ {{ pp ,, ,, ,, pp {{ {{ {C !! {A {{ rr "
                  "rr rr {{ {C !! {A {C ,, ,, ,, ,, ,, {{ !3 !! !! !5 {{ rr "
                  "rr rr {{ !3 !! !! !! ,, ,, ,, ,, ,, {{ !! bb bb !! {{ rr "
                  "rr rr {{ !! bb bb !! ,, ,, ,, ,, ,, {{ !! bb {& !e {{ rr "
                  "rr rr {{ !! {& bb !! pp {8 ,, ,, bb {{ !! {# {{ {{ {{ rr "
                  "rr {{ {{ !! {{ {% !! {# {{ {{ {{ {{ {{ !! {{ {{ {{ {{ rr "
                  "rr {{ {{ !! {{ {{ !e {{ {{ rr {{ {{ {{ !! {{ {{ {{ {{ rr "
                  "rr {{ {{ !e {{ {{ {{ {{ rr rr {{ {{ {{ !! {{ {{ {{ {{ rr "
                  "rr rr {{ {{ {{ rr {{ {{ rr rr rr {{ {{ !e {{ {{ rr bb rr "
                  "rr rr rr {{ rr rr {{ rr rr rr rr rr {{ {{ {{ rr rr rr rr "
                  "rr rr rr {{ rr rr rr rr rr rr rr rr rr {{ rr rr rr rr rr "
                  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
                )
              )
              #f #t #f #f
              ;; subplaces
              nil
              nil ;; neighbors
              (list ;; objects in p_road_to_absalot_1
                (list
                  (bind
                    (kern-mk-obj t_trap_door 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_absalot_passage
                      1
                      38
                    )
                  ) ;; bind
                18 6)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'troll
                    )
                  ) ;; bind
                4 12)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'troll
                    )
                  ) ;; bind
                13 4)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'headless
                    )
                  ) ;; bind
                9 4)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'headless
                    )
                  ) ;; bind
                5 4)
                (list
                  (bind
                    (kern-mk-obj t_spawn_pt 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'spawn-pt
                      'gazer
                    )
                  ) ;; bind
                3 17)
                (list
                  (bind
                    (kern-mk-obj t_monman 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'monman
                      (list
                        0
                        0
                        0
                        -1
                        6
                        -1
                      )
                    )
                  ) ;; bind
                0 0)
                (list
                  (bind
                    (kern-mk-obj t_ladder_up 1
                      ;; hooks
                      (list
                      )
                    )
                    (list
                      'p_gate_to_absalot
                      9
                      1
                    )
                  ) ;; bind
                9 9)
              ) ;; end of objects in p_road_to_absalot_1
              (list ;; on-entry-hooks
                'on-entry-to-dungeon-room
              )
              (list ;; edge entrances
                (list 0 18 18) ;; Northwest
                (list 1 9 18) ;; North
                (list 2 0 18) ;; Northeast
                (list 3 18 9) ;; West
                (list 4 9 9) ;; Here
                (list 5 0 9) ;; East
                (list 6 18 0) ;; Southwest
                (list 7 9 0) ;; South
                (list 8 0 0) ;; SoutheastUp
              )
            ) ;; end of place p_road_to_absalot_1

          7)
        ) ;; end neighbors of p_fire_bridge
        (list ;; objects in p_fire_bridge
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'ghast
              )
            ) ;; bind
          14 9)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'craven-archer
              )
            ) ;; bind
          13 9)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'death-knight
              )
            ) ;; bind
          8 3)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'death-knight
              )
            ) ;; bind
          10 3)
          (list
            (bind
              (kern-mk-obj t_spawn_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'demon
              )
            ) ;; bind
          16 2)
          (list
            (bind
              (kern-mk-obj t_monman 1
                ;; hooks
                (list
                )
              )
              (list
                'monman
                (list
                  0
                  0
                  0
                  -1
                  6
                  -1
                )
              )
            ) ;; bind
          0 0)
          (list
            (kern-tag 'fb-p2
              (bind
                (kern-mk-obj t_portcullis 1
                  ;; hooks
                  (list
                  )
                )
                (list
                  #f
                  '()
                  #f
                  '()
                )
              ) ;; bind
            ) ;; kern-tag
          8 4)
          (list
            (kern-tag 'fb-p1
              (bind
                (kern-mk-obj t_portcullis 1
                  ;; hooks
                  (list
                  )
                )
                (list
                  #f
                  'fb-p2
                  #f
                  '()
                )
              ) ;; bind
            ) ;; kern-tag
          10 4)
          (list
            (kern-tag 'fb-b1
              (bind
                (kern-mk-obj t_terrain_blitter 1
                  ;; hooks
                  (list
                  )
                )
                (list
                  'p_fire_bridge
                  8
                  6
                  3
                  3
                  'm_deck_section
                )
              ) ;; bind
            ) ;; kern-tag
          1 1)
          (list
            (bind
              (kern-mk-obj t_lever 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                'fb-p1
                #f
                '()
              )
            ) ;; bind
          3 10)
          (list
            (bind
              (kern-mk-obj t_lever 1
                ;; hooks
                (list
                )
              )
              (list
                #f
                'fb-b1
                #f
                '()
              )
            ) ;; bind
          15 10)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'craven-archer
              )
            ) ;; bind
          5 10)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'ghast
              )
            ) ;; bind
          3 11)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'skeletal-spear-thrower
              )
            ) ;; bind
          5 9)
          (list
            (bind
              (kern-mk-obj t_guard_pt 1
                ;; hooks
                (list
                )
              )
              (list
                'spawn-pt
                'craven-archer
              )
            ) ;; bind
          13 11)
        ) ;; end of objects in p_fire_bridge
        (list ;; on-entry-hooks
          'on-entry-to-dungeon-room
        )
        (list ;; edge entrances
          (list 0 18 18) ;; Northwest
          (list 1 9 18) ;; North
          (list 2 0 18) ;; Northeast
          (list 3 18 9) ;; West
          (list 4 9 9) ;; Here
          (list 5 0 9) ;; East
          (list 6 18 0) ;; Southwest
          (list 7 9 0) ;; South
          (list 8 0 0) ;; SoutheastUp
        )
      ) ;; end of place p_fire_bridge

    7)
  ) ;; end neighbors of p_road_to_absalot_3
  (list ;; objects in p_road_to_absalot_3
    (list
      (bind
        (kern-mk-obj t_ladder_up 1
          ;; hooks
          (list
          )
        )
        (list
          'p_tower_of_absalot
          9
          9
        )
      ) ;; bind
    9 7)
    (list
      (bind
        (kern-mk-obj t_trap_door 1
          ;; hooks
          (list
          )
        )
        (list
          'p_absalot_passage
          1
          2
        )
      ) ;; bind
    18 12)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ghast
        )
      ) ;; bind
    10 8)
    (list
      (bind
        (kern-mk-obj t_guard_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'death-knight
        )
      ) ;; bind
    8 8)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'craven-archer
        )
      ) ;; bind
    10 6)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'zorn
        )
      ) ;; bind
    8 6)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #t
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    9 10)
  ) ;; end of objects in p_road_to_absalot_3
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_road_to_absalot_3

p_fire_bridge
p_road_to_absalot_1
(kern-mk-place 'p_absalot_passage "Secret Passage"
  nil ;; sprite
  (kern-mk-map
    nil     19 40 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      ".. {8 .. .. {c {{ rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr {{ {2 rr {{ {3 rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. bb rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr .. {c {{ rr rr rr rr rr rr rr rr *7 rr rr "
      "rr rr rr rr rr rr {{ {3 rr rr rr *7 rr rr rr *3 vv *5 rr "
      "rr rr rr rr rr rr rr .. rr rr rr vv *5 rr rr vv vv vv rr "
      "rr rr rr rr rr rr rr .. rr rr *3 vv vv vv vv vv vv vv vv "
      "rr rr rr rr rr rr {{ {2 rr rr *2 vv vv vv vv vv vv vv vv "
      "rr rr rr rr rr rr {1 bb rr rr vv vv vv vv vv vv vv vv vv "
      "rr rr rr rr rr rr .. rr rr *3 vv vv vv vv vv *c rr vv vv "
      "rr rr rr rr rr {{ .. rr *3 vv vv vv vv vv *c rr rr *2 vv "
      "rr rr rr rr rr .. *3 vv vv vv vv vv vv vv rr rr rr *2 vv "
      "rr !! !! !! rr .. *2 vv vv vv vv vv vv vv rr rr bb vv vv "
      "!! !! !! !! !! !! ** vv vv vv vv vv vv vv *5 rr *3 vv vv "
      "!! !_ !_ !_ !_ !_ *. vv vv vv vv vv vv vv vv vv vv vv vv "
      "!_ !_ !_ !_ +s !! ** vv vv vv vv vv vv vv vv vv vv vv vv "
      "!_ !_ !! !_ !_ !_ ** vv vv vv vv vv vv vv vv vv vv vv vv "
      "!! !! !! !! !! !! *. vv vv vv vv vv vv vv vv vv vv vv vv "
      "!! !! rr rr rr {c {h vv vv vv vv vv vv vv vv vv vv vv vv "
      "rr rr rr rr rr {{ .. *2 vv vv vv vv vv vv vv vv vv vv vv "
      "rr rr rr rr rr rr .. *2 vv vv vv vv vv *c rr vv vv vv vv "
      "rr rr rr rr rr {{ .l vv vv vv vv vv vv bb rr *a vv vv vv "
      "rr rr rr rr rr .. *3 vv vv vv vv vv vv rr rr rr vv vv vv "
      "rr rr rr rr rr .. *2 vv vv vv vv vv vv *5 rr *3 vv vv vv "
      "rr rr rr rr rr {{ *2 vv vv vv vv vv vv vv vv vv vv vv vv "
      "rr rr rr rr {{ .. *a vv vv vv vv vv vv vv vv vv vv vv vv "
      "rr rr rr rr {1 {8 rr rr *a vv vv vv vv vv vv vv vv vv vv "
      "rr rr rr {{ {6 {{ rr rr rr *a vv vv vv vv vv vv vv vv vv "
      "rr rr rr {{ {2 rr rr rr rr rr vv vv vv vv vv vv *c rr rr "
      "rr rr rr {1 {8 rr rr rr rr rr *2 vv *c rr vv *c rr rr rr "
      "rr rr rr {4 {{ rr rr rr rr rr *e rr rr rr *e rr rr rr rr "
      "rr rr rr bb {1 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr {{ {2 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr {{ {2 rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr {4 {{ {3 rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      ".. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
    )
  )
  #f #t #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_absalot_passage
    (list
      (bind
        (kern-mk-obj t_trap_door 1
          ;; hooks
          (list
          )
        )
        (list
          'p_road_to_absalot_3
          17
          12
        )
      ) ;; bind
    0 2)
    (list
      (bind
        (kern-mk-obj t_trap_door 1
          ;; hooks
          (list
          )
        )
        (list
          'p_road_to_absalot_1
          17
          6
        )
      ) ;; bind
    0 38)
    (list
      (bind
        (kern-mk-char
          'ch_r2a_statue
          "Statue"
          sp_statue
          nil
          s_statue
          2
          0 0 0
          0 0
          0 0
          189 0
          9 9
          #f ;; dead?
          'r2a-statue-conv
          nil
          'ankh-ai
          nil ;; inventory
          nil
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        '()
      ) ;; bind
    4 17)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    15 22)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    10 10)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    10 30)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    16 30)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'bat
        )
      ) ;; bind
    16 15)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
  ) ;; end of objects in p_absalot_passage
  (list ;; on-entry-hooks
    'fix-lava
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 39) ;; Northwest
    (list 1 9 39) ;; North
    (list 2 0 39) ;; Northeast
    (list 3 18 20) ;; West
    (list 4 9 20) ;; Here
    (list 5 0 20) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_absalot_passage

(kern-mk-place 'p_gate_to_absalot "Gate To Absalot"
  s_keep ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ xx xx xx xx xx ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx xx xx xx ,, ,, ,, xx xx xx xx rr ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx .. ,, ,, xx ,, ,, ,, bb ^^ ^^ ^^ "
      "^^ ^^ ^^ xx bb ,, ,, ,, ,, ,, ,, ,, ,, .. ,, rr ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, .. ,, xx ,, ,, ,, xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx xx bb xx rr ,, ,, bb xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, bb xx bb bb bb xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, w+ .. bb .. w+ ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx .. .. .. xx bb ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx w+ xx rr .. .. .. xx xx w+ xx xx ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {a .. .. .. .. .. .. .. .. .. {c {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ {2 .. .. .. .. .. .. .. {4 {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ {2 .. .. .. && .. .. .. {4 {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ {a .. .. .. .. .. .. .. {c {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ {a .. .. .. .. .. {c {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. .. .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. .. .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ {3 .. .. .. .. .. {5 {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ {3 .. .. .. .. .. .. .. {5 {{ {{ ^^ ^^ ^^ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_gate_to_absalot
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_road_to_absalot_1
          9
          9
        )
      ) ;; bind
    9 1)
    (list
      (bind
        (kern-mk-char
          'ch_joel
          "Joel"
          sp_human
          nil
          s_companion_shepherd
          2
          0 0 0
          0 0
          0 0
          13 0
          5 3
          #f ;; dead?
          'joel-conv
          sch_joel
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    8 9)
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          28 1024
          4 4
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    12 12)
    (list
      (bind
        (kern-mk-char
          nil
          "bull"
          sp_bull
          nil
          s_bull
          0
          0 0 0
          0 0
          0 0
          28 1024
          4 4
          #f ;; dead?
          nil
          nil
          'animal-ai
          (kern-mk-container
            t_chest
            ;; trap
            nil
            ;; contents
            nil
            ;; hooks
            (list
            )
          )
          nil
          ;; hooks
          (list
          )
        )
        (list
          'npcg
          'bull
          #f
          #f
          '()
        )
      ) ;; bind
    10 15)
  ) ;; end of objects in p_gate_to_absalot
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_gate_to_absalot

(kern-mk-place 'p_tower_of_absalot "Tower of Absalot"
  s_keep ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "^^ tt tt tc %% te bb %% .. .. ta tc bb ta tc %% %% %c ^^ "
      "^^ ta tt %b ~f %% %% %% %% =| %d bb %b %% %% %% ~f ^3 ^^ "
      "^^ ^5 tt t5 %e t7 bb %e .. .. .. %f bb t7 %a ~f ^b ^^ ^^ "
      "^^ ^^ tt tt tt tt td bb .. .. .. bb tb tt t5 %a ~f ^a ^^ "
      "^^ ^^ tt tt tc xx w+ xx rr .. xx xx bb xx ta tt tt t5 ^^ "
      "^^ ^^ tt tt xx xx ,, ,, w+ .. w+ ,, ,, xx xx tt tt tt ^^ "
      "^^ ^^ tt tt w+ ,, ,, ,, rr d, xx ,, ,, ,, w+ tt tt tt ^^ "
      "^^ ^^ ta tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt tc ^^ "
      "^^ ^^ ^5 tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt ^3 ^^ "
      "^^ ^^ ^^ tt bb .. ,, ,, ,, /c ,, ,, ,, ,, w+ tt tt ^^ ^^ "
      "^^ ^^ ^^ tt t5 .. ,, ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
      "^^ ^^ ^c tt tc .. .. ,, ,, ,, ,, ,, ,, ,, rr tt tt ^^ ^^ "
      "^^ ^^ t3 tt bb .. ,, ,, ,, ,, ,, ,, ,, ,, w+ tt tt ^^ ^^ "
      "^^ ^^ ta tt rr rr ,, ,, ,, ,, ,, ,, ,, xx xx tt tc ^^ ^^ "
      "^^ ^^ ^5 tt t5 rr w+ xx xx w+ xx xx w+ xx t3 tt ^3 ^^ ^^ "
      "^^ ^^ ^^ ta tt tt tt tt tt tt tt tt tt tt tt tc ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^5 ta tt tt tt tt tt tt tt tt tc ^3 ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_tower_of_absalot
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'death-knight
        )
      ) ;; bind
    6 7)
    (list
      (bind
        (kern-mk-obj t_spawn_pt 1
          ;; hooks
          (list
          )
        )
        (list
          'spawn-pt
          'ghast
        )
      ) ;; bind
    12 11)
    (list
      (bind
        (kern-mk-obj t_monman 1
          ;; hooks
          (list
          )
        )
        (list
          'monman
          (list
            0
            0
            0
            -1
            6
            -1
          )
        )
      ) ;; bind
    0 0)
    (list
      (bind
        (kern-mk-obj t_ladder_down 1
          ;; hooks
          (list
          )
        )
        (list
          'p_road_to_absalot_3
          9
          7
        )
      ) ;; bind
    9 9)
  ) ;; end of objects in p_tower_of_absalot
  (list ;; on-entry-hooks
    'on-entry-to-dungeon-room
  )
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 9 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 9) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 9) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 9 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_tower_of_absalot

(kern-load "mesmeme.scm")

(kern-load "jake.scm")

(kern-load "slywan.scm")

(kern-load "tooth.scm")

(kern-load "tetzl.scm")

(kern-mk-place 'p_kun "Kun"
  s_town ;; sprite
  (kern-mk-map
    nil     19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx {{ {2 .. {4 {{ ^^ ^^ ^^ "
      "xx [[ .C .A .N .T .I .N .A ]] xx {{ {2 .. {4 {{ {{ ^^ ^^ "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ {2 .. {4 {{ {{ {{ ^^ "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ {2 .. {4 {{ ^^ ^^ ^^ "
      "xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ {2 .. {4 {{ ^^ ^^ {{ "
      "xx ,, ,, 00 ,, ,, ,, 00 ,, ,, xx {{ {2 .. {4 {{ {{ {{ {{ "
      "xx ,, ,, 00 00 00 00 00 ,, ,, xx {{ {2 .. .. {1 {1 {1 {1 "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ws {{ {2 .. .. .. .. .. .. "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx {{ {2 .. .. .. {8 .. {8 "
      "xx xx ws xx xx ,, sT xx ws xx xx {{ {2 .. .. bb {{ bb {{ "
      ".. .. .. .. .. .. .. .. .. .. .. {1 .. .. {4 {{ {{ {{ ^^ "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb {{ ^^ ^^ "
      "{8 .. {8 .. .. .. .. {8 .. .. .. .. .. .. {4 {{ ^^ ^^ ^^ "
      "{{ bb {{ bb .. .. {4 {{ xx xx xx xx sE ,, xx xx xx xx xx "
      "{{ {{ {{ {{ {2 .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ {{ {{ bb .. .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ ^^ {{ {{ {2 .. {4 {{ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "^^ ^^ ^^ {{ {2 .. {4 {{ xx .D .U .T .Y @@ .F .R .E .E xx "
      "^^ ^^ ^^ {{ {2 .. {4 {{ xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  nil ;; neighbors
  (list ;; objects in p_kun
    (list
      (bind
        (kern-mk-char
          'ch_tooth
          "Tooth"
          sp_rat
          oc_wrogue
          s_rat
          2
          0 0 0
          0 0
          0 1
          8 256
          4 2
          #f ;; dead?
          'tooth-conv
          sch_tooth
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    6 7)
    (list
      (bind
        (kern-mk-char
          'ch_tetzl
          "Tetzl"
          sp_spider
          oc_wright
          s_spider
          2
          0 0 0
          0 0
          0 0
          10 256
          4 2
          #f ;; dead?
          nil
          sch_tetzl
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    3 3)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_open_door_in_rock
              #f
              0
            )
            (list
              's_locked_windowed_wood_door_in_rock
              #f
              5
            )
            (list
              's_magically_locked_windowed_wood_door_in_rock
              #f
              5
            )
          )
        )
      ) ;; bind
    5 9)
    (list
      (bind
        (kern-mk-obj t_door 1
          ;; hooks
          (list
          )
        )
        (list
          #f
          0
          '()
          #f
          #f
          #f
          (list
            (list
              's_closed_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_open_door_in_stone
              #f
              0
            )
            (list
              's_locked_solid_wood_door_in_stone
              #t
              5
            )
            (list
              's_magically_locked_solid_wood_door_in_stone
              #t
              5
            )
          )
        )
      ) ;; bind
    13 13)
    (list
      (bind
        (kern-mk-char
          'ch_mesmeme
          "Mesmeme"
          sp_gazer
          oc_wizard
          s_gazer
          2
          0 0 0
          12 2
          0 1
          28 256
          18 2
          #f ;; dead?
          'mesmeme-conv
          sch_mesmeme
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        '()
      ) ;; bind
    8 4)
    (list
      (bind
        (kern-mk-char
          'ch_jake
          "Jake&Percival"
          sp_gint
          nil
          s_ettin
          2
          0 0 0
          0 0
          0 0
          60 256
          4 2
          #f ;; dead?
          'jake-conv
          sch_jake
          nil
          nil ;; inventory
          nil
          ;; hooks
          (list
          )
        )
        (list
          #t
        )
      ) ;; bind
    4 5)
    (list
      (bind
        (kern-char-force-drop          (kern-mk-char
            'ch_slywan
            "Slywan"
            sp_human
            oc_wrogue
            s_brigand
            2
            0 0 0
            0 0
            0 1
            12 256
            6 2
            #f ;; dead?
            'slywan-conv
            sch_slywan
            nil
            (kern-mk-container
              t_chest
              ;; trap
              nil
              ;; contents
              nil
              ;; hooks
              (list
              )
            )
            nil
            ;; hooks
            (list
            )
          )
        #t) ;; kern-char-force-drop
        '()
      ) ;; bind
    3 7)
  ) ;; end of objects in p_kun
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 18 18) ;; Northwest
    (list 1 5 18) ;; North
    (list 2 0 18) ;; Northeast
    (list 3 18 7) ;; West
    (list 4 9 9) ;; Here
    (list 5 0 11) ;; East
    (list 6 18 0) ;; Southwest
    (list 7 13 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_kun

(kern-load "raise-merciful-death.scm")

(kern-mk-place 'p_shard "The Shard Surface"
  nil ;; sprite
  (kern-mk-composite-map
    nil     4 3
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "*. *. *. *. ** ** *. *. ** *. *. *. *. ** *. ** ** ** *. *. *. ** ** *. *. ** *. *. ** ** ** ** "
        "** ** ** ** ** ** *. *. ** ** ** ** ** *. ** *. ** ** *. *. *. *. ** ** ** ** *. ** ** *. ** ** "
        "** *. *. *. *. *. *. *. *. *. *. ** ** ** ** *. ** ** ** ** ** ** ** ** ** ** *. ** *. *. ** ** "
        "*. ** *. *. *. ** ** ** *. ** *. ** ** ** ** *. ** *. ** ** ** *. *. ** *. ** ** ** *. *. *. *. "
        "*. *. *. ** ** *. *. *. *. ** ** ** ** ** ** *. ** ** ** ** *. ** ** *. ** *. *. ** ** ** *. ** "
        "** *. ** *. ** ** ** *. ** ** ** ** *. *8 *8 *8 *8 *8 *8 *8 *8 *8 *8 ** ** ** ** ** ** ** ** *. "
        "** *. *. ** ** ** *. *. ** *. *. *c ^g ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h *a *. ** ** *. *. ** ** "
        "** *. ** ** ** *. ** ** *. ** ^g ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h *a ** *. *. *. ** *. "
        "*. ** ** *. *. *. *. *. *. *4 ^^ ^^ ^^ .! ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h *a *8 *. ** ** *. "
        "*. ** ** ** ** ** ** ** *. *c ^^ ^c tb tt td ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h *a ** ** "
        "** ** ** *. ** ** *. *. ** ^g ^^ %b %5 tt %b %d ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h *. ** "
        "*. *. ** ** *. *. ** *. *4 ^^ ^^ t7 %% ta tt tt tt t5 ^a ^^ ^^ ^^ ^^ ^^ ^c %3 %% %5 ^a ^^ *a ** "
        "*. ** *. ** *. ** ** *. *c ^^ ^^ tt %% %5 ta tt tt tt tt tt tt tt tt tt td %% -7 %% %5 ^^ ^h *a "
        "** *. *. ** ** *. ** *. ^g ^^ ^c te %% %c ^3 ^^ ^d tt tt tc %3 %% %5 te %3 -b -- -d %% ^^ ^^ ^^ "
        "*. *. *. *. ** ** ** *4 ^^ ^^ t7 %3 %c ^3 ^^ ^c %7 ta tt %3 %c ^7 %a %% %% %% -e %% %c ^^ ^^ ^^ "
        "*. ** *. *. *. ** ** *4 ^^ ^^ tt %% ^b ^^ ^c %3 %% %d tt %% ^b ^^ ^d %% t7 %a %% %c {B {{ ^^ tf "
        "** ** *. ** *. *. ** *4 ^^ ^^ te %a %% %% %% %% %% ^f tt %a %5 ^e %3 %c tt tt tt td %7 {E t7 {E "
        "*. *. ** *. ** ** ** *4 ^^ ^^ ^5 t7 ^f %% %% %% %% tb tt t5 %a %% %c t3 tt tt tt %3 %% %d tt %3 "
        "** *. ** *. ** ** ** ** ^j ^^ ^^ te %3 %c ^3 ^5 %% %5 ta tt tt tt tt tc t& tt tt %a %c t3 tt %% "
        "*. *. ** ** ** ** *. *. *5 ^^ ^^ ^5 %% ^b ^^ ^^ %% %% %5 t% ta tt tt %3 %5 ta tt tt tt tt tc %a "
        "*. ** ** ** ** ** ** *. *4 ^^ ^^ ^c %% %5 ^^ ^c %% %% %% ^7 t% tt tc %% %% %5 t% tt {# {{ {% t3 "
        "** *. *. ** *. ** ** *. *4 ^^ ^^ %3 %% %% ^^ %3 %% %% %c {1 tC tt %3 %c ^7 %a %5 tt {{ {{ {{ tt "
        "*. ** ** *. *. ** *. ** *4 ^^ ^^ %% ^f %% ^^ %a %% %% tb tt tt tt %% ^b ^^ ^d %% te {A {{ {C te "
        "** *. ** ** *. *. ** *. *4 ^^ ^^ %a %% %% ^^ ^5 %% %% %% %5 ta tt %a %d ^e %3 %% %5 tb tt td %3 "
        "** ** ** ** *. ** *. *. ** ^j ^^ ^5 %a %% ^a ^c %% %% -7 %% %5 tt tt t5 %b %% %% %% %5 te %3 %% "
        "*. ** *. *. *. *. *. *. *. *5 ^j ^^ ^5 %a %% %% %% -b -- -d %% tt ^f tt tt tt t5 %% %% %% %% -b "
        "*. ** *. ** *. ** *. *. *. *. *5 ^^ ^^ ^^ ^5 %a %% %% -e %% %c tt tt tt tt tt tt %% %c ^7 %a %% "
        "** *. ** ** ** ** *. *. *. *. *. ^j ^^ ^^ ^^ ^5 t7 %a %% %c t3 tc %3 %% %5 ta tt %e ^b ^^ ^d %a "
        "** ** *. *. *. ** *. *. *. ** *. ** *1 *5 ^^ ^^ ta tt tt tt tt %3 %% -7 %% %5 tt t5 %7 ^e %7 t3 "
        "** *. *. *. ** ** *. *. *. ** *. ** *. *4 ^^ ^^ {2 .. .. t% tt %% -b -- -d %% tt tt %a %% %c tt "
        "*. *. ** *. ** ** ** *. *. *. ** ** *. *4 ^^ ^^ {a .. .. .. tt %a %% -e %% %c tt tt tt tt tt tc "
        "** *. ** ** *. *. ** *. *. *. *. *. *. *4 ^^ ^^ {{ {a {8 .. ta t5 %a %% %c t3 tc t# {8 {8 .. .. "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "** *. ** ** ** ** ** *. *. ** ** ** *. *. ** *. *. *. ** ** ** *. ** *. *. *. *. ** *. *. *. *. "
        "** *. ** *. ** *. ** *. *. ** ** ** ** ** *. ** ** *. *. *. *. *. ** *. ** ** *. *. *. ** *. *. "
        "** ** *. *. *. *. ** *. *. ** ** *. *. ** ** ** *. *. *. *. *. ** *. ** ** ** ** *. ** *. *. *. "
        "** *. ** ** *. *. *. *. ** ** *. *. *. ** *. *. *. *8 *8 *8 ** ** ** *. *. *. ** *. ** ** ** ** "
        "** *. *. ** ** *. *. ** ** ** ** *. *. ** ** *c ^g ^^ .. ^^ ^h *a ** ** ** ** ** *. *. ** *. *. "
        "** ** ** *. *. *. *. *. ** ** ** ** *. *. ** ^g ^^ {{ {6 {{ ^^ ^h ** ** *. *. *. *. ** *. ** ** "
        "*. *. *. ** *. ** *. *. ** ** *. *. *. *. *4 ^^ {{ {3 .. {5 {{ ^^ *2 ** ** ** *. ** ** ** *. ** "
        "*. *. *. ** ** ** *. *. ** ** *. ** ** *. ** ^j {{ {a .. {c ^^ ^l *. ** *. ** ** *. *. ** *. *. "
        "*. ** ** *. *. *. *. ** *. *. ** *. ** ** *. *5 {j {{ {e ^^ ^l *3 *. *. *. ** *. ** ** ** ** ** "
        "** ** *. *. *. ** *. ** ** ** ** *. *. ** *. *. *5 {j {{ {l *3 *. *. *. ** ** ** *. *. *. *. *. "
        "*. *. *. ** *. *. ** ** *. *. *. *. ** *. *8 *. *. *. *1 ** ** *. *. *. ** *. ** *. ** *. ** ** "
        "** *. *8 *8 ** *. *. ** *. ** ** *. *c {g {{ {h *a *. ** ** *. *. ** ** ** ** *. *. ** *. *. *. "
        "*c ^g ^^ ^^ ^h *a *8 *. ** *. *. *. {g {{ ^^ {{ {h ** *. *. ** *. ** ** *. ** ** *. ** *. *. *. "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^h ** *. ** *c {{ ^^ ^^ ^^ {{ *2 ** ** ** *. ** *. *. ** *. *. ** *. ** ** "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ *2 *. *c {g {{ {{ ^^ {{ {l *. ** ** *. ** *. ** *. ** ** *. *. ** ** *. "
        "{{ {{ {C tf {A {{ ^^ ^^ *2 *. {g {{ ^^ {{ {{ {l *3 ** *. *. *. ** *8 *. *. ** *. ** ** *. *8 *c "
        "%3 -7 %% %5 t7 {{ ^^ ^^ *a *4 {{ ^^ ^^ ^^ {{ *3 ** *. ** *8 *c ^g ^^ ^h *a *. ** *. ** ^g ^^ ^^ "
        "-b -- -d %% tt {{ ^^ ^^ ^h *e {{ {{ ^^ {{ {l ** ** *c ^g ^^ ^^ ^^ ^^ ^^ ^h *a *. *. *4 ^^ ^^ ^^ "
        "%% -e %% %c te {{ ^^ ^^ ^^ {{ {{ {{ {{ {{ *3 ** *c ^g ^^ ^^ ^^ {{ ^^ ^^ ^^ ^h *. *. *4 ^^ ^^ ^^ "
        "%% %% %% tf {B {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ *a *c ^g ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ *a *. *4 ^^ ^^ ^^ "
        "td %% %% %5 tf {A ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^h *e ^^ ^^ ^^ "
        "%3 %c ^7 %a %5 t7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "%% ^b ^^ ^d %% tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {3 {1 {1 {1 {5 {{ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "%% %5 ^e %3 %c te ^^ ^^ ^^ ^^ ^^ ^^ {{ {3 tC t3 tt t5 tA {5 ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "-7 %% %% %c tf ^3 ^^ ^^ ^^ ^^ ^^ {{ {3 .. t3 tt tt tt t5 {4 {{ ^^ ^^ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ "
        "-- -d %% t7 ^3 ^^ ^^ ^^ ^^ ^^ ^^ {{ {a .. tt tt ~7 tt tt {4 {{ {{ ^^ {{ {{ ^^ {{ {{ ^^ ^^ ^^ ^^ "
        "-e %% %c te ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {2 ta tt ~6 tt tt tA {5 {{ {{ {{ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ "
        "%% %c t7 {# ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {2 t% tt ~6 ta tt t5 tA {5 {{ {{ {{ ^^ {{ {{ {{ {{ ^^ ^^ "
        "tt tt tc {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. tt ~a ~9 ~5 ta t5 tA {1 {1 {1 {1 {5 {{ {{ {{ {{ {{ "
        "/3 .. .. {5 {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {3 .. tC tt tt t5 ~a ~5 tt tt tt tt tt t5 tA {1 {1 {1 {1 {1 "
        "/7 .. .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {2 .. t3 tt tt tt t5 ~6 te ~3 ~9 ~9 ~5 ta tt tt tt tt tt t5 "
        "/7 .. .. .. {5 {{ {{ ^^ ^^ ^^ ^^ {{ {2 .. ta tt tt tt tt ~2 ~9 ~c t3 t5 ~2 ~9 ~9 ~9 ~9 ~9 ~5 ta "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "*. *. *. ** ** ** ^j ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "** ** ** ** *. *. *5 ^j ^^ ^^ ^^ ^l *3 *1 *1 *5 ^j ^^ ^^ ^^ ^l *3 *1 *1 *5 ^j ^^ ^^ ^^ ^^ ^c |# "
        "** ** ** ** ** ** *. *5 ^j ^^ ^l *3 ** ** ** ** *5 ^j ^^ ^l *3 *. ** ** ** *5 ^j ^^ ^^ ^^ || tt "
        "*. *. *. ** *. *. *. *. *5 ^^ *3 ** *c ^g ^h *a ** *5 ^^ *3 *. *c ^g ^h *a ** *5 ^^ ^^ *b *d tt "
        "** ** *. *. *. *. *. ** *4 ^^ *2 *. ^g ^^ ^^ ^h *. *4 ^^ *2 *. ^g ^^ ^^ ^h *. *4 ^^ tf .. .. {4 "
        "*. ** ** *. ** *. ** *. ** ^n ** *4 ^^ ^^ ^^ ^^ *a ** ^n *. *4 ^^ ^^ ^^ ^^ *a *c ^^ {2 .. *f tt "
        "** *. ** ** ** *. *. ** *. *. *. *c ^^ ^^ ^^ ^^ ^h *a *. *. *c ^^ ^^ ^^ ^^ tt .. ^^ {2 tb tt tt "
        "*. ** ** *. *. ** ^g ^h *a *8 *c ^g ^^ ^^ ^^ ^^ ^^ ^h *a *c ^g ^^ ^^ ^^ ^^ ^^ {2 {1 {4 ^^ ^5 |A "
        "*. ** *. *. ** *. ^j ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tb tt td ^^ ^^ ^^ "
        "*. ** *. ** ** ** *5 ^j ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "** ** *. *. ** ** *. *5 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "** ** ** *. *. ** ** *c ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ "
        "*. *. *. *. ** ** ** ^g ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ "
        "** *. *. *. *. *. *4 ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^L -3 -1 -1 -1 -5 ^J {{ {{ {{ ^^ ^^ ^^ {{ {{ ^^ ^^ "
        "** *8 *8 *8 *. ** *c ^^ ^^ ^^ ^^ {{ {{ {{ ^L -3 -- -- -8 -- -- -5 {J {{ ^^ ^^ ^^ ^^ ^^ {{ ^^ {{ "
        "^g ^^ ^^ ^^ ^h *e ^g ^^ ^^ ^^ ^^ ^^ {{ {{ -3 -- -c ^G ^^ ^H -a -- -5 {{ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {{ -2 -- ^G ^^ ^^ ^^ ^H -- -4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ {{ -2 -4 ^^ ^^ ^^ ^^ ^^ -2 -4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^L -- -4 {5 ^^ {7 ^^ {3 -2 -- ^J ^^ ^^ ^^ ^^ ^^ {{ {{ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {7 ^^ -3 -- -4 .. {1 /7 {1 .. -2 -- -5 ^^ {{ ^^ ^^ ^^ {{ {{ {3 "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {b .. {1 -a __ -c .. .. /7 .. .. -a __ -c {5 {{ {{ ^^ {{ {{ {3 .. "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ /0 /d /d == /d /d /d /9 /e /c /e __ /c /d /2 {A {{ {{ {3 .. .. "
        "^^ {{ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ {{ /7 t7 -3 __ -- -5 ~A .. ~C -3 -- __ -5 .. /8 /d /e /c /e {8 .. "
        "^^ {{ {{ ^^ {{ {{ {{ {{ {{ {{ {C /7 tt -2 __ __ -- -- -1 -- -- __ __ -4 .. {c {{ {{ {{ {{ {{ {a "
        "{{ {{ {{ {{ {{ {3 {1 {1 {1 {1 /0 /a tt -a -- __ __ __ __ __ __ __ -- -c {4 {{ {{ ^^ ^^ {{ {{ {{ "
        "{{ {{ {{ {3 {1 tC tb tt tt t5 /7 tb tt tH -a -- -- __ __ __ __ -- -c ~# {4 {{ ^^ ^^ ^^ ^^ ^^ {{ "
        "^^ {{ {{ {2 t3 td ~3 ~9 ~5 tt /8 /2 tt tt tt tH -a -- __ __ -- -- ~B .. {4 {{ {{ ^^ ^^ ^^ ^^ ^^ "
        "{{ {{ {3 .. tt ~3 ~c t7 ~6 ta t5 /7 ta tt || tt tM -- __ __ -- -- -- -5 ~A {1 {5 {{ {{ {{ ^^ ^^ "
        "{{ {3 .. tC tt ~6 t3 tt ~a ~5 tt /7 t% ta tt tt -3 -- __ __ -- __ __ -- -- -5 ~A {1 {5 {{ ^^ ^^ "
        "{1 .. tC t3 tc ~6 tt tt t5 ~6 tt /8 /2 .. tD tt -2 __ __ __ __ __ __ __ __ -- -- -5 {4 {{ {{ ^^ "
        "tE t3 tt tc ~3 ~c tt t& tt ~6 ta td /7 tb tt tc -- __ __ __ __ __ __ __ __ __ __ -- ~A {5 {{ ^^ "
        "tt tc ~3 ~9 ~c t3 tc .. tt ~a ~9 ~9 =| ~9 ~9 ~9 -- -- __ __ __ __ __ __ __ __ __ -- -5 {4 {{ ^^ "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "tt tt tt *f ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        "tc t& ta tt || ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {7 {{ {{ ^^ ^^ ^^ ^^ ^^ "
        "^3 ^^ ^5 ta tt ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {3 .. {5 {{ {{ ^^ ^^ ^^ ^^ "
        "^^ .. ^^ tD tt ^^ {{ {{ {{ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {b .. .. .. {d {{ ^^ ^^ ^^ ^^ "
        "^^ ^~ ^^ tt *f ^^ {{ {{ {{ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {a .. {c {{ {{ ^^ ^^ ^^ ^^ "
        "t5 .. t3 tt || ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {6 {{ {{ ^^ ^^ ^^ ^^ ^^ "
        "tt *f tt |C ^3 ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {6 {{ {{ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {e ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ {{ {{ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ {7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ {{ {{ {{ {{ {3 {1 {1 {1 {1 {1 {1 {1 {1 {1 {1 {1 {1 {5 {{ {{ ^^ {{ /7 {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ {{ {{ {{ {3 {1 .. .. .. .. .. .. .. .. .. .. .. .. .. .. {5 {{ {{ {{ /b {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        "{{ {{ {{ {3 .. .. tC t3 tt tt tt tt tt tt tt tt tt tt t5 .. .. {5 {{ {{ {6 {{ {{ ^^ ^^ ^^ ^^ ^^ "
        "{{ {{ {{ {2 .. t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tA .. .. {1 {1 /3 {1 {5 ^^ ^^ ^^ ^^ ^^ "
        "{{ {{ {3 .. .. tt tt tt || || || || || || || || || tt tt t5 .. .. .. .. /b .. .. {5 ^^ ^^ ^^ ^^ "
        "{{ {3 .. .. tC tt tt || || || || || || || || || || tt tt tL -3 -1 -5 .. .. /c /d /2 {1 {5 ^^ ^^ "
        "{3 .. .. tC t3 tt tt || || ~b ~9 ~9 ~9 ~9 ~5 || || || tt -3 -- -- -- ~A .. .. .. /b .. .. {5 ^^ "
        "{2 .. t3 tt tt tt tt || || || || || || |% ~6 |A || || tt -2 -- -- -- -5 .. .. .. /3 .. .. {4 ^^ "
        ".. .. tt tt tt || || || || || || || || || ~a ~5 |A || tL -- -- -- -- -- ~A .. .. /b .. .. {4 ^^ "
        ".. .. tt tt || || || || || || || || || || |% ~a ~5 tL -3 -- -- -- -- -- -- -5 ~A .. .. ~C _3 -- "
        ".. .. tt tt || || ~b ~9 ~9 ~9 ~9 ~9 ~5 |A || |D -- -- -- -- -- __ -- -- -- -- -- -1 -1 -- -- -- "
        ".. .. tt tt || || || || || || || |% ~a ~9 ~1 ~9 -8 -- -- -- -- __ __ __ -- -- -- -- -- -- -- -- "
        ".. .. tt tt tt || || || || || || || || |D ~6 |# || tH -- -- __ __ __ __ __ __ __ __ __ __ __ __ "
        "{2 .. ta tt tt || || || |C ~3 ~9 ~9 ~9 ~9 ~c || || || -2 -- __ __ __ __ __ __ __ __ __ __ __ __ "
        "{a .. t% tt tt || || ~b ~9 ~c |# || || || || || || tL -- -- __ __ __ __ __ __ __ __ __ __ __ __ "
        "{{ {a .. tt tt || || || || || || || || || || || || -3 -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "{{ {{ {2 ta tt tt || || || || || || || tt tt tt tt -2 -- __ -- __ __ __ __ __ __ __ __ __ __ __ "
        "^^ {{ {2 t% ta tt tt tt tt tt tt tt tt tt tt tt tc -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "^^ {{ {a .. .. t% ta tt tt tt tt tt tt tt tc t# .. -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "^^ ^^ {{ {a {8 {8 {8 .. .. .. .. .. .. {8 {8 {8 {8 -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "^^ {{ {{ {{ {{ {{ {{ {a {8 {8 {8 {8 {c {{ {{ {{ {{ -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "*. ** *. *. ** ** ** ** *. ** *. *. ** *c ^^ ^^ {{ {{ {{ {a t% ta tt tt tt tc t# {# {{ {{ {a /0 "
        "*. ** ** *. *. ** ** *. *. *. *. *. ^g ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {a {8 {8 {4 {{ {{ ^^ {{ {{ /7 "
        "** *. *. ** ** ** *. *. ** *. *. *. ^j ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {6 {{ ^^ ^^ ^^ {{ /7 "
        "*. *. *. ** *. *. *. *. ** ** ** ** *5 ^j ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {6 {{ {{ ^^ {{ {C /7 "
        "** ** ** ** *. ** *. ** *. *. *. ** ** *5 ^j ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {a {5 {{ {{ {C /0 /a "
        "** ** ** ** *. *. *. ** *. *. *. *. *. *. *5 ^j ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {a {9 {9 /0 /a {# "
        "** ** *. ** ** ** *c .i *a *. ** *. *. *. ** *5 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ /7 {# {{ "
        "*. *. ** ** *. *. .k .. .m ** *. ** *. *. *. *4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ /7 {{ {{ "
        "*. *. *. *. ** ** *5 .n *3 *. ** *. ** ** ** *4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ /7 {1 {5 "
        "** ** ** ** ** *. ** *. ** *. *. ** ** *. ** *c ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {3 /7 .. .. "
        "** ** ** ** *. ** *. *. *. *. ** ** *. ** *c ^g ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {3 {1 .. /7 .. .. "
        "*. *. ** *. ** ** *. ** ** ** *. *. ** *c ^g ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {2 .. .. /7 .. .. "
        "*. ** ** ** *. ** *. *. ** *. ** ** *. ^g ^^ ^^ ^^ ^^ {{ {{ ~7 {{ {{ {{ {{ {3 .. .. .. /7 .. .. "
        "*. *. ** *. *. ** ** *. *. ** ** *. *4 ^^ ^^ ^^ {{ {{ {{ {{ ~6 {1 t3 tt t5 .. .. .. .. /7 .. .. "
        "*. ** *. *. *. *. *. *. *. *. *. *. *c ^^ ^^ ^^ {{ {{ {{ {3 ~6 .. te ~7 te .. .. .. .. /7 .. .. "
        "** *. ** *. *. ** ** ** *. *. *. ** ^g ^^ ^^ ^^ {{ {{ ~b ~9 ~4 .. .. ~6 .. .. .. .. .. /7 .. .. "
        "*. *. ** *. *. *. *. *. *. ** ** *4 ^^ ^^ ^^ ^^ {{ {{ {{ {2 ~6 .. .. ~6 .. .. .. .. .. /7 .. .. "
        "** *. ** *. *. *. *. ** *. *. *. *4 ^^ ^^ ^^ ^^ {{ {{ {3 .. ~a ~9 ~9 ~4 .. .. .. .. .. /7 .. .. "
        "*. *. ** ** ** ** ** *. ** *. ** *4 ^^ ^^ ^^ ^^ {{ {{ {2 .. .. .. .. ~6 .. .. .. .. .. /7 .. .. "
        "*. *. *. *. *. *. *. ** ** *. ** *c ^^ ^^ ^^ ^^ {{ {3 /0 /d /d /d /d == /d /d /d /d /d /7 .. .. "
        "** ** ** ** *. *. ** *. *. *c ^g ^^ ^^ ^^ ^^ ^^ {{ {2 /7 .. .. .. .. ~6 .. .. .. .. .. /8 /d /d "
        "*. *. *. *. ** ** *. ** *c ^g ^^ ^^ ^^ ^^ ^^ {{ {{ {2 /7 .. .. .. .. ~a ~9 ~9 ~5 .. .. .. .. .. "
        "** ** *. ** ** *. ** ** ^g ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {a /7 .. t3 td .. .. .. .. ~6 .. .. .. .. ~3 "
        "*. ** *. ** ** *. *. *4 ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ /7 tC tt ~b ~5 .. .. .. ~6 .. .. .. .. ~6 "
        "** ** ** *. ** ** ** *4 ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ {{ {{ /7 t3 tt td ~6 .. .. .. ~a ~9 -1 -1 -- -4 "
        "*. *. ** *. ** ** ** *4 ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ {{ {{ /7 tt ~b ~9 ~8 ~5 .. .. .. ~C -- __ __ -4 "
        "** *. *. *. *. *. ** *4 ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {{ /7 ta t5 tA .. ~a ~1 ~9 ~1 -- -- __ __ -4 "
        "*. ** *. *. ** ** *. *4 ^^ ^^ ^^ ^^ {{ {{ {{ ^^ {{ {{ /7 t% tt td .. .. ~6 .. -a -- __ __ -- -- "
        "*. *. *. *. *. ** ** *4 ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {3 /7 tC tt ~b ~5 .. ~6 .. ~% -- -- -- -- -- "
        "*. ** ** ** ** ** *. *4 ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {{ {2 /7 t3 tt td ~~ ~9 ~c .. .. -2 -- tG tH ~a "
        "** *. ** ** ** *. *. *4 ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {2 /7 tt ~b ~9 ~c .. .. .. ~C -- -c tt tt tt "
        "** *. ** ** ** *. *. *. ^j ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {2 /7 ta td .. .. .. .. .. -3 -- tG tt tt tt "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "/a .. .. {8 .. {5 {{ {{ ^^ ^^ ^^ ^^ {2 .. t% tt tt tt tc ~6 t3 tt tt tt ~6 t3 tt tt tt t5 ~a ~9 "
        ".. .. {c {{ {a .. {5 {{ {{ ^^ ^^ ^^ {2 .. .. tt ~3 ~9 ~9 ~c tt t# tD tt ~6 tt t# {8 t% ta tt tt "
        ".. {c {{ {{ {{ {a {c {{ {{ ^^ ^^ ^^ {a .. .. tt ~6 t3 tt tt tc .. t3 tc ~6 tt {4 ^^ {a {8 {8 {8 "
        "{c {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ {{ {{ {2 tC tt ~6 tt t# {8 {8 .. tt ~3 ~c tt {4 ^^ ^^ ^^ {{ {{ "
        "{{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ {{ {{ {2 t3 tc ~6 tt {4 ^^ {{ {2 tt ~6 t3 tt tA {5 ^^ ^^ ^^ {{ "
        "{{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {3 tC tt ~3 ~c tt {4 ^^ ^^ {2 tt ~6 ta tt t5 {4 {{ ^^ ^^ ^^ "
        "{{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {b .. t3 tt ~6 t3 tt {4 {{ ^^ {2 tt ~a ~9 ~5 tt {4 {{ {{ ^^ ^^ "
        "{{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {2 tt tt ~e tt tt {c {{ {{ {2 ta tt t5 ~e tt {4 {{ {{ ^^ ^^ "
        "{{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {2 ta tt tt tt tc {{ ^^ ^^ {a {8 t% ta tt tc {4 {{ ^^ ^^ ^^ "
        "{1 {1 {5 {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {a t% ta tt tc t# {{ ^^ ^^ {{ {{ {a {8 {8 {8 {c ^^ ^^ ^^ ^^ "
        ".. .. t7 {A {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {a {8 {8 {c ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        ".. .. tt ~7 t7 {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        ".. .. te ~6 tt {A {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {C tf {{ tf {A ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. ~6 tt td {5 {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tf t# {1 t% tf {{ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. ~6 tt ~3 ~9 ~9 ~9 ~9 ~d ^^ ^^ ^^ ^^ ^^ {{ {{ {2 .. {4 {{ {{ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. ~6 te ~6 t7 {# {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ tf tA /3 tC tf {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. ~a ~9 ~4 te {5 {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {% tf /7 tf {# ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {7 {{ "
        ".. .. .. .. .. ~~ ~9 ~9 ~5 tb td {A ^^ ^^ ^^ ^^ ^^ ^^ {% /7 {# ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {3 .. {5 "
        ".. ~3 ~9 ~9 ~9 ~c t3 t5 ~a ~1 ~d tf {{ ^^ ^^ ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {b .. .. .. "
        ".. ~6 .. .. .. .. ta tt td ~6 tf {# {3 {1 tf ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {a .. {c "
        "/d == /d /d /2 .. .. .. .. == .. {1 {8 {c {{ ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {e {{ "
        ".. ~6 .. .. /7 .. .. {8 tf ~6 tb td {A {{ {{ ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "~9 ~c .. .. /7 .. {4 {{ {% ~a ~9 ~5 tf {{ ^^ ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. .. /7 .. {4 {{ ^^ ^^ ^5 ~a ~d ^^ ^^ ^^ ^^ ^^ ^^ /7 ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        ".. .. .. .. /7 .. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ /7 ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ "
        ".. .. .. .. /7 .. .. {1 {5 {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {C /7 {A {{ {{ {C t7 {A {{ {{ ^^ ^^ ^^ {{ "
        ".. .. .. .. /8 /d /d /d /d /2 {{ ^^ ^^ ^^ ^^ {{ t3 tt t5 /7 t3 tt tt tt tt tt tt t5 {{ ^^ {{ {C "
        "~A .. .. .. .. .. .. {c {% /7 {{ ^^ ^^ {{ {{ {{ ta tt tc /7 ta tt tt tt tt tt tt tc {A {{ {C tf "
        "~5 ~A .. .. .. .. {c {{ {{ /7 {A ^^ ^^ ^^ {{ {3 .. /0 /d /9 /d /d /d /d /d /d /d /d /d /d /d /d "
        "-- -- ~5 ~A .. {4 {{ {{ {{ /8 /d {{ ^^ {{ /d /d /d /a .. .. .. .. t3 tt tt tt tt tt tt tt tt tt "
        "tH ~a -- ~~ ~9 ~9 ~9 ~9 ~5 {A {{ ^^ ^^ ^^ {{ {a .. .. .. .. .. .. ta tt tt tt tt tt tt || || tt "
        "tt tH ~~ -c {# {{ {{ {% ~a ~5 {{ {{ ^^ ^^ {{ {{ {2 .. .. .. .. .. .. t% tt tt tt tt || || || || "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "~9 ~9 ~c t3 tt tc t# .. ta tt tt td /7 tb tt tt tH -- __ __ -- -- -- -- __ __ __ -- -4 {4 {{ ^^ "
        "tt tt tt tc t# {8 {8 .. .. .. .. .. /7 .. .. .. .. -a -8 -8 -c ~# ~% -- __ __ __ __ -4 {4 {{ {{ "
        "{8 {8 {8 {8 {c {{ {{ {a {8 .. /0 /d /a {8 .. .. .. .. {8 {8 {8 {8 {8 -a -- __ __ __ -- ~A {5 {{ "
        "{{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {a /7 {# {{ {{ {a {8 {8 {c {{ {{ {{ {{ {{ {H -- __ __ __ -- -- -1 -1 "
        "{{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ /7 {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ -2 __ __ __ __ __ __ __ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ /7 {{ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ -2 __ __ __ __ __ __ __ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ /7 ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ -a -- __ __ __ __ __ __ "
        "^c |# || |% ^a ^^ ^^ ^^ ^^ ^^ /8 /2 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^H -a -8 -- -- __ __ __ "
        "|# || || || |% ^a ^^ ^^ ^^ ^^ ^d /7 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^c tf ^a ^^ ^^ ^^ ^H -a -- __ __ "
        "|| || || || || t5 ^a ^^ ^^ ^^ /0 /a ^^ ^^ ^^ ^c |& ^a ^^ tb tt td /7 t7 ^^ ^^ ^^ ^^ ^H -- -- __ "
        "|A || || || || tt t5 ^a ^^ ^c /7 ^3 ^^ ^^ ^^ |# || |% ^e /0 /d /d /a te ^^ ^^ ^^ ^^ ^^ -2 -- __ "
        "^5 |A || |C ^7 ta tt tt tt t5 /7 ^^ ^^ ^^ ^c || || || t5 /7 tb tt td ^3 ^^ ^^ ^^ ^^ ^^ -2 -- __ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^5 ta tt tc /7 ^a ^^ ^^ t3 tt || tt tt /7 ^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^L -- -- __ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^5 /0 /d /9 /2 ^a ^c ta tt tt tt tc /7 ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ -b -- -- -- "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ /7 t3 t5 /8 /d /d /d /d /d /d /d /a t7 ^a ^^ ^^ ^^ ^^ ^^ ^^ ~% -a -- -- "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ /7 tt tt tt tt tt tt tt tt tt tt tt tt t5 ^a ^^ ^^ ^^ ^^ ^^ {2 .. ~% -- "
        "^^ ^^ ^^ ^^ ^^ {{ {{ {C /7 tt || || || || || tt tt tt || || || tt tt t5 ^^ ^^ ^^ ^^ {a .. .. -a "
        "{{ {{ {{ {{ ^^ {{ /0 /d /a tt || || || || || tt tt tt tt || || || tt tt ^^ ^^ ^^ ^^ ^^ {a .. ~% "
        "{9 {9 {5 {{ {{ {C /7 t3 tt tt || || || || || || tt tt || || || || || tt ^^ ^^ ^^ ^^ ^^ ^^ {a {8 "
        "{{ {{ {a {5 {C tf /7 tt || || || || || || || tt tt tt || || || || || tt ^a ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ {{ {{ /0 /d /d /a tt || || || || || || || || tt tt || || || || || tt t5 ^^ ^^ ^^ ^^ ^^ ^^ ^c "
        "^^ ^^ {{ /7 t3 tt tt || || || || || || || || || tt tt tt || || || tt tt tt ^a ^^ ^^ ^^ ^^ ^^ t3 "
        "^^ {{ {C /7 tt || || || || || || || || || || || || tt tt || || || tt tt tt || |% ^a ^^ ^^ ^c tt "
        "{{ {C t7 /7 tt || || || || || || tt || || || || || tt tt tt || || || tt || || || || || || tt tt "
        "{C tb tc /7 tt || || || || tt tt tt || || || || || || tt tt || || || || || || || || || || tt tt "
        "/0 /d /d /a tt || || || tt tt tt tt tt || || || || || tt tt tt || || || || || |C ^7 |A || tt tt "
        "/7 t3 tt tt tt tt || || tt tt || tt tt tt || tt tt tt tt tt || || || || || || ^b ^^ ^5 tt tt tt "
        "/7 tt tt tt tt tt tt tt tt tt || || tt tt || tt || tt tt tt || || || || || || |% ^^ ^^ ta tt tt "
        "/a tt tt tt tt tt tt tt tt || || || tt tt tt tt tt tt tt || || || || || || || tt ^^ ^^ ^5 ta tt "
        "tt tt tt tt tt tt tt || || || || || || tt tt || || || || || || || || || || || tc ^^ ^^ ^^ ^5 ta "
        "|| || tt tt tt || || || || || || || || || || || || || || || || || || || || tt ^3 ^^ ^^ ^^ ^^ ^d "
        "|| || || tt tt || || || || || || || || || || || || || || || || || || || || tc ^^ ^^ ^^ ^^ ^^ t3 "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {L -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "^^ ^^ ^^ ^^ -7 {J {{ {{ {{ {{ {{ {{ {{ {{ {C ~C -3 -- __ __ __ __ __ __ __ __ __ ~~ ~~ ~~ __ __ "
        "^^ ^^ ^^ ^^ -2 -- _1 _1 _1 _1 _1 -1 -1 -1 -1 -- -- -- __ __ __ __ __ __ __ __ ~~ -- -- -- ~~ __ "
        "-1 -5 ^J ^L -- -- __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ __ __ ~~ -- -- -c ^I -a -- __ "
        "-- -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ ~~ -- -- ^K ^^ ^M -- __ "
        "-- -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ -- -- -- -5 ^N -3 -- __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ ~~ ~~ ~~ ~~ ~~ -- -- -- -- ~~ ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ -- -- -8 -8 -- -- -- -- -- -- -- -- __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c {G {{ {{ ^H -a -- -- -- -- -- -- __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- {G {{ ^^ ^^ ^^ ^M -- -c ^I -a -- -- __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -4 {{ ^^ ^^ ^^ ^^ ~3 -- ^K ^^ ^M -- __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -4 {{ {{ ^^ ^^ ^^ ~a -- -5 ^N -3 -- -- __ -- "
        "-- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ -4 {{ {{ ^^ ^^ ^^ ^H ~a -- -- -- -- -- -- -- "
        "-- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c {{ ^^ ^^ ^^ {{ ^^ ^^ ^^ -- -- -- ^I -a -- "
        "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ ~~ -- {G {{ ^^ !3 !5 {{ !3 !5 ^^ -a -- -4 ^^ ^M -- "
        "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c {{ ^^ ^^ !a !! =! !! !c ^^ ^H -a -c ^^ -3 -- "
        "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ -- {G {{ ^^ ^^ ^^ !e {{ !e ^^ ^^ ^^ {{ {{ ^^ -2 __ "
        "-2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ -4 {{ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ {{ {{ {{ ^^ -2 __ "
        "-2 -- -- __ __ __ __ __ __ __ __ __ __ __ __ -4 {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ {{ ^^ _2 __ "
        "-a -- -- -- -- __ __ __ __ __ __ __ __ __ __ -- {J {{ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^L __ __ "
        "tH -a -- -- -- -- __ __ __ __ __ __ __ __ __ -- -5 {{ ^^ ^^ ^^ {{ ^^ ^^ {{ {{ {{ ^^ ^L _3 __ __ "
        "tt tt tH -a -- -- __ __ __ __ __ __ __ __ __ __ -- {J {{ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^L _3 __ __ __ "
        "tt tt tt tH -- -- __ __ __ __ __ __ __ __ __ __ -- -- -5 ^J ^^ ^^ ^^ ^^ ^^ ^^ ^^ _3 __ __ __ __ "
        "tt tt tt tt -2 -- __ __ __ __ __ __ __ __ __ __ __ __ -- -- -5 ^J ^L -3 -5 ^J ^L __ __ __ __ __ "
        "tt tt tt tt -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- __ __ __ __ __ __ "
        "tt tt tt tt -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt tL -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt -3 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- __ __ __ __ __ __ __ __ __ __ "
        "tt tt tL -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ -- -c ^I -a -- __ __ __ __ __ __ __ __ __ "
        "tt tt -3 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ -- ^K ^^ ^M -- __ __ __ __ __ __ __ __ __ "
        "tt tt -2 -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ -- -5 ^N -3 -- __ __ __ __ __ __ __ __ __ "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "*. ** *. ** *. *. ** ** *. *5 ^^ ^^ ^^ ^^ ^^ {{ {3 .. /7 .. .. .. .. .. .. ~C -- -- tJ tt tt tt "
        "** *. ** ** *. ** ** ** ** *4 ^^ ^^ ^^ {{ {{ {3 .. .. /7 .. .. .. .. .. ~C ~3 -- -- -5 tJ tt tt "
        "** ** ** ** ** ** ** ** *. *c ^^ ^^ {{ {{ /0 /d /d /d /9 /d /d /2 .. .. -3 -- -- __ -- -5 tt tt "
        "*. ** ** *. ** *. ** ** *c ^g ^^ ^^ {{ {3 /7 {c {{ {{ {{ {{ {a /7 .. .. -2 -- __ __ __ -4 tt tt "
        "** *. *. *. ** *. ** *c ^g ^^ ^^ ^^ {{ {2 /7 {{ {{ ^^ ^^ {{ {{ /7 .. ~C -- -- -- __ -- -c tt tt "
        "*. *. *. *. *. *. *c ^g ^^ ^^ ^^ ^^ {{ {2 /7 {{ ^^ ^^ ^^ ^^ {{ /7 .. -3 -- -- -- -- -c tG tt tt "
        "*. ** *. ** *. *c {g {{ ^^ ^^ ^^ ^^ {{ {2 /7 {{ ^^ ^^ ^^ ^^ {{ /7 .. -2 -- -- -c tG tt tt tt tt "
        "** *8 *8 *c {g {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {2 /7 {{ {{ ^^ ^^ {{ {C /7 .. -2 -- -- tG tt tt tt tt tt "
        "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {3 .. /7 {5 {{ {{ {{ {C /0 /a ~C -- -- -- tJ tt tt {# {{ {{ "
        "^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ {{ {{ {3 /0 /d /9 /d /d /d /d /d /a ~C -3 -- -- -- -5 tJ tc {{ ^^ ^^ "
        "^^ ^^ ^^ {{ {{ ^^ ^^ ^^ {{ {{ {3 .. /7 .. .. .. .. .. .. .. .. -3 -- -- -- ~~ -- -- -5 ^^ ^^ ^^ "
        "^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ {3 .. .. /7 .. .. .. .. .. .. .. ~C -- ~~ -- -- -- -- -- -c ^^ ^^ ^^ "
        "^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ {2 .. .. /7 .. ~C _3 _1 _5 ~A ~C ~3 ~~ -- -- -- -- -- -- ^K ^^ ^^ ^^ "
        "^^ ^^ {{ {{ {{ ^^ ^^ ^^ {3 .. .. .. /7 ~C _3 __ __ __ __ __ __ __ __ -- -- -- -- -- -5 ^^ ^^ ^^ "
        "^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ {2 .. .. .. .. _3 __ __ __ __ __ __ __ __ __ __ -- __ __ __ -- ^J ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ {{ {{ {{ {2 .. .. .. .. _a __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -5 ^^ ^^ "
        "^^ ^^ ^^ ^^ {{ {L ~3 ~1 ~5 ~A .. .. .. ~% _a __ __ __ _c ~& _a __ __ __ __ __ __ __ __ -- ^J ^L "
        "^^ ^^ ^^ {{ {L ~3 ~~ ~~ ~~ ~~ ~5 .. .. .. ~% _a _8 _c ~# .. ~% -- __ __ __ __ __ __ __ -- -- -- "
        "^^ ^^ ^^ {{ ~3 ~~ -- ~~ ~~ ~~ ~~ ~A .. .. .. .. .. .. .. .. ~C -- __ __ __ __ __ __ __ __ __ __ "
        "^^ ^^ ^^ {{ ~2 -- -- -- -- -- ~~ ~5 ~A .. .. .. .. .. .. ~C -3 -- -- -- -- __ __ __ __ __ __ __ "
        "^^ ^^ ^^ {{ ~2 -- -- -- -- -- ~~ ~~ ~~ ~5 ~A .. .. ~C -3 -- -- ~~ ~~ ~~ -- -- __ __ __ __ __ __ "
        "^^ ^^ ^^ {{ ~a ~~ -- -- -- -- ~~ ~~ ~~ ~~ ~~ -1 -1 -- -- -- ~~ ~c ~& ~a ~~ -- __ __ __ __ __ __ "
        "^^ ^^ ^^ {{ {H ~a ~~ -- -- ~~ ~c ~& ~a ~~ ~~ ~~ ~~ ~~ ~~ -- ~~ ~B .. ~D ~~ -- __ -- -- -- -8 -- "
        "^^ ^^ ^^ ^^ {{ {H ~a ~~ ~~ ~~ ~B .. ~D ~~ -- -- -- -- -- -- ~~ ~5 ~E ~3 ~~ -- -- -- -c {G {{ {H "
        "^^ ^^ ^^ ^^ ^^ {{ {{ {H ~~ ~~ ~5 ~E ~3 ~~ -- -- -- -- -- -- -- ~~ ~~ ~~ -- -- -c {G {{ {{ ^^ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ~a ~~ ~~ ~~ ~~ -- -- -- -- -- -- -- -- -- -- -- -- ~~ {G {{ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {H ~a ~~ -- -- -- -- ~~ ~~ ~8 ~8 ~8 ~8 ~~ ~~ ~~ ~~ ~c {{ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {H ~a ~8 ~8 ~8 ~8 ~c {G {{ {{ {{ {{ {H ~a ~c {G {{ {{ ^^ ^^ ^^ ^^ {{ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {L "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ -b -- "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {H -- "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ -- "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "tt tt ~6 {# {{ ^^ {{ {{ {% ~6 {A {{ {{ ^^ ^^ {{ tb tt td .. .. .. .. .. tt tt tt || || || || || "
        "tt tt ~6 {{ ^^ ^^ ^^ ^^ {{ ~a ~d {{ ^^ ^^ ^^ {{ {{ {{ {{ {a {8 {8 .. tC tt || || || || || || || "
        "tt tt ~6 {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ tb tt tt tt || || tt tt tt || "
        "tt tt ~6 {A {{ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {% ta tt tt tt tt tt tt tt "
        "tt tt ~a ~5 {A {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {% tt tt || tt tt tt || "
        "tt tt t5 ~a ~9 ~9 ~d {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ {{ tt tt || || || || || "
        "tt tt tt tt tt tt td {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {L ~7 {J {{ ^^ ^^ ^^ {{ tt tt tt tt tt tt tt "
        "tt tt tt tt tc {# {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ~b -- ~d {{ ^^ ^^ ^^ {{ te {# {{ {{ {{ {% ta "
        "{{ {% te {# {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {% ~6 {# {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ "
        "^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~6 ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^c ~6 ^^ ^^ ^^ {{ {C ~7 {J {{ ^^ ^^ ^^ ^^ ^^ "
        "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ~3 ~c ^^ ~3 ~9 ~9 ~9 -- ~d {{ ^^ ^^ ^^ ^^ ^^ "
        "{{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^c ~6 ^b ^c ~6 ^3 {{ {% ~e {G {{ ^^ ^^ {{ {{ {{ "
        "{{ ^^ {{ {{ {{ {{ {{ {7 {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ~3 ~8 ~9 ~9 ~4 ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ {{ {C "
        "{{ {{ {{ {{ {{ {{ {3 .. {5 {{ {{ ^^ ^^ ^^ ^^ ^c ~6 ^3 ^^ ^5 ~6 ^a ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ t3 "
        "{L -3 -5 {J {{ {3 .. .. .. {5 {{ ^^ ^^ ^^ ^c ~3 ~c ^^ ^^ ^^ ~a ~9 ~d ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ta "
        "-3 -- -- -- -5 ~A .. .. .. {4 {{ {{ ^^ ^L -3 ~~ ^J ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {3 .. "
        "-- __ __ __ -- -5 ~A .. ~C -3 -1 -1 -1 -- -- -- -5 ^J ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {2 .. "
        "__ __ __ __ __ -- -- -1 -- -- __ __ __ __ __ __ -- -- -5 ^J ^^ ^L -3 -1 -5 {J {{ {{ {{ {3 .. .. "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -1 -- -- __ -- -- _5 {J {{ {2 .. .. "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ _1 _1 _5 ~A "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ _5 "
        "-- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-a -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "{H -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "{{ -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "{{ -2 -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "{L -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-3 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "|| || || || tt || || || || || || || || || || || || || || || || || || || ^3 ^^ ^^ ^^ ^^ ^^ ^^ tt "
        "|| || || || tt tt || || || || tt || || || || || || || || || || || || || ^a ^^ ^^ ^^ ^^ ^^ ^^ tt "
        "tt tt tt || || tt || || tt tt tt || || || || || || || || || || || || || |% ^^ ^^ ^^ ^^ ^^ ^^ tt "
        "tt || tt || || tt tt tt tt || tt tt || || || || || || tt tt || || || || |C ^^ ^^ ^^ ^^ ^^ ^^ tt "
        "|| || tt tt tt tt tt || || || || tt || || || || || || tt tt tt || || || ^b ^^ ^^ ^^ ^^ ^^ ^c tt "
        "|| || || tt tt || || || || || tt tt || || || || || tt tt || tt tt tt tt t5 ^a ^^ ^^ ^^ ^c t3 tt "
        "|| || tt tt || || || || tt tt tt tt tc {# {{ {{ {% tt || || || tt tt tt tt t5 ^a ^^ ^c t3 tt tt "
        "tt tt tt tt || || tt tt tt tc {# {{ {{ {{ ^^ ^^ {{ tt tt || tt tt tt tt tt tt t5 ^e t3 tt tt tL "
        "{% ta tt || || || tt {# {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ tt ~b ~9 ~9 ~9 ~9 ~5 tt tt tt tt tt tt tt -3 "
        "{{ {% tt tt || tt tc {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ta tt tt tt tt t5 ~6 tt tt tt tt tt tt tL -- "
        "{{ {C tt tt tt tt {# {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ tf {{ {{ {% ta tt tt ~6 ta tt tL -3 -1 -1 -- -- "
        "{{ t3 tt tt tt tc {{ {{ {{ {{ {{ {{ {{ ^^ ^^ {{ {{ ^^ ^^ {{ {% tt tt ~a ~1 ~9 ~~ ~~ -- -- -- -- "
        "{C tt tt tt t# .. {5 {{ {{ tb tt t5 {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt tt t5 ~6 t7 -a -- -- -- -- -- "
        "t3 tt tt tc .. .. .. {1 {1 .. t% te {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ ta tt tt ~6 tt tM -- -- -- -- -8 "
        "tt t# .. .. .. .. .. .. .. .. .. .. {5 {{ {{ {{ ^^ ^^ ^^ ^^ {{ {% ta tc ~6 tL -3 -- -- -c tG tt "
        "tc .. .. .. .. .. .. .. .. .. ~C -3 -5 ~A {5 {{ {{ ^^ ^^ ^^ ^^ {{ {{ {% ~2 -- -- -- tG tt tt tt "
        ".. .. .. .. .. .. .. .. -b -1 -- -- -- -5 ~A {5 {{ ^^ ^^ ^^ ^^ ^^ {{ {{ -2 -- -- -- tJ tt tt tt "
        ".. .. .. .. .. .. .. .. ~% -- -- -- -- -- -5 ~A {{ ^^ ^^ ^^ ^^ {{ {{ {L -- -- -- -- -5 tJ tt tt "
        ".. .. .. .. .. .. .. .. ~C -- -- __ __ -- -- -5 {{ ^^ ^^ ^^ ^^ {{ {L -3 -- ~~ -- -- -- -5 tJ tL "
        ".. .. .. .. .. .. .. ~C -3 -- -- __ __ -- -- -- {J {{ {{ ^^ {{ -3 -- -- -- ~~ -- -- ~~ -- -- -- "
        ".. .. .. .. .. ~C -3 -- -- -- -- __ __ __ -- -- -- -5 {{ ^^ {{ -2 -- -- ~~ ~~ ~~ __ -- ~~ -- -- "
        "~A .. .. ~C -3 -- -- -- -- -- __ __ __ __ __ -- -- -- {J {{ {L -- -- __ __ ~~ __ __ __ __ ~~ ~~ "
        "__ _1 _1 -- -- -- -- -- __ __ __ __ __ __ __ __ -- -- -- -1 -- -- -- __ __ __ __ __ __ __ __ __ "
        "__ __ __ -- -- -- __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
      )
    )
    (kern-mk-map nil 32 32 pal_expanded
      (list
        "tt tt -a -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt tH -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt tL -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt -3 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tt -2 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tt tL -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt tL -3 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-3 -- -- -- -- ~~ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- -- -- -- ~~ ~~ ~~ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- -c tI -- -- ~~ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- tG tt -2 -- -- -8 -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- tJ tL -- -c tG tt tH -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- -- -- -- tG tt tt tt -2 __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "-- -- -- -4 tt tt tt tL -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tH -- -- -- tJ tt tL -3 -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
        "tt -2 ~~ -- -- -1 -- -- ~~ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- __ __ __ __ __ __ __ __ "
        "tL -- -- ~~ ~~ ~~ ~~ ~~ -- __ __ __ __ __ __ __ __ __ __ __ -- -c ~& -a -- __ __ __ __ __ __ __ "
        "-3 -- ~~ -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c ~# .. ~% -- __ __ __ __ __ __ __ "
        "-- -- ~~ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c ~# tC t7 .. -2 __ __ __ __ __ __ __ "
        "-- ~~ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ~# tC t3 tt .. -a -- __ __ __ __ __ __ "
        "~~ -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -c tC t3 |. tt tA ~% -a -- __ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ~# t3 |. |. |. tt td .. ~D ~~ __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -4 .. tt |. |. |. tc t# ~C -3 -- ~~ ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ~A ta |. |. tc t# -3 -- -- -- -- ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -5 t% tt |. t# -3 -- __ __ -- -- ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ~A ta tc {8 -2 -- __ __ -- -- ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -5 {# {{ {{ -a -- -- -- -- -- ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- {J ^^ ^^ {H -a -8 -c {G {M ~~ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -5 {J {{ ^^ {{ {{ {{ {L -3 -- __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -5 {J ^^ ^^ ^^ {L -3 -- __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -5 {J {L -3 -- __ __ __ __ "
        "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ "
      )
    )
  )
  #f #f #t #f
  ;; subplaces
  (list
    (list
      p_moongate_clearing
    51 46) ;; coords of p_moongate_clearing
    (list
      p_gregors_hut
    61 67) ;; coords of p_gregors_hut
    (list
      p_abandoned_farm
    43 51) ;; coords of p_abandoned_farm
    (list
      p_trigrave
    29 51) ;; coords of p_trigrave
    (list
      p_enchanters_tower
    29 21) ;; coords of p_enchanters_tower
    (list
      p_green_tower
    80 59) ;; coords of p_green_tower
    (list
      p_bole
    86 40) ;; coords of p_bole
    (list
      p_glasdrin
    82 18) ;; coords of p_glasdrin
    (list
      p_oparine
    12 78) ;; coords of p_oparine
    (list
      p_absalot
    120 4) ;; coords of p_absalot
    (list
      p_mans_hideout
    92 10) ;; coords of p_mans_hideout
    (list
      p_engineers_hut
    50 4) ;; coords of p_engineers_hut
    (list
      p_void_temple
    7 39) ;; coords of p_void_temple
    (list
      p_poor_house
    19 65) ;; coords of p_poor_house
    (list
      p_ankh_shrine
    97 4) ;; coords of p_ankh_shrine
    (list
      p_westpass
    45 61) ;; coords of p_westpass
    (list
      p_eastpass
    43 61) ;; coords of p_eastpass
    (list
      p_ancient_derelict
    43 17) ;; coords of p_ancient_derelict
    (list
      p_gate_to_absalot
    120 10) ;; coords of p_gate_to_absalot
    (list
      p_tower_of_absalot
    120 8) ;; coords of p_tower_of_absalot
    (list
      p_kun
    60 38) ;; coords of p_kun
  ) ; end of subplaces
  nil ;; neighbors
  (list ;; objects in p_shard
    (list
      (kern-tag 'mg-4
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    21 19)
    (list
      (kern-tag 'mg-5
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #t
            '()
            #t
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    56 27)
    (list
      (kern-tag 'mg-6
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    90 26)
    (list
      (kern-tag 'mg-7
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    125 18)
    (list
      (kern-tag 'mg-8
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    50 7)
    (list
      (kern-mk-obj t_edge_spawn 1
        ;; hooks
        (list
        )
      )
    0 0)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_kurpolis_entrance
          9
          10
        )
      ) ;; bind
    53 18)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_mushroom_cave
          7
          12
        )
      ) ;; bind
    78 74)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_lost_halls_1
          2
          2
        )
      ) ;; bind
    39 75)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_necromancers_lair
          9
          9
        )
      ) ;; bind
    40 70)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_smoldering_cave
          9
          9
        )
      ) ;; bind
    118 46)
    (list
      (bind
        (kern-mk-obj t_dungeon 1
          ;; hooks
          (list
          )
        )
        (list
          'p_slimy_cavern
          8
          30
        )
      ) ;; bind
    13 8)
    (list
      (bind
        (kern-mk-obj t_raise_listener 1
          ;; hooks
          (list
          )
        )
        (list
          'raise-merciful-death
          '()
        )
      ) ;; bind
    121 87)
    (list
      (bind
        (kern-mk-obj t_step_trig 1
          ;; hooks
          (list
            (list
              ef_permanent_invisibility
              '()
              2
              0
            )
          )
        )
        (list
          'mk-angriss-lair
          '()
        )
      ) ;; bind
    89 70)
    (list
      (kern-tag 'mg-1
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    62 50)
    (list
      (kern-tag 'mg-2
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    34 56)
    (list
      (kern-tag 'mg-3
        (bind
          (kern-mk-obj t_moongate 1
            ;; hooks
            (list
            )
          )
          (list
            'ord
            #f
            '()
            #f
            #f
            10
          )
        ) ;; bind
      ) ;; kern-tag
    19 82)
  ) ;; end of objects in p_shard
  nil ;; on-entry-hook
  (list ;; edge entrances
    (list 0 127 95) ;; Northwest
    (list 1 64 95) ;; North
    (list 2 0 95) ;; Northeast
    (list 3 127 48) ;; West
    (list 4 64 48) ;; Here
    (list 5 0 48) ;; East
    (list 6 127 0) ;; Southwest
    (list 7 64 0) ;; South
    (list 8 0 0) ;; SoutheastUp
  )
) ;; end of place p_shard

(kern-mk-player
  'player
  s_wanderer
  "Walk"
  sound-walking
  22 71
  651 ;; turns to next meal
  nil
  m_campsite
  nil
  nil ; player's vehicle
  (kern-mk-container
    nil
    ;; trap
    nil
    ;; contents
    (list
      (list 5 sulphorous_ash)
      (list 5 ginseng)
      (list 5 garlic)
      (list 6 spider_silk)
      (list 3 blood_moss)
      (list 3 black_pearl)
      (list 1 nightshade)
      (list 1 mandrake)
      (list 1 t_heal_potion)
      (list 1 t_cure_potion)
      (list 1 t_mana_potion)
      (list 4 t_torch)
      (list 4 t_picklock)
      (list 1 t_manual)
      (list 1 t_letter_from_enchanter)
      (list 3 t_arrow)
    )
    ;; hooks
    (list
    )
  )
  (list
    ch_wanderer
    ch_amy
  )
)
;;--------------
;; Miscellaneous
;;--------------
(kern-set-damage-sprite s_hit)
(kern-set-crosshair t_crosshair)
(kern-set-clock 0 0 0 0 21 23)
(kern-set-time-accel 1)
(kern-mk-dtable
  (list    2    0   -1   -1   -1   -2   -2   -2    0   -2   -2    0 )
  (list    0    2    2   -2   -2   -2   -2   -2   -2   -2   -2   -2 )
  (list   -1    2    2   -2   -1   -2   -2   -2   -2   -2   -2   -2 )
  (list   -1   -2   -2    2   -1   -2    0   -2   -2   -1   -2   -2 )
  (list   -1   -2   -1   -1    2   -2   -1   -1   -2   -1   -2   -2 )
  (list   -2   -2   -2   -2   -2    2   -1    0   -2    0   -2    0 )
  (list   -2   -2   -2    0   -1   -1    2   -2   -2   -1   -2   -2 )
  (list   -2   -2   -2   -2   -1    0   -2    2   -2   -1   -2    0 )
  (list    0   -2   -2   -2   -2   -2   -2   -2    2   -2   -2   -1 )
  (list   -2   -2   -2   -1   -1    0   -1   -1   -2    2   -2   -1 )
  (list   -2   -2   -2   -2   -2   -2   -2   -2   -2   -2    2   -2 )
  (list    0   -2   -2   -2   -2    0   -2    0   -1   -1   -2    2 )
)
;; ---------
;; Astronomy
;; ---------
(kern-mk-astral-body
  'sun	; tag
  "Fyer (the sun)"	; name
  1	; distance
  1	; minutes_per_phase
  4	; minutes_per_degress
  0	; initial_arc
  0	; initial_phase
  nil	; gifc
  (list
    (list s_sun 255 "full")
  )
)
(bind-astral-body
  (kern-mk-astral-body
    'lumis	; tag
    "Lumis"	; name
    0	; distance
    300	; minutes_per_phase
    10	; minutes_per_degress
    22	; initial_arc
    0	; initial_phase
    'source-moon-ifc
    (list
      (list s_new_moon 0 "new")
      (list s_wax_quarter_moon 32 "1/4 waxing")
      (list s_wax_half_moon 64 "1/2 waxing")
      (list s_wax_three_quarter_moon 96 "3/4 waxing")
      (list s_full_moon 128 "full")
      (list s_wane_three_quarter_moon 96 "3/4 waning")
      (list s_wane_half_moon 64 "1/2 waning")
      (list s_wane_quarter_moon 32 "1/4 waning")
    )
  )
  (list
    'mg-1
    'mg-2
    'mg-3
    'mg-4
    'mg-5
    'mg-6
    'mg-7
    'mg-8
  )
) ;; bind-astral-body
(bind-astral-body
  (kern-mk-astral-body
    'ord	; tag
    "Ord"	; name
    0	; distance
    540	; minutes_per_phase
    6	; minutes_per_degress
    67	; initial_arc
    7	; initial_phase
    nil	; gifc
    (list
      (list s_new_moon 0 "new")
      (list s_wax_quarter_moon 32 "1/4 waxing")
      (list s_wax_half_moon 64 "1/2 waxing")
      (list s_wax_three_quarter_moon 96 "3/4 waxing")
      (list s_full_moon 128 "full")
      (list s_wane_three_quarter_moon 96 "3/4 waning")
      (list s_wane_half_moon 64 "1/2 waning")
      (list s_wane_quarter_moon 32 "1/4 waning")
    )
  )
  (list
    'mg-1
    'mg-2
    'mg-3
    'mg-4
    'mg-5
    'mg-6
    'mg-7
    'mg-8
  )
) ;; bind-astral-body
(kern-set-wind 2 0)
(kern-add-reveal 0)
(kern-add-quicken 0)
(kern-add-time-stop 0)
(kern-add-magic-negated 0)
(kern-add-xray-vision 0)
