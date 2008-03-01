(mk-tower
 'p_westpass "Westpass"
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
              (list ; edge entrances
              	(list southwest 12 0)
              	(list northwest 12 18)
              	)
 (put (mk-ladder-down 'p_eastpass 14 9) 4 14)
 (put (guard-pt 'ranger) 9 7)
 (put (guard-pt 'ranger) 9 11)
 (put (guard-pt 'ranger) 5 4)
 (put (guard-pt 'ranger) 4 12)

 (put (spawn-pt 'forest-goblin-stalker) 11 8)
 (put (spawn-pt 'forest-goblin-hunter)  11 9)
 (put (spawn-pt 'forest-goblin-stalker) 11 10)
 )

(mk-place-music p_westpass 'ml-castle)
