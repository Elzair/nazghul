(mk-tower
 'p_westpass "Westpass"
 (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ .. tt tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt || || || || || || "
      "^^ ^^ ^^ {{ bb && bb {{ ^^ ^^ {{ tt tt || || || || || || "
      "^^ ^^ {{ .. .. .. .. .. {{ ^^ {{ tt tt tt || || || || || "
      "^^ ^^ {{ .. .. .. .. .. .. {{ bb .. tt tt tt || || || || "
      "^^ ^^ bb .. .. .. .. .. .. bb {{ .. tt tt tt tt || || || "
      "^^ ^^ ^^ bb .. .. .. .. .. .. bb .. .. tt tt tt tt tt tt "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. bb .. .. .. tt tt tt tt tt "
      "^^ ^^ ^^ ^^ bb {{ .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ ^^ bb {{ .. .. .. /c /d /d /d /d /d /d /d /d /d "
      "^^ ^^ ^^ ^^ bb bb .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. bb .. .. .. tt tt tt tt tt "
      "^^ ^^ {{ .. .. .. .. .. .. .. bb .. .. tt tt tt tt tt tt "
      "^^ {{ .. .. .. .. .. bb ^^ bb {{ .. tt tt tt tt || || || "
      "^^ {{ .. .. .. .. .. {{ ^^ {{ {{ tt tt tt tt || || || || "
      "^^ {{ .. .. .. .. .. {{ ^^ ^^ {{ tt tt tt || || || || || "
      "^^ {{ {{ .. .. .. {{ ^^ ^^ ^^ {{ {{ tt || || || || || || "
      "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt || || || || || || "
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
