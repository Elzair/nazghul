(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map 'm_moongate_clearing 23 28 pal_expanded
    (list
                 "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ";  //  0
                 "^^ {{ {{ {{ ^^ ^^ ^^ ^^ tt tt tt || || ^^ ^^ ^^ {{ {{ {{ ^^ {{ {{ ^^ ";  //  1
                 "^^ {{ {{ {{ {{ ^^ tt tt tt || tt tt || || ^^ ^^ {{ ^^ {{ ^^ ^^ {{ ^^ ";  //  2
                 "^^ {{ {{ {{ {{ {{ tt || || || || tt || || || ^^ || ^^ {{ {{ ^^ {{ ^^ ";  //  3
                 "^^ ^^ {{ {{ {{ tt || || || || tt tt tt || || || || ^^ ^^ {{ {{ {{ ^^ ";  //  4
                 "^^ ^^ ^^ ^^ tt tt || || || tt tt tt tt tt tt || || || ^^ ^^ {{ ^^ ^^ ";  //  5
                 "^^ ^^ ^^ ^^ || || || || tt tt tt b tt tt tt || || || || ^^ ^^ ^^ ^^ ";  //  6
                 "^^ ^^ ^^ ^^ || || || tt tt b .. .. .. b tt tt || || || || ^^ ^^ || ";  //  7
                 "^^ ^^ ^^ ^^ || || tt tt tt tt .. .. .. tt tt tt tt tt || || || || || ";  //  8
                 "^^ ^^ || || || || tt tt b .. .. .. .. .. b tt tt tt tt tt tt tt tt ";  //  9
                 "^^ || || || || || tt tt .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt ";  // 10
                 "{{ || || || || || tt b .. .. .. .. .. .. .. b tt tt tt tt tt tt {{ ";  // 11
                 "{{ {{ || || || tt tt tt .. .. .. .. .. .. .. tt tt tt {{ {{ tt {{ {{ ";  // 12
                 "^^ {{ {{ {{ {{ tt tt tt b .. .. .. .. .. b tt tt {{ {{ {{ {{ {{ {{ ";  // 13
                 "^^ ^^ ^^ {{ {{ {{ tt tt tt tt .. .. .. tt tt tt {{ ^^ ^^ {{ {{ ^^ ^^ ";  // 14
                 "^^ ^^ ^^ ^^ {{ {{ {{ tt tt b .. .. .. b tt tt {{ ^^ ^^ ^^ {{ {{ ^^ ";  // 15
                 "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. tt tt tt tt {{ ^^ ^^ {{ {{ ^^ ";  // 16
                 "^^ .. .. .. .. ^^ {{ {{ tt b .. .. .. b tt tt {{ {{ {{ {{ {{ ^^ ^^ ";  // 17
                 "^^ .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt {{ ^^ ^^ ^^ ";  // 18
                 "^^ .. .. .. .. ^^ {{ {{ tt b .. .. .. b tt b tt b tt tt ^^ ^^ ^^ ";  // 19
                 "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ ";  // 20
                 "^^ ^^ ^^ ^^ {{ {{ {{ {{ tt b .. .. .. .. .. .. .. .. b tt ^^ ^^ ^^ ";  // 21
                 "^^ ^^ {{ {{ {{ {{ || tt tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ ";  // 22
                 "^^ {{ {{ {{ tt || || || tt tt b tt b tt b .. .. .. b tt tt ^^ ^^ ";  // 23
                 "^^ {{ tt tt tt tt || tt tt tt tt tt tt tt tt .. .. .. tt tt tt tt ^^ ";  // 24
                 "^^ tt tt tt ^^ tt tt tt tt || tt tt tt tt b .. .. .. b tt tt tt tt ";  // 25
                 "^^ ^^ tt ^^ ^^ ^^ tt tt || || || tt tt tt .. .. .. .. .. tt tt tt tt ";  // 26
                 "^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt || tt tt tt b .. .. .. .. .. b tt tt tt ";  // 27
    )
  )
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil
  ;; contents
  nil
  (list
  )
  (list  ;; edge entrances
   (list north 16 27)
   (list east  0  11)
   (list west 22 10)
   )
) ;; end of place p_moongate_clearing
