(mk-dungeon-room
 'p_fire_sea "Fire Sea"
 (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr !_ !_ !! {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr "
      "rr !_ !_ !_ !! {{ {{ {{ .. .. .. .. .. .. {{ {{ ~! rr rr "
      "rr !! !_ !! !! !! {{ .. .. {{ {{ {{ {{ .. {{ ~! !_ ~! rr "
      "rr {{ !! {{ {{ !! {{ .. {{ {{ !! !! {{ .. {{ {{ ~! {{ rr "
      "rr {{ {{ .. .. == .. .. {{ !! !_ !! {{ .. {{ !! {{ {{ rr "
      "rr {{ .. .. {{ !! {{ {{ !_ !_ !_ !_ {{ .. .. == .. .. rr "
      "rr {{ .. {{ {{ !_ !_ !_ rr rr rr rr !! {{ {{ !! {{ .. rr "
      "rr {{ .. {{ {{ {{ !! !_ rr .. .. rr rr !! !! !! {{ .. rr "
      "rr {{ .. .. .. .. {{ rr rr .. .. .. rr {{ !! {{ {{ .. rr "
      "rr {{ {{ {{ {{ .. {{ rr rr .. bb .. .. .. == .. .. .. rr "
      "rr {{ !! !! !! =| !! !_ rr .. .. .. rr {{ !! {{ {{ {{ rr "
      "rr !! !_ !! {{ .. {{ !_ rr rr rr .. rr rr !_ !! !! {{ rr "
      "rr {{ !! {{ {{ .. {{ !! !_ !_ rr rr rr rr !_ !_ !_ !! rr "
      "rr rr {{ {{ {{ .. {{ !! !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr "
      "rr rr rr {{ {{ .. {{ {{ !! !! !! !_ !_ !_ !_ !_ !_ !_ rr "
      "rr rr rr bb {{ .. .. {{ {{ {{ {{ !! !! !! !_ !_ !_ !_ rr "
      "rr rr rr rr bb {{ .. .. .. .. {{ {{ {{ {{ !! !_ !_ !_ rr "
      "rr rr rr rr rr rr rr rr .. .. .. {{ {{ .. bb !! !! ~! rr "
  )
 (put (kern-mk-obj t_rune_w 1) 11 12)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 8)
 (put (kern-mk-obj t_iron_helm_4 1) 10 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 9)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 10 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 10)
 (put (kern-mk-obj t_sword_4 1) 9 10)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 10)
 (put (kern-mk-obj t_armor_plate_4 1) 11 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 9 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 11)
 (put (kern-mk-obj t_shield_4 1) 10 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 11 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 12)

 (put (spawn-pt 'dragon) 11 10)
 )

(mk-dungeon-room
 'p_smoldering_cave "Smoldering Cave"
 (list
            "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
            "rr rr rr rr rr !_ !_ rr {{ .. {{ rr !_ !_ rr rr rr rr rr "
            "rr rr rr rr !_ !_ !_ !! {{ .. {{ !! !_ !_ !_ rr rr rr rr "
            "rr rr rr !_ !_ !_ !_ !_ !! =| !! !_ !_ !_ !_ !_ rr rr rr "
            "rr rr rr !_ !_ !_ !_ !! {{ .. {{ !! !_ !_ !_ !_ rr rr rr "
            "rr rr !_ !_ !_ !_ !! bb {{ .. {{ bb !! !_ !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !! bb {{ {{ .. {{ {{ bb !! !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !! {{ {{ .. .. .. {{ {{ !! !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !! {{ {{ .. .. .. {{ {{ !! !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !! {{ {{ .. .. .. {{ {{ !! !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !! bb {{ {{ {{ {{ {{ bb !! !_ !_ !_ rr rr "
            "rr rr !_ !_ !_ !_ !! bb {{ {{ {{ bb !! !_ !_ !_ !_ rr rr "
            "rr rr rr !_ !_ !_ !_ !! !! !! !! !! !_ !_ !_ !_ rr rr rr "
            "rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr "
            "rr rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr rr "
            "rr rr rr rr rr rr !_ !_ !_ !_ !_ !_ !_ rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      )
 (put (mk-ladder-up 'p_shard 118 46) 9 9)
 )

(mk-dungeon-level 
 (list p_fire_sea)
 (list p_smoldering_cave)
 )
