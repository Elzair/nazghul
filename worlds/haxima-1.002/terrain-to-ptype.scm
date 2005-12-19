;; terrain to party type

(define terrain-to-ptype-tbl
  (list
   (list t_grass 
         'forest-goblin-party-l1
         'forest-goblin-party-l2
         'bandit-party-l1
         'bandit-party-l2
         'bandit-party-l3
         'bandit-party-l4
         'bandit-party-l5
         'headless-party-l1
         'headless-party-l3
         'headless-party-l5
         'accursed-party-l4
         'accursed-party-l5
         'accursed-party-l6
         'militia-party-l4
         )
   (list t_forest 
         'forest-goblin-party-l1
         'forest-goblin-party-l3
         'forest-goblin-party-l4
         'wisp-party-l5         
         'wolf-party-l1
         'wolf-party-l2
         'dryad-party-l3      
         'dryad-party-l4      
         'dryad-party-l5
         'spider-party-l3
         'spider-party-l4
         )
   (list t_trees
         'forest-goblin-party-l1
         'forest-goblin-party-l2
         'forest-goblin-party-l3
         'wolf-party-l1
         'wolf-party-l2
         'spider-party-l3
         'spider-party-l4
         'snake-party-l1
         'bat-party-l1
         'rat-party-l1
         )
   (list t_hills 
         'troll-party-l4
         'troll-party-l3
         'gint-party-l4
         'gint-party-l5
         'gint-party-l6
         'dragon-party-l7
         'spider-party-l3
         'spider-party-l4
         )
   (list t_mountains 
         'gint-party-l4
         'dragon-party-l6
         'dragon-party-l8
         'bat-party-l1
         )
   (list t_bog
         'skeleton-party-l2
         'skeleton-party-l3
         'skeleton-party-l4
         'lich-party-l5
         'ghast-party       
         'green-slime-party-l1
         'yellow-slime-party-l3
         'hydra-party-l5
         'ghast-party
         )
   (list t_shoals
         'kraken-party-l3
         'sea-serpent-party-l3
         'nixie-party-l2
         )
   (list t_shallow
         'dragon-party-l6
         'kraken-party-l3
         'sea-serpent-party-l3
         'nixie-party-l3
         'nixie-party-l4
         )
   (list t_deep
         'dragon-party-l6
         'dragon-party-l8
         'kraken-party-l3
         'sea-serpent-party-l3
         'nixie-party-l3
         'nixie-party-l4
         )
   (list t_lava
         'dragon-party-l6
         'dragon-party-l8
         )
   ))

(define (terrain-to-ptypes kter)
  (let ((entry (assoc kter terrain-to-ptype-tbl)))
    (if (not entry)
        nil
        (cdr entry)
        )))
