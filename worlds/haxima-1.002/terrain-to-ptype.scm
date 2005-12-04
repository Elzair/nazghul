;; terrain to party type

(define terrain-to-ptype-tbl
  (list
   (list t_grass 
         (cons 'forest-goblin-party 100)
         (cons 'bandit-party        100)
         (cons 'militia-party       5)
         )
   (list t_forest 
         (cons 'forest-goblin-party 100)
         (cons 'wisp-party          25)
         (cons 'wolf-party          100)
         (cons 'ranger-party        5)
         (cons 'dryad-party         10)
         )
   (list t_hills 
         (cons 'troll-party         100)
         (cons 'giant-party         10)
         )
   (list t_mountains 
         (cons 'dragon-party        1)
         )
   (list t_bog
         (cons 'lich-party          5)
         (cons 'skeleton-party      100)
         (cons 'ghast-party         25)
         (cons 'green-slime-party   100)
         )
   ))

(define (terrain-to-ptype kter)
  (println "terrain-to-ptype:kter=" kter)
  (println "  tbl=" terrain-to-ptype-tbl)
  (let ((entry (assoc kter terrain-to-ptype-tbl)))
    (println "terrain-to-ptype:entry=" entry)
    (if (not entry)
        nil
        (let ((x (random-select (cdr entry))))
          (if (>= (modulo (random-next) 100) (- 100 (cdr x)))
              (car x)
              nil)))))
