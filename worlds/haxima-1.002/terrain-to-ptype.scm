;; terrain to party type

(define terrain-to-ptype-tbl
  (list
   (list t_grass 'forest-goblins)
   (list t_forest 'forest-goblins)
   ))

(define (terrain-to-ptype kter)
  (println "terrain-to-ptype:kter=" kter)
  (println "  tbl=" terrain-to-ptype-tbl)
  (let ((entry (assoc kter terrain-to-ptype-tbl)))
    (println "terrain-to-ptype:entry=" entry)
    (if (not entry)
        nil
        (random-select (cdr entry)))))
