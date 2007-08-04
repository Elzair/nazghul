;; terrain to party type

;; ttp-entry: each row in the subtable for a terrain in the terrain-to-ptype
;; table is one of these
(define (ttp-entry-mk ptype level occur)
  (list ptype level occur))
(define (ttp-entry-ptype ttpe) (car ttpe))
(define (ttp-entry-level ttpe) (cadr ttpe))
(define (ttp-entry-occur ttpe) (caddr ttpe))

(define terrain-to-ptype-tbl
  (list
   (list t_grass 
         (ttp-entry-mk 'forest-goblin-party-l1 1 1)
         (ttp-entry-mk 'bandit-party-l1        1 1)
         (ttp-entry-mk 'headless-party-l1      1 1)
         (ttp-entry-mk 'forest-goblin-party-l2 2 1)
         (ttp-entry-mk 'bandit-party-l2        2 1)
         (ttp-entry-mk 'bandit-party-l3        3 1)
         (ttp-entry-mk 'headless-party-l3      3 1)
         (ttp-entry-mk 'bandit-party-l4        4 1)
         (ttp-entry-mk 'accursed-party-l4      4 1)
         (ttp-entry-mk 'militia-party-l4       4 1)
         (ttp-entry-mk 'bandit-party-l5        5 1)
         (ttp-entry-mk 'headless-party-l5      5 1)
         (ttp-entry-mk 'accursed-party-l5      5 1)
         (ttp-entry-mk 'accursed-party-l6      6 1)
         )
   (list t_forest 
         (ttp-entry-mk 'forest-goblin-party-l1 1 1)
         (ttp-entry-mk 'wolf-party-l1          1 1)
         (ttp-entry-mk 'wolf-party-l2          2 1)
         (ttp-entry-mk 'dryad-party-l3         3 1)
         (ttp-entry-mk 'forest-goblin-party-l3 3 1)
         (ttp-entry-mk 'spider-party-l3        3 1)
         (ttp-entry-mk 'forest-goblin-party-l4 4 1)
         (ttp-entry-mk 'spider-party-l4        4 1)
         (ttp-entry-mk 'dryad-party-l4         4 1)
         (ttp-entry-mk 'wisp-party-l5          5 1)
         (ttp-entry-mk 'dryad-party-l5         5 1)
         )
   (list t_trees
         (ttp-entry-mk 'forest-goblin-party-l1 1 1)
         (ttp-entry-mk 'forest-goblin-party-l2 2 1)
         (ttp-entry-mk 'forest-goblin-party-l3 3 1)
         (ttp-entry-mk 'wolf-party-l1          1 1)
         (ttp-entry-mk 'wolf-party-l2          2 1)
         (ttp-entry-mk 'spider-party-l3        3 1)
         (ttp-entry-mk 'spider-party-l4        4 1)
         (ttp-entry-mk 'snake-party-l1         1 1)
         (ttp-entry-mk 'bat-party-l1           1 1)
         (ttp-entry-mk 'rat-party-l1           1 1)
         )
   (list t_hills 
         (ttp-entry-mk 'spider-party-l3 3 30)
         (ttp-entry-mk 'spider-party-l4 4 30)
         (ttp-entry-mk 'troll-party-l3  3 30)
         (ttp-entry-mk 'troll-party-l4  4 30)
         (ttp-entry-mk 'gint-party-l4   4 20)
         (ttp-entry-mk 'gint-party-l5   5 20)
         (ttp-entry-mk 'gint-party-l6   6 20)
         (ttp-entry-mk 'dragon-party-l7 7 6)
         )
   (list t_mountains 
         (ttp-entry-mk 'nil              0 100)
         (ttp-entry-mk 'bat-party-l1     1 20)
         (ttp-entry-mk 'griffin-party-l3 3 10)
         (ttp-entry-mk 'dragon-party-l6  6 1)
         (ttp-entry-mk 'dragon-party-l8  8 1)
         )
   (list t_bog
         (ttp-entry-mk 'skeleton-party-l2     2 1)
         (ttp-entry-mk 'ghast-party           2 1)  
         (ttp-entry-mk 'green-slime-party-l2  2 1)
         (ttp-entry-mk 'skeleton-party-l3     3 1)
         (ttp-entry-mk 'yellow-slime-party-l3 3 1)
         (ttp-entry-mk 'skeleton-party-l4     4 1)
         (ttp-entry-mk 'lich-party-l5         5 1)
         (ttp-entry-mk 'hydra-party-l5        5 1)
         )
   (list t_shoals
         (ttp-entry-mk 'nil                  0 4)
         (ttp-entry-mk 'nixie-party-l2       2 4)
         (ttp-entry-mk 'kraken-party-l3      3 2)
         (ttp-entry-mk 'sea-serpent-party-l3 3 1)
         )
   (list t_shallow
         (ttp-entry-mk 'nil                  0 16)
         (ttp-entry-mk 'kraken-party-l3      3 6)
         (ttp-entry-mk 'sea-serpent-party-l3 3 6)
         (ttp-entry-mk 'nixie-party-l3       3 16)
         (ttp-entry-mk 'nixie-party-l4       4 16)
         (ttp-entry-mk 'nixie-party-l5       5 16)
         (ttp-entry-mk 'dragon-party-l6      6 3)
         )
   (list t_deep
         (ttp-entry-mk 'nil                  0 96)
         (ttp-entry-mk 'kraken-party-l3      3 8)
         (ttp-entry-mk 'sea-serpent-party-l3 3 8)
         (ttp-entry-mk 'pirate-party-l3      3 8)
         (ttp-entry-mk 'pirate-party-l4      4 8)
         (ttp-entry-mk 'skeleton-pirates-l4  4 8)
         (ttp-entry-mk 'dragon-party-l6      6 3)
         (ttp-entry-mk 'dragon-party-l8      8 3)
         )
   (list t_lava
         (ttp-entry-mk 'fire-slime-party-l4 4 20)
         (ttp-entry-mk 'dragon-party-l6 6 1)
         (ttp-entry-mk 'dragon-party-l8 8 1)
         )
   ))

;;----------------------------------------------------------------------------
;; "private"
(define (terrain-to-ptypes kter)
  (let ((entry (assoc kter terrain-to-ptype-tbl)))
    (if (not entry)
        nil
        (cdr entry)
        )))

(define (ttp-list-modulus entries)
  (foldr (lambda (sum entry)
           (+ sum (ttp-entry-occur entry)))
         0
         entries))

(define (ttp-list-lookup entries n)
  (cond ((null? entries) nil)
        (else
         (if (< n (ttp-entry-occur (car entries)))
             (begin
               (eval (ttp-entry-ptype (car entries)))
               )
             (ttp-list-lookup (cdr entries)
                              (- n (ttp-entry-occur (car entries)))
                              )))))

(define (ttp-list-filter entries lvl)
  (filter (lambda (entry)
            (<= (ttp-entry-level entry) lvl))
          entries))

(define (ttp-list-select entries lvl)
  (let ((entries (ttp-list-filter entries lvl)))
    (if (null? entries)
        nil
        (let ((n (modulo (random-next) 
                         (ttp-list-modulus entries))))
          (ttp-list-lookup entries n)))))

;;----------------------------------------------------------------------------
;; public
(define (terrain-to-ptype kter lvl)
  (let ((subtable (assoc kter terrain-to-ptype-tbl)))
    (if (not subtable)
        nil
        (ttp-list-select (cdr subtable) lvl))))
