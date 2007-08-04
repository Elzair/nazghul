;; mimic.scm -- define a mechanism which looks like a chest, but when opened it
;; spawns a hostile mimic npc

;; Spawn a mimic npc where the mimic mechanism is and remove the mechanism.
(define (mimic-open kobj kchar)
  (kern-obj-put-at (mk-npc 'mimic (calc-level))
                   (kern-obj-get-location kobj))
  (kern-obj-remove kobj)
  )

;; The trap detection handlers are nops
(define (mimic-get-traps kobj) nil)
(define (mimic-rm-traps kobj) nil)

;; Emulate the container-ifc, including the trap detection signal handlers,
;; otherwise An Sanct will give the mimic away.
(define mimic-ifc
  (ifc '()
       (method 'open mimic-open)
       (method 'get-traps mimic-get-traps)
       (method 'rm-traps mimic-rm-traps)
       ))

(mk-obj-type 't_mimic "chest" s_chest layer-mechanism mimic-ifc)

(define (mk-mimic)
  (kern-mk-obj t_mimic 1))