;;----------------------------------------------------------------------------
;; item.scm - utility procs for creating types that support the 'use' method in
;; addition to the default 'get' method
;;----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; use-item -- use an item and decrement the action points, unless the usage
;; result is an abort code (currently I think result-no-target is the only such
;; case). The 'use' proc should return one of the result-* codes.
;; ----------------------------------------------------------------------------
(define (use-item ktype kuser use ap)
  (let ((result (use ktype kuser)))
    (if (not (abortive-result? result))
        (kern-obj-dec-ap kuser ap))
    result))

;; ----------------------------------------------------------------------------
;; use-and-remove-item -- use an item type and remove it from the user's
;; inventory.  The 'use' proc should return one of the standard result-* codes.
;; ----------------------------------------------------------------------------
(define (use-and-remove-item ktype kuser use ap)
  (let ((result (use-item ktype kuser use ap)))
    (if (not (abortive-result? result))
        (kern-obj-remove-from-inventory kuser ktype 1))
    result))

;; ----------------------------------------------------------------------------
;; mk-usable-item -- make a type for an object that can be U)sed by the
;; player. The use-proc parm should be a procedure which takes the object and
;; the user as parameters. ap is the action points required to use the item.
;; ----------------------------------------------------------------------------
(define (mk-usable-item tag name sprite ap use-proc)
  (let ((item-ifc (ifc obj-ifc 
                       (method 'use 
                               (lambda (ktype kuser) 
                                 (use-and-remove-item ktype 
                                                      kuser 
                                                      use-proc 
                                                      ap))))))
    (mk-obj-type tag name sprite layer-item item-ifc)))

;; ----------------------------------------------------------------------------
;; mk-usable-clingy-item -- extension to mk-usable-item that gives an item
;; the ability to cause nearby npc's that want the item to get the item
;; ----------------------------------------------------------------------------
(define (mk-usable-clingy-item tag name sprite ap use-proc wants-it?)
  (let ((item-ifc (ifc obj-ifc 
                       (method 'use 
                               (lambda (ktype kuser) 
                                 (use-and-remove-item ktype 
                                                      kuser 
                                                      use-proc 
                                                      ap)))
                       (method 'exec
                               (lambda (kitem)
                                 (if (not (kern-place-is-wilderness? (loc-place (kern-obj-get-location kitem))))
                                     (let ((kchars (filter (lambda (kchar)
                                                             (and (wants-it? kchar)
                                                                  (not (has-ap-debt? kchar))
                                                                  (not (is-player-party-member? kchar))))
                                                           (get-beings-in-range kitem 
                                                                                1))))
                                       (if (notnull? kchars)
                                           (kobj-get kitem 
                                                     (car kchars))))))))))
    (mk-obj-type tag name sprite layer-item item-ifc)))
  
                                              

;; ----------------------------------------------------------------------------
;; mk-reusable-item -- make a type for an object that can be U)sed by the
;; player any number of times.
;; ----------------------------------------------------------------------------
(define (mk-reusable-item tag name sprite ap use-proc)
  (let ((item-ifc (ifc obj-ifc 
                       (method 'use 
                               (lambda (ktype kuser) 
                                 (use-item ktype kuser use-proc ap))))))
    (mk-obj-type tag name sprite layer-item item-ifc)))

(load "potions.scm")
(load "books.scm")
(load "tools.scm")
(load "scrolls.scm")
(load "keys.scm")
