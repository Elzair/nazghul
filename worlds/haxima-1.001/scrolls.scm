;; ============================================================================
;; scrolls.scm -- useable scrolls, most of which just invoke existing spells.
;; Thus, spells.scm needs to be included before this.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; use-and-remove -- use an item type and remove it from the user's inventory
;; ----------------------------------------------------------------------------
(define (use-and-remove ktype kuser use)
  (use ktype kuser)
  (kern-obj-remove-from-inventory kuser ktype 1))

;; ----------------------------------------------------------------------------
;; mk-scroll -- make a scroll. The usage parm should be a procedure which takes
;; the scroll and the user as parameters.
;; ----------------------------------------------------------------------------
(define (mk-scroll tag name sprite usage)
  (let ((scroll-ifc (ifc obj-ifc 
                         (method 'use 
                                 (lambda (ktype kuser) (use-and-remove ktype kuser usage))))))
    (mk-obj-type tag name sprite layer-item scroll-ifc)))

;; ----------------------------------------------------------------------------
;; Scrolls
;; ----------------------------------------------------------------------------
(mk-scroll 'poison-bolt-scroll-type "In Nox Por scroll" s_scroll_spell_29 in-nox-por)
