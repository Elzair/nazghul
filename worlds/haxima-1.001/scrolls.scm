;; ============================================================================
;; scrolls.scm -- useable scrolls, most of which just invoke existing spells.
;; Thus, spells.scm needs to be included before this. It also uses procedures
;; defined in items.scm, so that also needs to precede this.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Scrolls
;; ----------------------------------------------------------------------------
(mk-usable-item 'poison-bolt-scroll-type "In Nox Por scroll" s_scroll_spell_29 
                in-nox-por)
(mk-usable-item 'death-bolt-scroll-type  "Xen Corp scroll"   s_scroll_spell_25 
                xen-corp)
