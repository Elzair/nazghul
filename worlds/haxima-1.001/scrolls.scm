;; ============================================================================
;; scrolls.scm -- useable scrolls, most of which just invoke existing spells.
;; Thus, spells.scm needs to be included before this.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; The only purpose of this list is to prevent the scheme gc from harvesting
;; the scroll interfaces which are created on-the-fly in mk-scroll. Without
;; this I'd have to explicitly assign a variable to each ifc, which is
;; needlessly verbose.
;; ----------------------------------------------------------------------------
(define scroll-ifcs '())

;; ----------------------------------------------------------------------------
;; poison bolt scroll
;; ----------------------------------------------------------------------------
(define poison-bolt-scroll-ifc
  (ifc obj-ifc
       (method 'use in-nox-por)))

(mk-obj-type 'poison-bolt-scroll-type "In Nox Por scroll" s_scroll_spell_29 layer-item 
             poison-bolt-scroll-ifc)
