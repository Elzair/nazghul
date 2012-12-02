;; The standard glyph palette for ascii maps. Terrain maps are represented as
;; lists of two-character strings called glyphs. Each glyph corresponds to a
;; terrain type. The conversion of glyphs to terrains is done via lookup table
;; called a palette. This file defines the standard lookup table used by all
;; maps in haxima.
;;
;; (Note that each map specifies which palette to use, so that one have
;; multiple palettes, even ones with glyphs that have other than two
;; characters, but in practice this has feature never been used.)
;;
;; Palettes muse be registered with the kernel via kern-mk-palette. It's args
;; are:
;;
;;     tag: so maps can reference it
;; entries: list, where each entry is a two-element list of:
;;     glyph: str, string representing the terrain
;;   terrain: sym, the terrain reference

(kern-mk-palette 'pal_expanded
		 (list
		  (list "__" t_deep)
		  (list "^^" t_mountains)
		  (list ".." t_grass)
		  (list "ee" t_deck)
		  ))
