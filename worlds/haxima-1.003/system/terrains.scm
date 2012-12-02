;; The haxima terrains. Terrains must be registered with the engine via
;; kern-mk-terrain.
;;
;; The args to kern-mk-terrain:
;;
;;                        tag : qstr, to reference the occupation elsewhere in the script
;;                       name : str, name for UI display
;;  passability class (pclass): int, index into the passability table
;;                sprite (spr): sym, sprite to show the terrain
;;         transparency (tnsp): int, where 0 is transparent and 12 is opaque.
;;             luminance (lum): int, how much light the terrain emits
;;              on-step (step): sym, closure to run when a character steps on the terrain

;; Note: I use 'let' here to wrap the definitions of 'opq', 'hvy', etc so
;; that they are usable in the kern-mk-terrain calls but won't hang around in
;; the global environment afterward. (This saves space in the global lookup
;; table and infinitesimally reduces lookup times for other variables; a
;; perhaps pointless optimization but it makes me feel better).
(let ((opaque 12)
      (heavy 5)
      (dense 3)
      (light 2)
      (transparent 0))
  (kern-mk-terrain 't_deep      "deep water" pclass-deep      s_deep      transparent 0 nil)
  (kern-mk-terrain 't_mountains "mountains"  pclass-mountains s_mountains opaque      0 nil)
  (kern-mk-terrain 't_grass     "grass"      pclass-grass     s_grass     transparent 0 nil)
  (kern-mk-terrain 't_deck      "deck"       pclass-grass     s_deck      transparent 0 nil)
  )