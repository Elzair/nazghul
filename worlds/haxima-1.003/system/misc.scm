;; This file loads miscellaneous one-off types of things that don't fit in any
;; general category. It is loaded automatically for every save file or on new
;; game. Because of this, it does not need to use the normal convention for
;; creating named sprites and sprite sets. Specifically, it does not need to
;; define them with names, since they do not ever need to be referenced outside
;; of where they are used here. By keeping them "anonymous" they do not take up
;; space in the global symbol table and are more self-contained within this
;; single definition file.

;; The crosshair cursor used for targeting.
(let* ((sprite (sprite-from-image "images/system/crosshair.png"))
       (type (mk-obj-type nil "crosshair" sprite layer-crosshair nil)))
  (kern-set-crosshair type))

;; The damage sprite. This is flashed over a character when it takes damage.
(kern-set-damage-sprite (sprite-from-image "images/system/damage.png"))
