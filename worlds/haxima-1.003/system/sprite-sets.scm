;; The sprites sets. These reference sheets of tile images that sprites point
;; into.

;; Args to kern-mk-sprite-set:
;;
;;          tag : qsym, for reference by other parts of the script
;;        width : int, of the tiles in pixels
;;       height : int, of the tiles in pixels
;;         rows : int, of tiles in the sheet
;;         cols : int, of tiles in the sheet
;;     x offset : int, x-offset in pixels of upper right corner [1]
;;     y offset : int, y-offset in pixels of upper right corner [1]
;;     filename : string, path to image file
;;
;; [1] None of the haxima sheets use a corner offset.

(kern-mk-sprite-set 'ss_characters 32 32 16 8 0 0 "images/system/characters.png")
(kern-mk-sprite-set 'ss_terrains   32 32 2  2 0 0 "images/system/terrains.png")

(kern-mk-sprite-set 'ss_crosshair 32 32 1  1 0 0 "images/system/crosshair.png")
