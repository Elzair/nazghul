;; The occupations data file. Most characters have an occupation, which defines
;; certain attributes of the character.

;; Args to kern-mk-occ:
;;
;;              tag : qstr, to reference the occupation elsewhere in the script
;;             name : str, name for UI display
;;            magic : float, [1]
;;      hp modifier : int, additional base hit points [2]
;;      mp modifier : int, additional base mana [2]
;;    hp multiplier : int, additional hit points per level [2]
;;    mp multiplier : int, additional mana per level [2]
;;     hit modifier : int, [1]
;; defense modifier : int, [1]
;;  damage modifier : int, [1]
;;      ac modified : int, [1]
;;         xp value : int, additional xp rewarded to opponent
;;        skill set : sym, reference to occupation skill set (or nil)
;;
;; [1] Obsolete. The kern-mk-occ call requires them for backwards-compatiblity
;;     with older scripts but the engine never makes any use of them. The
;;     script can use them, if it likes.
;;
;; [2] Unused in haxima. If non-zero the engine will use them in calculating a
;;     character's max hp and mp. Haxima customizes elevation differently.

;; Define a wrapper for kern-mk-occ that fills in the obsolete and unused values.
(define (mk-occ tag name xp skills)
  (kern-mk-occ tag name 1.0 0 0 0 0 0 0 0 0 xp skills))

;; Occupations.
(mk-occ 'oc_wanderer "wanderer" 1 nil)