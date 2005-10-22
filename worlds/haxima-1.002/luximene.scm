;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define lux-lvl 7)
(define lux-species sp_ghast)
(define lux-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (lux-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (lux-hail knpc kpc)
  (say knpc "[You meet a shade who burns with malice] What do you want, "
       "mortal?"))

(define (lux-default knpc kpc)
  (say knpc "Do not waste my time."))

(define (lux-name knpc kpc)
  (say knpc "I am Luximene the Great, Lord of the Middle Realm."))

(define (lux-rune knpc kpc)
  (say knpc "[He suddenly looks disturbed] That is a key, one of eight, "
       "made by the Wise to lock the Demon Gate."))

(define (lux-gate knpc kpc)
  (say knpc "Ask a wizard, not a king."))

(define lux-conv
  (ifc basic-conv

       ;; basics
       (method 'default lux-default)
       (method 'hail lux-hail)
       (method 'name lux-name)
       (method 'rune lux-rune)
       (method 'gate lux-gate)

       ))

(define (mk-luximene)
  (bind 
   (kern-mk-char 
    'ch_lux           ; tag
    "Luximene"             ; name
    lux-species         ; species
    lux-occ              ; occ
    s_ghost     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp lux-species lux-occ lux-lvl 0 0) ; hp
    0                   ; xp
    (max-mp lux-species lux-occ lux-lvl 0 0) ; mp
    lux-lvl
    #f               ; dead
    'lux-conv         ; conv
    sch_my           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (lux-mk)))
