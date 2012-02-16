;;----------------------------------------------------------------------------
;; haxima.scm - wrapper for the essentials
;;
;; This loads almost everything needed for a minimal haxima game or test. It
;; does not load any places or npcs, but it does include all the object types,
;; spells, effects, ai's, etc. The file that loads this one still needs to
;; define the player character, player party and a place to put it.
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too. But, do NOT use kern-load WITHIN a kern-loaded file, or
;; the saved game will include the file twice, sometimes with bad effect.
;; ----------------------------------------------------------------------------
(load "naz.scm")
(kern-script-version "0.8.0")

;;----------------------------------------------------------------------------
;; Load the read-only game data.
;;----------------------------------------------------------------------------
(kern-load "game.scm")
(kern-load "camping-map.scm")
(kern-load "player.scm")

;;----------------------------------------------------------------------------
;; Time -- this needs to be set before loading any dungeon rooms
;;----------------------------------------------------------------------------
(define year 1611)
(define hour 07)
(define minutes 00)
(define time-in-minutes (+ (* hour 60) minutes))
(define game-start-time (time-mk year 0 0 0 hour minutes))

(kern-set-clock 
 year
 00 ; month
 00 ; week
 00 ; day
 hour
 minutes
 )

;; ----------------------------------------------------------------------------
;; Diplomacy Table
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;      non pla men cgb acc mon tro spd out gnt dem fgb prs gla                
 (list   2   0   0   0  -1  -2  -2  -2   0  -2  -2   0   0   0  ) ;; none
 (list   0   2   2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2   2  ) ;; player
 (list  -1   2   2  -1  -2  -2  -2  -2  -2  -2  -2  -2   2   2  ) ;; men
 (list  -1  -2  -2   2  -1  -2   0  -2  -2  -1  -2  -2   0  -2  ) ;; cave gbln
 (list  -1  -2  -1  -1   2  -2  -1  -1  -2  -1  -2  -2   0  -2  ) ;; accursed
 (list  -2  -2  -2  -2  -2   2  -2   0  -2   0  -2   0   0  -2  ) ;; monsters
 (list  -2  -2  -2   0  -1  -2   2  -2  -2  -1  -2  -1   0  -2  ) ;; hill trlls
 (list  -2  -2  -2  -2  -1   0  -2   2  -2  -1  -2   0   0  -2  ) ;; wood spdr
 (list   0  -2  -2  -2  -2  -2  -2  -2   2  -2  -2  -1   0  -2  ) ;; outlaws
 (list  -2  -2  -2  -1  -1   0  -1  -1  -2   2  -2  -1   0  -2  ) ;; gint
 (list  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2  -2   0  -2  ) ;; demon
 (list   0  -2  -2  -2  -2   0  -2   0  -1  -1  -2   2   0  -2  ) ;; forst gbln
 (list   0   2   2   0   0   0   0   0   0   0   0   0   2   2  ) ;; prisoners
 (list  -1   2   2  -1  -2  -2  -2  -2  -2  -2  -2  -2   2   2  ) ;; glasdrin
)

;;----------------------------------------------------------------------------
;; Fyer, the sun
;;
;; Needed for ambient light in non-dungeon places.
;;----------------------------------------------------------------------------
(if (defined? 'include-sun)
    (kern-mk-astral-body
     'sun              ; tag
     "Fyer (the sun)"  ; name
     1                 ; relative astronomical distance 
     1                 ; minutes per phase (n/a for sun)
     (/ (* 24 60) 360) ; minutes per degree
     0                 ; initial arc
     0                 ; initial phase
     '()               ; script interface
     ;; phases:
     (list 
      (list s_sun 255 "full"))))

;;----------------------------------------------------------------------------
;; Lumis is the source gate, which means it opens the source moongates on its
;; phases. We designate this by using the source-moon-ifc as its ifc.
;;
;; Note: the arc and phase are calculated to give the moon the right orientation
;; with respect to phase vs sun position
;;----------------------------------------------------------------------------
(if (defined? 'include-moons)
    (mk-moon 'lumis  ; tag
	     "Lumis" ; name
	     5       ; hours per phase
	     60      ; hours per revolution
	     22      ; initial arc
	     0       ; initial phase
	     'source-moon-ifc ; ifc
	     ;; gates (moons are fixed at 8 phases in mk-moon):
	     (list 'mg-1 'mg-2 'mg-3 'mg-4
		   'mg-5 'mg-6 'mg-7 'mg-8
		   )
	     "yellow"))

;;----------------------------------------------------------------------------
;; Ord is the destination gate, which means its phase decides the destination
;; when the player steps through a moongate. We designate this by giving it a
;; nil ifc. Note that its gates do not need to be listed in the same order as
;; Lumis. In fact, they don't even need to be the same set of gates.
;;
;; Note: the arc and phase are calculated to give the moon the right orientation
;; with respect to phase vs sun position
;;----------------------------------------------------------------------------
(if (defined? 'include-moons)
    (mk-moon 'ord    ; tag
	     "Ord"   ; name
	     9       ; hours per phase
	     36      ; hours per revolution
	     67     ; initial arc
	     7       ; initial phase
	     nil     ; ifc
	     ;; gates (moons are fixed at 8 phases in mk-moon):
	     (list 'mg-1 'mg-2 'mg-3 'mg-4
		   'mg-5 'mg-6 'mg-7 'mg-8
		   )
	     "blue"))
