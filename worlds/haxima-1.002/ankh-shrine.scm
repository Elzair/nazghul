(kern-mk-map 
 'm_ankh_shrine 19 19 pal_expanded
 (list
	"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn  "
	"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn  "
	"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn  "
	"rn rn rn rn rn r8 r8 r8 r8 r8 r8 r8 r8 r8 rn rn rn rn rn  "
	"rn rn rn rn rc *3 *1 *1 *1 *1 *1 *1 *1 *5 ra rn rn rn rn  "
	"rn rn rn r4 *3 ** *. *8 *8 *8 *8 *8 ** *. *5 r2 rn rn rn  "
	"rn rn rn r4 *2 ** xx xx xx xx xx xx xx ** *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *4 xx pp ,, ,, ,, pp xx *2 *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *4 xx ,, cc cc cc ,, xx *2 *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *4 xx ,, cc cc cc ,, xx *2 *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *4 xx ,, cc cc cc ,, xx *2 *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *4 xx pp ,, cc ,, pp xx *2 *4 r2 rn rn rn  "
	"rn rn rn r4 *2 *. xx xx ,, cc ,, xx xx *. *4 r2 rn rn rn  "
	"rn rn rn r4 *a *. ** *5 ,, cc ,, *3 ** *. *c r2 rn rn rn  "
	"rn rn rn rn r5 *a ** *4 ,, cc ,, *2 *. *c r3 rn rn rn rn  "
	"rn rn rn rn rn r5 ** *4 ,, cc ,, *2 *. r3 rn rn rn rn rn  "
	"rn rn rn rn rn r4 *2 *4 ,, cc ,, *2 *4 r2 rn rn rn rn rn  "
	"rn rn rn rn rn r4 *2 *4 ,, cc ,, *2 *4 r2 rn rn rn rn rn  "
	"rn rn rn rn rn r4 ** ** ,, cc ,, ** *. r2 rn rn rn rn rn  "
  ))


;;----------------------------------------------------------------------------
;; Include files
;;----------------------------------------------------------------------------
(kern-load "talking-ankh.scm")
(kern-load "demon-gate.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_ankh_shrine     ; tag
 "Ankh Shrine"      ; name
 s_shrine      ; sprite
 m_ankh_shrine      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects
  (put (mk-talking-ankh) 9 9)
  )
  nil ; hooks
 (list  ;; edge entrances
  (list east  9 18)
  (list south 9 18) 
  (list north 9 18)
  (list west  9 18)
  )
 )

(mk-place-music p_ankh_shrine 'ml-outdoor-adventure)

;; Make the actual get location to the west
(kern-mk-map
 'm_demon_gate 19 19 pal_expanded
 (list
                "*2 ** ** *. *. *. dd .. tb || || || td .. %% .. tt || || "
                "*2 ** *. *. ** *. *5 dd t% ta || tc t# %3 %% %5 t% ta || "
                "*2 *. *. *. *. *. ** *d dd tD te t# %3 %% _7 %% %5 t% tt "
                "*2 *. *. *c dd dd *e dd .. t7 t# %3 %% _b __ _d %% %5 .. "
                "*2 *c dd dd .. .. dd .. t. tt .. %a %% %% _e %% %% %c .. "
                "*2 dd .. .. ar t. .. t. ar t. t. .. %a %% %% %% %c tC tt "
                "*6 dd .. t. t. .. tf .. t. |. t. t. .. %a %% %c tC t3 || "
                "*. dd ar .. tt .. bb .. .. t. ar t. t. .. %% .. t3 || || "
                "*. dd .. tf bb t. .. t. .. .. t. |. t. .. %% .. te .. t. "
                "*. *d dd .. t. .. {f .. t. .. .. tt .. %b gg %d .. t. .. "
                "*. dd .. tf .. t. .. t. .. .. t. |. t. .. %% .. .. .. t. "
                "*. dd ar tD t7 .. t. bb .. t. ar t. t. .. gg .. tb || || "
                "*6 dd .. t3 tc tA .. .. t. |. t. tt t. .. gg %5 tD tt || "
                "*6 dd .. tt ar t7 .. t. ar t. tt t& tt t. tt gg t3 tt || "
                "*6 dd .. te .. te tA .. t. tt t# dd .. t. tt gg tt tt || "
                "*6 dd dd .. dd t% tb tt tt t# dd *7 dd .. te .. ta || || "
                "*2 dd dd dd *7 dd .. .. .. dd *b ** *d dd .. dd t% ta || "
                "*2 *5 dd *3 ** *5 dd dd .. .. dd *e dd .. dd *7 dd t% tt "
                "*2 *. *. *. *. ** *. *d dd .. .. dd .. dd *b *8 *d dd .. "
  )
 )

(kern-mk-place 
 'p_demon_gate  ; tag
 "Demon Gate"   ; name
 nil     ; sprite
 m_demon_gate  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 nil
 
 ;; objects
 (list
  (put (mk-demon-gate) 0 0)
  )

 nil ; hooks
 nil ; edge entrances
 )

(mk-dungeon-level 
 (list p_demon_gate p_ankh_shrine)
 )

(mk-place-music p_demon_gate 'ml-battle-music)
