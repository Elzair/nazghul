;;----------------------------------------------------------------------------
;; Oparine
;;
;; Main trade port for the peninsula.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_oparine 64 64 pal_expanded
 (list
            "tt tt tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. xx xx xx xx xx .W .A .R .E .H .O .U .S .E xx xx xx xx xx xx .. .. .. .. .. ~~ ~~ -- -- __ __ __ __ __ __ __ "
            "tt tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb .. cc cc cc .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. ~~ ~~ ~~ -- -- -- -- __ __ __ __ "
            "tt .. .. xx xx xx xx tt .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. .. ~~ ~~ ~~ ~~ ~~ -- -- -- -- __ "
            ".. .. xx xx ,, ,, xx tt tt tt .. .. .. .. .. .. .. .. .. .. .. .. .. cc cc cc .. .. xx ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, xx cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee "
            ".. .. xx && ,, ,, ,, .. .. tt tt .. .. .. .. .. .. .. .. .. .. bb .. cc cc cc .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee "
            ".. .. xx xx ,, ,, xx tt .. xx xx xx xx xx xx xx xx xx xx .. .. .. .. cc cc cc .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx cc cc cc cc cc xx xx xx xx xx xx xx xx xx ee ee "
            ".. .. .. xx xx ws xx tt .. xx .C .H .E .E .R .F .U .L xx .. .. .. .. cc cc cc .. .. xx xx xx ,, ,, ,, xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx cc cc cc cc cc oo .L .O .A .D .I .N .G oo ee ee "
            ".. .. .. .. .. .. tt tt .. xx .B .U .N .K .M .A .T .E xx .. .. bb .. cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. tt .. ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. tt tt xx [[ @@ @@ @@ @@ @@ @@ ]] xx tt bb .. tt cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. .. tt xx ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt tt tt cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. .. .. ws ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc oo [[ .D .O .C .K .S ]] oo ee ee "
            ".. .. .. .. .. .. .. .. .. xx xx xx x! ,, ,, x! xx xx xx tt tt tt tt cc .. .. .. .. .. xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx ws xx xx cc cc cc xx xx xx xx xx xx xx xx xx ee ee "
            ".. .. .. .. .. .. .. .. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx tt bb .. tt cc tt .. .. .. tt xx @@ @@ .B .L .A .C .K @@ .B .A .R .T .S @@ @@ xx ,, ,, ,, xx cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. .. .. ws ,, ,, ,, ,, ,, xx ,, ,, ws .. .. .. .. cc tt tt bb tt tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ws cc cc cc ee ee ee ee ee ee ee ee ee ee ee "
            ".. .. .. .. .. .. .. .. .. xx xx xx xx ,, ,, xx xx xx xx .. .. .. .. cc cc cc cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. ~~ ~~ ~~ ~~ ~~ -- -- -- -- __ "
            "tt tt tt tt tt tt tt tt .. xx ,, ,, xx ,, ,, ,, ,, ,, xx .. .. bb .. cc tt tt tt tt tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx .. .. .. .. ~~ ~~ ~~ -- -- -- -- __ __ __ "
            "tt tt tt tt tt tt tt tt tt ws ,, ,, ,, ,, ,, xx ,, ,, ws .. .. .. .. cc tt .. bb .. tt ws ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. ~~ ~~ ~~ -- -- __ __ __ __ __ "
            "tt tt tt ~~ ~~ ~~ tt tt tt xx xx xx xx xx ,, xx xx xx xx .. .. .. .. cc .. .. .. .. .. xx ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee xx ,, ,, ,, xx .. .. .. ~~ ~~ ~~ -- -- -- __ __ __ __ __ "
            "tt tt tt tt tt ~~ ~~ tt tt tt tt tt xx ,, ,, xx .. .. .. .. .. bb .. cc .. .. .. .. .. xx ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee xx ,, ,, ,, xx .. .. ~~ ~~ -- -- -- -- __ __ __ __ __ __ "
            "tt tt tt tt tt tt ~~ ~~ tt tt tt tt xx ,, ,, xx .. .. .. .. .. .. .. cc .. bb .. .. .. xx ee ee __ __ __ __ __ __ __ __ __ __ __ ee ee xx ,, ,, ,, xx ~~ ~~ ~~ -- -- -- -- __ __ __ __ __ __ __ "
            "tt tt tt tt tt tt tt ~~ tt tt tt tt xx xx xx xx tt tt tt tt .. .. .. cc .. .. .. .. .. ws ee ee __ __ __ __ __ __ __ __ __ __ __ ee ee xx ,, ,, ,, xx ~~ ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ "
            "tt tt tt tt tt ~~ ~~ ~~ ~~ ~~ tt tt tt tt tt tt ~~ ~~ ~~ tt tt bb .. cc .. .. .. .. .. xx ee ee __ __ __ __ ## ## ## __ __ __ __ ee ee xx ?? xx xx xx .. ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ "
            "tt tt tt ~~ ~~ ~~ tt tt tt ~~ ~~ ~~ tt tt tt tt tt tt ~~ ~~ tt tt tt cc .. bb .. .. .. xx ee ee __ __ __ ## ## ee ## ## __ __ __ ee ee xx ,, ,, ,, xx .. .. ~~ -- -- __ __ __ __ __ __ __ __ __ "
            "tt tt tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ tt tt tt tt ~~ ~~ ~~ ~~ cc tt tt .. .. .. xx ee ee __ __ __ ## ee ee ee ## __ __ __ ee ee xx xx xx ?? xx .. .. ~~ -- -- __ __ __ __ __ __ __ __ __ "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt ~~ tt tt tt ~~ ~~ ~~ bb ~~ == ~~ ~~ ~~ .. ~~ -- ee ee __ __ ## ## ee ee ee ## ## __ __ __ __ .. .. .. .. .. .. .. -- -- -- __ __ __ __ __ __ __ __ __ "
            "tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ tt ~~ ~~ ~~ cc ~~ bb ~~ ~~ -- -- ee ee __ __ ## ee ee ee ee ee ## __ __ __ __ __ __ .. .. .. .. __ -- -- __ __ __ __ __ __ __ __ __ __ "
            "tt tt tt tt tt tt tt ~~ ~~ ~~ tt tt ~~ tt tt tt tt tt tt tt tt tt tt cc ~~ ~~ ~~ -- -- -- ee ee ee ee ee ee ee oo ee ee ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            "tt tt .. .. .. .. tt tt tt tt tt ~~ ~~ tt tt .. .. .. .. .. .. bb .. cc tt tt ~~ -- -- -- ee ee __ __ ## vv vv ee ee vv ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            "tt .. .. .. .. .. .. .. .. .. tt tt tt tt .. .. .. .. .. .. .. .. .. cc .. bb .. ~~ -- -- ee ee __ __ ## ee ee ee ee ee ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            "bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. bb tt cc .. .. .. ~~ -- -- -- __ __ __ ## vv vv vv vv vv ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt cc .. .. .. ~~ ~~ -- -- __ __ __ ## ee ee oo ee ee ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. bb .. .. ~~ -- -- __ __ __ ## ## vv vv vv ## ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt cc .. .. .. .. ~~ -- -- -- __ __ __ ## ee ee ee ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. bb .. .. tt cc .. .. .. .. ~~ -- -- -- __ __ __ ## ## vv ## ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. cc .. bb .. .. ~~ ~~ -- -- __ __ __ __ ## ## ## __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx xx xx xx xx xx xx xx xx xx xx xx xx .. .. .. .. .. cc .. .. .. .. .. ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx ,, ,, xx .B .I .L .G .E xx ,, ,, xx .. .. .. bb .. cc .. .. .. .. .. ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx ,, ,, xx .W .A .T .E .R xx ,, ,, xx .. .. .. .. .. cc .. bb .. .. .. ~~ -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. cc .. .. .. .. ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx xx xx xx [[ @@ @@ @@ ]] xx xx ,, xx .. .. .. bb .. cc .. .. .. .. ~~ -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. cc .. bb .. .. ~~ -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. .. .. .. .. ws ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ws .. .. .. .. .. cc .. .. .. .. ~~ ~~ -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
            ".. xx xx xx xx xx @@ @@ @@ ,, ,, ,, ,, ,, @@ @@ @@ xx xx .. .. bb .. cc .. .. .. .. .. ~~ -- -- -- -- -- -- -- __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- __ __ __ "
            ".. xx ,, ,, xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. .. .. .. cc .. bb .. .. .. ~~ ~~ -- -- -- -- -- -- -- -- -- __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- -- -- __ __ "
            ".. xx ,, ,, xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. .. .. .. cc .. .. .. .. .. .. ~~ ~~ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ -- -- -- -- -- __ "
            ".. xx xx ~x xx xx @@ @@ @@ ,, ,, ,, ,, ,, @@ @@ @@ xx xx .. .. bb .. cc .. .. .. .. .. tt tt ~~ ~~ ~~ ~~ ~~ -- -- -- -- -- -- -- -- -- -- ~~ ~~ ~~ tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ -- -- __ "
            ".. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ws .. .. .. .. .. cc .. bb .. .. tt tt tt tt tt tt tt ~~ ~~ -- -- -- ~~ -- -- ~~ ~~ ~~ ~~ tt tt tt tt tt tt tt tt tt tt tt tt tt ~~ -- -- -- "
            ".. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. cc tt .. .. .. tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ tt tt tt tt xx xx xx ws xx xx xx ws xx xx xx tt ~~ -- -- -- "
            ".. xx ,, xx xx xx xx xx xx xx xx ,, xx xx ws xx xx xx .. .. bb .. .. cc tt tt tt tt tt tt tt tt tt tt tt tt tt tt ~~ ~~ bb ~~ tt tt tt tt tt tt xx .S .E .A @@ .W .I .T .C .H xx tt ~~ ~~ -- -- "
            ".. xx ,, xx ,A ,L ,K ,E ,M ,Y xx cc tt tt .. .. .. .. .. .. .. .. tt cc tt tt bb tt tt tt tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ tt tt tt tt tt tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx tt tt ~~ -- -- "
            ".. xx ,, xx ,, ,, ,, ,, ,, ,, xx cc tt .. .. .. .. .. .. .. .. tt tt cc .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ tt tt .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ws tt tt ~~ -- -- "
            "tt xx ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc cc cc cc cc tt tt tt tt .. .. .. .. .. bb tt tt tt bb tt tt tt ~~ bb ~~ tt bb .. tt xx ,, ,, [[ @@ @@ @@ ]] ,, ,, xx tt tt ~~ -- -- "
            "tt xx xx xx ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. .. .. .. .. .. tt tt cc tt tt bb tt tt tt tt bb .. .. .. .. .. .. .. .. .. == .. ~~ tt .. .. tt xx xx xx ,, ,, ,, ,, ,, xx xx xx tt tt ~~ -- -- "
            "tt tt tt xx ,, ,, aa && ,, ,, xx .. .. .. .. .. .. .. .. .. bb .. tt cc tt .. tt tt tt tt tt tt tt tt tt tt tt bb tt ~~ ~~ ~~ .. == .. .. bb tt tt tt xx xx xx ,, xx xx xx tt tt tt tt ~~ -- -- "
            "tt tt tt xx ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. .. .. .. .. .. .. .. cc .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt ~~ bb ~~ ~~ ~~ tt tt tt tt tt tt xx ,, ,, ,, ,, ,, xx tt tt tt tt ~~ -- -- "
            "tt tt tt xx ,, ,, ,, ,, ,, ,, xx .. .. .. .. .. .. .. .. .. .. .. .. cc .. bb .. .. .. tt tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ bb ~~ tt tt tt tt tt tt xx ,, ,, ,, ,, ,, ws tt tt tt tt ~~ -- -- "
            "tt tt tt xx @@ @@ @@ @@ @@ @@ xx .. .. .. .. .. .. .. .. .. .. bb .. cc .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt ~~ ~~ ~~ ~~ tt tt tt tt tt tt xx xx xx ,, xx xx xx tt tt tt ~~ ~~ -- -- "
            "tt tt tt xx xx xx xx xx xx xx xx tt tt tt tt .. .. .. .. .. .. .. .. cc .. .. .. .. .. .. tt tt tt tt tt tt tt tt ~~ ~~ ~~ -- ~~ ~~ tt tt tt tt tt tt tt tt tt .. tt tt tt tt tt tt ~~ ~~ -- -- "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. .. .. .. cc .. bb .. .. .. tt tt tt tt tt tt tt tt ~~ ~~ -- -- -- -- ~~ ~~ tt tt tt tt tt tt tt .. .. .. tt tt tt ~~ ~~ ~~ ~~ -- -- "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. .. .. cc .. .. .. .. tt tt tt tt tt tt tt ~~ ~~ ~~ -- -- -- -- -- -- ~~ ~~ ~~ tt tt tt tt .. .. .. .. .. tt ~~ ~~ ~~ ~~ -- -- -- "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. bb .. cc .. .. .. tt tt tt tt tt tt tt ~~ ~~ -- -- -- -- -- -- -- -- -- -- ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ bb ~~ ~~ ~~ -- -- -- -- -- -- "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. .. cc .. bb .. tt tt tt tt tt ~~ ~~ ~~ -- -- -- -- __ __ -- -- -- -- -- -- -- -- -- -- -- ~~ ~~ ~~ -- -- -- -- -- -- -- -- -- "
            "tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. .. .. .. cc .. .. .. tt tt tt tt tt ~~ -- -- -- -- -- __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- __ "
            ))

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "alchemist.scm")
(mk-alchemist)

(kern-load "oscar.scm")
(mk-oscar)

(kern-load "henry.scm")
(mk-henry)

(kern-load "bart.scm")
(mk-bart)

(kern-load "lia.scm")
(mk-lia)

(kern-load "fing.scm")
(mk-fing)

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_oparine     ; tag
 "Oparine"      ; name
 s_town          ; sprite
 m_oparine      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects

  ;; npc's
  (put ch_alchemist 0 0)
  (put ch_oscar 0 0)
  (put ch_henry 0 0)
  (put ch_bart 0 0)
  (put ch_lia 0 0)
  (put ch_fing 63 63)

  ;; inn
  (put (mk-locked-door) 9 8)
  (put (kern-tag 'oparine-inn-room-1-door (mk-locked-door)) 12 14)
  (put (mk-locked-door) 12 17)
  (put (mk-magic-locked-door) 14 18)
  (put (mk-locked-door) 15 16)
  (put (mk-locked-door) 15 13)
  (put (mk-windowed-door) 18 11)
  (put (mk-bed) 10 13)
  (put (mk-bed) 10 16)
  (put (mk-bed) 13 19)
  (put (mk-bed) 17 16)
  (put (mk-bed) 17 13)

  ;; tavern
  (put (mk-locked-door) 16 40)
  (put (mk-windowed-door) 11 49)
  (put (mk-door) 8 39)
  (put (mk-door) 14 39)

  ;; inn-keeper's house
  (put (mk-door) 6 4)

  ;; sea witch
  (put (mk-windowed-door) 48 51)
  (put (mk-locked-door) 53 57)
  (put (mk-door) 53 54)

  ;; black bart's
  (put (mk-windowed-door) 29 15)
  (put (mk-windowed-door) 45 15)
  (put (mk-locked-door) 45 17)


  ;; Alkemysts
  (put (mk-door) 10 52)
  (put (mk-magic-locked-door) 3 52)
  )


 nil ; hooks

 (list  ;; edge entrances
  (list south 24 0) 
  (list north 23 63)
  (list west  63 9)
  )
)
