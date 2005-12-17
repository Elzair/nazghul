;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define ankh-lvl 9)
(define ankh-species sp_statue)
(define ankh-occ nil)

(kern-mk-map
 'm_hidden_city 31 31 pal_expanded
 (list
  ".. ** ** ** ** ** ** xx xx ** ** xx [[ ,W ,E ,L ,C ,O ,M ,E ]] xx ** xx xx ** ** ** ** ** ** "
  ".. .. ** ** ** ** ** xx [[ ,T ,O @@ ,T ,H ,E cc cc cc ,S ,H ,A ,R ,D ]] xx ** ** ** ** ** ** "
  ".. .. .. ** ** ** ** .. .. .. .. ** cc cc cc cc ar cc cc cc cc .. ** xx xx xx xx xx xx xx xx "
  "xx xx xx xx xx xx xx xx xx .. ** ** cc ar cc cc cc cc cc ar cc .. .. xx ,G ,U ,A ,R ,D ,S xx "
  "xx ,C ,U ,S ,T ,O ,M ,S xx .. ,, ** cc cc ,, ,, ,, ,, ,, cc cc .. xx xx ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ** ** ,, ,, cc ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc ar cc ** cc cc cc ,, cc ar cc ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx cc ,, cc ,, cc ,, cc ,, ,, ,, cc cc cc ,, ,, cc ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx cc 00 cc 00 cc 00 cc xx ** ,, ,, cc cc ,, ,, ,, ** ,, ** cc ** xx xx ,, ,, ,, ,, ,, ,, xx "
  "xx cc 00 cc 00 cc 00 cc xx ** ** ** cc ar cc cc cc ** ** ar cc ** ** xx xx xx ,, ,, xx xx xx "
  "xx cc 00 cc 00 cc 00 cc xx ** ** ** cc cc cc cc ar cc ** cc cc ** ** xx ,, ,, ,, ,, ,, ,, xx "
  "xx cc cc cc cc cc cc cc xx ** ** ** ** ** .. cc cc cc ** ** ** ** .. xx ,, ,, ,, ,, ,, ,, xx "
  "xx xx ,, ,, cc ,, ,, xx xx ** .. .. .. ** ** .. ,, ** ** ** ** .. .. xx ,, ,, ,, ,, ,, ,, xx "
  ".. xx xx ,, cc ,, xx xx ** ** .. .. .. ** xx xx ,, xx ** ** .. .. .. xx ,A ,R ,M ,O ,R ,Y xx "
  ".. .. xx ,, cc ,, xx .. ** .. .. .. .. ** xx ,, ,, xx ** xx xx xx xx xx xx xx xx xx xx xx xx "
  "** .. .. ,, cc ,, ** ** ** .. .. .. ** ** xx ,, ,, ,, ** ,, ,, xx ,, ,, xx ,, ,, xx ,, ,, xx "
  "** ** .. ,, cc ,, ** .. ** ** .. .. ** ** xx xx xx xx ** xx ,, xx ,, ,, xx ,, ,, xx ,, ,, xx "
  "** ** .. ,, cc ,, .. .. .. ** .. .. ** ** ** ** ** ** ** xx ,, xx xx ,, xx ,, xx xx ,, xx xx "
  "** ** .. ,, cc ,, .. .. .. ** .. xx xx xx xx xx xx xx ** xx ,, ,, xx ,, ,, ,, ,, ,, ,, xx xx "
  "** ** .. ,, cc ,, .. .. .. .. .. xx pp ,, ,, ,, pp xx ** xx ,, ,, xx xx xx ,, xx xx xx xx xx "
  "** ** .. ,, cc ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, xx ** xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "** ** .. ,, cc cc cc cc cc cc cc cc cc cc cc cc ,, xx ** xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "** .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, xx ** xx ,Q ,U ,A ,R ,A ,N ,T ,I ,N ,E xx "
  ".. .. .. .. ,, .. ** ** ,, .. ,, xx pp ,, cc ,, pp xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx xx xx xx ,, xx xx ** ,, .. ** xx xx ,, cc ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, xx ** ,, .. ** ** ** ,, cc ,, ** ** xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, .. ** ** ** ,, cc ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ @@ @@ ]] ,, ,, xx "
  "xx [[ @@ @@ @@ ]] xx .. .. ** ** ** ** ,, cc ,, ** ** xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, xx ** ** ** ** ** .. ,, cc ,, ** ** xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,T ,A ,X ,E ,S xx ** ** ** ** .. .. ,, cc ,, .. .. xx ,I ,M ,M ,I ,G ,R ,A ,T ,I ,O ,N xx "
  "xx xx xx xx xx xx xx ** ** ** ** .. .. ,, cc ,, .. .. xx xx xx xx xx xx xx xx xx xx xx xx xx "
  ))

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (ankh-mk)
  (list #f))
(define (ankh-done? gob) (car gob))
(define (ankh-done! gob) (set-car! gob #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (ankh-hail knpc kpc)
  (let ((gob (kobj-gob-data knpc)))
    (if (not (ankh-done? gob))
        (begin
          (say knpc "[A vibrating voice fills your head] "
               "At the dawn of the next age, how will the world be made?")
          (let ((resp (kern-conv-get-reply kpc)))
            (if (not (eq? resp 'anew))
                (say knpc "Then the new age has not yet come.")
                (begin
                  (say knpc "THEN THE NEW AGE HAS BEGUN!")
                  (shake-map 15)
                  (kern-map-flash 500)
                  (shake-map 15)
                  (kern-map-flash 500)          
                  (shake-map 15)
                  (ankh-done! gob)
                  (blit-map (loc-place (kern-obj-get-location knpc))
                    0 0 31 31 m_hidden_city)
                  ))
            (kern-conv-end))))))
        

(define ankh-conv
  (ifc basic-conv
       (method 'hail ankh-hail)
       ))

(define (ankh-ai knpc)
  #t)

(define (mk-talking-ankh)
  (bind 
   (kern-mk-char 
    'ch_ankh           ; tag
    "Ankh"             ; name
    ankh-species         ; species
    ankh-occ              ; occ
    s_ankh     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp ankh-species ankh-occ ankh-lvl 0 0) ; hp
    0                   ; xp
    (max-mp ankh-species ankh-occ ankh-lvl 0 0) ; mp
    ankh-lvl
    #f               ; dead
    'ankh-conv         ; conv
    nil           ; sched
    'ankh-ai              ; special ai
    nil              ; container
    nil              ; readied
    )
   (ankh-mk)))
