(kern-mk-map
 'm_lava_flow 1 5 pal_expanded
 (list "!!" "!_" "!!" "!_" "!!")
)

(kern-mk-map
 'm_lava_bridge 1 5 pal_expanded
 (list ".." "~!" ".." "~!" "..")
) 

(define (r2a-statue-hail knpc kpc)
        (begin
          (let ((resp (kern-conv-get-reply kpc)))
            (if (eq? resp 'onus) 
                (begin
					(shake-map 15)
					(blit-map p_absalot_passage 5 15 1 5 m_lava_bridge)
                  ))
            (kern-conv-end))))
        

(define r2a-statue-conv
  (ifc basic-conv
       (method 'hail r2a-statue-hail)
       ))
 
(define (fix-lava kplace kplayer)
	(blit-map p_absalot_passage 5 15 1 5 m_lava_flow)
)
