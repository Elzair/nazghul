;;----------------------------------------------------------------------------
;; unused - takes 1.5s to load for some strange reason
;;----------------------------------------------------------------------------


;; The 'init signal is sent after the instance is created. This is our chance
;; to initialize fields to non-default values. Note: all the pclass values are
;; defined in game.scm.
(define (road-init kroad)
  (kern-obj-set-pclass kroad pclass-road))

;; Define the interface for the type
(define road-ifc
  (ifc '()
       (method 'init road-init)))

;; Setup a list of road sections to feed to the declration function below
(define road-sections
  (list
   (list 'TF_road_3x3_nw  s_trail_0)
   (list 'TF_road_3x3_n   s_trail_1)
   (list 'TF_road_3x3_ne  s_trail_2)
   (list 'TF_road_1x3_n   s_trail_3)

   (list 'TF_road_3x3_w   s_trail_4)
   (list 'TF_road_3x3_c   s_trail_5)
   (list 'TF_road_3x3_e   s_trail_6)
   (list 'TF_road_1x3_c   s_trail_7)

   (list 'TF_road_3x3_sw  s_trail_8)
   (list 'TF_road_3x3_s   s_trail_9)
   (list 'TF_road_3x3_se  s_trail_a) 
   (list 'TF_road_1x3_s   s_trail_b) 

   (list 'TF_road_3x1_w   s_trail_c)
   (list 'TF_road_3x1_c   s_trail_d)
   (list 'TF_road_3x1_e   s_trail_e)
   ))

;; Declare all the road sections
(map (lambda (section) (mk-obj-type (car section)
                                    "road"
                                    (cadr section)
                                    layer-tfeat
                                    road-ifc))
     road-sections)

;; Make a section of road
(define (mk-road type)
  (bind (kern-mk-obj type 1) nil))

(define (translate-feature code)
  (case code
    ((r0) (mk-road TF_road_3x3_nw))
    ((r1) (mk-road TF_road_3x3_n ))
    ((r2) (mk-road TF_road_3x3_ne))
    ((r3) (mk-road TF_road_1x3_n ))
    ((r4) (mk-road TF_road_3x3_w ))
    ((r5) (mk-road TF_road_3x3_c ))
    ((r6) (mk-road TF_road_3x3_e ))
    ((r7) (mk-road TF_road_1x3_c ))
    ((r8) (mk-road TF_road_3x3_sw))
    ((r9) (mk-road TF_road_3x3_s ))
    ((ra) (mk-road TF_road_3x3_se))
    ((rb) (mk-road TF_road_1x3_s ))
    ((rc) (mk-road TF_road_3x1_w ))
    ((rd) (mk-road TF_road_3x1_c ))
    ((re) (mk-road TF_road_3x1_e ))
    (else nil)
))

(define (lay-feature code x y)
  (let ((feat (translate-feature code)))
    (cond ((null? feat) nil)
          (else
           (list feat x y)))))

(define (lay-featmap-row row x y)
  (cond ((null? row) nil)
        (else
         (cons (lay-feature (car row) x y)
               (lay-featmap-row (cdr row) (+ x 1) y)))))

(define (lay-featmap featmap x y)
  (cond ((null? featmap) nil)
        (else
         (append (filter (lambda (x) (not (null? x)))
                         (lay-featmap-row (car featmap) x y))
               (lay-featmap (cdr featmap) x (+ y 1))))))
