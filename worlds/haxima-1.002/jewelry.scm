(kern-mk-sprite-set 'ss_jewelry 32 32 2 2 0 0 "jewelry.png")

(kern-mk-sprite 's_skull_ring ss_jewelry 1 0 #f 0)

(mk-quest-obj-type 't_skull_ring "skull ring" s_skull_ring layer-item obj-ifc)


(define (skullring-basic-receive kchar questtag)
	(quest-data-update-with 'questentry-ghertie questtag 1 (quest-notify (grant-party-xp-fn 10)))
	)

(define (skullring-basic-get kobj kchar questtag)
	(if (not (null? kobj))
		(kern-obj-remove kobj)
	)
	(skullring-basic-receive kchar questtag)
	(kobj-get (kern-mk-obj t_skull_ring 1) kchar)
	)

(define (skullring-m-get kobj kchar)
	(skullring-basic-get kobj kchar 'ring-meaney)
	)
(define skullring-m-ifc
  (ifc obj-ifc
       (method 'get skullring-m-get)))
(mk-quest-obj-type 't_skull_ring_m "skull ring" s_skull_ring layer-item skullring-m-ifc)

(define (skullring-j-get kobj kchar)
	(skullring-basic-get kobj kchar 'ring-jorn)
	)
(define skullring-j-ifc
  (ifc obj-ifc
       (method 'get skullring-j-get)))
(mk-quest-obj-type 't_skull_ring_j "skull ring" s_skull_ring layer-item skullring-j-ifc)

(define (skullring-g-get kobj kchar)
	(skullring-basic-get kobj kchar 'ring-gholet)
	)
(define (skullring-g-receive ktype kchar)
	(skullring-basic-receive kchar 'ring-gholet)
	)
	
(define skullring-g-ifc
  (ifc obj-ifc
       (method 'get skullring-g-get)
       (method 'receive skullring-g-receive)))
(mk-quest-obj-type 't_skull_ring_g "skull ring" s_skull_ring layer-item skullring-g-ifc)

