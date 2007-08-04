(kern-mk-sprite-set 'ss_jewelry 32 32 2 2 0 0 "gfx/jewelry.png")

(kern-mk-sprite 's_skull_ring ss_jewelry 1 0 #f 0)

(mk-quest-obj-type 't_skull_ring "skull ring" s_skull_ring layer-item obj-ifc)
