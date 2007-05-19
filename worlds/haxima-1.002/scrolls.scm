;; ============================================================================
;; scrolls.scm -- useable scrolls
;; ============================================================================

(kern-mk-sprite-set 'ss_scrolls 32 32 3 4 0 0 "scrolls.png")

(kern-mk-sprite 's_an_tym_scroll          ss_scrolls 1 0 #f 0) ;; stop time
(kern-mk-sprite 's_in_mani_corp_scroll    ss_scrolls 1 1 #f 0) ;; resurrect
(kern-mk-sprite 's_vas_rel_por_scroll     ss_scrolls 1 2 #f 0) ;; gate travel
(kern-mk-sprite 's_xen_corp_scroll        ss_scrolls 1 3 #f 0) ;; kill

(kern-mk-sprite 's_sanct_lor_scroll       ss_scrolls 1 4 #f 0) ;; invisibility
(kern-mk-sprite 's_in_quas_xen_scroll     ss_scrolls 1 5 #f 0) ;; clone
(kern-mk-sprite 's_in_vas_por_ylem_scroll ss_scrolls 1 6 #f 0) ;; tremor
(kern-mk-sprite 's_an_xen_ex_scroll       ss_scrolls 1 7 #f 0) ;; charm

(kern-mk-sprite 's_in_an_scroll           ss_scrolls 1 8 #f 0) ;; negate
(kern-mk-sprite 's_in_ex_por_scroll       ss_scrolls 1 9 #f 0) ;; unlock magic
(kern-mk-sprite 's_vas_mani_scroll        ss_scrolls 1 10 #f 0) ;; great heal
(kern-mk-sprite 's_wis_quas_scroll        ss_scrolls 1 11 #f 0) ;; reveal

(kern-mk-sprite 's_wis_an_ylem_scroll     ss_scrolls 1 13 #f 0) ;; xray

(define (mk-scroll tag name sprite spell)
  (mk-usable-item tag name sprite norm 
                  (lambda (kscrolltype kuser)
                    (apply spell (list kuser)))))

(define (player-in-wilderness?)
	(kern-place-is-wilderness? (loc-place (kern-obj-get-location (kern-get-player))))
	)

;;-----------------------------------------------------------------------------------------
;; Scroll Functions
;;-----------------------------------------------------------------------------------------

(define (scroll-xen-corp caster)
	(if (player-in-wilderness?)
		result-not-here
		(user-cast-ranged-targeted-spell caster 4 cast-kill-proc)
	))
		
(define (scroll-in-quas-xen caster)
	(if (player-in-wilderness?)
		result-not-here
		(cast-ui-basic-ranged-spell powers-clone
			caster 
			(powers-clone-range 12)
			12)
	))

(define (scroll-in-vas-por-ylem caster)
	(if (player-in-wilderness?)
		result-not-here
		(powers-tremor caster caster 12)
	))

(define (scroll-an-xen-ex caster)
	(if (player-in-wilderness?)
		result-not-here
		(cast-ui-basic-ranged-spell powers-charm
			caster 
			(powers-charm-range 12)
			12)
	))
		
;;-----------------------------------------------------------------------------------------
;; Scroll List
;;-----------------------------------------------------------------------------------------

(mk-scroll 't_an_tym_scroll "An Tym scroll" s_an_tym_scroll an-tym) ;; context-any
(mk-scroll 't_in_mani_corp_scroll "In Mani Corp scroll" s_in_mani_corp_scroll in-mani-corp)  ;; context-any
(mk-scroll 't_vas_rel_por_scroll "Vas Rel Por scroll" s_vas_rel_por_scroll vas-rel-por)  ;; context-any
(mk-scroll 't_xen_corp_scroll "Xen Corp scroll" s_xen_corp_scroll scroll-xen-corp) 
(mk-scroll 't_sanct_lor_scroll "Sanct Lor scroll" s_sanct_lor_scroll sanct-lor) ;; context-any
(mk-scroll 't_in_quas_xen_scroll "In Quas Xen scroll" s_in_quas_xen_scroll scroll-in-quas-xen)
(mk-scroll 't_in_vas_por_ylem_scroll "In Vas Por Ylem scroll" s_in_vas_por_ylem_scroll scroll-in-vas-por-ylem) 
(mk-scroll 't_an_xen_ex_scroll "An Xen Ex scroll" s_an_xen_ex_scroll scroll-an-xen-ex)
(mk-scroll 't_in_an_scroll "In An scroll" s_in_an_scroll in-an) ;; context-any
(mk-scroll 't_in_ex_por_scroll "In Ex Por scroll" s_in_ex_por_scroll in-ex-por)  ;; context-any??
(mk-scroll 't_vas_mani_scroll "Vas Mani scroll" s_vas_mani_scroll vas-mani) ;; context-any
(mk-scroll 't_wis_quas_scroll "Wis Quas scroll" s_wis_quas_scroll wis-quas) ;; context-any
(mk-scroll 't_wis_an_ylem_scroll "Wis An Ylem scroll" s_wis_an_ylem_scroll wis-an-ylem) ;; context-any
