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
(kern-mk-sprite 's_an_xen_ex_scroll      ss_scrolls 1 7 #f 0) ;; charm

(kern-mk-sprite 's_in_an_scroll           ss_scrolls 1 8 #f 0) ;; negate
(kern-mk-sprite 's_in_ex_por_scroll       ss_scrolls 1 9 #f 0) ;; unlock magic
(kern-mk-sprite 's_vas_mani_scroll        ss_scrolls 1 10 #f 0) ;; great heal
(kern-mk-sprite 's_wis_quas_scroll        ss_scrolls 1 11 #f 0) ;; reveal

(define (mk-scroll tag name sprite spell)
  (mk-usable-item tag name sprite 1 
                  (lambda (kscrolltype kuser)
                    (let ((result (apply spell (list kuser))))
                      (if (eq? result result-ok)
                          #t
                          '())))))

(mk-scroll 't_an_tym_scroll "An Tym scroll" s_an_tym_scroll an-tym)
(mk-scroll 't_in_mani_corp_scroll "In Mani Corp scroll" s_in_mani_corp_scroll in-mani-corp)
(mk-scroll 't_vas_rel_por_scroll "Vas Rel Por scroll" s_vas_rel_por_scroll vas-rel-por)
(mk-scroll 't_xen_corp_scroll "Xen Corp scroll" s_xen_corp_scroll xen-corp)
(mk-scroll 't_sanct_lor_scroll "Sanct Lor scroll" s_sanct_lor_scroll sanct-lor)
(mk-scroll 't_in_quas_xen_scroll "In Quas Xen scroll" s_in_quas_xen_scroll in-quas-xen)
(mk-scroll 't_in_vas_por_ylem_scroll "In Vas Por Ylem scroll" s_in_vas_por_ylem_scroll in-vas-por-ylem)
(mk-scroll 't_an_xen_ex_scroll "An Xen Ex scroll" s_an_xen_ex_scroll an-xen-ex)
(mk-scroll 't_in_an_scroll "In An scroll" s_in_an_scroll in-an)
(mk-scroll 't_in_ex_por_scroll "In Ex Por scroll" s_in_ex_por_scroll in-ex-por)
(mk-scroll 't_vas_mani_scroll "Vas Mani scroll" s_vas_mani_scroll vas-mani)
(mk-scroll 't_wis_quas_scroll "Wis Quas scroll" s_wis_quas_scroll wis-quas)


;; Temp shims to keep saved games working
(define t_an_xen_exe_scroll t_an_xen_ex_scroll)
(define t_in_exe_por_scroll t_in_ex_por_scroll)