(define (mk-goblin-party)
  (let ((kparty (kern-mk-party)))
    (kern-obj-set-sprite kparty s_orc)
    (kern-being-set-base-faction kparty faction-forest-goblin)
    (kern-party-add-member kparty (mk-npc 'forest-goblin-stalker 1))
    kparty
    ))
