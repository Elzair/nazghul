;; The object and type system used by haxima. This is a library of utilities
;; intended to make it easy to add new types of objects in a consistent way.
;;
;; The engine call to add a new type is kern-mk-obj-type. The args are:
;;
;;                        tag : qstr, to reference the type elsewhere in the script
;;                       name : str or list of two str, for singular and plural
;;                     sprite : sym, sprite used by these objects
;;                      layer : int, layer occupied by these objects
;; supported methods (ifccap) : int, bitmask of methods supported by these objects
;;              methods (ifc) : closure implementing the methods
;;
;; A simple form of polymorphism is implemented by the type system shown
;; here. Every type has an optional method table. These methods are callable
;; from the engine. There is a standard set of methods the engine knows about,
;; and it can quickly check at runtime if a type supports a method by
;; inspecting the interface capabilities field (ifccap) -- a bitmask indicating
;; which fields are in the call table.
;;
;; The call table itself is implemented as a closure, usually abbreviated as
;; the 'ifc' (for interface) of the type.

;; Map standard interface calls to a bitmap for fast lookup in the kernel.
(define (ifc-cap ifc)
  (define (cap ifc calls)
    (if (null? calls) 0
        (+ (* 2 (cap ifc (cdr calls)))
           (if (ifc 'can (car calls)) 1 0))))
  (if (null? ifc) 0
      (cap ifc (list 'get 'use 'exec 'open 'handle 'step 'attack 'mix 
                     'enter 'cast 'bump 'hit-loc 'buy 'search 'sense 'xamine 'describe 'on-attack
                     'describe))))

;; A wrapper for kern-mk-obj-type which inserts the interface caps info and
;; assumes the type uses the "small object" movement mode.
(define (mk-obj-type tag name sprite layer ifc)
  (kern-mk-obj-type tag name sprite layer (ifc-cap ifc) ifc mm_smallobj))
  
