#lang racket/base
(require slide-easy/generic racket/draw (except-in racket/class super) racket/runtime-path (for-syntax racket/base))
(provide (contract-out (current-theme-color (parameter/c (or/c string? (is-a?/c color%))))
                       (current-theme-font (parameter/c (or/c font-family/c (is-a?/c font%))))
                       (titlet text-format/c)
                       (normalt text-format/c)
                       (smallt text-format/c)
                       (install-template (opt/c (->i ((rooth (lambda (r) (and (not (type=? r (make-type))) (not (root? r)) (installed? (super r))))))
                                                     ()
                                                     (values
                                                      (create
                                                       (-> any/c any/c ... any))
                                                      (filter
                                                       (rooth)
                                                       (->
                                                        (lambda (o) (has-prefix? (type o) rooth))
                                                        (-> any/c pict?)
                                                        any))
                                                      (install (-> any/c
                                                                   list-contract?
                                                                   any/c
                                                                   any))))))))

(define-runtime-module-path-index pict 'pict)

(define tag-or-tag-list/c (or/c tag? (listof tag?)))

(define current-theme-color
  (make-parameter "Firebrick"))
(define current-theme-font
  (make-parameter 'roman))

(define BLACK (make-object color% "Black"))

(define text-format/c (->* (string?) ((is-a?/c color%)) any))

(define (titlet s (color BLACK)) (text s (cons 'bold (cons color (current-theme-font))) (* 2 (current-font-size))))
(define (normalt s (color BLACK)) (text s (cons color (current-theme-font)) (current-font-size)))
(define (smallt s (color BLACK)) (text s (cons color (current-theme-font)) (floor (/ (current-font-size) 2))))

(define (install-template rooth)
  ;;You have to first install the super type of `rooth` and it must handle pairs that contain layout information and pictures.
  ;;And you can specify the current workspace through the current type tag.
  
  ;;the core datatype
  (define root (current rooth))
  
  (install rooth (cons/c (or/c 'lt 'ltl 'lc 'lbl 'lb
                               'ct 'ctl 'cc 'cbl 'cb
                               'rt 'rtl 'rc 'rbl 'rb)
                         pict?)
           values ;;do nothing

           (cons 'filter (lambda (pair proc) (tag rooth (cons (car pair) (proc (cdr pair)))))))
  
  (define (filter obj proc)
    (apply-generic 'filter (coerce obj root) proc))

  ;;the installer and the constructor
  (define (add-prefix t) (make-type t rooth))
  
  (define (create t . ls) (tag (add-prefix t) ls))
  
  (define ((make-handler p) ls) (apply p ls))

  (define (n:install t ct cc)
    (install (add-prefix t) ct (make-handler cc)))
  
  ;;subtypes
  (define (handler p proc)
    (if (pict? p) p (proc p)))

  (define (空白->pict loc pict)
    (cons loc pict))
  
  (define (封面->pict title member)
    (define mb (handler member normalt))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (cons 'lc (vl-append (current-gap-size) tt (hline (floor (/ (+ (pict-width tt) (pict-width mb)) 2)) (current-gap-size)) mb)))

  (define (目录->pict title . items)
    (define tt (handler title titlet))
    (define ls (apply vl-append (current-gap-size) (map (lambda (i) (item i #:fill? #f)) items)))
    (define txt (vl-append (current-gap-size) tt (hline (floor (/ (+ (pict-width tt) (pict-width ls)) 2)) (current-gap-size)) ls))
    (define rec (filled-rectangle (* 4 (current-gap-size)) (pict-height txt) #:color (current-theme-color) #:draw-border? #f))
    (cons 'lc (hc-append (current-gap-size) rec txt)))

  (define (节->pict index title)
    (define id (handler index titlet))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (cons 'cc (vc-append (current-gap-size) id (hline (floor (/ (+ (pict-width tt) (pict-width id)) 2)) (current-gap-size)) tt)))

  (define (图示->pict intr image source)
    (cons 'cc (vc-append (current-gap-size)
                         (handler intr (lambda (str) (titlet str (make-object color% (current-theme-color)))))
                         image
                         (handler source smallt))))

  (define (致谢->pict greeting member)
    (封面->pict greeting member))

  (define elem/c (or/c string? pict?))
  
  (define names '(空白 封面 目录 节 图示 致谢))
  (define funcs (list 空白->pict 封面->pict 目录->pict 节->pict 图示->pict 致谢->pict))
  (define contracts (list (list/c any/c any/c)
                          (list/c elem/c elem/c)
                          (cons/c elem/c (non-empty-listof elem/c))
                          (list/c elem/c elem/c)
                          (list/c elem/c pict? elem/c)
                          (list/c elem/c elem/c)))
  
  (map n:install names contracts funcs)
  
  (values create filter n:install))
