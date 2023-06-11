#lang racket/base
(require (submod slide-easy generic) racket/draw (except-in racket/class super) racket/runtime-path (for-syntax racket/base))
(provide (contract-out (current-theme-color (parameter/c (or/c string? (is-a?/c color%))))
                       (current-theme-font (parameter/c (or/c font-family/c (is-a?/c font%))))
                       (current-background-size (parameter/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))
                       #:exists tagged
                       (install-template (opt/c (->* () (string?)
                                                     (values
                                                      (-> tag-or-tag-list/c
                                                          any/c ...
                                                          tagged)
                                                      (-> tagged (-> any/c pict?) tagged)
                                                      (-> tag-or-tag-list/c
                                                          list-contract?
                                                          any/c
                                                          any)))))))

(define-runtime-module-path-index pict 'pict)

(define tag-or-tag-list/c (or/c tag? (listof tag?)))

(define current-theme-color
  (make-parameter "Firebrick"))
(define current-theme-font
  (make-parameter 'roman))
(define current-background-size
  (make-parameter (cons 1000 700)))

(define (install-template (prefix ""))
  ;;the core datatype
  (define root (string->symbol (string-append prefix "幻灯片")))
  
  (define rooth (make-type root))

  (install rooth (cons/c (or/c 'lt 'ltl 'lc 'lbl 'lb
                               'ct 'ctl 'cc 'cbl 'cb
                               'rt 'rtl 'rc 'rbl 'rb)
                         pict?)
           (lambda (pair)
             (define size (current-background-size))
             ((dynamic-require pict (string->symbol (string-append (symbol->string (car pair)) "-superimpose")))
              (blank (car size) (cdr size))
              (cdr pair)))

           (cons 'filter (lambda (pair proc) (tag root (cons (car pair) (proc (cdr pair)))))))

  (define (root-or-subtype? h)
    (eq? root (ref h (sub1 (depth h)))))
  
  (define/contract (filter obj proc)
    (-> (lambda (o) (root-or-subtype? (type o))) any/c any)
    (apply-generic 'filter (coerce obj root) proc))

  ;;the installer and the constructor
  (define (add-prefix t) (make-type (if (tag? t) t (apply make-type t)) rooth))
  
  (define (create t . ls) (tag (add-prefix t) ls))
  
  (define ((make-handler p) ls) (apply p ls))

  (define (n:install t ct cc)
    (install (add-prefix t) ct (make-handler cc)))
  
  ;;subtypes
  (define (handler p proc)
    (if (pict? p) p (proc p)))

  (define BLACK (make-object color% "Black"))
  
  (define (titlet s (color BLACK)) (text s (cons 'bold (cons color (current-theme-font))) (* 2 (current-font-size))))
  (define (normalt s (color BLACK)) (text s (cons color (current-theme-font)) (current-font-size)))
  (define (smallt s (color BLACK)) (text s (cons color (current-theme-font)) (floor (/ (current-font-size) 2))))

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
