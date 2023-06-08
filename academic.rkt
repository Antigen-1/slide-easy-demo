#lang racket/base
(require (submod slide-easy generic) racket/draw racket/class racket/runtime-path (for-syntax racket/base))
(provide (contract-out (current-theme-color (parameter/c (or/c string? (is-a?/c color%))))
                       (current-theme-font (parameter/c (or/c font-family/c (is-a?/c font%))))
                       (current-background-size (parameter/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))
                       (install-template (opt/c (->* () (string?)
                                                     (values
                                                      (-> (or/c '图示 '封面 '节 '致谢 '空白 '目录)
                                                          any/c ...
                                                          tagged-object?)
                                                      (-> tagged-object? (-> any/c pict?) tagged-object?)))))))

(define-runtime-module-path-index pict 'pict)

(define current-theme-color
  (make-parameter "Firebrick"))
(define current-theme-font
  (make-parameter 'roman))
(define current-background-size
  (make-parameter (cons 1000 700)))

(define (install-template (prefix ""))
  ;;the root type
  (define root (string->symbol (string-append prefix "幻灯片")))

  (install root (cons/c (or/c 'lt 'ltl 'lc 'lbl 'lb
                              'ct 'ctl 'cc 'cbl 'cb
                              'rt 'rtl 'rc 'rbl 'rb)
                        pict?)
           (lambda (pair)
             (define size (current-background-size))
             ((dynamic-require pict (string->symbol (string-append (symbol->string (car pair)) "-superimpose")))
              (blank (car size) (cdr size))
              (cdr pair)))

           (cons 'filter (lambda (pair proc) (tag root (cons (car pair) (proc (cdr pair)))))))

  (define root-or-subtype?
    (or/c root (*list/c any/c root)))
  
  (define/contract (filter obj proc)
    (-> (lambda (o) (root-or-subtype? (type o))) any/c any)
    (apply-generic 'filter (coerce obj root) proc))

  (define (add-prefix symbol) (list symbol root))
  
  (define (create t . ls) (tag (add-prefix t) ls))
  
  ;;other subtypes
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
    (define ls (apply vl-append (current-gap-size) (map item items)))
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

  (define ((make-handler p) ls) (apply p ls))

  (define elem/c (or/c string? pict?))
  
  (define names '(空白 封面 目录 节 图示 致谢))
  (define funcs (list 空白->pict 封面->pict 目录->pict 节->pict 图示->pict 致谢->pict))
  (define contracts (list (list/c any/c any/c)
                          (list/c elem/c elem/c)
                          (cons/c elem/c (non-empty-listof elem/c))
                          (list/c elem/c elem/c)
                          (list/c elem/c pict? elem/c)
                          (list/c elem/c elem/c)))
  
  (map (lambda (n c f) (install (add-prefix n) c (make-handler f))) names contracts funcs)
  
  (values create filter))
