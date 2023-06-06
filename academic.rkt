#lang racket/base
(require (submod slide-easy generic) racket/draw racket/class racket/runtime-path (for-syntax racket/base))
(provide (contract-out (current-theme-color (parameter/c (or/c string? (is-a?/c color%))))
                       (current-theme-font (parameter/c (or/c font-family/c (is-a?/c font%))))
                       (current-background-size (parameter/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))
                       (install-template (opt/c (->* () (string?)
                                                     (->i ((prompt (or/c '图示 '封面 '节 '致谢 #f)))
                                                          #:rest (rest (prompt) (if prompt list? (list/c any/c)))
                                                          (result tagged-object?)))))))

(define-runtime-module-path-index pict 'pict)

(define current-theme-color
  (make-parameter "Firebrick"))
(define current-theme-font
  (make-parameter 'roman))
(define current-background-size
  (make-parameter (cons 1000 700)))

(define (install-template (prefix ""))
  (define root (string->symbol (string-append prefix "幻灯片")))

  (define (add-prefix symbol) (list symbol root))
  
  (define (create t . ls) (if t (tag (add-prefix t) ls) (apply tag root ls)))
  
  (define (handler p proc)
    (if (pict? p) p (proc p)))

  (define BLACK (make-object color% "Black"))
  
  (define (titlet s (color BLACK)) (text s (cons 'bold (cons color (current-theme-font))) (* 2 (current-font-size))))
  (define (normalt s (color BLACK)) (text s (cons color (current-theme-font)) (current-font-size)))
  (define (smallt s (color BLACK)) (text s (cons color (current-theme-font)) (floor (/ (current-font-size) 2))))

  (install root (cons/c (or/c 'lt 'ltl 'lc 'lbl 'lb
                              'ct 'ctl 'cc 'cbl 'cb
                              'rt 'rtl 'rc 'rbl 'rb)
                        pict?)
           (lambda (pair)
             (define size (current-background-size))
             ((dynamic-require pict (string->symbol (string-append (symbol->string (car pair)) "-superimpose")))
              (blank (car size) (cdr size))
              (cdr pair))))
  
  (define (封面->pict title member)
    (define mb (handler member normalt))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (cons 'lc (vl-append (current-gap-size) tt (hline (floor (/ (+ (pict-width tt) (pict-width mb)) 2)) (current-gap-size)) mb)))

  (define (节->pict index title)
    (define id (handler index titlet))
    (define tt (handler title (lambda (str) (titlet str (make-object color% (current-theme-color))))))
    (cons 'cc (vc-append (current-gap-size) id (hline (floor (/ (+ (pict-width tt) (pict-width id)) 2)) (current-gap-size)) tt)))

  (define (图示->pict intr image source)
    (cons 'cc (vc-append (current-gap-size)
                         (handler intr (lambda (str) (titlet str (make-object color% (current-theme-color)))))
                         image
                         (handler source smallt))))

  (define (致谢->pict member)
    (封面->pict "THANKS FOR YOUR LISTENING" member))

  (define ((make-handler p) ls) (apply p ls))

  (define elem/c (or/c string? pict?))
  
  (define names '(封面 节 图示 致谢))
  (define funcs (list 封面->pict 节->pict 图示->pict 致谢->pict))
  (define contracts (list (list/c elem/c elem/c)
                          (list/c elem/c elem/c)
                          (list/c elem/c pict? elem/c)
                          (list/c elem/c)))
  
  (map (lambda (n c f) (install (add-prefix n) c (make-handler f))) names contracts funcs)
  
  create)
