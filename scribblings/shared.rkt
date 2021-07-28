#lang racket/base

(provide before-after)

(require (for-syntax racket/base)
         racket/list
         scribble/core
         ;; conflicts with scribble/core
         (only-in scribble/struct make-flow)
         scribble/examples
         scribble/html-properties
         syntax/parse/define)

(define (sty columns width #:valign? [valign? #t])
  ;; https://github.com/racket/racket/blob/2f33f60e51cce9ac1fb4f5f232baf8352c6a9152/pkgs/racket-doc/scribblings/style/shared.rkt#L75
  (define space
    (style #f `(,(attributes `((width . ,(format "~a" width)) (align . "left")
                                                              ,@(if valign?
                                                                  (list '(valign . "top"))
                                                                  (list)))))))
  ;; -- in --
  (style #f
         (list
           (attributes '((border . "1") (cellpadding . "1")))
           (table-columns (make-list columns space)))))

(define-syntax-parse-rule (before-after eval:expr [before:expr ...+] [after:expr ...+])
  ;; see racket's style guide, section units, and the shared.rkt implementation
  ;; of compare
  ;; https://github.com/racket/racket/blob/2f33f60e51cce9ac1fb4f5f232baf8352c6a9152/pkgs/racket-doc/scribblings/style/shared.rkt#L44
  #:with contents #'(list (list (examples #:eval eval
                                          #:label "Before"
                                          before ...))
                          (list (examples #:eval eval
                                          #:label "After"
                                          after ...)))
  (table (sty 2 200)
         (apply map (compose make-flow list) contents)))
