#lang racket/base

(provide try)

(require (for-syntax racket/base)
         syntax/parse/define)

(begin-for-syntax
  (define-syntax-class catch-clause
    #:attributes ((pred 1) (name 1) (body 2))
    #:datum-literals (catch)
    (pattern (catch ([(pred:expr name:id) body:expr ...+] ...))))

  (define-syntax-class finally-clause
    #:attributes ((body 1))
    #:datum-literals (finally)
    (pattern (finally body:expr ...+))))

;; Calls value-thunk, then post-thunk, with post-thunk guaranteed to be run
;; even if execution exits value-thunk through an exception or continuation
;;
;; value-thunk is prevented from re-entry and continutation shenanigans by a
;; continuation-barrier
;;
;; thanks to Alex Knauth & SamPh on Discord
(define (call-with-try-finally value-thunk post-thunk)
  (call-with-continuation-barrier
    (λ () (dynamic-wind void value-thunk post-thunk))))

(define-syntax-parser try
  [(_ {~and body:expr {~not _:catch-clause} {~not _:finally-clause}} ...+
      {~optional c:catch-clause}
      {~optional f:finally-clause})
   #'(call-with-try-finally
       (λ ()
         (with-handlers ((~? (~@ [c.pred (λ (c.name) c.body ...)] ...)))
           body ...))
       (~? (λ () f.body ...) void))])

(module+ test
  (require racket
           rackunit)

  (check-equal?
    (try 1)
    1)

  (check-equal?
    (try (/ 1 0)
         (catch ([(exn:fail? e) (exn-message e)])))
    "/: division by zero")

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try 1
               (finally (displayln "cleaning up")))
          1)))
    "cleaning up\n")

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try (/ 1 0)
               (catch ([(exn:fail? _) 0]))
               (finally (displayln "cleaning up")))
          0)))
    "cleaning up\n"))
