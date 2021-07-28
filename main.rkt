#lang racket/base

(provide try catch catch/match finally)

(require (for-syntax racket/base)
         racket/match
         syntax/parse/define)

(begin-for-syntax
  (define ((only-in-try name) stx)
    (raise-syntax-error name "not allowed except in try" stx)))

(define-syntax catch (only-in-try 'catch))
(define-syntax catch/match (only-in-try 'catch/match))
(define-syntax finally (only-in-try 'finally))

(begin-for-syntax
  (define-syntax-class catch-clause
    #:attributes ((pred 1) (name 1) (body 2))
    #:literals (catch)
    (pattern (catch [pred:expr name:id body:expr ...+] ...)))

  (define-syntax-class finally-clause
    #:attributes ((body 1))
    #:literals (finally)
    (pattern (finally body:expr ...+)))

  ;; this one's for you, notjack
  (define-syntax-class catch-match-clause
    #:attributes ((clause 1) (body 2))
    #:literals (catch/match)
    (pattern (catch/match [clause:expr body:expr ...+] ...))))

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
  [(_ {~and body:expr {~not _:catch-clause} {~not _:catch-match-clause} {~not _:finally-clause}} ...+
      {~optional c:catch-clause}
      {~optional m:catch-match-clause}
      {~optional f:finally-clause})
   #'(call-with-try-finally
       (λ ()
         (with-handlers ((~? (~@ [c.pred (λ (c.name) c.body ...)] ...))
                         (~? [(λ (_) #t) ;; catch 'em all
                              (match-lambda
                                [m.clause m.body ...] ...
                                ;; rethrow as last resort
                                [e (raise e)])]))
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
         (catch [exn:fail? e (exn-message e)]))
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
               (catch [exn:fail? _ 0])
               (finally (displayln "cleaning up")))
          0)))
    "cleaning up\n")

  (check-equal?
    (try (/ 1 0)
         (catch/match [(? exn:fail? e) (exn-message e)]))
    "/: division by zero")

  (struct posn [x y])
  (check-equal?
    (try (raise (posn 1 2))
         (catch/match [(posn 1 y) y]))
    2)
  (check-equal?
    (try (raise (posn 1 2))
         (catch [exn? e (exn-message e)])
         (catch/match [(posn 1 y) y]))
    2)

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
               (catch/match [(? exn:fail?) 0])
               (finally (displayln "cleaning up")))
          0)))
    "cleaning up\n"))
