#lang scribble/manual

@(require (for-label try-make-sarna-happy
                     racket/base
                     syntax/parse
                     syntax/parse/define
                     rackunit
                     racket/function
                     racket/port
                     racket/match)
          scribble/examples
          racket/file
          "shared.rkt")

@title{try-make-sarna-happy}
@author{D. Ben Knoble}

@(define stx-bee @(hyperlink "https://github.com/syntax-objects/Summer2021"
                             "2021 Syntax Parse bee"))
@(define try-eval (make-base-eval))
@(examples #:eval try-eval
           #:hidden
           (require "main.rkt")
           (define (get-handle) (box #f))
           (define (use-might-break _) (error 'use-might-break "something went wrong"))
           (define (close b) (box-cas! b #f #t))
           (define is-closed? unbox))

@defmodule[try-make-sarna-happy]

This package was written for the @stx-bee and to satisfy sarna's complaints on
Discord with @racket[with-handlers], which leads to a "backwards" looking
program:

@#reader scribble/comment-reader
(racketblock
;; say first what to do with the exception
(with-handlers ([exn:fail:syntax?
                   (λ (e) (displayln "got a syntax error"))])
  ;; and only then what to actually do
  (raise-syntax-error #f "a syntax error")))

(from the @racket[with-handlers] examples).

@section{The try macro}

@defform[#:literals (catch catch/match finally)
         (try body-expr ...+ maybe-catch maybe-catch/match maybe-finally)
         #:grammar
         [(maybe-catch (code:line)
                       (catch [pred-expr exn-id handler-expr ...+] ...))
          (maybe-catch (code:line)
                       (catch/match [match-expr handler-expr ...+] ...))
          (maybe-finally (code:line)
                         (finally finally-expr ...+))]]{
Tries @racket[body-expr]s in order, returning the value of the last. If an
exception is raised, it may be handled by one of the @racket[catch] clauses,
whose value becomes the value of the overall form. The optional
@racket[finally] clause is @emph{always} run.

The @racket[catch] clauses use @racket[with-handlers], but in a different
format: when @racket[pred-expr] returns true for a thrown exception,
@racket[exn-id] is bound to the exception for the body @racket[handler-expr].

The @racket[catch/match] clauses are @racket[match] forms tested against the
exception.

When both @racket[catch]-style and @racket[catch/match]-style clauses are
present, all of the @racket[catch]-style clauses are tried before any of the
@racket[catch/match] clauses.
}

@subsection{Examples}

@examples[#:eval try-eval
          (try
            (/ 10 0)
            (catch [exn? e (exn-message e)]))

          (struct my-error [x y])
          (try
            (raise (my-error 1 2))
            (catch/match [(my-error 1 y) y]))

          (let ([resource (get-handle)])
            (try
              (use-might-break resource)
              (catch [exn? e (displayln (exn-message e))])
              (finally
                (close resource)))
            (is-closed? resource))]

@subsection{Before and After}

This is a "Code Cleaning" macro: it tidies up a common pattern and makes it read
in a forward direction. In this it is similar to the
@hyperlink["https://docs.racket-lang.org/threading/index.html"]{threading
library} but for exceptions.

The following Before/After pairs do not show the
@racket[call-with-continuation-barrier] call because they assumes none of the
shown procedures muck with continuations. It is needed in the general case,
however, to prevent a captured continuation from re-entering the dynamic-wind
and thus causing the finally clause to be run more than once.

@before-after[try-eval

               [(with-handlers ([exn:fail:syntax?
                                  (λ (e)
                                    (displayln "got a syntax error"))])
                  (raise-syntax-error #f "a syntax error"))]

               [(try
                  (raise-syntax-error #f "a syntax error")
                  (catch [exn:fail:syntax? e
                          (displayln "got a syntax error")]))]]

@before-after[try-eval

              [(let ([resource (get-handle)])
                 (dynamic-wind
                   void
                   (λ () (with-handlers ([exn? (λ (e) (displayln (exn-message e)))])
                                        (use-might-break resource)))
                   (λ () (close resource)))
                 (is-closed? resource))]

              [(let ([resource (get-handle)])
                 (try
                   (use-might-break resource)
                   (catch [exn? e (displayln (exn-message e))])
                   (finally
                     (close resource)))
                 (is-closed? resource))]]

The following are from @hyperlink["https://beautifulracket.com/explainer/errors-and-exceptions.html"]{Beautiful Racket: Errors and Exceptions}.

@before-after[try-eval

               [(with-handlers ([exn:fail:contract:divide-by-zero?
                                  (λ (exn) 'got-zero-exn)]
                               [exn:fail:contract? (λ (exn) 'got-contract-exn)]
                               [exn:fail? (λ (exn) 'got-other-exn)])
                 (car 42))]

              [(try
                 (car 42)
                 (catch [exn:fail:contract:divide-by-zero? exn 'got-zero-exn]
                        [exn:fail:contract? exn 'got-contract-exn]
                        [exn:fail? exn 'got-other-exn]))]]

@before-after[try-eval

               [(with-handlers ([exn:fail:contract:divide-by-zero?
                                  (λ (exn) 'got-zero-exn)]
                                [exn:fail:contract? (λ (exn) 'got-contract-exn)]
                                [exn:fail? (λ (exn) 'got-other-exn)])
                  (car (/ 42 0)))]

               [(try
                  (car (/ 42 0))
                  (catch [exn:fail:contract:divide-by-zero? exn 'got-zero-exn]
                         [exn:fail:contract? exn 'got-contract-exn]
                         [exn:fail? exn 'got-other-exn]))]]

@before-after[try-eval

               [(with-handlers ([exn:fail:contract:divide-by-zero?
                                  (λ (exn) 'got-zero-exn)]
                                [exn:fail:contract? (λ (exn) 'got-contract-exn)]
                                [exn:fail? (λ (exn) 'got-other-exn)])
                  (error "boom"))]

               [(try
                  (error "boom")
                  (catch [exn:fail:contract:divide-by-zero? exn 'got-zero-exn]
                         [exn:fail:contract? exn 'got-contract-exn]
                         [exn:fail? exn 'got-other-exn]))]]

@subsection{Implementation}

The following is provided explicitly for the @|stx-bee|.

@(typeset-code (file->string "main.rkt")
               #:context #'here)

@section{License and Acknowledgements}

The code is licensed with the
@hyperlink["https://github.com/racket/racket/blob/master/racket/src/LICENSE-MIT.txt"]{MIT
license of the Racket project}. The text of this documentation is licensed with
the @hyperlink["http://creativecommons.org/licenses/by/4.0/"]{CCA 4.0
International License}.

Thanks to @itemize[@item{sarna, for suggesting the macro}
                   @item{Alex Knauth and SamPh, for improving the @racket[dynamic-wind] usage with @racket[call-with-continuation-barrier]}
                   @item{notjack, for the excellent suggestion to add @racket[match] forms to @racket[catch] clauses}]
