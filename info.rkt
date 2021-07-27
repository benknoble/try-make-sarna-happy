#lang info
(define collection "try-make-sarna-happy")
(define deps '("base"))
(define build-deps '("rackunit-doc"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/try-make-sarna-happy.scrbl" ())))
(define pkg-desc "A try macro for sarna")
(define version "0.1")
(define pkg-authors '(benknoble))
