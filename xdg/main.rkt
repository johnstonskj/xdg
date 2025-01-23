#lang racket/base

(require "./private/get.rkt")

(provide current-program-name)

;; ============================================================================
;; Parameters
;; ============================================================================

(define current-program-name (make-parameter (get-program-name)
                                             #f
                                             'program-name))
