#lang racket/base

(require "./private/get.rkt")

(provide binary-home
         binary-home/for-program
         set-binary-home!)

;; ============================================================================
;; Individual location variables
;; ============================================================================

(define (path/for-program program-name var-name default-path)
  (let ((base-dir (maybe-string->path (get-xdg-var var-name
                                                   'base
                                                   default-path))))
    (if program-name
        (build-path base-dir program-name)
        base-dir)))

;; ----------------------------------------------------------------------------

(define (binary-home) (binary-home/for-program #f))

(define (binary-home/for-program program-name)
  (path/for-program program-name
                    "XDG_X_BIN_HOME"
                    (expand-user-path "~/.local/bin")))

(define (set-binary-home! path)
  (putenv "XDG_X_BIN_HOME" (maybe-path->string path)))
