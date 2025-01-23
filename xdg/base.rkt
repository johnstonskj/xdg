#lang racket/base

(require "./private/get.rkt")

(provide cache-home
         cache-home/for-program
         set-cache-home!
         config-home
         config-home/for-program
         set-config-home!
         data-home
         data-home/for-program
         set-data-home!
         runtime-home
         runtime-home/for-program
         set-runtime-home!
         state-home
         state-home/for-program
         set-state-home!
         ;; ===================================================================
         make-path-list-string
         split-path-list-string
         data-dirs
         set-data-dirs!
         config-dirs
         set-config-dirs!)

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

(define (cache-home) (cache-home/for-program #f))

(define (cache-home/for-program program-name)
  (path/for-program program-name
                    "XDG_CACHE_HOME"
                    (expand-user-path "~/.cache")))

(define (config-home)
  (config-home/for-program #f))

(define (config-home/for-program program-name)
  (path/for-program program-name
                    "XDG_CONFIG_HOME"
                    (expand-user-path "~/.config")))

(define (data-home) (data-home/for-program #f))

(define (data-home/for-program program-name)
  (path/for-program program-name
                    "XDG_DATA_HOME"
                    (expand-user-path "~/.local/share")))

(define (runtime-home) (runtime-home/for-program #f))

(define (runtime-home/for-program program-name)
  (path/for-program program-name
                    "XDG_RUNTIME_HOME"
                    (build-path "/var/run/" (get-user-name))))

(define (state-home) (state-home/for-program #f))

(define (state-home/for-program program-name)
  (path/for-program program-name
                    "XDG_STATE_HOME"
                    (expand-user-path "~/.local/state")))

(define (set-cache-home! path)
  (putenv "XDG_CACHE_HOME" (maybe-path->string path)))

(define (set-config-home! path)
  (putenv "XDG_CONFIG_HOME" (maybe-path->string path)))

(define (set-data-home! path)
  (putenv "XDG_DATA_HOME" (maybe-path->string path)))

(define (set-runtime-home! path)
  (putenv "XDG_RUNTIME_HOME" (maybe-path->string path)))

(define (set-state-home! path)
  (putenv "XDG_STATE_HOME" (maybe-path->string path)))

;; ============================================================================
;; Path variables
;; ============================================================================

(define (data-dirs)
  (get-xdg-var "XDG_DATA_DIRS"
               'base
               (make-path-list-string
                (filter directory-exists?
                        (append (map string->path
                                     '("/opt/homebrew/share"
                                       "/usr/share"
                                       "/usr/local/share"))
                                (list (data-home)))))))

(define (set-data-dirs! path-list)
  (putenv "XDG_DATA_DIRS" path-list))

(define (config-dirs)
  (get-xdg-var "XDG_CONFIG_DIRS"
               'none
               (make-path-list-string (config-dir-candidates #:defaults? #t))))

(define (set-config-dirs! path-list)
  (putenv "XDG_CONFIG_DIRS" path-list))
