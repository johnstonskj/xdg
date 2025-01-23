#lang racket/base

(require racket/bool
         racket/function
         racket/list
         racket/path
         racket/port
         racket/string
         threading)

(provide get-user-name
         get-program-name
         ;; ===================================================================
         maybe-string->path
         maybe-path->string
         make-path-list-string
         split-path-list-string
         ;; ===================================================================
         get-xdg-var
         set-xdg-var!
         ;; ===================================================================
         config-dir-candidates
         config-file-candidates
         find-config-file
         )

;; ============================================================================
;; Internal
;; ============================================================================

(define (getenv/path name)
  (and~> (getenv name) string->path))

(define (replace-home s)
  (string-replace s "$HOME" (path->string (find-system-path 'home-dir))))

;; ============================================================================
;; Non-XDG Stuff
;; ============================================================================

(define (get-user-name)
  (or (getenv "USER")
      (getenv "LOGNAME")
      (error)))

(define (get-program-name)
  (path->string
   (path-replace-extension
    (file-name-from-path
     (find-system-path 'run-file))
    #"")))

;; ============================================================================
;; Path Functions
;; ============================================================================

(define (maybe-string->path path)
  (cond ((path? path) path)
        ((string? path) (simplify-path path))
        ((symbol? path) (simplify-path (symbol->string path)))
    (else (error))))

(define (maybe-path->string path)
  (cond ((string? path) path)
        ((symbol? path) (symbol->string path))
        ((path? path) (path->string path))
        (else (error))))

(define (make-path-list-string paths)
  (string-join (map maybe-path->string paths) ":"))

(define (split-path-list-string path-list)
  (map string->path (string-split path-list ":" #:repeat? #t)))

;; ============================================================================
;; Shared functions for xdg/base and xdg/user modules
;; ============================================================================

(define (get-from-xdg-file file-name var-name)
  (let* ((lines (call-with-input-file file-name (lambda (p) (port->lines p))))
         (re (regexp (string-append "^" var-name "=\"([^\\\"\"]+)\"$")))
         (value (findf identity
                       (filter-map (lambda (line) (regexp-match re line))
                                   lines))))
    (if value (replace-home (cadr value)) (error))))

(define (get-xdg-var var-name var-group default)
  (or (getenv/path var-name)
      (if (not (symbol=? var-group 'none))
          (get-from-xdg-file (find-config-file var-group) var-name)
          #f)
      default))

(define (set-xdg-var! var-name new-path)
  (putenv var-name (maybe-path->string new-path)))

(define (config-dir-candidates #:defaults? (defaults? #f))
  (filter directory-exists? (map expand-user-path
                                 (if defaults?
                                     '("/etc/xdg"
                                       "/opt/xdg"
                                       "/opt/homebrew/etc/xdg")
                                     '("~/.config")))))

(define (config-file-candidates kind #:defaults? (defaults? #f))
  (let* ((file-name (cond ((and (symbol=? kind 'base) (not defaults?))
                           "base-dirs.dirs")
                          ((and (symbol=? kind 'user) (not defaults?))
                           "user-dirs.dirs")
                          ((and (symbol=? kind 'base) defaults?)
                           "base-dirs.defaults")
                          ((and (symbol=? kind 'user) defaults?)
                           "user-dirs.defaults")
                          (else (error))))
         (dir-candidates (config-dir-candidates #:defaults? defaults?)))
    (map (lambda (d) (build-path d file-name)) dir-candidates)))

(define (find-config-file kind)
  (findf file-exists? (config-file-candidates kind)))
