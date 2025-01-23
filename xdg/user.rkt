#lang racket/base

(require "./private/get.rkt")

(provide desktop-dir
         desktop-dir/for-program
         set-desktop-dir!
         download-dir
         download-dir/for-program
         set-download-dir!
         templates-dir
         templates-dir/for-program
         set-templates-dir!
         public-share-dir
         public-share-dir/for-program
         set-public-share-dir!
         documents-dir
         documents-dir/for-program
         set-documents-dir!
         music-dir
         music-dir/for-program
         set-music-dir!
         pictures-dir
         pictures-dir/for-program
         set-pictures-dir!
         videos-dir
         videos-dir/for-program
         set-videos-dir!)

;; ============================================================================
;; Internal
;; ============================================================================

(define (env-name name)
  (string-append "XDG_" (string-upcase (symbol->string name)) "_DIR"))

(define (home-path dir-name)
  (build-path (find-system-path 'home-dir) dir-name))

(define (path/for-program program-name var-name default-dir-name)
  (let ((user-dir (maybe-string->path
                   (get-xdg-var (env-name var-name)
                                'user
                                (home-path default-dir-name)))))
    (if program-name
        (build-path user-dir program-name)
        user-dir)))

;; ============================================================================
;; Individual location variables
;; ============================================================================

(define (desktop-dir)
  (desktop-dir/for-program #f))

(define (desktop-dir/for-program program-name)
  (path/for-program program-name 'desktop "Desktop"))

(define (set-desktop-dir! path)
  (set-xdg-var! (env-name 'desktop) path))

;; ----------------------------------------------------------------------------

(define (download-dir)
  (download-dir/for-program #f))

(define (download-dir/for-program program-name)
  (path/for-program program-name 'download "Downloads"))

(define (set-download-dir! path)
  (set-xdg-var! (env-name 'download) path))

;; ----------------------------------------------------------------------------

(define (templates-dir)
  (templates-dir/for-program #f))

(define (templates-dir/for-program program-name)
  (path/for-program program-name 'templates "Templates"))

(define (set-templates-dir! path)
  (set-xdg-var! (env-name 'templates) path))

;; ----------------------------------------------------------------------------

(define (public-share-dir)
  (public-share-dir/for-program #f))

(define (public-share-dir/for-program program-name)
  (path/for-program program-name "PUBLICSHARE" "Public"))

(define (set-public-share-dir! path)
  (set-xdg-var! (env-name "PUBLICSHARE") path))

;; ----------------------------------------------------------------------------

(define (documents-dir)
  (documents-dir/for-program #f))

(define (documents-dir/for-program program-name)
  (path/for-program program-name 'documents "Documents"))

(define (set-documents-dir! path)
  (set-xdg-var! (env-name 'documents) path))

;; ----------------------------------------------------------------------------

(define (music-dir)
  (music-dir/for-program #f))

(define (music-dir/for-program program-name)
  (path/for-program program-name 'music "Music"))

(define (set-music-dir! path)
  (set-xdg-var! (env-name 'music) path))

;; ----------------------------------------------------------------------------

(define (pictures-dir)
  (pictures-dir/for-program #f))

(define (pictures-dir/for-program program-name)
  (path/for-program program-name 'pictures "Pictures"))

(define (set-pictures-dir! path)
  (set-xdg-var! (env-name 'pictures) path))

;; ----------------------------------------------------------------------------

(define (videos-dir)
  (pictures-dir/for-program #f))

(define (videos-dir/for-program program-name)
  (path/for-program program-name 'videos "Movies"))

(define (set-videos-dir! path)
  (set-xdg-var! (env-name 'videos) path))
