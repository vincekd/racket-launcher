#!/usr/bin/racket

#lang racket

(require racket/base)
;;(require racket/gui/base)

;;get environment paths to generate list of applications
(define (getpath envname)
  (path-list-string->path-list (getenv envname) empty))

;;reads the name from the .desktop file
(define (get-desktop-file-name file)
  ;;read in file and parse it for [name]="" & [exec]=""
  (define str (file->string file))
  (define name1 (regexp-match #rx"(?i:name=([^\n]+))" str))
  (define exec1 (regexp-match #rx"(?i:exec=([^\n]+))" str))
  (if (and name1 exec1)
	  (cons (list-ref name1 1) (list-ref exec1 1)) null))

(define (trie-add hasht strs)
  ;;if strs empty, return hasht
  (if (empty? strs) hasht
	  (if (equal? (first strs) "") (trie-add hasht (rest strs))
		  (trie-add (hash-ref! hasht (first strs) (make-hash)) (rest strs)))))

(define (get-root hasht strs)
  (if (or (empty? strs) (not hasht)) hasht
	  (if (equal? (first strs) "") (get-root hasht (rest strs))
		  (get-root (hash-ref hasht (first strs) #f) (rest strs)))))

(define (selected-app name exec)
  (display name)
  (newline))

(define (traverse-trie trie)
  (hash-for-each trie (lambda (key value)
						(cond [(equal? key "name")
							   (selected-app value (hash-ref trie "exec"))]
							  [(not (equal? key "exec"))
							   (traverse-trie value)]))))
							   ;;(thread (lambda () (traverse-trie value)))]))))
(define (cull-list hasht str)
  (define root (get-root hasht (regexp-split #rx"" (string-trim str))))
  (if (not root) #f
	  (traverse-trie root)))

;;define & get apps
(define apps (make-hash))
;;make these into recursive lambda functions, then thread them
;;(define thd (thread (lambda ()
(for-each (lambda (path)
			(define files (directory-list path #:build? path))
			(cond [files
				   (for-each (lambda (file)
							   (let-values ([(path name dir?) (split-path file)])
								 (cond [(not dir?)
										((lambda ()
										   (set! name (path->string name))
										   (let ([splits (regexp-split #rx"" (string-downcase name))])
											 (define final-hash (trie-add apps splits))
											 ;;get terminal hash and save data there for execution
											 (hash-set! final-hash "name" name)
											 (hash-set! final-hash "exec" file))))]))) files)]))
		  (filter directory-exists? (getpath "PATH")))
;;)))
;;adds desktop files
(for-each (lambda (path)
			(define files (directory-list path #:build? path))
			(cond [files
				   (for-each (lambda (file)
							   (cond [(file-exists? file)
									  (define desktop (get-desktop-file-name file))
									  (cond [(not (null? desktop))	
											 (let ([splits (regexp-split #rx"" (string-downcase (car desktop)))])
											   (define final-hash (trie-add apps splits))
											   (hash-set! final-hash "name" (car desktop))
											   (hash-set! final-hash "exec" (cdr desktop)))])]))
							 files)])) (filter directory-exists?
											   (map (lambda (path)
													  (build-path path "applications"))
													(getpath "XDG_DATA_DIRS"))))

;;(thread-wait thd)

(cull-list apps "aa")

;;get gui




