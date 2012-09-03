#!/usr/bin/gracket

#lang racket/base

(require racket/string racket/list racket/file
		 racket/class racket/udp racket/gui/base)

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

(define (traverse-trie trie proc)
  (hash-for-each trie (lambda (key value)
						(cond [(equal? key "name")
							   (proc value (hash-ref trie "exec"))]
							  [(not (equal? key "exec"))
							   (traverse-trie value proc)]))))

;;define & get apps
(define apps (make-hash))
;;make these into recursive lambda functions, then thread them
;;recursively through paths
(define (get-paths paths proc)
  (cond [(or (not paths) (empty? paths)) #f]
		[(not (directory-exists? (first paths))) (get-paths (rest paths) proc)]
		;;make proc execute in a new thread and wait for it to return
		[else (begin
				(proc (directory-list (first paths) #:build? (first paths)))
				(get-paths (rest paths) proc))]))

(define (get-files files)
  (cond [(or (not files) (empty? files)) #f]
		[else (let-values ([(path name dir?) (split-path (first files))])
				(set! name (string-downcase (path->string name)))
				(cond [(not dir?) (let ([splits (regexp-split #rx"" name)])
									(define final-hash (trie-add apps splits))
									(hash-set! final-hash "name" name)
									(hash-set! final-hash "exec" (first files)))])
				(get-files (rest files)))]))

(define (get-desktop-files files)
  (cond [(or (not files) (empty? files)) #f]
		[(file-exists? (first files))
		 (let ([desktop (get-desktop-file-name (first files))])
		   (if (null? desktop) #f
			   (let ([splits
					  (regexp-split #rx"" (string-downcase (car desktop)))])
				 (define final-hash (trie-add apps splits))
				 (hash-set! final-hash "name" (car desktop))
				 (hash-set! final-hash "exec" (cdr desktop))
				 (get-desktop-files (rest files)))))]
		[else (get-desktop-files (rest files))]))

;;get gui
;;(define rkt (new (dynamic-require 'racket/gui/base 'frame%) [label "hi"]))
(define rlw (new (class frame%
						(super-new)
						;;to override mouse events
						;;if user changes selection
						;; (define/override (on-subwindow-event e)
						;;   )
						(define/override (on-traverse-char e)
						  (define keycode (send e get-key-code))
						  (cond
						   ;;TODO: add emacs- style navigation
						   ;;CTRL key (backspace,fb,np)
						   [(equal? keycode #\tab) (handle-tab)]
						   [(equal? keycode #\return) (handle-enter)]
						   [(equal? keycode 'escape) (control-racket-launcher #f)]
						   [(equal? keycode 'down) (scroll-selection #f)]
						   [(equal? keycode 'up) (scroll-selection #t)]
						   ;;TODO:send to textarea no matter focus
						   [(or (equal? keycode 'right) (equal? keycode 'left))
							(handle-horizontal-arrows #f e)]
						   [else #f]))
						
						;;tabs- autocomplete
						(define (handle-tab)
						  ;;if equals first selection, increment and get next
						  (define str (send listbox get-string-selection))
						  (define text (send textbox get-value))
						  (cond [(string-ci=? text str) ((lambda()
														   (scroll-selection #f)
														   (set! str (send listbox get-string-selection))))])
						  (send textbox set-value (string-downcase str)) #t)
						
						;;if left/right arrows, focus textbox, move
						(define (handle-horizontal-arrows left e)
						  (send textbox on-subwindow-char this e) #f)
						
						;;handle up and down arrows
						(define (scroll-selection up)
						  (define selection (send listbox get-selection))
						  (if selection
							  ((lambda ()
								 ;;wrap around?
								 (cond [(and up (> selection 0))
										(set! selection (- selection 1))]
									   [(and (not up) (< selection (- (send listbox get-number) 1)))
										(set! selection (+ selection 1))])
								 (send listbox set-selection selection)
								 (send listbox set-first-visible-item selection))) #f))

						;;on enter:launch current program, hide box
						;;use get-data to get correct executable
						(define (handle-enter)
						  (define cmd (send listbox get-selection))
						  (if (not cmd) (set! cmd (send textbox get-value))
							  (begin
								 (set! cmd (send listbox get-data cmd))
								 (cond [(not cmd) (set! cmd (send listbox get-string-selection))])))
						  (define splits (regexp-split #rx" " cmd))
						  (parameterize ([subprocess-group-enabled #t])
										(subprocess #f #f #f
													(find-executable-path
													 (first splits))
													(string-join
													 (rest splits))))
						  (control-racket-launcher #f) #t)
						
						) ;;end new frame class
				 [label "Racket Launcher"]
				 [width 400]
				 [height 75]
				 [style (list 'no-resize-border 'no-caption)]
				 [alignment (list 'center 'center)]))

;;center on screen
(send rlw center 'both)

(define (selected-app name exec)
  (send listbox append name exec))

(define (cull-list hasht str)
  (define root (get-root hasht (regexp-split #rx"" (string-trim str))))
  (if (not root) (send listbox clear)
	  (begin
		(send listbox clear)
		(traverse-trie root selected-app))))

;;instantiate textbox
(define textbox (new text-field%
					 [parent rlw]
					 [label #f]
					 [callback (lambda (t e)
								 (cull-list apps
								  (string-downcase (send t get-value))))]))

;;use 'get-data' and 'set-data'-> match labels, set items
(define listbox (new list-box%
					 [parent rlw]
					 [label #f]
					 [choices empty]
					 [style (list 'single)]))

(define (init-launcher)
  (set! apps (make-hash))
  (get-paths (getpath "PATH") get-files)
  (get-paths (map (lambda (dir)
					(build-path dir "applications"))
				  (getpath "XDG_DATA_DIRS")) get-desktop-files)
  ;;add apps to select box
  (cull-list apps ""))

;;udp socket
(define socket (udp-open-socket "localhost" 34543))
(udp-bind! socket "localhost" 34543)

;;use on resume event && on suspend events
(define (control-racket-launcher on)
  (if on
	  (begin
		(send rlw show #t)
		(init-launcher))
	  (begin
		(send textbox set-value "")
		(send rlw show #f)
		(let-values ([(nint str? intin)
					  (udp-receive! socket (make-bytes 5454))])
		  (control-racket-launcher #t)))))
(control-racket-launcher #f)






