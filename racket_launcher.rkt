#!/usr/bin/racket

#lang racket
(require racket/base)
;;(require racket/class)
(require racket/gui/base)

;; Roadmap
;; 1) hash
;; 2) serialize/unserialize list ;;(serialize apps)
;;    --file for most common/favorites, history, etc.
;; 3) keep process running, use keybindings to show/hide gui
;; 4) speed up somehow



(define racket_launcher%
  (class object%
		 (super-new)
		 
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
		 		 
		 ;;get all apps to be searched into hash table
		 (define apps (make-hash))
		 
		 ;;iterates over path && creates hash table
		 (define (get-apps)
		   (if (> (hash-count apps) 0)
			   apps
			   ((lambda ()
				  (define paths (filter directory-exists? (getpath "PATH")))
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
																(hash-set! final-hash "exec" file))))]))) files)])) paths)
				  ;;adds desktop files
				  (set! paths (filter directory-exists?
									  (map (lambda (path)
											 (build-path path "applications"))
										   (getpath "XDG_DATA_DIRS"))))
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
											   files)])) paths)
				  apps))))
		 
		 (define (trie-add hash strs)
		   ;;if strs empty, return hash
		   (if (empty? strs) hash
			   (if (equal? (first strs) "") (trie-add hash (rest strs))
				   (trie-add (hash-ref! hash (first strs) (make-hash)) (rest strs)))))
		 
		 (define (get-root hash strs)
		   (if (or (empty? strs) (not hash)) hash
			   (if (equal? (first strs) "") (get-root hash (rest strs))
				   (get-root (hash-ref hash (first strs) #f) (rest strs)))))

		 ;;create gui
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
									[(equal? keycode 'escape) (send this show #f)]
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
								   (send textbox on-subwindow-char this e) #t)
								 
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
									   ((lambda ()
										  (set! cmd (send listbox get-data cmd))
										  (cond [(not cmd) (set! cmd (send listbox get-string-selection))]))))
								   (define splits (regexp-split #rx" " cmd))
								   (subprocess #f #f #f (find-executable-path
														 (first splits))
											   (string-join (rest splits)))
								   (send this show #f) #t)
								 
								 ) ;;end new frame class
						  [label "Racket Launcher"]
						  [width 400]
						  [height 75]
						  [style (list 'no-resize-border 'no-caption)]
						  [alignment (list 'center 'center)]))

		 ;;center on screen
		 (send rlw center 'both)

		 ;;instantiate textbox
		 (define textbox (new text-field% [parent rlw]
		 					  [label #f]
		 					  [callback (lambda (t e)
		 								  (update-listbox
										   (send t get-value)))]))
		 
		 ;;use 'get-data' and 'set-data'-> match labels, set items
		 (define listbox (new list-box% [parent rlw]
		 					  [label #f]
							  [choices empty]
							  [style (list 'single)]))
		 
		 ;;use this to update the listbox when text is entered
		 (define (update-listbox str)
		   ;;TODO: don't clear every time?
		   (send listbox clear)
		   ;;traverse trie and find all values
		   (define root (get-root apps (regexp-split #rx"" (string-trim str))))
		   (if (not root) #f
			   (traverse-trie root)))

		 (define (traverse-trie trie)
		   ;;traverse from root node adding all "name" keys to listbox
		   (hash-for-each trie (lambda (key value)
								 (cond [(equal? key "name")
										(send listbox append value
											  (hash-ref trie "exec"))]
									   [(not (equal? key "exec"))
										(traverse-trie value)]))))

		 ;;show gui
		 (send rlw show #t)
		 ;;get apps
		 ;;TODO: serialize/cache
		 (get-apps)
		 (update-listbox "")
		 ;;(send rlw show #f)
		 ;;(display (get-apps))
		 
		 )) ;;end racket_launcher object

;;initialize racket_launcer object-- creates gui
;;do if proc exists here and call it?
(define rkt (new racket_launcher%))


