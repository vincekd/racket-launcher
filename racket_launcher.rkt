#!/usr/bin/racket

#lang racket
(require racket/class)
(require racket/gui)

;; Roadmap
;; 1) get the gui up and running
;; 2) serialize/unserialize list ;;(serialize apps)
;;    --file for most common/favorites, history, etc.
;; 3) keep process running, use keybindings to show/hide gui


;; Improvements:
;; 1) get exec values from .desktop files and use those in the search
;; 2) improvements to matching-> see below @ regexp-match-app

(define racket_launcher%
  (class object%
		 (super-new)
		 
		 ;;get environment paths to generate list of applications
		 (define (getpath envname)
		   (path-list-string->path-list (getenv envname) empty))
		 
		 ;;get all files from each path directory, filtering out directories
		 (define (getdir path withpath)
		   (define tmp-arr empty)
		   (if withpath
			   (filter file-exists? (directory-list path #:build? path))
			   ((lambda ()
				 (for-each (lambda (file)
							 ;;if file-exists? strip path and return name
							 (cond [(file-exists? file)
									(set! tmp-arr (append tmp-arr (let-values ([(path name dir)
																				(split-path file)])
																	(list (path->string name)))))]))
						   (directory-list path #:build? path)) tmp-arr))))

		 
		 ;;merges list of lists into single list
		 (define (merge-lists lists newlist)
		   (if (empty? lists)
			   newlist
			   (merge-lists (rest lists) (append (first lists) newlist))))

		 ;;reads the name from the .desktop file
		 (define (get-desktop-file-name file)
		   ;;read in file and parse it for [name]="" & [exec]=""
		   (define str (file->string file))
		   (define name1 (regexp-match #rx"(?i:name=([^\n]+))" str))
		   (define exec1 (regexp-match #rx"(?i:exec=([^\n]+))" str))
		   (if (and name1 exec1)
			   (cons (list-ref name1 1) (list-ref exec1 1)) null))

		 ;;match case insensitively
		 ;;TODO: add better matching
		 ;;   1) if all one case match insensitively, else match sensitive
		 ;;   2) allow typos, close matches
		 ;;   3) allow regexps, only sometimes
		 ;;   4) allow path searching 
		 (define (regexp-match-app app str)
		   (regexp-match (regexp (string-append "(?i:^" str ")")) app))
		 
		 ;;take in a string and an app pair, returns #t if matches
		 (define (filter-app-str app str)
		   (if (pair? app)
			   (regexp-match-app (car app) str)
			   (regexp-match-app app str)))
		 
		 ;;combines names for each app
		 (define (check-desktop-files appdirs)
		   (define dfiles empty)
		   (define out empty)
		   (for-each (lambda (dir)
					   (set! dir (build-path dir "applications"))
					   (cond [(directory-exists? dir)
							  (set! dfiles (append (getdir dir #t) dfiles))])) appdirs)
		   (for-each (lambda (file)
					   (define tmp (get-desktop-file-name file))
					   ;;if not null and name != exec, add to list
					   (cond [(and (not (null? tmp))
								   (not (string-ci=? (car tmp) (cdr tmp))))
							  (set! out (append out (list tmp)))])) dfiles) out)
		 		 
		 ;;get all apps to be searched
		 ;; if pair, then second is data
		 (define apps (make-hash))
		 (define (get-apps)
		   (if (> (hash-count apps) 0)
			   apps
			   ((lambda ()
				  ;; 1) get files
				  ;; 2) foreach file, split name into characters
				  ;; 3) create hash at each letter
				  ;; 4) if final letter, mark that hash as a terminal one
				  (define paths (getpath "PATH"))
				  (for-each (lambda (path)
							  (define files (directory-list path #:build? path))
							  (cond [files
									  (for-each (lambda (file)
												  (let-values ([(path name dir?) (split-path file)])
													(cond [(not dir?) 
														   (let ([splits (regexp-split #rx"" name)])
															 (define final-hash (trie-add apps splits))
															 ;;get terminal hash and save data there for execution
															 (hash-set! final-hash "exec" file))]))) files)])) paths)
				  
				  ;;TODO: finish-- get .desktop files now
				  (set! paths (getpath "XDG_DATA_DIRS"))
				  
				  apps))))
		 
		 (define (trie-add hash strs)
		   ;;if strs empty, return hash
		   (if (empty? strs) hash
			   (if (equal? (first strs) "") (trie-add hash (rest strs))
				   (trie-add (hash-ref! hash (first strs) (make-hash)) (rest strs)))))

		 ;; (define apps empty)
		 ;; (define (get-apps)
		 ;;   (if (empty? apps)
		 ;; 	   ;;TODO: parse for duplicates as .desktop files are being read in
		 ;; 	   ;;      as well as #fs
		 ;; 	   ((lambda ()
		 ;; 		  (define pathapps (merge-lists (map (lambda (dir)
		 ;; 											   (getdir dir #f))
		 ;; 											 (getpath "PATH")) empty))
		 ;; 		  (define desktopapps (check-desktop-files
		 ;; 									   (getpath "XDG_DATA_DIRS")))
		 ;; 		  (set! apps (append pathapps desktopapps)) apps)) apps))
		 
		 ;;to be called on user input
		 (define (filter-input-string str)
		   (filter (lambda (li) (filter-app-str li str)) (get-apps)))

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
								   (send textbox set-value str) #t)
								 
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
								   (set! cmd (first splits))
								   (subprocess #f #f #f (find-executable-path cmd) (string-join (rest splits)))
								   (send this show #f) #t)
								 
								 ) ;;end new frame class
						  [label "Racket Launcher"]
						  [width 400]
						  [height 75]
						  [style (list 'no-resize-border 'no-caption)] ;;(list 'no-caption)]
						  [alignment (list 'center 'center)]))
		 (send rlw center 'both)

		 (define textbox (new text-field% [parent rlw]
		 					  [label #f]
		 					  [callback (lambda (t e)
		 								  (update-listbox (filter-input-string
														   (send t get-value))))]))
		 ;;use 'get-data' and 'set-data'-> match labels, set items
		 (define listbox (new list-box% [parent rlw]
		 					  [label #f]
							  [choices empty]
							  [style (list 'single)]))
		 
		 ;;use this to update the listbox when text is entered
		 (define (update-listbox items)
		   (send listbox clear)
		   (for-each (lambda (app)
					   (cond [(pair? app) (send listbox append (car app)
												(cdr app))]
							 [else (send listbox append app #f)])) items))

		 ;;show gui
		 
		 ;;(send rlw show #t)
		 ;;get apps
		 ;;TODO: serialize/cache
		 ;;(update-listbox (get-apps))
		 ;;(get-apps)
		 (display (get-apps))
		 
		 )) ;;end racket_launcher object

;;initialize racket_launcer object-- creates gui
;;do if proc exists here and call it?
(define rkt (new racket_launcher%))


