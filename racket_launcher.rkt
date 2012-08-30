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
							  (set! out (append out (list tmp)))])) dfiles)
		   out)
		 		 
		 ;;get all apps to be searched
		 ;; if pair, then second is data
		 (define apps empty)
		 (define (get-apps)
		   (if (empty? apps)
			   ;;TODO: parse for duplicates as .desktop files are being read in
			   ;;      as well as #fs
			   ((lambda ()
				  (define pathapps (merge-lists (map (lambda (dir)
													   (getdir dir #f))
													 (getpath "PATH")) empty))
				  (define desktopapps (check-desktop-files
											   (getpath "XDG_DATA_DIRS")))
				  (set! apps (append pathapps desktopapps))
				  apps))
			   apps))
		 
		 ;;to be called on user input
		 (define (filter-input-string str)
		   (filter (lambda (li) (filter-app-str li str)) (get-apps)))

		 ;;create gui
		 (define rlw (new (class frame%
								 (super-new)
								 (define/override (on-traverse-char e)
								   (define keycode (send e get-key-code))
								   (cond
									;;TODO:
									;;if tab autocomplete
									;;if down arrow, start navigating listbox
									[(equal? keycode #\return) (handle-enter)]
									[(equal? keycode 'escape) (send this show #f)]
									[(equal? keycode 'down) (handle-vertical-arrows #f)]
									[(equal? keycode 'up) (handle-vertical-arrows #t)]
									;; [(equal? keycode 'right) (handle-horizontal-arrows #f)]
									;; [(equal? keycode 'left) (handle-horizontal-arrows #t)]
									[else #f]))
								 ;;if left/right arrows, focus textbox, move
								 (define (handle-horizontal-arrows left)
								   ;;(send key-event to text area if not there)
								   (display left))
								 (define (handle-vertical-arrows up)
								   (define selection (send listbox get-selection))
								   ;;TODO: handle 0/length cases
								   (send listbox set-selection
										 (if up (- selection 1)
											 (+ selection 1))))
								 (define (handle-enter)
								   ;;on enter:launch current program, hide box
								   ;;use get-data to get correct executable
								   (define cmd (send listbox get-data (send listbox get-selection)))
								   (cond [(not cmd) (set! cmd (send listbox get-string-selection))])
								   (subprocess #f #f #f (find-executable-path cmd))
								   (send this show #f) #t))
						  [label "Racket Launcher"]
						  [width 400]
						  [height 75]
						  [style (list 'no-resize-border 'no-caption)]
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
		 (send rlw show #t)
		 ;;get apps
		 ;;TODO: serialize/cache
		 (update-listbox (get-apps))
		 
		 ;;end racket_launcher object
		 ))

;;initialize racket_launcer object-- creates gui
;;do if proc exists here and call it?
(define rkt (new racket_launcher%))


