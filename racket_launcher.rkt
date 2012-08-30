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

		 ;;get environment paths to generate list of applications
		 (define (getpath envname)
		   (path-list-string->path-list (getenv envname) empty))
		 
		 ;;get all files from each path directory, filtering out directories
		 (define (getdir path)
		   (filter file-exists? (directory-list path #:build? path)))
		 
		 ;;merges list of lists into single list
		 (define (merge-lists lists newlist)
		   (if (empty? lists)
			   newlist
			   (merge-lists (rest lists) (append (first lists) newlist))))

		 ;;reads the name from the .desktop file
		 ;; TODO: will eventually read the exec option as well
		 (define (get-desktop-file-name file)
		   ;;read in file and parse it for [name]=""
		   (define str (file->string file))
		   (list-ref (regexp-match #rx"Name=([^\n]*)" str) 1))
		 
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
		   (if (pair? (first app))
			   (or (regexp-match-app (car (first app)) str)
				   (regexp-match-app (cdr (first app)) str))
			   (regexp-match-app (first app) str)))
		 
		 ;;combines names for each app
		 ;;TODO: redo for exec='' stuff
		 (define (check-desktop-files file appdirs)
		   (let-values ([(base name dir) (split-path file)])
			 (set! name (path->string name))
			 (define newlist (list name file))
			 (map (lambda (dir)
					(define tmp (build-path dir "applications" (string-append name ".desktop")))
					(if (file-exists? tmp)
						(set! newlist (list (cons name (get-desktop-file-name tmp)) file))
						#f)) appdirs)
			 newlist))

		 ;;get all apps to be searched
		 (define apps (map (lambda (li)
		 					 (check-desktop-files li (getpath "XDG_DATA_DIRS")))
		 				   (merge-lists (map getdir (getpath "PATH")) empty)))

		 ;;create gui
		 (define rlw (new (class frame%
								 (super-new)
								 (define/override (on-traverse-char e)
								   (define keycode (send e get-key-code))
								   (cond
									[(equal? keycode #\return) (handle-enter)]
									[(equal? keycode 'escape) (send this show #f)]
									[else #f]))
								 (define (handle-enter)
								   ;;handle incoming enters-launch current program, hide box
								   (display (send listbox get-string-selection))
								   ;;(subprocess #f #f #f (get-process))
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
		 					  [choices (list "mirage" "rhythmbox")]))
		 (send rlw show #t)

		 ;;use this to update the listbox when text is entered
		 (define (update-listbox items)
		   (length items) #f)
		 
		 ;;to be called on user input
		 (define (filter-input-string str)
		   (filter (lambda (li) (filter-app-str li str)) apps))
		 (super-new)))

;;initialize racket_launcer object-- creates gui
(define rkt (new racket_launcher%))


