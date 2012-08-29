#!/usr/bin/racket

#lang racket
(require racket/class)
(require racket/gui)

;; Roadmap
;; 1) get the gui up and running
;; 2) serialize/unserialize list ;;(serialize apps)
;; 3) keep process running, use keybindings to show/hide gui


;; Improvements:n
;; 1) get exec values from .desktop files and use those in the search
;; 2) improvements to matching-> see below @ regexp-match-app

(define racket_launcher%
  (class object%

		 ;;create gui
		 (define frame (new frame% [label "Test"]
							[width 400]
							[height 75]
							[style (list 'no-resize-border 'no-caption)]
							[alignment (list 'center 'center)]
							[callback (lambda (t e)
										(display (get-field event-type e)))]))
		 (define textbox (new text-field% [parent frame]
							  [label #f]
							  [callback (lambda (t e)
										  (display t))]))
		 (define listbox (new list-box% [parent frame]
							  [label #f]
							  [choices (list "hi" "bye")]))
		 (send frame show #t)
		 
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

		 ;;to be called on user input
		 (define/public (filter-input-string str)
		   (filter (lambda (li) (filter-app-str li str)) apps))
		 (super-new)))

;;user in- will be changed when gui
(define inputstr "")
(if (< 0 (vector-length (current-command-line-arguments)))
	(set! inputstr (vector-ref (current-command-line-arguments) 0)) #f)
(define rkt (new racket_launcher%))
;;(send rkt filter-input-string inputstr)


