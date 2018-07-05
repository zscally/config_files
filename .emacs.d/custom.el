(defun sum-d10-rolls (num)
	"Roll 'num' d10 and return the sum"
	(interactive "nNum dice? ")
	(progn
		(random t)
		(message
		 (format "%s"
						 (let ((lst ()))
							 (dotimes (i num)
								 (setf lst (cons (+ (random 9) 1) lst)))
							 (apply '+ lst))))))


(eval-after-load "dired"
	'(progn
		 (define-key dired-mode-map "F" 'my-dired-find-file)
		 (defun my-dired-find-file (&optional arg)
			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
			 (interactive "P")
			 (let ((fn-list (dired-get-marked-files nil arg)))
				 (mapc 'find-file fn-list)))))

(defun my-mark-word (N)
	(interactive "p")
	(if (and
			 (not (eq last-command this-command))
			 (not (eq last-command 'my-mark-word-backward)))
			(set-mark (point)))
	(forward-word N))

(defun my-mark-word-backward (N)
	(interactive "p")
	(if (and
			 (not (eq last-command this-command))
			 (not (eq last-command 'my-mark-word)))
			(set-mark (point)))
	(backward-word N))


;; xah fly keymap
(defun xah-run-current-file ()
	"Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2018-03-01"
	(interactive)
	(let (
				($outputb "*xah-run output*")
				(resize-mini-windows nil)
				($suffix-map
				 ;; (‹extension› . ‹shell program name›)
				 `(
					 ("php" . "php")
					 ("pl" . "perl")
					 ("py" . "python")
					 ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
					 ("ps1" . "start powershell.exe")
					 ("psm1" . "start powershell_ise.exe")
					 ("rb" . "ruby")
					 ("go" . "go run")
					 ("hs" . "runhaskell")
					 ("js" . "node")
					 ("ts" . "tsc") ; TypeScript
					 ("tsx" . "tsc")
					 ("sh" . "bash")
					 ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
					 ("rkt" . "racket")
					 ("ml" . "ocaml")
					 ("vbs" . "cscript")
					 ("tex" . "pdflatex")
					 ("latex" . "pdflatex")
					 ("java" . "javac")
					 ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
					 ))
				$fname
				$fSuffix
				$prog-name
				$cmd-str)
		(when (not (buffer-file-name)) (save-buffer))
		(when (buffer-modified-p) (save-buffer))
		(setq $fname (buffer-file-name))
		(setq $fSuffix (file-name-extension $fname))
		(setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
		(setq $cmd-str (concat $prog-name " \""   $fname "\""))
		(cond
		 ((string-equal $fSuffix "el")
			(load $fname))
		 ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
			(if (fboundp 'xah-ts-compile-file)
					(xah-ts-compile-file current-prefix-arg)
				(if $prog-name
						(progn
							(message "Running")
							(shell-command $cmd-str $outputb ))
					(message "No recognized program file suffix for this file."))))
		 ((string-equal $fSuffix "go")
			;; (when (fboundp 'gofmt) (gofmt) )
			(shell-command $cmd-str $outputb ))
		 ((string-equal $fSuffix "java")
			(progn
				(shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))) $outputb )))
		 (t (if $prog-name
						(progn
							(message "Running")
							(shell-command $cmd-str $outputb ))
					(message "No recognized program file suffix for this file."))))))

(defun org-archive-done-tasks ()
	(interactive)
	(org-map-entries
	 (lambda ()
		 (org-archive-subtree)
		 (setq org-map-continue-from (outline-previous-heading)))
	 "/DONE" 'tree))
