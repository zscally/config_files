(require 'package)
(add-to-list 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
						 '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	 '("marmalade" . "http://marmalade-repo.org/packages/")
	 t)

(setq package-list '(xah-fly-keys which-key better-defaults elpy helm flycheck smex rainbow-delimiters web-mode js2-mode json-mode go-mode go-errcheck rust-mode flycheck-rust cargo lua-mode magit markdown-mode latex-preview-pane chef-mode ansible puppet-mode salt-mode docker flyspell writegood-mode wc-mode el-get emr csharp-mode auto-indent-mode undo-tree epa flycheck))
																				; company mode and auto-complete and auto-install-el from package manager on gnu/linux
; TODO: Add to .lisp and unify

(add-hook 'after-init-hook 'global-company-mode)

(if (eq system-type 'gnu/linux)
		'add-to-list 'package-list (ps-ccrypt))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; keep old versions, much version control
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
			'(kill-ring
	search-ring
	regexp-search-ring))


(setq indent-tab-mode t)
(show-paren-mode 1)
(setq-default tab-width 2)
(setq tab-width 2)
(display-time-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(server-start)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(if (eq system-type 'windows-nt)
		(require 'windows-path))

(setq visible-bell 1)

(when (boundp 'w32-pipe-read-delay)
	(setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
	(setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(package-initialize)

(require 'auto-indent-mode)

(load-theme 'leuven)

(or (file-exists-p package-user-dir) (package-refresh-contents))

(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))

(require 'ido)
(ido-mode t)

;; xah-fly-keys configuration and shortcuts
(require 'xah-fly-keys)

;; custom functions used


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

(xah-fly-keys-set-layout "dvorak") ; required if you use qwertyb

(xah-fly-keys 1)

(define-key xah-fly-c-keymap (kbd "l") (kbd "s u RET")) ; insert new line after
(define-key xah-fly-c-keymap (kbd "u") 'capitalize-word)

;; org mode settings
(setq org-default-notes-file "~/Dropbox/Notes.org")

(define-key xah-fly-dot-keymap (kbd "f") (lambda () (interactive) (find-file "~/Dropbox/Notes.org")))
(define-key xah-fly-dot-keymap (kbd "r") 'org-refile)
(define-key xah-fly-dot-keymap (kbd "n") 'org-capture)

(define-key xah-fly-c-keymap (kbd "s") 'bookmark-set)

(define-key xah-fly-c-keymap (kbd "w") 'writegood-mode)

(define-key xah-fly--tab-key-map (kbd "t") 'toggle-truncate-lines)
(define-key xah-fly--tab-key-map (kbd "l") 'fill-region)
(define-key xah-fly--tab-key-map (kbd "c") 'comment-or-uncomment-region)

(define-key xah-fly-comma-keymap (kbd ".") 'backward-kill-sentence)
(define-key xah-fly-comma-keymap (kbd ",") 'kill-sentence)
(define-key xah-fly--tab-key-map (kbd "e") 'xah-run-current-file)
(define-key xah-fly--tab-key-map (kbd "n") (lambda () (interactive) (find-file "~/Desktop/Notes.org")))
(define-key xah-fly--tab-key-map (kbd "b") 'switch-to-buffer)
(define-key xah-fly--tab-key-map (kbd "o") 'find-file)
(define-key xah-fly--tab-key-map (kbd "d") 'ido-find-file-in-dir)

;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

;; Text processing

(set-default 'truncate-lines nil)

;; Python and coding tools

(elpy-enable)

(which-key-mode)

(prettify-symbols-mode)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'emr-c-mode)
(add-hook 'elpy-mode-hook 'auto-fill-mode)

(set-face-attribute 'default nil :height 200)
(add-to-list 'load-path "~/.cargo/bin")

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(defun my-hook ()
	(local-unset-key "\""))
(add-hook 'LaTeX-mode-hook 'my-hook)

(hl-line-mode)

(require 'writegood-mode)
(wc-mode)

;;(setq langtool-language-tool-jar "~/LanguageTool-4.1/languagetool-commandline.jar")
;; (require 'langtool)

(menu-bar-mode 1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq inhibit-splash-screen t)

(setq latex-run-command "pdflatex")

(setq python-shell-interpreter "/usr/bin/python3.6")
