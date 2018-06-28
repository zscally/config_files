(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(setq dired-hide-details-mode nil)


(winner-mode 1)

(setq dired-dwim-target t)

(require 'package)
(add-to-list 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
						 '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	 '("marmalade" . "http://marmalade-repo.org/packages/")
	 t)

(setq package-list '(which-key better-defaults elpy flycheck rainbow-delimiters web-mode js2-mode json-mode go-mode go-errcheck rust-mode flycheck-rust cargo lua-mode magit markdown-mode latex-preview-pane chef-mode ansible puppet-mode salt-mode docker flyspell writegood-mode wc-mode el-get emr csharp-mode auto-indent-mode undo-tree epa flycheck visual-regexp visual-regexp-steroids aggressive-indent powershell))
																				; company mode and auto-complete and auto-install-el from package manager on gnu/linux
																				; TODO: Add to .lisp and unify

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'define-word)
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
																				; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

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

(require 'unbound)

(require 'icicles)
(icy-mode 1)
(require 'dirtree)

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

(winner-mode 1)
(require 'ido)
(require 'dired+)
(ido-mode t)

;; xah-fly-keys configuration and shortcuts
(require 'xah-fly-keys)

;; custom functions used


;; xah fly keymap
(xah-fly-keys-set-layout "dvorak") ; required if you use qwertyb

(xah-fly-keys 1)

(define-key xah-fly-c-keymap (kbd "l") (kbd "s u RET")) ; insert new line after
(define-key xah-fly-c-keymap (kbd "u") 'capitalize-word)

;; TODO: Fix and make system specific
(setq org-default-notes-file "~/Dropbox/Notes.org")

(define-key xah-fly-dot-keymap (kbd "f") (lambda () (interactive) (find-file "~/Dropbox/Notes.org")))
(define-key xah-fly-dot-keymap (kbd "r") 'org-refile)
(define-key xah-fly-dot-keymap (kbd "n") 'org-capture)

(define-key xah-fly-c-keymap (kbd "s") 'bookmark-set)

(define-key xah-fly-c-keymap (kbd "w") 'writegood-mode)

(define-key xah-fly--tab-key-map (kbd "t") 'toggle-truncate-lines)
(define-key xah-fly--tab-key-map (kbd "l") 'fill-region)
(define-key xah-fly--tab-key-map (kbd "c") 'comment-or-uncomment-region)


(define-key xah-fly--tab-key-map (kbd "e") 'xah-run-current-file)
(define-key xah-fly--tab-key-map (kbd "n") (lambda () (interactive) (find-file "~/Desktop/Notes.org")))
(define-key xah-fly--tab-key-map (kbd "b") 'switch-to-buffer)
(define-key xah-fly--tab-key-map (kbd "o") 'find-file)
(define-key xah-fly--tab-key-map (kbd "d") 'ido-find-file-in-dir)

(define-key xah-fly-comma-keymap (kbd ".") 'backward-kill-sentence)
(define-key xah-fly-comma-keymap (kbd "p") 'kill-sentence)




;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

;; Text processing

(setq-default truncate-lines t)

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

(defvar my-keys-minor-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-,") 'scroll-other-window-down)
		(define-key map (kbd "C-.") 'scroll-other-window)
		(define-key map (kbd "M-g") 'my-mark-word-backward)
		(define-key map (kbd "M-r") 'my-mark-word)
		map)
	"my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
	"A minor mode so that my key settings override annoying major modes."
	:init-value t
	:lighter " my-fly")

(my-keys-minor-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends (quote (ascii html latex md odt confluence)))
 '(package-selected-packages
	 (quote
		(powershell xah-fly-keys writegood-mode which-key web-mode wc-mode visual-regexp-steroids undo-tree smex salt-mode rainbow-delimiters puppet-mode magit lua-mode latex-preview-pane js2-mode go-mode go-errcheck flycheck-rust emr elpy el-get drag-stuff docker csharp-mode company-irony chef-mode cargo better-defaults auto-indent-mode ansible aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
