(setq package-list '(xah-fly-keys which-key better-defaults elpy helm flycheck smex rainbow-delimiters web-mode js2-mode json-mode go-mode go-errcheck rust-mode flycheck-rust cargo lua-mode magit markdown-mode latex-preview-pane chef-mode ansible puppet-mode salt-mode docker flyspell writegood-mode wc-mode el-get ps-ccrypt emr undo-tree epa company-mode flycheck))

(load-file "~/powermacs/powermacs.el")

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

(when (>= emacs-major-version 24)
	(require 'package)
	(add-to-list
	 'package-archives
	 ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
	 '("melpa" . "http://melpa.milkbox.net/packages/")
	 t))

(package-initialize)

(load-theme 'leuven)

(or (file-exists-p package-user-dir) (package-refresh-contents))

(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))

(require 'ido)
(ido-mode t)

;; xah-fly-keys configuration and shortcuts

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "dvorak") ; required if you use qwertyb

(xah-fly-keys 1)

(define-key xah-fly-c-keymap (kbd "l") (kbd "s u RET")) ; insert new line after
(define-key xah-fly-c-keymap (kbd "u") 'capitalize-word)

;; org mode settings
(setq org-default-notes-file "~/Dropbox/Notes.org")

(define-key xah-fly-dot-keymap (kbd "f") (lambda () (interactive) (find-file "~/Dropbox/Notes.org")))
(define-key xah-fly-dot-keymap (kbd "r") 'org-refile)
(define-key xah-fly-dot-keymap (kbd "n") 'org-capture)

(define-key xah-fly-dot-keymap (kbd "e") (lambda () (interactive) (find-file "~/Dropbox/Fiction/Edge/Notes.org")))
(define-key xah-fly-c-keymap (kbd "w") 'writegood-mode)

(define-key xah-fly--tab-key-map (kbd "t") 'toggle-truncate-lines)
(define-key xah-fly--tab-key-map (kbd "l") 'fill-region)
(define-key xah-fly--tab-key-map (kbd "c") 'comment-or-uncomment-region)

(define-key xah-fly-comma-keymap (kbd ".") 'backward-kill-sentence)
(define-key xah-fly-comma-keymap (kbd ",") 'kill-sentence)

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
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'emr-c-mode)
(add-hook 'elpy-mode-hook 'auto-fill-mode)

(set-face-attribute 'default nil :height 200)
(add-to-list 'load-path "~/.cargo/bin")
(toggle-frame-maximized)


(defun my-hook ()
	(local-unset-key "\""))
(add-hook 'LaTeX-mode-hook 'my-hook)

(hl-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
	 (quote
		("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(elpy-project-root "~/breeder/")
 '(elpy-rpc-python-command "~/breeder/env/bin/python3.6")
 '(package-selected-packages
	 (quote
		(emr spacemacs-theme wc-mode better-defaults elpy which-key xah-fly-keys rust-mode markdown-mode)))
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3.6"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(define-key key-translation-map "\C-t" "\C-x")



(flyspell-mode)
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)
(wc-mode)

(setq langtool-language-tool-jar "~/LanguageTool-4.1/languagetool-commandline.jar")
(require 'langtool)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'ps-ccrypt)

(setq inhibit-splash-screen t)

(setq latex-run-command "pdflatex")

 (setq python-shell-interpreter "/usr/bin/python3.6")
