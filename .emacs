(setq package-list '(xah-fly-keys which-key better-defaults elpy helm flycheck smex rainbow-delimiters web-mode js2-mode json-mode go-mode go-errcheck rust-mode flycheck-rust cargo lua-mode magit markdown-mode latex-preview-pane chef-mode ansible puppet-mode salt-mode docker flyspell writegood-mode wc-mode el-get))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(package-initialize)

(or (file-exists-p package-user-dir) (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'ido)
(ido-mode t)

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "dvorak") ; required if you use qwertyb

(xah-fly-keys 1)

(define-key xah-fly-c-keymap (kbd "l") (kbd "s u RET"))


;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

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
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages
   (quote
    (wc-mode better-defaults elpy which-key xah-fly-keys rust-mode markdown-mode))))
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
(toggle-scroll-bar -1) 
(add-hook 'before-save-hook 'whitespace-cleanup)

