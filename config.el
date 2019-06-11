(setq evil-move-cursor-back t)
(projectile-global-mode)
(setq mac-command-modifier 'control)
(toggle-truncate-lines 0)

(if (eq system-type 'gnu/linux)
    (setq notes-file "~/Desktop/Notes.org.gpg")
  (setq notes-file "~/Desktop/Notes.org"))

(setq org-use-speed-commands t)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

(evil-define-key 'insert ein:edit-cell-mode-map
  (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'hybrid ein:edit-cell-mode-map
  (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'normal ein:edit-cell-mode-map
  (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))

(evil-define-key 'normal ein:edit-cell-mode-map
  (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'insert ein:edit-cell-mode-map
  (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'hybrid ein:edit-cell-mode-map
  (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))

(if (eq system-type 'gnu/linux)
    (with-eval-after-load 'ox-latex
      (add-to-list 'org-latex-classes
                   '("memoir-book"
                     "\\documentclass[11pt,a4paper]{memoir}"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ))))

(defun load-notes ()
  "Load notes org"
  (interactive)
  (find-file notes-file))

(spacemacs/set-leader-keys
  "on" 'load-notes
  "or" 'rename-buffer
  )
(setq org-agenda-files (list notes-file))

(setq flyspell-mode nil)

(load "~/.emacs.d/private/custom.el")
(load "~/.emacs.d/private/blacken.el")

(add-hook 'python-mode-hook 'blacken-mode)

;; Todo: Move to layer / contrib
(use-package ein
  :ensure t
  :config
  (require 'ein-multilang)
  (require 'ein-notebook)
  (require 'ein-subpackages))

(setq python-indent 4)
(setq-default python-indent 4)
(setq python-indent-offset 4)
(setq-default python-indent-offset 4)

(setq projectile-globally-ignored-directories '("env"))

(setq python-shell-interpreter "python3")

(setq-default dotspacemacs-configuration-layers
              '((shell :variables shell-default-shell 'term)))

(load "~/.emacs.d/private/capture-templates.el")
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq org-catch-invisible-edits 'show-and-error)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
;; keep old versions, much version control
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(setq backup-directory-alist
      '((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/private/auto-save/" t)))

(setq savehist-file "~/.emacs.d/private/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(global-set-key (kbd "M-z") 'zap-up-to-char)

(spacemacs/toggle-maximize-frame-on)

(global-set-key (kbd "C-+") 'spacemacs/scale-up-font)
(global-set-key (kbd "C--") 'spacemacs/scale-down-font)

