(setq mac-command-modifier 'control)


(when (memq window-system '(mac ns x))
  (setq php-boris-command "~/boris/bin/boris"))

(after! eshell
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))


(setq term-input-ring-size 200)
(setq term-buffer-maximum-size 8191)
(setq eshell-history-ring 1028)

(load-theme 'doom-solarized-light t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)


(add-hook 'doom-after-init-modules-hook (lambda () (set-face-attribute 'default nil :height 170)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(show-paren-mode 1)
(display-time-mode 1)

(after! org
  (load-file "~/capture-templates.el")
  (setq org-imenu-depth 15)
  (setq imenu-max-items 0))

(defvar my-workspace-map (make-sparse-keymap)
  "Project and workspace shifting")

(map! (:map my-workspace-map
        "." #'+workspace/switch-to
        "n" #'+workspace/new
        "s" #'+workspace/save
        "l" #'+workspace/load
        "r" #'+workspace/rename
        "o" #'+workspace/other
        "u" #'winner-undo
        "R" #'winner-redo
        [tab]  #'+workspace/display
        "d"  #'+workspace/delete
        "1" #'+workspace/switch-to-0
        "2" #'+workspace/switch-to-1
        "3" #'+workspace/switch-to-2
        "4" #'+workspace/switch-to-3
        "5" #'+workspace/switch-to-4
        "6" #'+workspace/switch-to-5
        "7" #'+workspace/switch-to-6
        "8" #'+workspace/switch-to-7
        "9" #'+workspace/switch-to-8)
      :g "C-c w" my-workspace-map)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Set the dvorak version of the improve home row bindings
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))

(load "~/config_files/blacken.el")
(add-hook 'python-mode-hook 'blacken-mode)

(evil-define-key 'insert ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'hybrid ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'normal ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))

(evil-define-key 'normal ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'insert ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'hybrid ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))

(after! counsel
  (setq counsel-rg-base-command "rg -S --no-heading -g !TAGS --line-number --color never %s ."))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'darkroom-mode-hook 'visual-line-mode)

(if (eq system-type 'gnu/linux)
    (with-eval-after-load 'ox-latex
      (add-to-list 'org-latex-classes
                   '("memoir-book"
                     "\\documentclass[11pt,a4paper]{memoir}"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ))))

(after! treemacs
  (treemacs-follow-mode))

(message "Hello, we reached the end")
