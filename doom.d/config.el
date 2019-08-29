(let ((private-config "~/.doom.d/private.el"))
    (when (file-exists-p private-config)
      (load-file private-config)))
(load-file "~/.doom.d/custom.el")

(setq mac-command-modifier 'control)


(when (memq window-system '(mac ns x))
  (setq php-boris-command "~/boris/bin/boris"))

(after! eshell
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))


(setq term-input-ring-size 200)
(setq term-buffer-maximum-size 8191)
(setq eshell-history-ring 1028)

(load-theme 'leuven t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)


(add-hook 'doom-after-init-modules-hook (lambda () (set-face-attribute 'default nil :height 170)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(show-paren-mode 1)
(display-time-mode 1)

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (load-file "~/config_files/capture-templates.el")
  ;; NOTE: Be sure to load the repo one (config files) first, as it does a
  ;; setq while the others append
  (let ((personal-templates "~/org/capture-templates.el"))
    (when (file-exists-p personal-templates)
      (load-file personal-templates)))
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook '(lambda () (auto-fill-mode -1)))
  (setq org-imenu-depth 15
        imenu-max-items 0
        org-agenda-skip-deadline-prewarning-if-scheduled t))

(after! org-brain
  (add-hook 'org-brain-visualize-mode-hook 'visual-line-mode)
  (defadvice org-brain-visualize (after maximize-win activate)
    (progn
      (switch-to-buffer "*org-brain*")
      (delete-other-windows))))

(after! evil
  (evil-set-initial-state 'org-brain-visualize-mode 'normal))

(defvar my-workspace-map (make-sparse-keymap)
  "Project and workspace shifting")
(defvar my-brain-map (make-sparse-keymap)
  "Org-brain control")
(defvar my-org-map (make-sparse-keymap)
  "Org mode control")

(require 'org-brain)
(map! :leader
      :desc "org mode" "O" my-org-map
      (:prefix-map ("B" . "brain")
        :desc "visualize" "v" #'counsel-brain
        (:prefix ("p" . "parent")
          "a" #'org-brain-add-parent
          "r" #'org-brain-remove-parent)
        (:prefix ("c" . "child")
          "a" #'org-brain-add-child
          "r" #'org-brain-remove-child
          )
        (:prefix ("r" . "resource")
          "i" #'org-brain-visualize-add-resource
          )
        (:prefix ("l" . "link")
          :desc "store current line" "l" #'org-brain-add-file-line-as-resource
          :desc "store current file" "f" #'org-brain-add-file-as-resource
          )))
(map!
 (:map org-brain-visualize-mode-map
   (:prefix ("R" . "grandchildren")
     :n [tab] #'org-brain-show-descendant-level
     :n [backtab] #'org-brain-show-ancestor-level)
   :n "p" 'org-brain-add-parent
   :n "P" #'org-brain-remove-parent
   :n "c" #'org-brain-add-child
   :n "C" #'org-brain-remove-child
   :n "*" #'org-brain-add-child-headline
   :n "h" #'org-brain-add-child-headline
   :n "n" #'org-brain-pin
   :n "t" #'org-brain-set-title
   :n "j" #'forward-button
   :n "k" #'backward-button
   :n [tab] #'forward-button
   :n [backtab] 'backward-button
   :n "u" #'org-brain-visualize-parent
   :n "o" #'org-brain-goto-current
   :n "O" #'org-brain-goto
   :n "v" #'org-brain-visualize
   :n "f" #'org-brain-add-friendship
   :n "F" #'org-brain-remove-friendship
   :n "d" #'org-brain-delete-entry
   :n "l" #'org-brain-add-resource
   :n "r" #'org-brain-open-resource
   :n "a" #'org-brain-visualize-attach
   :n "A" #'org-brain-archive
   :n "b" #'org-brain-visualize-back
   :n "C-y" 'org-brain-visualize-paste-resource
   :n "T" #'org-brain-set-tags
   :n "q" #'org-brain-visualize-quit
   :n "w" #'org-brain-visualize-random
   :n "W" #'org-brain-visualize-wander
   :n "m" #'org-brain-visualize-mind-map
   :n "+" #'org-brain-show-descendant-level
   :n "-" #'org-brain-hide-descendant-level
   :n "#z" 'org-brain-show-ancestor-level
   :n "#Z" 'org-brain-hide-ancestor-level
   :n "#e" 'org-brain-annotate-edge))



(map!
 :g "C-x 5 l" #'lower-frame
 :g "C-x b" #'+ivy/switch-workspace-buffer
 :g "C-x B" #'+ivy/switch-buffer)

(map! (:map my-workspace-map
        "." #'+workspace/switch-to
        "n" #'+workspace/new
        "s" #'+workspace/save
        "l" #'+workspace/load
        "r" #'+workspace/rename
        "o" #'+workspace/other
        "i" #'clone-indirect-buffer
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
      :g "C-c w" my-workspace-map
      :g "M-z" #'zap-up-to-char
      :g [f12] #'org-pomodoro
      (:map my-org-map
        "ls" #'org-store-link
        "li" #'org-insert-link
        ))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)

(map!
 (:after term
   :map term-mode-map
   :i "C-y" #'term-paste
   :map term-raw-map
   :i "C-y" #'term-paste)
 (:prefix ("M-RET" . "multi-term menu")
   "n" #'multi-term-next
   "p" #'multi-term-previous
   "c" #'multi-term
   "q" #'multi-term-dedicated-close))

(map!
 :leader
 :desc "multi-term" "om" #'multi-term-dedicated-toggle)

;; Set the dvorak version of the improve home row bindings
(after! ace-window
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)
        aw-scope 'frame
        aw-ignore-current nil
        aw-background t)
  (ace-window-display-mode))

(load "~/config_files/blacken.el")
(add-hook 'python-mode-hook 'blacken-mode)

(evil-define-key 'insert ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'hybrid ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))
(evil-define-key 'normal ein:edit-cell-mode-map (kbd "C-x C-s") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-save)))

(evil-define-key 'normal ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'insert ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))
(evil-define-key 'hybrid ein:edit-cell-mode-map (kbd "C-c '") (lambda () (interactive) (blacken-buffer) (ein:edit-cell-exit)))

(setq line-move-visual nil)

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
