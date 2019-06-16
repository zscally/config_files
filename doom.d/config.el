(setq mac-command-modifier 'control)

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


(add-to-list 'default-frame-alist '(fullscreen . maximized))

(show-paren-mode 1)
(display-time-mode 1)

(after! org
  (load-file "~/capture-templates.el")
  (setq org-imenu-depth 15)
  (setq imenu-max-items 0))

;; Set the dvorak version of the improve home row bindings
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))

(defvar my-workspace-map (make-sparse-keymap)
  "Project and workspace shifting")

(map! (:map my-workspace-map
        "." #'+workspace/switch-to
        "n" #'+workspace/new
        "s" #'+workspace/save
        "l" #'+workspace/load
        "r" #'+workspace/rename
        "o" #'+workspace/other
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
