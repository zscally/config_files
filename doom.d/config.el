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

(setq org-imenu-depth 5)

(after! org
  (load-file "~/capture-templates.el"))

;; Set the dvorak version of the improve home row bindings
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
