(defun system-integration/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")))

(setq system-integration-packages
      '(
  exec-path-from-shell
  ))
