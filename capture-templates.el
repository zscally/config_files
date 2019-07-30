;;; capture-templates.el --- description -*- lexical-binding: t; -*-

(setq org-capture-templates '(("t" "Personal todo" entry
                               (file+headline +org-capture-todo-file "Inbox")
                               "* TODO %?\n%i\n" :prepend t :kill-buffer t)
                              ("n" "Personal notes" entry
                               (file+headline +org-capture-notes-file "Inbox")
                               "* %u %?\n%i\n" :prepend t :kill-buffer t)
                              ("p" "Templates for projects")
                              ("pt" "Project todo" entry
                               (file+headline +org-capture-project-todo-file "Inbox")
                               "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
                              ("pn" "Project notes" entry
                               (file+headline +org-capture-project-notes-file "Inbox")
                               "* TODO %?\n%i\n" :prepend t :kill-fer t)))
(provide 'capture-templates)
;;; capture-templates.el ends here
