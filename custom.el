(defun sql-table-make-header (beginning end)
  (interactive "r")
  (if (use-region-p)
      (message "Region is active from %d to %d" beginning end)
    (message "Region is inactive (from %d to %d)" beginning end)))

(defun sql-table-make-rows (beginning end)
  (interactive "r")
  (if (use-region-p)
      (message "Region is active from %d to %d" beginning end)
    (message "Region is inactive (from %d to %d)" beginning end)))

(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))
