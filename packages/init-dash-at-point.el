(require 'dash-at-point)

(defun dash-search-at-point (&optional edit-search)
  "Search for the word at point in Dash.
If the optional prefix argument EDIT-SEARCH is specified,
the user will be prompted to edit the search string first."
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol)))
    (dash-at-point-run-search
     (if (or edit-search (null thing))
         (read-string "Dash search: " thing)
       thing))))

(rplacd (assoc 'js2-mode dash-at-point-mode-alist) "lodash,jquery,moment,angularjs,javascript,backbone,rambda")
(global-set-key (kbd "C-c d p") 'dash-at-point)
(global-set-key (kbd "C-c d P") 'dash-search-at-point)
