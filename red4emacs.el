
(setq red4emacs-path "~/.emacs.d/my-elisp/redbaron4emacs/red4emacs.py")

(defun red4e-add-arg (arg)
  "Add an argument to the current method definition. Have it well sorted with redbaron."
  (interactive "sArgument? ")
  (let* ((args-list (-concat (my-py-get-function-args) (list arg)))
        (def (concat
              "def "
              (which-function) ;; warning: can output "name (def)"
              "("
              (mapconcat #'identity
                         args-list
                         ",")
              "): pass")))
    (insert (shell-command-to-string (concat "python " red4emacs-path " '" def "'")))
  ))
