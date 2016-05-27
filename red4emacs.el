
(setq red4emacs-path "~/.emacs.d/my-elisp/redbaron4emacs/red4emacs.py")

(defun current-line ()
  "returns the current line."
  ;; http://ergoemacs.org/emacs/elisp_all_about_lines.html
         (let ( (p1 (line-beginning-position))
                (p2 (line-end-position)))
           (buffer-substring-no-properties p1 p2)
           ))

(defun current-line-indentation ()
  "returns the str of the current indentation (spaces)."
  ;; https://github.com/magnars/s.el#s-match-strings-all-regex-string
  (car (car (s-match-strings-all "^\s+" (current-line)) ) ))

(defun my-py-get-function-args ()
  "get a list of the method's arguments. They must be separated
  by a comma followed by a space (this is dumb but the solidity is
  considered satisfactory. As of today it will fail when arguments have parenthesis.). "
  (interactive)
  (save-excursion
    (save-restriction
      (beginning-of-defun)
      (setq myfoo-str (my-string-matching  "(\\(.*\\))" (current-line) 1))
      (s-split ", " myfoo-str)
      ;; (message myfoo-str)
)))
(defun red4e-add-arg (arg)
  "Add an argument to the current method definition. Have it well sorted with redbaron."
  (interactive "sArgument? ")
  (let* ((args-list (-concat (my-py-get-function-args) (list arg)))
         (indentation (save-excursion
                        (python-nav-beginning-of-defun)
                        (current-line-indentation)))
        (def (concat
              "def "
              ;; (which-function) ;; inconsistent with "name (def)"
              ;; (python-info-current-defun) ;; no: includes class names
              (my-python-info-current-defun)
              "("
              (mapconcat #'identity
                         args-list
                         ",")
              "): pass")))
    (save-excursion
      (python-nav-beginning-of-defun)
      (beginning-of-line)
      (kill-line)
      (insert (concat indentation
                      (s-trim (shell-command-to-string (concat "python " red4emacs-path " '" def "'"))))))
  ))

(defun my-python-info-current-defun (&optional include-type)
  "Return name of surrounding function with Python compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
`add-log-current-defun-function' since it returns nil if point is
not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          ;; (while
              (python-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (python-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              python-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     python-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (python-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t)))
        ;; )
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))
