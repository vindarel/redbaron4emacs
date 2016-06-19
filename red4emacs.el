;; redbaron4emacs
;; python code manipulation and edition.
;; Vindarel 2016
;; https://gitlab.com/vindarel/redbaron4emacs
;; wtf public license

(setq red4emacs-path (concat (file-name-directory (buffer-file-name))
                             "red4emacs.py"))

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
      (red4e--beginning-of-defun-or-line)
      (setq myfoo-str (my-string-matching  "(\\(.*\\))" (current-line) 1))
      (s-split ", " myfoo-str)
      ;; (message myfoo-str)
)))

(defun red4e--beginning-of-defun-or-line ()
  "Don't move to the precedent beginning of defun if we're already on one."
  (unless (progn
            (beginning-of-line-text)
            (looking-at-p "def "))
    (python-nav-beginning-of-defun)))

(defun red4e--beginning-of-defun-or-line-point ()
  ""
  (save-excursion
    (red4e--beginning-of-defun-or-line)
    (point)))

(defun red4e--write-args (args)
  "Write the given list of args inline."
  (let* ((def (concat
              "def "
              (my-python-info-current-defun)
              "("
              (mapconcat #'identity
                         args
                         ",")
              "): pass"))
         (indentation (save-excursion
                        (red4e--beginning-of-defun-or-line)
                        (current-line-indentation))))

    (save-excursion
      (red4e--beginning-of-defun-or-line)
      (beginning-of-line)
      (kill-line)
      (insert (concat indentation
                      (s-trim (shell-command-to-string (concat "python " red4emacs-path " '" def "'"))))))
    ))

(defun red4e--replace-in-def (old new)
  "Search and replace in current def"
  (save-excursion
    (let ((beg (red4e--beginning-of-defun-or-line-point))
          (end (save-excursion
                 (end-of-defun)
                 (point))))
      (query-replace old new nil beg end)
      )))

(defun red4e--replace-in-buffer (old new)
  "Search and replace in all current buffer"
  (save-excursion
    (let ((beg (point-min))
          (end (point-max)))
      (query-replace old new nil beg end)
      )))

(defun red4e--replace-in-project (old new)
  "Interactively search and replace in project (as projectile)."
  ;; see projectile's projectile-replace source
  (let ((files (projectile-files-with-string old (projectile-project-root))))
    (tags-query-replace old new nil (cons 'list files))
    ))

(defun red4e-add-arg (arg)
  "Add an argument to the current method definition. Have it well sorted with redbaron."
  (interactive "sArgument? ")
  (let* ((args-list (-concat (my-py-get-function-args) (list arg))))
    (red4e--write-args args-list)
  ))

(defun red4e-mv-arg ()
  "Select an argument to change"
  (interactive)
  (let* ((args-list (my-py-get-function-args))
         (arg (ido-completing-read+ "Arg to change ? " args-list))
         (new (read-from-minibuffer "Change to: " arg))
         (args (-concat (-remove-item arg args-list)
                        (list new))))

    (red4e--write-args args)
    (red4e--replace-in-def arg new)
    ))

(defun red4e-rm-arg ()
  "Select an argument to remove"
  (interactive)
  (let* ((args-list (my-py-get-function-args))
         ;; (ido-separator "\n")
         (arg (ido-completing-read+ "Arg to remove ? " args-list))
         (args (-remove-item arg args-list)))
    (red4e--write-args args)))

(defun red4e-rename-method ()
  "Rename the current method. No refacto. This doesn't even use redbaron."
  (interactive)
  (let* ((def (my-python-info-current-defun))
         (name (read-from-minibuffer "Name ? " def)))
    (save-excursion
      (red4e--beginning-of-defun-or-line-point)
      (forward-word)
      (delete-region (point) (save-excursion (search-forward "(")))
      (insert (concat " " name "("))
      (red4e--replace-in-project def name)
      )))

(defhydra red4e-hydra (:color blue :columns 4)
  "redbaron4emacs"
  ("a" (call-interactively 'red4e-add-arg) "add an argument")
  ("m" (call-interactively 'red4e-mv-arg) "rename an argument")
  ("r" (call-interactively 'red4e-rm-arg) "rm an argument")
  ("n" (call-interactively 'red4e-rename-method) "rename the def")
  )

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
