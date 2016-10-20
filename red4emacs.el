;; redbaron4emacs
;; python code manipulation and edition.
;; Vindarel 2016
;; https://gitlab.com/vindarel/redbaron4emacs
;; wtf public license

(require 'dash)
(require 'm-buffer)
(require 'projectile)
(require 's)

(defvar red4e--decorator-regexp "^\s*@[a-z_.]*")

(setq red4emacs-path (concat (file-name-directory
                              (or buffer-file-name
                                  (concat load-file-name "redbaron4emacs")))
                            "red4emacs.py")) ;; buffer-file-name for local eval, load-file-name for loading from somewhere else, when the buffer isn't open.

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

(defun my-string-matching (regexp str match)
  "return the string matching the regexp."
  ;; s-match-strings-all is similar.
  (string-match regexp str)
  (substring str (match-beginning match) (match-end match)))

(defun red4e--get-function-args ()
  "get a list of the method's arguments. They must be separated
  by a comma followed by a space (this is dumb but the solidity is
  considered satisfactory. As of today it will fail when arguments have parenthesis.).

  That can be done with redbaron."
  (interactive)
  (save-excursion
    (save-restriction
      (red4e--beginning-of-defun-or-line)
      (s-split ", "
               (my-string-matching  "(\\(.*\\))" (current-line) 1) ))
      ;; (message myfoo-str)
      ))

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
                      (s-trim (shell-command-to-string (format "python %s --sort --txt '%s' " red4emacs-path def))))))
    ))

(defun red4e--replace-in-def (old new)
  "Search and replace in current def"
  (save-excursion
    (let ((beg (save-excursion
                 (red4e--beginning-of-defun-or-line)
                 (next-line)
                 (point)))
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

(defun red4e--current-def-txt ()
  "Get the current def line, return valid code (adds 'pass')"
  (save-excursion
    (red4e--beginning-of-defun-or-line)
    (concat (buffer-substring-no-properties
             (red4e--beginning-of-defun-or-line-point)
             (save-excursion
               (end-of-line)
               (point)))
            " pass")
    ))

(defun red4e-add-arg (arg)
  "Add an argument to the current method definition. Have it well sorted with redbaron."
  (interactive "sArgument? ")
  (let* ((args-list (-concat (red4e--get-function-args) (list arg))))
    (red4e--write-args args-list)
  ))

(defun red4e--do-rename-arg (def argpos new)
  "Rename an argument. Call python function.
   - def: string, valid code 'def <name>(<args>): pass'
   - argpos: nb, position of the arg
   - new: new arg
  "
  (shell-command-to-string (format "python %s --rename --txt '%s' --pos %s --new %s" red4emacs-path def argpos new))
  )

(defun red4e-rename-arg ()
  "Select an argument to change"
  (interactive)
  ;; Get the arguments list
  (let* ((args-list (red4e--get-function-args))
         ;; Choose which arg to modify
         (arg (if (equal (length args-list) 1)
                  (-first-item args-list)
                (ido-completing-read+ "Arg to change ? " args-list)))
         ;; arg position
         (argpos (-elem-index arg args-list))
         ;; Ask for the modif
         (new (read-from-minibuffer "New value: " arg))
         ;; Build a new list of args
         (def (red4e--current-def-txt))
         ;; Call redbaron to replace the arg, get the new def
         (newdef (s-trim (red4e--do-rename-arg def argpos new))))

    ;; Write the new signature
    (save-excursion
      (red4e--beginning-of-defun-or-line)
      (kill-line)
      (insert newdef))

    ;; Search and replace the old arg inside the methode.
    (red4e--replace-in-def arg new)
    ))

(defun red4e-rename-symbol-in-defun ()
  "Rename any symbol (default: word at point) inside the current def."
  (interactive)
  (let* ((default (thing-at-point 'symbol))
         (what (if (string-equal ""
                                 (read-from-minibuffer (format "Replace: [%s]: " default)))
                 default))
         (with (read-from-minibuffer (format "Replace '%s' with: " what)))
         (beg (red4e--beginning-of-defun-or-line-point))
         (end (save-excursion
                (end-of-defun)
                (point))))
    (query-replace what with nil beg end)
    ))

(defun red4e-rm-arg ()
  "Select an argument to remove"
  (interactive)
  (let* ((args-list (red4e--get-function-args))
         ;; (ido-separator "\n")
         (arg (ido-completing-read+ "Arg to remove ? " args-list))
         (args (-remove-item arg args-list)))
    (red4e--write-args args)))

(defun red4e-rename-method (&optional in-project)
  "Rename the current method. No refacto. This doesn't even use redbaron."
  (interactive)
  (let* ((def (my-python-info-current-defun))
         (name (read-from-minibuffer "Name ? " def)))
    (save-excursion
      (red4e--beginning-of-defun-or-line)
      (forward-word)
      (delete-region (point) (save-excursion (search-forward "(")))
      (insert (concat " " name "("))
      (save-buffer))
    (if in-project
        (red4e--replace-in-project def name))))

(defun red4e-rename-method-in-project ()
  "Rename the current method, then use projectile to rename in whole project. Doesn't call redbaron."
  (red4e-rename-method t))

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


(defun red4e--decorators ()
  "Return a list of strings of all decorators used in this buffer.
  "
  (let ((results (m-buffer-match-string-no-properties
                  (m-buffer-match (current-buffer) red4e--decorator-regexp))))
    (--map (s-trim it)
           (-distinct results))
    ))

(defun red4e--def-has-self ()
  "Return t if the current def has the self argument."
  (save-excursion
    (beginning-of-defun)
    (search-forward "(")
    (looking-at-p "self")))

(defun red4e--cleanup-self ()
  "Remove 'self' from the method signature."
  (save-excursion
    (beginning-of-defun)
    (search-forward "(")
    (if (looking-at-p "self")
        (delete-char 4))
    (if (looking-at-p ",")
        (delete-char 1))
    (if (looking-at-p " ") ;; mmmh... better idea, without calling redbaron ?
        (delete-char 1))
    ))

(defun red4e-decorator-add ()
  "Ask for a decorator name (with or without the @) and add it
  above the first def we encounter.

  Suggest all already used decorators on this file (bypass the ido prompt with C-f).

  If the new decorator is '@staticmethod', remove 'self' from the method signature.

  This doesn't use redbaron.
  "
  (interactive)
  (let* ((decorators-list (red4e--decorators))
         (decorator (ido-completing-read+ "Decorator: " decorators-list))
         (decorator (if (s-starts-with? "@" decorator)
                     decorator
                     (concat "@" decorator)))
         (indentation (save-excursion
                       (beginning-of-defun)
                       (current-line-indentation))))
    (save-excursion
      (save-restriction
        (beginning-of-defun)
        (previous-line)
        (insert (concat "\n" indentation decorator))
        ))
    (if (and (string-equal decorator "@staticmethod")
             (red4e--def-has-self))
        (red4e--cleanup-self))
    (save-buffer))
  )
(defhydra red4e-hydra (:color red :columns 4)
  "redbaron4emacs"
  ("a" (call-interactively 'red4e-add-arg) "add an argument")
  ("n" (red4e-rename-arg) "rename an argument")
  ("S" (red4e-rename-symbol-in-defun) "rename a symbol inside this method")
  ("r" (red4e-rm-arg) "delete an argument")
  ("f" (red4e-rename-method) "rename the def")
  ("F" (red4e-rename-method-in-project) "rename the def in whole project")
  ("@" (red4e-decorator-add) "Add a decorator")
  ("i" (helm-imenu) "imenu")
  )
