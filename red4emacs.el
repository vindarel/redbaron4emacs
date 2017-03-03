;; redbaron4emacs
;; python code manipulation and edition.
;; Vindarel 2016
;; https://gitlab.com/vindarel/redbaron4emacs
;; wtf public license

(require 'dash)
(require 'm-buffer)
(require 'projectile)
(require 's)

(defvar red4e-selection-method 'red4e-selection-default "The selection method for imenu: default (completing-read), helm, counsel.")

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

(defun red4e-end-of-args ()
  "Go to the end of the current 'def', according point is on its line."
  (red4e--beginning-of-defun-or-line)
  (search-forward "):"))

(defun red4e-end-of-args-point ()
  "Get point of the end of the definition of args."
  (save-excursion
    (red4e-end-of-args)
    (- (point) 2)))

(defun red4e--get-function-args ()
  "Get a list of the method's arguments.
  Solidity considered ok. That could be done with redbaron.
  Return a list of args (str)."
  (interactive)
  (save-excursion
    (save-restriction
      (red4e--beginning-of-defun-or-line)
      (let* ((beg (progn
                    (search-forward "(") (point)))
             (end (red4e-end-of-args-point))
             (args (buffer-substring-no-properties beg end)))
        (s-split ", " (s-collapse-whitespace args))))))

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
  "Write the given list of args inline. 'args' can be in a wrong
order. Thanks to RedBaron, we sort them correctly, preserving the
initial order."
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
      (kill-region (point) (save-excursion
                             (red4e-end-of-args)
                              (point)))
      (insert (concat indentation
                      (s-trim (shell-command-to-string (format "python %s --sort --txt '%s' " red4emacs-path def))))))
    ))

(defun red4e-args-multi-line ()
  "Write one argument per line. `multi-line' nor `fillcode' did the trick, but they could."
  (interactive)
  (let ((beg (save-excursion
               (red4e--beginning-of-defun-or-line)
               (point)))
        (end (red4e-end-of-args-point)))
    (save-excursion
        (replace-regexp "," ",\n" nil beg end)
        ;; and indent
        (red4e--beginning-of-defun-or-line)
        (while (re-search-forward "," (red4e-end-of-args-point) t)
          (progn
            (forward-line)
            (indent-according-to-mode)))
        (forward-line)
        (indent-according-to-mode)
        )))

(defun red4e-args-oneliner ()
  "Write args in one line."
  (interactive)
  (let ((end (red4e-end-of-args-point)))
    (save-excursion
      (red4e--beginning-of-defun-or-line)
      (replace-regexp "\n\s*" " " nil (point) end))))

(defun red4e-args-on-one-line-p ()
  "Return t if the args of the current defun are on one line only. Point can be anywhere in the method."
  (let ((beg (save-excursion
               (red4e--beginning-of-defun-or-line)
               (line-number-at-pos)))
        (end (save-excursion
               (red4e-end-of-args)
               (line-number-at-pos))))
      (= beg end)))

(defun red4e-args-multi-line-toggle ()
  "Toggle args on one line / one many lines."
  (interactive)
  (if (red4e-args-on-one-line-p)
        (red4e-args-multi-line)
      (red4e-args-oneliner)))

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
  "Add an argument to the current method definition. Have it well sorted with redbaron.

If your args are on multiple lines, they will be written back in one. You'll need to manually call `red4e-args-multi-line' to put them back on multiple lines."
  (interactive "sArgument? ")
  (let* ((args-list (-concat (red4e--get-function-args) (list arg)))
         (args-one-line (red4e-args-on-one-line-p)))
    (red4e--write-args args-list)
    (or args-one-line (red4e-args-multi-line))
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

(defun red4e--decorator-add (decorator)
  "Do add the decorator."
  (let ((decorator (if (s-starts-with? "@" decorator)
                     decorator
                     (concat "@" decorator)))
         (indentation (save-excursion
                       (beginning-of-defun)
                       (current-line-indentation))))

    (save-excursion
      (save-restriction
        (beginning-of-defun)
        (previous-line)
        ;; and if there's a decorator already ?
        (end-of-line)
        (newline)
        (insert (concat indentation decorator))
        ))
    (if (and (string-equal decorator "@staticmethod")
             (red4e--def-has-self))
        (red4e--cleanup-self)))
    )

(defun red4e-decorator-add ()
  "Ask for a decorator name (with or without the @) and add it
  above the first def we encounter.

  Suggest all already used decorators on this file (bypass the ido prompt with C-f).

  If the new decorator is '@staticmethod', remove 'self' from the method signature.

  This doesn't use redbaron.
  "
  (interactive)
  (let* ((decorators-list (red4e--decorators))
         (decorator (ido-completing-read+ "Decorator: " decorators-list)))
    (red4e--decorator-add decorator))
  (save-buffer)
  )

(defun red4e--decorator-read ()
  "What is the (first) decorator of this method ?
Could use redbaron."
  (save-excursion
    (beginning-of-defun)
    (previous-line)
    (s-trim (current-line))))

(defun red4e--decorator-remove ()
  "Do remove the first decorator."
  (save-excursion
    (save-restriction
      (beginning-of-defun)
      (previous-line)
      (if (s-starts-with? "@" (s-trim (current-line)))
          (progn
            (let ((deleted-line (current-line))
                  (kill-whole-line t))
              (kill-line)
              deleted-line
              ))))))

(defun red4e-decorator-remove ()
  "Remove a decorator (experimental)."
  (interactive)
  (let ((decorator (red4e--decorator-remove)))
    (save-buffer)
    (if decorator
        (message (concat "Removed " deleted-line))
      (message "You don't have a decorator.")
      )))

(defun red4e--beginning-of-method-point ()
  ""
  (let ((beg (save-excursion
               (red4e--beginning-of-defun-or-line)
               (backward-paragraph)  ;; include decorators. Could be cleaner.
               (point))))
    beg))

(defun red4e--end-of-method-point ()
  (let ((end (save-excursion
               (end-of-defun)
               (point))))
    end))

(defun red4e-method-copy ()
  "Copy the current method definition and body."
  (interactive)
  (let ((beg (red4e--beginning-of-method-point))
        (end (red4e--end-of-method-point)))
    (copy-region-as-kill beg end)))

(defun red4e-method-kill ()
  "Kill the current method definition and body."
  (interactive)
  (let ((beg (red4e--beginning-of-method-point))
        (end (red4e--end-of-method-point)))
    (kill-region beg end)))

(defun red4e-method-comment ()
  "Comment the current method definition and body (with decorators)"
  (interactive)
  (let ((beg (red4e--beginning-of-method-point))
        (end (red4e--end-of-method-point)))
    (comment-or-uncomment-region beg end)))

;; Methods for an imenu completion: basic, helm, counsel.
(defun red4e-selection-default ()
  "Default interface for an interactive completion method of
imenu: the basic completing-read (imenu). Others choices are
helm (helm-imenu) and ivy (counsel-imenu).

Choose by setting `red4e-selection-method'."
  ;; When possible, may be nice to put those choices in different files, like
  ;; https://github.com/d12frosted/flyspell-correct
  ;; see also https://www.emacswiki.org/emacs/idomenu.el ?
  (call-interactively 'imenu))

(defun red4e-selection-helm ()
  "Helm interface for imenu."
  (helm-imenu))

(defun red4e-selection-ivy ()
  "Ivy interface for imenu."
  (counsel-imenu))

(defhydra red4e-hydra-method (:color red :columns 4)
  "Current method:"
  ("y" (red4e-method-copy) "Copy" :color blue)
  ("f" (red4e-rename-method) "rename the def")
  ("F" (red4e-rename-method-in-project) "rename the def in whole project")
  ("k" (red4e-method-kill) "Kill method")
  ("c" (red4e-method-comment) "Comment method")
  ("i" (funcall red4e-selection-method) "imenu")
  )

(defhydra red4e-hydra (:color red :columns 4)
  "redbaron4emacs"
  ("a" (call-interactively 'red4e-add-arg) "add an argument")
  ("n" (red4e-rename-arg) "rename an argument")
  ("S" (red4e-rename-symbol-in-defun) "rename a symbol inside this method")
  ("r" (red4e-rm-arg) "delete an argument")
  ("l" (red4e-args-multi-line-toggle) "toggle args on multiple lines")
  ("m" (red4e-hydra-method/body) "Current method…" :color blue)
  ("@" (red4e-decorator-add) "Add a decorator")
  ("D" (red4e-decorator-remove) "Remove a decorator")
  ("i" (funcall red4e-selection-method) "imenu")
  )

(provide 'red4e)
