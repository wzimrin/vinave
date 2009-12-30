(provide 'vinave)

(defmacro vinave-define-keys (map bindings)
  (let ((mp (make-symbol "map")))
    `(let ((,mp ,map))
       (progn
         ,@(mapcar (lambda (binding)
                     `(define-key ,mp ,(car binding) ,(cadr binding)))
                   bindings)))))

(defun vinave-switch-to-insert-mode ()
  (interactive)
  (vinave-command-mode 0)
  (vinave-insert-mode 1))

(defun vinave-switch-to-command-mode ()
  (interactive)
  (vinave-insert-mode 0)
  (vinave-command-mode 1))

(defun vinave-replace-char (c)
  (interactive "*cChar:\n")
  (delete-char 1 t)
  (insert c)
  (backward-char))

(defmacro vinave-kill-fn (yes no)
  (let ((i (make-symbol "i")))
    `(lambda (,i)
       (interactive "*p")
       (if (eq last-command 'kill-region)
           (execute-kbd-macro ,yes)
         (execute-kbd-macro ,no))
       (if (> ,i 1)
           (execute-kbd-macro ,yes (- ,i 1)))
       (setq this-command 'kill-region))))

(defmacro vinave-change-fn (kill-fn &optional after)
  (let ((i (make-symbol "i")))
    `(lambda (,i)
       (interactive "*p")
       (if (eq last-command 'kill-region)
           (setq this-command 'kill-region))
       (funcall (key-binding ,kill-fn t) ,i)
       (execute-kbd-macro ,(if after
                               after
                             "i")))))

(defun vinave-search-forward-char (c)
  (interactive "cChar:\n")
  (search-forward (string c))
  (backward-char))

(defvar vinave-command-mode-map (make-keymap))

(defun vinave-empty ()
  (interactive)
  (ding))

(substitute-key-definition 'self-insert-command
                           'vinave-empty
                           vinave-command-mode-map
                           global-map)

(vinave-define-keys vinave-command-mode-map
                    (((kbd "RET") 'vinave-empty)
                     ((kbd "<backspace>") 'vinave-empty)
                     

                     (":" 'execute-extended-command)
                     ("u" 'undo)
                     ("U" 'undo-only)
                     ("p" 'yank)
                     ("P" 'yank-pop)
                     ("n" 'universal-argument)
                     
                     ("h" 'backward-char)
                     ("j" 'next-line)
                     ("k" 'previous-line)
                     ("l" 'forward-char)
                     
                     ("H" (kbd "C-M-b"))
                     ("J" (kbd "C-M-d"))
                     ("K" (kbd "C-M-u"))
                     ("L" (kbd "C-M-f"))
                     
                     ("e" "\M-f")
                     ("b" "\M-b")
                     ("," "\C-a")
                     ("." "\C-e")
                     ("<" (kbd "C-M-a"))
                     (">" (kbd "C-M-e"))
                     
                     ("f" 'vinave-search-forward-char)
                     
                     ("o" (kbd "C-e i RET"))
                     ("O" (kbd "C-a i RET C-SPC k TAB i"))
                     ("a" (kbd "l i"))
                     ("i" 'vinave-switch-to-insert-mode)
                     ("A" (kbd "C-e i"))
                     ("I" (kbd "C-a i"))
                     
                     ("x" (lambda (i)
                            (interactive "*p")
                            (if (eq last-command 'kill-region)
                                (setq this-command 'kill-region))
                            (delete-char i t)
                            (setq this-command 'kill-region)))
                     ("s" "xi")
                     ("r" 'vinave-replace-char)
                     ("w" 'newline-and-indent)
                     ("W" 'join-line)
                     ("t" 'transpose-chars)
                     ("T" 'transpose-sexps)
                     
                     ("d" (make-sparse-keymap))
                     ("de" (vinave-kill-fn (kbd "C-M-w M-d")
                                           (kbd "M-d")))
                     ("db" (vinave-kill-fn (kbd "C-M-w M-DEL")
                                           (kbd "M-DEL")))
                     ("dd" (vinave-kill-fn (kbd "C-a C-M-w C-k C-k")
                                           (kbd "C-a C-k C-k")))
                     ("d." (vinave-kill-fn (kbd "C-M-w C-k")
                                           (kbd "C-k")))
                     ("d," (vinave-kill-fn (kbd "C-M-w C-u -1 C-k")
                                           (kbd "C-u -1 C-k")))
                     ("dL" (vinave-kill-fn (kbd "C-M-w C-M-k")
                                           (kbd "C-M-k")))
                     ("dH" (vinave-kill-fn (kbd "C-M-w C-u -1 C-M-k")
                                           (kbd "C-u -1 C-M-k")))
                     ("df" 'zap-to-char)
                     ("D" 'kill-region)
                     
                     ("c" (make-sparse-keymap))
                     ("ce" (vinave-change-fn (kbd "dw")))
                     ("cb" (vinave-change-fn (kbd "db")))
                     ("c." (vinave-change-fn (kbd "d.")))
                     ("c," (vinave-change-fn (kbd "d,")))
                     ("cL" (vinave-change-fn (kbd "dL")))
                     ("cH" (vinave-change-fn (kbd "dH")))
                     ("cc" (vinave-change-fn (kbd "d d") (kbd "O")))
                     ("cf" (lambda (c i)
                             (interactive "*cChar\np")
                             (if (eq last-command 'kill-region)
                                 (setq this-command 'kill-region))
                             (zap-to-char i c)
                             (execute-kbd-macro "i")))
                     ("C" (lambda (i j)
                            (interactive "*r")
                            (if (eq last-command 'kill-region)
                                (setq this-command 'kill-region))
                            (kill-region i j)
                            (execute-kbd-macro "i")))
                     
                     ("y" (make-sparse-keymap))
                     ("ye" (vinave-change-fn (kbd "dw") (kbd "p")))
                     ("yb" (vinave-change-fn (kbd "db") (kbd "p")))
                     ("y." (vinave-change-fn (kbd "d.") (kbd "p")))
                     ("y," (vinave-change-fn (kbd "d,") (kbd "p")))
                     ("yL" (vinave-change-fn (kbd "dL") (kbd "p")))
                     ("yH" (vinave-change-fn (kbd "dH") (kbd "p")))
                     ("yy" (vinave-change-fn (kbd "d d") (kbd "p")))
                     ("yf" (lambda (c i)
                             (interactive "*cChar\np")
                             (if (eq last-command 'kill-region)
                                 (setq this-command 'kill-region))
                             (zap-to-char i c)
                             (execute-kbd-macro "p")))
                     ("Y" 'kill-ring-save)))

(defvar vinave-insert-mode-map (make-sparse-keymap))

(vinave-define-keys vinave-insert-mode-map
                    (((kbd "C-SPC") 'vinave-switch-to-command-mode)))

(define-minor-mode vinave-command-mode
  "Toggles vinave command mode.  vinave is not a vi emulation.
It provides modes and keybindings that are similar to VI but
anything that can be described as a 'vi emulation' is not a goal.
There are many differences in the key bindings.  However, the only
Control/Meta binding that was changed is C-SPC in Insert mode, which
now exits insert mode.  All other Control/Meta bindings are untouched."
  :lighter " Command"
  :group editing)

(define-minor-mode vinave-insert-mode
  "Toggles vinave insert mode.  See vinave-command-mode for details
on vinave."
  :lighter " Insert"
  :group editing)
