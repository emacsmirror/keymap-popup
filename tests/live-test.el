;;; live-test.el --- Interactive test harness -*- lexical-binding: t; -*-

;; Eval this buffer to test keymap-popup interactively.
;; Then run: M-x kp-test

(load (expand-file-name
       "../keymap-popup.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;; Force fresh keymaps on re-eval (defvar won't re-set bound variables)
(mapc #'makunbound
      (cl-remove-if-not #'boundp '(kp-test--map kp-test--sub-map)))

;;; Buffer rendering

(defvar-local kp-test--name nil)


(defun kp-test--render ()
  "Redraw the *kp-test* buffer from buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "keymap-popup live test\n" 'face 'bold)
            (make-string 40 ?-) "\n\n"
            (format "  Name:     %s\n" (or kp-test--name "(not set)"))
            "\n"
            (propertize "Press h for popup, q to quit.\n" 'face 'shadow))))

(defun kp-test--refresh ()
  "Refresh the display (stay-open)."
  (interactive)
  (kp-test--render)
  (message "Refreshed"))

;;; Commands

(defun kp-test--greet ()
  "Greet using buffer-local state."
  (interactive)
  (let ((name (or kp-test--name "world"))
        (loud current-prefix-arg))
    (message (if loud
                 (format "%s!!!" (upcase name))
               (format "Hello, %s." name)))
    (kp-test--render)))

(defun kp-test--sub-action ()
  (interactive)
  (message "Sub-menu action! prefix=%s" current-prefix-arg))

;;; Sub-menu keymap

(keymap-popup-define kp-test--sub-map
  :group "Sub-menu"
  "s" ("Sub action" kp-test--sub-action)
  "x" ("Greet from sub" kp-test--greet))

;;; Root keymap

(keymap-popup-define kp-test--map
  "Test popup"
  :description "keymap-popup live test"
  :group "Actions"
  "a" ("Greet" kp-test--greet :c-u "SHOUT (C-u)")
  "g" ("Refresh" kp-test--refresh :stay-open t)
  :group "Infixes"
  "v" ("Verbose" :switch kp-test--verbose)
  "n" ((lambda () (concat "Name ="
                         (if (and kp-test--name (not (string-empty-p kp-test--name)))
                             (propertize kp-test--name 'face 'success)
                           (propertize "?" 'face 'warning))))
       (lambda () (interactive)
         (setq-local kp-test--name (read-string "Your name: ")))
       :stay-open t)
  :group "Navigate"
  "s" ("Sub-menu" :keymap kp-test--sub-map)
  "q" ("Quit" quit-window)
  :row
  :group "Inapt (entry-level)"
  "m" ("Merge (always blocked)" kp-test--greet :inapt-if (lambda () t))
  "d" ("Dynamic inapt" kp-test--greet
       :inapt-if (lambda () (not kp-test--verbose)))
  :group ("Group inapt (when verbose off)" :inapt-if (lambda () (not kp-test--verbose)))
  "x" ("Group-blocked cmd" kp-test--greet)
  :group ("Toggle (visible when verbose)" :if (lambda () kp-test--verbose))
  "t" ("Verbose-only action" kp-test--greet))

;;; Entry point

(defun kp-test ()
  "Open the *kp-test* buffer and activate the popup."
  (interactive)
  (let ((buf (get-buffer-create "*kp-test*")))
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (kp-test--render)
      (use-local-map kp-test--map))
    (pop-to-buffer-same-window buf)
    (keymap-popup kp-test--map)))

(provide 'live-test)
;;; live-test.el ends here
