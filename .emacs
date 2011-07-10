; TODO: use after-make-frame-hook to make 80 column wide frames
; TODO: replace split-window-sensibly to use available space on the right

(defun unwrap-line ()
  "Un-line-wrap the expression at point.
Point must either be in the whitespace at the end of the first
line, or in the whitespace at the start of the second line."
  (interactive)
  (let* ((spaces "[ \t\r\n]")
         (beg (save-excursion (skip-chars-backward spaces)
                              (point)))
         (end (save-excursion (skip-chars-forward spaces)
                              (point))))
    (delete-region beg end)
    (insert " ")))

; TODO: move window management to its own library
(defun three-columns ()
  "Splits the frame horizontally into three windows of 80 columns."
  (interactive)
  (let ((desired-frame-width 245))
    (when (< (frame-width) desired-frame-width)
      (error "Frame too narrow for three columns, must be at least %d was %d"
             desired-frame-width
             (frame-width)))
    (when (not (eq (selected-window) (next-window nil 'no-minibuf)))
      (error "Too many windows"))
    (set-frame-width nil desired-frame-width)
    (dotimes (n 2)
      (split-window-horizontally)
      (setf (window-width) 81)
      (select-window (next-window nil 'no-minibuf) t))))

(server-start)

(add-to-list 'load-path (concat (getenv "HOME") "/site-lisp"))

(require 'whitespace)
(global-whitespace-mode 't)

(add-to-list 'load-path (concat (getenv "HOME") "/webkit-tools"))
(require 'webkit-stuff)
(wk-setup)

(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(require 'color-moccur)
(require 'moccur-edit)

(add-hook 'change-log-mode-hook
          (function (lambda () (setq indent-tabs-mode nil))))

(add-hook 'c++-mode-hook
          (function (lambda () (local-set-key "\C-o" 'ff-get-other-file))))

(add-hook 'c-mode-hook
          (function (lambda () (local-set-key "\C-o" 'ff-get-other-file))))

(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
(setq slime-lisp-implementations
     (list (list 'sbcl (list (replace-regexp-in-string "\n" "" (shell-command-to-string "which sbcl"))))))
(require 'slime)
(slime-setup  '(slime-repl slime-asdf slime-fancy slime-banner))

(add-to-list 'load-path "~/site-lisp/js2-mode-read-only")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; display the column number in the modeline
(column-number-mode ())

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(setq three-step-help t)
(setq inhibit-splash-screen t)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(indent-tabs-mode nil)
 '(whitespace-style (quote (tabs trailing space-before-tab empty space-after-tab tab-mark))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "goldenrod")))))
