; TODO: use after-make-frame-hook to make 80 column wide frames
; TODO: replace split-window-sensibly to use available space on the right

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

(setq inhibit-splash-screen t)

(add-to-list 'load-path (concat (getenv "HOME") "/site-lisp"))

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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(indent-tabs-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "goldenrod")))))