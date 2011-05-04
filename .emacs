(server-start)

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