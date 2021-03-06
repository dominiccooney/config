; TODO: use after-make-frame-hook to make 80 column wide frames
; TODO: replace split-window-sensibly to use available space on the right

(package-initialize)

(eval-after-load "org"
 '(progn
    (require 'cl)
    (require 'ox)
    (require 'org-drill)))

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

(defun unstringify (start end)
  "Converts the region from C-style strings to text.
Narrows the buffer to the region, on the assumption that you will
now edit this text."
  ; FIXME: This doesn't understand escapes.
  ; FIXME: It would be nice to save the indent and restore it later.
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "^ *\"\\(.*\\)\\\\n\"$" nil t)
      (replace-match "\\1" nil nil))))

(defun stringify (start end)
  "Converts the region to C-style strings."
  ; FIXME: This doesn't understand escapes.
  (interactive "r")
  (save-excursion
    (goto-char start)
    (push-mark end t)
    (while (re-search-forward "^\\(.*\\)$" (mark) t)
      (replace-match "\"\\1\\\\n\"" nil nil))
    (pop-mark)))

(defun pbcopy (begin end)
  "Copies the text between BEGIN and END to the system clipboard."
  (interactive "r")
  (let ((str (buffer-substring-no-properties begin end))
        (shell-command-switch "-ic"))
    (shell-command (concat "echo " (shell-quote-argument str) " | xsel --clipboard --input"))))
    ;(shell-command "xsel --clipboard --clear")))

; TODO: move window management to its own library
(defun eighty-cols ()
  "Sets the frame to 80 columns."
  (interactive)
  (when (< (frame-width) 81)
    (error "Frame too narrow, must be at least 81 was %d"
           (frame-width)))
  (when (not (eq (selected-window) (next-window nil 'no-minibuf)))
    (error "Too many windows"))
  (set-frame-width nil 81))

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

(load-theme 'material t)

(require 'whitespace)
(global-whitespace-mode 't)

(let ((go-path
       (or (getenv "GOROOT")
           (and (eq system-type 'darwin) "/usr/local/go"))))
  (if go-path
      (progn
        (add-to-list 'load-path (concat go-path "/misc/emacs"))
        (ignore-errors (require 'go-mode-load)))))

(add-to-list 'load-path (concat (getenv "HOME") "/blink-tools"))
(require 'blink-stuff)
(bk-setup)

(add-hook 'change-log-mode-hook
          (function (lambda () (setq indent-tabs-mode nil))))

(add-hook 'c++-mode-hook
          (function (lambda () (local-set-key "\C-o" 'ff-get-other-file))))

(add-hook 'c-mode-hook
          (function (lambda () (local-set-key "\C-o" 'ff-get-other-file))))

; org-mode

(setq org-agenda-files (list (concat (getenv "HOME") "/plan/plan.org")))
(setq org-default-notes-file (car org-agenda-files))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook
  (lambda ()
    (set (make-local-variable 'whitespace-style)
         (quote (trailing space-before-tab empty space-after-tab)))))

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
 '(custom-safe-themes
   (quote
    ("c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-doc turn-on-haskell-indent)))
 '(indent-tabs-mode nil)
 '(js2-dynamic-idle-timer-adjust 1000)
 '(js2-idle-timer-delay 1.0)
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(kill-do-not-save-duplicates t)
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-capture-templates
   (quote
    (("x" "TODO" entry
      (file "~/plan/plan.org")
      "TODO %^{TODO}" :prepend t :clock-keep t))))
 '(org-clock-idle-time 15)
 '(org-drill-left-cloze-delimiter "(|")
 '(org-drill-right-cloze-delimiter "|)")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends (quote (ascii html latex)))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-archives
   (quote
    (("org" . "http://orgmode.org/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (org org-plus-contrib material-theme julia-mode js2-mode go-mode)))
 '(python-indent 2)
 '(python-indent-offset 2)
 '(sentence-end-double-space nil)
 '(standard-indent 2)
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab empty space-after-tab tab-mark lines-tail))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "goldenrod"))))
 '(js2-error ((t (:inherit flycheck-error :background "tomato" :foreground "sienna" :underline nil))))
 '(whitespace-line ((t (:foreground "GreenYellow")))))

(put 'upcase-region 'disabled nil)

(add-hook 'go-mode-hook
  (lambda ()
    (set (make-local-variable 'whitespace-style)
         (quote (trailing space-before-tab empty space-after-tab)))))
