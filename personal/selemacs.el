;; INIT
(defun maximize ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz"))
(maximize)

(scroll-bar-mode -1)

;; PACKAGES

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq extra-packages
      '(rainbow-delimiters ;; nested parenthesis are now different
                           ;; in color
        smex               ;; IDO for M-x
        auto-complete      ;; auto complete mode
        icomplete+         ;; better icomplete
        ace-jump-mode      ;; allows to jump in visible part of the
                           ;; buffer
        ;; language-specific modes
        coffee-mode js2-mode
        haskell-mode
        clojure-mode
        ess
        haml-mode sass-mode yaml-mode
        markdown-mode
        auctex ;; mode for LaTeX
))

(defun package-check (p)
  (unless (package-installed-p p)
    (package-install p)))

(mapcar 'package-check extra-packages)

;; DEFUNS

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defun disable-guru-mode ()
  (guru-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)
(add-hook 'prelude-prog-mode-hook 'auto-complete-mode)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

;; BINDINGS

(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") nil) ;; I hate that suspend feature

(global-set-key (kbd "C->") 'textmate-shift-right)
(global-set-key (kbd "C-<") 'textmate-shift-left)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-`") 'ace-jump-word-mode)

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)

;; SETTINGS
(set-default-font "Dejavu Sans Mono-10")
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-10"))

(global-linum-mode 1)
(global-hl-line-mode 1)
(global-rainbow-delimiters-mode 1)

(mouse-avoidance-mode 'cat-and-mouse)

(add-hook 'linum-before-numbering-hook
          (lambda ()
            (let ((w (length (number-to-string
                              (count-lines (point-min) (point-max))))))
              (setq linum-format
                    `(lambda (line)
                       (propertize (concat
                                    (truncate-string-to-width
                                     "" (- ,w (length (number-to-string line)))
                                     nil ?\x2007)
                                    (number-to-string line))
                                   'face 'linum))))))

(fset 'yes-or-no-p 'y-or-n-p)

(when (and (require 'auto-complete nil t)
           (require 'auto-complete-config nil t))
  (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat"
        ac-candidate-limit 20
        ac-ignore-case nil)
  (global-auto-complete-mode))

;; whenever an external process changes a file underneath emacs, and
;; there was no unsaved changes in the corresponding buffer, just
;; revert its content to reflect what's on-disk.
(global-auto-revert-mode 1)
