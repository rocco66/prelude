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
        undo-tree          ;; visual undo tree
        gist               ;; integration with github's gist
        expand-region      ;; helps to select large chunks of code
        ;; language-specific modes
        coffee-mode js2-mode
        haskell-mode
        clojure-mode clojurescript-mode paredit
            highlight nrepl nrepl-eval-sexp-fu ;; clojure REPL-related stuff
        ess
        haml-mode sass-mode yaml-mode
        markdown-mode
        yaml-mode
        auctex ;; mode for LaTeX
        python
))

(defun package-check (p)
  (unless (package-installed-p p)
    (package-install p)))

(mapcar 'package-check extra-packages)

;; DEFUNS

(defun disable-guru-mode ()
  (guru-mode -1))

(defun disable-flyspell-mode ()
  (flyspell-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)
(add-hook 'prelude-prog-mode-hook 'disable-flyspell-mode t)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered 1 character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) 1))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun run-prog-hook ()
  (interactive)
  (run-hooks 'prog-hook))

(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; REQUIRES

(require 'smex)
(smex-initialize)

(require 'ace-jump-mode)

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/personal/temp/recentf"
      recentf-max-saved-items 100
      recentf-max-menu-items 10)
(recentf-mode t)

(require 'auto-complete nil t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config nil t)
(ac-config-default)
(setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat"
      ac-candidate-limit 20
      ac-ignore-case nil)
(global-auto-complete-mode)

;; BINDINGS

(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") nil) ;; I hate that suspend feature

(global-set-key (kbd "C->") 'textmate-shift-right)
(global-set-key (kbd "C-<") 'textmate-shift-left)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

(define-key global-map (kbd "C-`") 'ace-jump-word-mode)

(global-set-key [f11] 'switch-full-screen)

(global-set-key (kbd "C-x f") 'prelude-recentf-ido-find-file)

;; SETTINGS

(set-default-font "Dejavu Sans Mono-10")
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-10"))

(setq-default tab-width 4)

(global-linum-mode 1)
(global-hl-line-mode 1)
(global-rainbow-delimiters-mode 1)

(desktop-save-mode 1)

(mouse-avoidance-mode 'cat-and-mouse)

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 whitespace-style '(trailing
                    lines
                    space-before-tab space-after-tab
                    indentation))
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; remove trailing ws

(fset 'yes-or-no-p 'y-or-n-p)

(pending-delete-mode t) ;; replace region's content when typing
                        ;; with active region. Not sure if effective here

(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#6f6f6f")
(set-face-foreground 'show-paren-match-face "#94bff3")

;; whenever an external process changes a file underneath emacs, and
;; there was no unsaved changes in the corresponding buffer, just
;; revert its content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; ERLANG

(setq erlang-root-dir "/usr/local/lib/erlang")

(add-to-list 'load-path
             (concat erlang-root-dir "/lib/tools-2.6.6.6/emacs"))
(add-to-list 'exec-path
             (concat erlang-root-dir "/bin"))

(add-to-list 'ac-modes 'erlang-mode)

(add-hook 'erlang-mode-hook 'run-prog-hook)

(require 'erlang-start)

;; LaTeX

;; (require 'tex-site)
;; (require 'preview-latex)

(setq
 TeX-auto-save t
 TeX-parse-self t
 TeX-DVI-via-PDFTeX t)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (TeX-fold-mode 1)
             (TeX-PDF-mode 1)
             (outline-minor-mode 1)
             (add-to-list 'TeX-command-list
                          '("XeLaTeX" "xelatex -interaction=nonstopmode -shell-escape %s"
                            TeX-run-command t t :help "Run xelatex") t)))

;; Clojure

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun paredit-mode-enable () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'paredit-mode-enable)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'electrify-return-if-match)))

;; highlight expression on eval

;; taken from emacs-live project, needed for nrepl-eval-sexp-fu
(defun live-paredit-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'"
  (not
   (save-excursion
     (ignore-errors
       (paredit-forward-up)
       t))))

(require 'highlight)
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5
      nrepl-eval-sexp-fu-flash-face 'compilation-info-face
      nrepl-eval-sexp-fu-flash-error 'compilation-error-face)
