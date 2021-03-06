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
        git-timemachine
        coffee-mode js2-mode json-mode
        livescript-mode
        haskell-mode
        tuareg
        clojure-mode
            highlight nrepl nrepl-eval-sexp-fu ;; clojure REPL-related stuff
        ess
        haml-mode sass-mode jade-mode yaml-mode
        markdown-mode
        yaml-mode
        auctex ;; mode for LaTeX
        python
          sphinx-doc
          ;; flycheck-pyflakes
          ;; jedi jedi-direx;;python autocomplete
        evil ;; vim like keybindings
            evil-paredit ;; evil-nerd-commenter
        paredit paredit-everywhere
        surround
        full-ack ;; better than grep-find
        grizzl projectile
        scala-mode2 sbt-mode
        dired+
        manage-minor-mode
))


(defvar python-indent 4)

(defun package-check (p)
  (unless (package-installed-p p)
    (package-install p)))

(mapcar 'package-check extra-packages)

;; DEFUNS

(defun disable-guru-mode ()
  (guru-mode -1))

(defun disable-flyspell-mode ()
  (flyspell-mode -1))

(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

(defun fix-underscore ()
  ;; fix evil-search-word-backward work with underscore
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))


(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)
(add-hook 'prelude-prog-mode-hook 'disable-flyspell-mode t)
(remove-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(remove-hook 'prelude-coding-hook 'flyspell-prog-mode)
(remove-hook 'text-mode-hook 'prelude-turn-on-flyspell)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'fix-underscore)
(setq jedi:complete-on-dot t)
(setq require-final-newline t)

(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

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

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; REQUIRES

(require 'smex)
(smex-initialize)

(require 'evil)

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

(global-set-key (kbd "C-C C-SPC") 'copy-all)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x f") 'helm-projectile)
(global-set-key (kbd "C-x b") 'helm-mini)
; evil mode bindings

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-replace-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'magit-blame-mode 'emacs)
;; (evil-set-initial-state 'direx 'emacs)  ;; direx-jedi window
(set-variable 'magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10.9/emacsclient")


(evil-set-initial-state 'direx-mode 'emacs)

;; (add-hook 'direx-mode-hook 'evil-emacs-state)
;; (add-hook 'Direx-mode-hook 'evil-emacs-state)
;;(add-to-list 'evil-disabled-modes-list 'Direx)
;;(add-to-list 'evil-disabled-modes-list 'direx)

;;(evilnc-default-hotkeys)

;; SETTINGS

(projectile-global-mode)
;; (setq projectile-enable-caching t)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-completion-system 'grizzl)
(set-default-font "PT Mono-14")
(add-to-list 'default-frame-alist '(font . "PT Mono-14"))

(setq-default tab-width 4)

(global-linum-mode 1)
(global-hl-line-mode 1)

(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

;; (load-theme 'solarized-dark t)

(evil-mode 1)
(global-surround-mode 1)
(desktop-save-mode 1)
(paredit-everywhere-mode 1)

(mouse-avoidance-mode 'cat-and-mouse)

(setq
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 whitespace-style '(
                    tab-mark
                    space-mark
                    face
                    trailing
                    lines
                    tabs
                    spaces
                    space-before-tab space-after-tab
                    indentation))
;; nice dot-spaces colors
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray25")
(global-whitespace-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; (add-hook 'after-init-hook 'global-flycheck-mode)

;; (require 'erlang-start)

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

(setq nrepl-popup-stacktraces nil)

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

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-tab-command 'indent-for-tab-command)
;; (setq nrepl-popup-stacktraces nil)


;; haskell
(defun haskell-hook ()
  ;; Build the Cabal project.
  (turn-on-haskell-indentation)
  (define-key haskell-mode-map (kbd "C-c C-c")
    '(lambda ()
       (interactive)
       (haskell-process-cabal-build)
       (haskell-interactive-mode-clear)
       (haskell-interactive-switch))))
(add-hook 'haskell-mode-hook 'haskell-hook)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; ocaml
(push "~/.opam/system/share/emacs/site-lisp" load-path)
;; (autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)


;; jinja2 localizastion
(defun wrap-jinja2-underscore ()
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "{{ _(\""))
  (save-excursion
    (goto-char (region-end))
    (insert "\") }}")))

(defun wrap-jinja2-block ()
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "{% trans -%}"))
  (save-excursion
    (goto-char (region-end))
    (insert "{%- endtrans %}")))

;; angular l10n
(defun wrap-angular-gettext-string ()
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "{{'"))
  (save-excursion
    (goto-char (region-end))
    (insert "'|translate}}")))

(defun wrap-angular-gettext-block ()
  (interactive)
  (save-excursion
    (insert ", translate")))

(defun wrap-angular-gettext-empty-block ()
  (interactive)
  (save-excursion
    (insert "(translate)")))


(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
  the lowercase version preceded by an underscore.

  The first char, if capitalized (eg, PascalCase) is just
  downcased, no preceding underscore.
  "
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(global-set-key (kbd "C-x M-w") 'wrap-angular-gettext-string)
(global-set-key (kbd "C-x M-e") 'wrap-angular-gettext-empty-block)
(global-set-key (kbd "C-x M-b") 'wrap-angular-gettext-block)
(global-set-key (kbd "C-x M-l") 'un-camelcase-word-at-point)

;; some JIRA/STASH integration
(defun insert-issue-number-from-branch ()
  "Get current issue name from branch and insert in commit message"
  (interactive)
  (let* ((branch-name (magit-get-current-branch))
         (pat "#\\([A-Z]+-[0-9]+\\)$")
         (pos (string-match pat branch-name)))
    (if pos
        (save-excursion
          (end-of-buffer)
          (insert "\n")
          (insert (match-string 1 branch-name)))
      (message "#PROJECT-number not found in the end of branch name"))))

(global-set-key (kbd "C-x M-i") 'insert-issue-number-from-branch)

;; ack
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(global-set-key (kbd "C-c C-b") 'livescript-compile-buffer)
(global-set-key (kbd "C-c C-r") 'livescript-compile-region)

;; fix stupid missing linenumbers
;; http://stackoverflow.com/questions/16094303/global-line-numbering-is-intermittent-conflict-with-global-whitespace
(setq linum-format "%d")
