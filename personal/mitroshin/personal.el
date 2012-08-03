;; save open files
(desktop-save-mode 1)

;; always trim trailing whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)

;; upgrade coffeescript mode
(defun my-coffee-hook ()
  (whitespace-mode 1))
(add-hook 'coffee-mode-hook 'my-coffee-hook)
