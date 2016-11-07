(defun reload-settings ()
  "Reload configuration settings after changes have been made."
  (interactive)
  (load-file "~/.emacs.d/init.el"))


(defun load-if-exists (file)
  "Load `file` if it exists."
  (when (file-exists-p file)
    (load file)))


(defun revert-this-buffer ()
  "Revert the current buffer without confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

(bind-key "<f6>" #'revert-this-buffer)


(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring."))

(global-set-key (kbd "C-`") #'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(bind-key "M-`" #'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] #'exchange-point-and-mark-no-activate)


(defun toggle-show-trailing-whitespace ()
  "Toggle `show-trailing-whitespace' between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (redraw-display))
