(when (version< emacs-version "24.1")
  (error "This config requires at least GNU Emacs 24.1, but you're running %s" emacs-version))


;;; Hey, I want to use a few of these features
(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'upcase-region               'disabled nil)   ; Let upcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'company-coq-fold            'disabled nil)
(put 'TeX-narrow-to-group         'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)


;;; Always try to use UTF-8 for encoding.
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;;; package management
;; prepage emacs package handling.  really just boot-strap use-package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq pacqage-enable-at-startup nil)
(package-initialize)

;; auto-magic installation of use-package if doesn't already exist
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;;;  Make some short cuts for loading common files
(set-register ?A `(file . ,(expand-file-name "~/Documents/agenda.org")))
(set-register ?I `(file . ,user-init-file))


;;; Set Location
(setq calendar-latitude 47.598347
      calendar-longitude -122.344133
      calendar-location-name "Seattle, WA")


;;; Diminish minor-modes we don't need to see
(diminish 'auto-fill-mode)


;;; advice functions
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)


;;; Colors/Theme
(use-package color-theme     :ensure t)
(use-package spacegray-theme
  :ensure t
  :config
  (load-theme 'spacegray t))

;; Set default font and configure font re-sizing
(setq bml/default-font "Source Code Pro")
(setq bml/default-font-size 12)
(setq bml/current-font-size bml/default-font-size)
(setq bml/font-change-increment 1.1)

(defun bml/set-font-size ()
  "Set the font to `bml/default-font' at `bml/current-font-size'."
  (set-frame-font
   (concat bml/default-font "-" (number-to-string bml/current-font-size))))

(defun bml/reset-font-size ()
  "Change font size back to `bml/default-font-size'."
  (interactive)
  (setq bml/current-font-size bml/default-font-size)
  (bml/set-font-size))

(defun bml/increase-font-size ()
  "Increase current font size by a factor of `bml/font-change-increment'."
  (interactive)
  (setq bml/current-font-size
        (ceiling (* bml/current-font-size bml/font-change-increment)))
  (bml/set-font-size))

(defun bml/decrease-font-size ()
  "Decrease current font size by a factor of `bml/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq bml/current-font-size
          (max 1
               (floor (/ bml/current-font-size bml/font-change-increment))))
    (bml/set-font-size))

(bind-key "C-)" #'bml/reset-font-size)
(bind-key "C-+" #'bml/increase-font-size)
(bind-key "C-=" #'bml/increase-font-size)
(bind-key "C-_" #'bml/decrease-font-size)
(bind-key "C--" #'bml/decrease-font-size)


;;; Utility Functions
(defun bml/reload-settings ()
  "Reload configuration settings after changes have been made."
    (interactive)
    (load-file "~/.emacs.d/init.el"))


(defun bml/load-if-exists (file)
  "Load `file` if it exists."
  (when (file-exists-p file)
    (load file)))


(defun bml/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun bml/unfill-region (begin end)
  "Change isolated newlines in region into spaces."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (save-restriction
    (narrow-to-region (or begin (point-min)) (or end (point-max)))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (if (eq (char-after) ?\n)
          (skip-chars-forward "\n")
        (delete-char -1)
        (insert ?\s)))))


(defun bml/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))


(defun bml/indent-buffer ()
  "Indent the entire buffer based on the current mode settings."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))


(defun bml/toggle-show-trailing-whitespace ()
  "Toggle `show-trailing-whitespace' between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (redraw-display))


(defun bml/enable-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'bml/enable-show-trailing-whitespace)


(defun bml/narrow-to-line (&optional arg)
  "Narrow to the text of the current line.
A numeric prefix arg means move forward (backward if negative)
that many lines, thus narrowing to a line other than the one
point was originally in."
  (interactive "P")
  (setq arg (if arg
                (prefix-numeric-value arg)
              0))
  (let ((inhibit-field-motion t))
    (save-excursion
      (forward-line arg)
      (narrow-to-region (line-beginning-position) (line-end-position)))))


(defun bml/switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and
forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(bind-key "C-`" #'bml/switch-to-previous-buffer)

(defun screenshot-frame ()
  "Take screenshot.
Default image: ~/Pictures/emacs-TIMESTAMP.png
Usage:
M-x screenshot-frame
Enter custom-name or RET to save image with timestamp."
  (interactive)
  (let* ((insert-default-directory t)
	 (screenshots-dir "~/Pictures/")
	 (sframe-name (concat (format-time-string "emacs_%FT%H%M%S") ".png"))
	 (sframe-full-path (concat screenshots-dir sframe-name)))
    (if (not (file-accessible-directory-p screenshots-dir))
	     (make-directory-internal screenshots-dir))
    (shell-command-to-string
     (concat "import " sframe-full-path))
    (message "Screenshot saved to %s" sframe-full-path)))


;;; Packages

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer tabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  :bind (("C-S-RET" . crux-smart-open-line-above)
	 ("C-a" . crux-move-beginning-of-line)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("S-RET" . crux-smart-open-line)))


(use-package cc-mode
  :ensure t
  :init
  (setq c-default-style '((c-mode . "stroustrup")
                          (c++-mode . "stroustrup")
                          (java-mode . "java")
                          (awk-mode . "awk")
                          (other . "gnu")))
  (defun bml/c-mode-common-hook ()
    (setq fill-column 78)
    (setq column-number-mode t)
    (c-set-offset 'arglist-cont-nonempty '*)
    (c-set-offset 'inline-open 0))
  :bind (:map c-mode-base-map
              ("C-;" . comment-region)
              ("C-c o" . ff-find-other-file)
              ("C-c w" . delete-trailing-whitespace)
              ("C-M-a" . c-beginning-of-defun)
              ("C-M-e" . c-end-of-defun)
              ("RET" . reindent-then-newline-and-indent))
  :config
  (add-hook 'c-mode-common-hook #'bml/c-mode-common-hook))


(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))


(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)


(use-package emacs-lisp-mode
  :preface (provide 'emacs-lisp-mode)
  :init
  (defun remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook))
  (defun remove-elc-on-save-hook ()
    "Function run in the after-save hook to remove the .elc when saving an .el file."
    (if (file-exists-p (concat buffer-file-name "c"))
        (delete-file (concat-buffer-file-name "c"))))
  :bind (:map emacs-lisp-mode-map
              ("RET"   . reindent-then-newline-and-indent)
              ("C-\\"  . lisp-complete-symbol)
              ("C-c t" . indent-buffer)
              ("C-c v" . eval-buffer)
              ("M-."   . find-function-at-point))
  :config
  (add-hook 'after-save-hook #'remove-elc-on-save-hook)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'remove-elc-on-save))


(use-package eshell
  :ensure t
  :init
  (defun eshell/x ()
    "Closes the EShell session and gets rid of the EShell window."
    (kill-buffer)
    (delete-window))
  (defun bml/eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple shell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)))
  :config
  (use-package em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (setq eshell-scroll-to-bottom-on-input t)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  :bind
  ("C-!" . bml/eshell-here))


(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fci-mode))


(use-package flycheck-ledger
  :ensure t)


(use-package flyspell
  :diminish flyspell-mode)


(use-package ggtags
  :ensure t
  :init
  (defun bml/maybe-turn-on-ggtags-mode ()
    "Possibly turn on `ggtags-mode' if the current mode is derived from one of
the major programming modes.  `c-mode', `c++-mode', `java-mode', `asm-mode'."
    (interactive)
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
      (ggtags-mode 1)))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark))
  :config
  (add-hook 'prog-mode-hook #'bml/maybe-turn-on-ggtags-mode))


(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'")


(use-package magit
  :ensure t
  :commands magit-status
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-argumnets nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by defualt
        magit-default-tracking-name-function #'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; get rid of the previous advice to go fullscreen
        magit-restore-window-configuration t)
  :bind ("C-x g" . magit-status))


(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  (setq org-agenda-window-setup (quote current-window))
  (setq org-default-notes-file "~/Documents/notes.org")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (defface org-block-begin-line
    '((t (:foreground "#99968b" :background "#303030")))
    "Face used for the line delimiting the begin of source blocks.")
  (defface org-block-end-line
    '((t (:foreground "#99968b" :background "#303030")))
    "Face used for the line delimiting the end of source blocks.")
  :config
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode))


;; we like showing the matching parens
(use-package paren
  :init (show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren nil
	show-paren-when-point-in-periphery t))


(use-package python
  :ensure t
  :init
  (defun isilon/python-mode-hook ()
    (set-variable 'py-indent-offset 4))
  :bind (:map python-mode-map
	      ("C-;" . comment-region))
  :config
  (add-hook 'python-mode-hook #'isilon/python-mode-hook))


(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "clisp")
  (setq slime-contribs '(slime-fancy)))


(use-package smartscan
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartscan-mode-turn-on))


(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t      ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers


(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))


(use-package whitespace
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  (setq-default show-trailing-whitespace t)
  (defun no-trailing-whitespace ()
    (setq show-trailing-whitespace nil))
  (add-hook 'minibuffer-setup-hook #'no-trailing-whitespace)
  (add-hook 'ielm-mode-hook #'no-trailing-whitespace)
  (add-hook 'gdb-mode-hook #'no-trailing-whitespace)
  (add-hook 'help-mode-hook #'no-trailing-whitespace)
  :diminish whitespace-mode)


(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))))
  (add-hook 'org-mode-hook #'wrap-region-mode)
  (add-hook 'latex-mode-hook #'wrap-region-mode))


;;; My Key Bindings

;; Main keymaps for personal bindings are:
;; C-x <letter> - primary map (has many defaults too)
(bind-key "C-x C-b" #'ibuffer)

;; C-c <letter> - secondary map (not just for mode-specific)

;; C-c t <letter> - toggles
(bind-key "C-c t w" #'whitespace-mode)
(bind-key "C-c t t" #'bml/toggle-show-trailing-whitespace)
(bind-key "C-c t f" #'auto-fill-mode)
(bind-key "C-c t e" #'toggle-debug-on-error)

;; C-. <letter> - tertiary map
;; M-g <letter> - goto map
;; M-s <letter> - search map
;; M-o <letter> -  markup map (even if only temporarily)
;; C-<capital letter>
;; M-<capital letter>
;; A-<anything>
;; M-A-<anything>
;; All Other Keybindings

;; Single-letter bindings still available:
;;  C- ,'";:?<>|!#$%^&*`~ <tab>
;;  M- ?#

(bind-key "M-/" #'hippie-expand)
(bind-key "M-o" #'other-window)
;; (bind-key "C-s" #'isearch-forward-regexp)
;; (bind-key "C-r" #'isearch-backward-regexp)
;; (bind-key "C-M-s" #'isearch-forward)
;; (bind-key "C-M-r" #'isearch-backward)
(bind-key "RET" #'newline-and-indent)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/auto-save-list/" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(calendar-week-start-day 1)
 '(delete-old-versions -1)
 '(gdb-many-windows t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (flycheck-ledger ledger-mode wrap-region which-key volatile-highlights use-package spacegray-theme smartscan magit ggtags fill-column-indicator discover color-theme cmake-mode)))
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(user-full-name "Brendan Leber")
 '(user-mail-address "brendan@brendanleber.com")
 '(vc-make-backup-files t)
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 120 :width normal :family "Source Code Pro")))))
