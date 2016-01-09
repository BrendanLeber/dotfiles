;; -*- coding: utf-8; lexical-binding: t; -*-

(when (version< emacs-version "24.1")
  (error "This config requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;;;; some packages we always want available
(require 'cl-lib)
(require 'uniquify)
(require 'whitespace)


;;;; save our customizations externally to keep this file "clean."
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(set-register ?I `(file . ,user-init-file))
(set-register ?C `(file . ,custom-file))
(set-register ?A `(file . ,(expand-file-name "~/Documents/agenda.org")))


;;;; some things i haven't found a way to store in custom.el
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;;; always try to use UTF-8 for encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)


;;;; special character C-x 8 shortcuts
(require 'iso-transl)
(dolist (binding '((">"   . nil) ; First unbind ">" from the map
                   (">="  . [?≥])
                   (">>"  . [?≫])
                   (">\"" . [?»])
                   (">'"  . [?›])
                   ("<"   . nil) ; First unbind "<" from the map
                   ("<="  . [?≤])
                   ("<<"  . [?≪])
                   ("<\"" . [?«])
                   ("<'"  . [?‹])
                   ("\"[" . [?“]) ; magic curly quotes
                   ("\"]" . [?”])
                   ("'["  . [?‘])
                   ("']"  . [?’])))
  (define-key iso-transl-ctl-x-8-map (kbd (car binding)) (cdr binding)))


;;;; don't suspend when we're running in a graphic display
(when (display-graphic-p)
  (put 'suspend-frame 'disabled t))


;;;; package management
;; prepage emacs package handling.  really just boot-strap use-package
(require 'package)
(setq pacqage-enable-at-startup nil)
(package-initialize)

;; auto-magic installation of use-package if doesn't already exist
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)
(require 'diminish)


;;;; programming support
(use-package smartscan
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartscan-mode-turn-on))

;; fill-column-indicator
(use-package fill-column-indicator
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fci-mode))


;;;; my elisp and third-party elisp modules
(defvar modules-dir (concat user-emacs-directory "modules/")
  "This directory contains my lisp modules.")

(defvar third-party-dir (concat user-emacs-directory "third-party/")
  "This directory contains lisp modules from third party sources.")

(add-to-list 'load-path third-party-dir)
(add-to-list 'load-path modules-dir)


;;;; color/font customizations
(use-package spacegray-theme
  :ensure t)

(use-package material-theme
  :ensure t)

(if (display-graphic-p)
    (load-theme 'spacegray t)
  (load-theme 'material t))


;;;; my functions
(defun indent-buffer ()
  "Indent the entire buffer based on the current mode settings."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun narrow-to-line (&optional arg)
  "Narrow to the text of the current line.
A numeric prefix arg means move forward (backward if negative) that
many lines, thus narrowing to a line other than the one point was
originally in."
  (interactive "P")
  (setq arg (if arg
                (prefix-numeric-value arg)
              0))
  (let ((inhibit-field-motion t))
    (save-excursion
      (forward-line arg)
      (narrow-to-region (line-beginning-position) (line-end-position)))))


(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(bind-key "C-`" 'switch-to-previous-buffer)


;;;; global key bindings
(use-package bind-key
  :ensure t
  :config
  (bind-key "M-/" 'hippie-expand)
  (bind-key "C-x C-b" 'ibuffer)
  (bind-key "M-o" 'other-window)
  (bind-key "C-s" 'isearch-forward-regexp)
  (bind-key "C-r" 'isearch-backward-regexp)
  (bind-key "C-M-s" 'isearch-forward)
  (bind-key "C-M-r" 'isearch-backward)
  (bind-key "RET" 'newline-and-indent))


;;;; lisp
(use-package lisp-mode
  :config
  (bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)
  (bind-key "C-\\" 'lisp-complete-symbol lisp-mode-shared-map)
  (bind-key "C-;" 'comment-region lisp-mode-shared-map)
  (bind-key "C-c t" 'indent-buffer lisp-mode-shared-map))

;;;; elisp
(use-package emacs-lisp-mode
  :preface (provide 'emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
  (bind-key "RET" 'reindent-then-newline-and-indent emacs-lisp-mode-map)
  (bind-key "C-\\" 'lisp-complete-symbol emacs-lisp-mode-map)
  (bind-key "C-;" 'comment-region emacs-lisp-mode-map)
  (bind-key "C-c t" 'indent-buffer emacs-lisp-mode-map)
  (bind-key "C-c v" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
  (defun remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook 'remove-elc-on-save-hook))
  (defun remove-elc-on-save-hook ()
    "Function run in the after-save hook to remove the .elc when saving an .el file."
    (if (file-exists-p (concat buffer-file-name "c"))
        (delete-file (concat-buffer-file-name "c")))))


;;;; gnu global (gtags)
(use-package ggtags
  :ensure t
  :config
  (bind-key "C-c g s" 'ggtags-find-other-symbol ggtags-mode-map)
  (bind-key "C-c g h" 'ggtags-view-tag-history ggtags-mode-map)
  (bind-key "C-c g r" 'ggtags-find-reference ggtags-mode-map)
  (bind-key "C-c g f" 'ggtags-find-file ggtags-mode-map)
  (bind-key "C-c g c" 'ggtags-create-tags ggtags-mode-map)
  (bind-key "C-c g u" 'ggtags-update-tags ggtags-mode-map)
  (bind-key "M-," 'pop-tag-mark ggtags-mode-map)
  (add-hook 'prog-mode-hook 'maybe-turn-on-ggtags-mode))

(defun maybe-turn-on-ggtags-mode ()
  "Possibly turn on `ggtags-mode' if the current mode is derived from one of
the major programming modes.  `c-mode', `c++-mode', `java-mode', `asm-mode'."
  (interactive)
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
    (ggtags-mode 1)))


;;;; c/c++
(require 'cc-mode)

(bind-key "C-;" 'comment-region c-mode-base-map)
(bind-key "C-c o" 'ff-find-other-file c-mode-base-map)
(bind-key "C-c w" 'delete-trailing-whitespace c-mode-base-map)
(bind-key "C-M-a" 'c-beginning-of-defun c-mode-base-map)
(bind-key "C-M-e" 'c-end-of-defun c-mode-base-map)
(bind-key "RET" 'reindent-then-newline-and-indent c-mode-base-map)


;;;; java
;; (use-package java-mode
;;   :init
;;   (defun my-java-mode-hook ()
;;     (setq indent-tabs-mode nil)
;;     (setq fill-column 78)
;;     (setq column-number-mode t))
;;   :config
;;   (add-hook 'java-mode-hook 'my-java-mode-hook))

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq column-number-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)


;;;; python
(use-package python
  :config
  (defun isilon-python-mode-hook ()
    (set-variable 'py-indent-offset 4))
  (add-hook 'python-mode-hook 'isilon-python-mode-hook)
  (bind-key "C-;" 'comment-region python-mode-map))


;;;; svn/git/whatever
(use-package ediff
  :config
  (defun ediff-current-buffer-revision ()
    "Run Ediff to diff current buffer's file against VC depot.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
    (interactive)
    (let ((file (or (buffer-file-name)
                    (error "Current buffer is not visiting a file"))))
      (if (and (buffer-modified-p)
               (y-or-n-p (message "Buffer %s is modified. Save buffer? "
                                  (buffer-name))))
          (save-buffer (current-buffer)))
      (ediff-load-version-control)
      (funcall
       (intern (format "ediff-%S-internal" ediff-version-control-package))
       "" "" nil))))

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'ediff-current-buffer-revision))

(use-package magit
  :ensure t)


;;;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" . markdown-mode) ("\\.md\\'" . markdown-mode))


;;;; eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))


;;;; irc (erc)
(use-package erc
  :ensure t
  :bind ("C-c I" . irc)
  :init
  (make-variable-buffer-local 'erc-fill-column)
  (add-hook 'window-configuration-change-hook
            '(lambda ()
               (save-excursion
                 (walk-windows
                   (lambda (w)
                     (let ((buffer (window-buffer w)))
                       (set-buffer buffer)
                       (when (eq major-mode 'erc-mode)
                         (setq erc-fill-column (- (window-width w) 2)))))))))
  :config
  (setq erc-prompt-for-nickserv-password nil
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
        erc-auto-query 'buffer
        erc-server-auto-reconnect t
        erc-server-reconnect-attempts 5
        erc-server-reconnect-timeout 3
        erc-rename-buffers t
        erc-interpret-mirc-color t)
  (add-to-list 'erc-mode-hook (lambda ()
                                (set (make-local-variable 'scroll-conservatively) 101))))

;  (defun my-rcirc-mode-hook ()
;    (flyspell-mode 1)
;    (rcirc-omit-mode)
;    (set (make-local-variable 'scroll-conservatively) 8192))
;  :config
;  (add-hook 'rcirc-mode-hook 'my-rcirc-mode-hook))


;;;; wrap region
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
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode))


;;;; isbn/bibtex
(load "isbn.el")

(defun sort-bibtex-entries ()
  "Sort a BibTeX bibliography file by the cite keys.  @string{} entries
at the start of the file are left in place.  The remaining entries are
sorted, and left with one intervening blank line between each of them."
  (interactive)
  (let ((sort-tag "\001SORT-TAG:"))	;sort-tag is a unique prefix string
    (goto-char (point-min))
    (insert "\f" sort-tag "\001\n")	;Ctl-A tag ensures we keep @string{}
    (while				;stuff in first page
	(re-search-forward "^@string" nil t)) ;skip past @string{} entries
    (save-excursion
      (while				;find bibliography entry
	  (re-search-forward "^@[^{]*{" nil t)
	(progn
	  (let ((the-tag))
	    (let ((k (point)))
	      (end-of-line)
	      (setq the-tag (buffer-substring k (point))))
	    (beginning-of-line)
	    (let ((k (point)))
	      (insert "\f" sort-tag the-tag "\n") ;and tag it for sorting
	      (upcase-region k (point))) ;alas, sort is case-sensitive
	    (forward-line 1)))))
    (sort-pages nil (point-min) (point-max)) ;do the sort
    (goto-char (point-min))
    (flush-lines sort-tag))		;remove the sort tags
  (goto-char (point-min))
  (replace-string "\f" "")		;remove left-over page marks
  (goto-char (point-min))
  (while
      (re-search-forward "^@string" nil t)) ;skip past @string{} entries
  (while				;find bibliography entry
      (re-search-forward "^@[^{]*{" nil t)
    (progn
      (beginning-of-line)
      (insert "\n\n")			;and regularize space before it
      (forward-line -1)
      (delete-blank-lines)
      (forward-line 2)))
  (goto-char (point-max))
  (delete-blank-lines)
  (goto-char (point-min))
  (replace-regexp "[ \t]+$" "")		;blank trim lines
  (goto-char (point-min)))


;;;; which-key gives extra help on keys
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;;;; emacs server/client
;; (require 'server)

(defun server-shutdown ()
  "Save buffers, quit and shutdown the Emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; (or (server-running-p) (server-start))
