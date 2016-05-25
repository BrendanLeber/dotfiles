(when (version< emacs-version "24.1")
  (error "This config requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

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
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet wrap-region which-key volatile-highlights use-package spacegray-theme smartscan ox-mediawiki org-bullets magit ggtags fill-column-indicator discover color-theme cmake-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
