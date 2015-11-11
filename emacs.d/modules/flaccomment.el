;;; flaccomment.el --- Edit comments in FLAC files

;; Copyright (C) 2008 Brendan Leber

;; Author: Brendan Leber <brendan@brendanleber.com>
;; Created: 19 Jul 2008
;; Version: 0.1
;; Location: 
;; Keywords: multimedia, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; flaccomment.el is based on vorbiscomment.el by Jason Harder.

;; Use `M-x flaccomment' to extract the comment headers from an FLAC
;; file.  `C-c C-c' in the comment buffer writes the comments
;; back to the file.  You need the program "metaflac".

;;; History:

;; Version 0.1  Initial revision.

;;; Code:

(defvar flaccomment-keywords
  (list (concat "^"
		(regexp-opt '("title" "version" "album" "tracknumber" "artist"
			      "performer" "copyright" "license" "organization"
			      "description" "genre" "date" "location" "contact"
			      "isrc" "discnumber" "label" "labelno" "opus"
			      "sourcemedia" "composer" "arranger" "lyricist"
			      "author" "conductor" "lyricist" "performer"
			      "ensemble" "part" "partnumber") t) "="))
  "Canonical FLAC tag names.")

(define-derived-mode flaccomment-mode text-mode "flaccomment"
  (setq font-lock-defaults '(flaccomment-keywords t t nil nil))
  (make-variable-buffer-local 'flaccomment-file))

(define-key flaccomment-mode-map (kbd "C-c C-c")
  'flaccomment-save-and-exit)

(defun flaccomment (file)
  "Edit comments in FLAC file FILE.\\<flaccomment-mode-map>
Use \\[flaccomment-save-and-exit] to save comments."
  (interactive "fFLAC file: ")
  (switch-to-buffer (generate-new-buffer file))
  (flaccomment-mode)
  (setq flaccomment-file (expand-file-name file))
  (let ((coding-system-for-read 'utf-8))
    (call-process "metaflac" nil t t
		  "--no-utf8-convert"
		  "--export-tags-to=-"
		  flaccomment-file))
  (goto-char (point-min)))

(defun flaccomment-save-and-exit ()
  "Save comments and exit."
  (interactive)
  (let (status)
    (message "Saving comments to %s" flaccomment-file)
    (encode-coding-region (point-min) (point-max) 'utf-8)
    (setq status
	  (with-output-to-string
	    (call-process-region (point-min) (point-max)
				 "metaflac" nil standard-output nil
				 "--no-utf8-convert"
				 "--remove-all-tags"
				 "--import-tags-from=-"
				 flaccomment-file)))
    (if (string= status "")
	(message "Comments saved in %s" flaccomment-file)
      (error status)))
  (kill-buffer nil))

(provide 'flaccomment)

;;; flaccomment.el ends here
