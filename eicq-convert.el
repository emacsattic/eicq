;;; eicq-convert.el --- Utilities to convert other ICQ configurations to EICQ

;; Copyright (C) 2001,02,03 Steve Youngs, Erik Arneson

;; RCS: $Id$
;; OriginalAuthor: Erik Arneson <erik@aarg.net>
;; Maintainer: Erik Arneson <erik@aarg.net>
;; Created: Aug 06, 2001
;; Last-Modified: <2003-09-19 11:46:46 (steve)>
;; Homepage: http://eicq.sf.net/
;; Keywords: comm ICQ

;; This file is part of Eicq.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(eval-and-compile
  (require 'eicq-world))

;;;###autoload
(defun eicq-import-from-licq ()
  "Import your contact list from LICQ.  It doesn't import ignored UIDs."
  (interactive)
  (let (uin udat user-alist ignored)
    (setq user-alist
          (loop for file in (directory-files
                             (expand-file-name "~/.licq/users/") t nil nil t)
            when (file-readable-p file)
            do (progn
                 (string-match "/\\([0-9]+\\)\\.uin$" file)
                 ;; Why can't I use 'match-string' here?  Gah!
                 (setq uin (subseq file (match-beginning 1) (match-end 1)))
                 (set-buffer (find-file-noselect file))
                 ;; Need to also look for 'Groups.System = 24' to find
                 ;; out if the user is ignored.
                 (if (re-search-forward "^Groups.System = 24$" nil t)
                     (setq ignored t)
                   (setq ignored nil)
                   (goto-char (point-min))
                   ;; If an Alias is listed, use that.  Otherwise, set
                   ;; the alias to the UIN.
                   (if (re-search-forward "^Alias = \\(.*\\)$" nil t)
                       (setq udat (cons uin (match-string 1)))
                     (setq udat (cons uin uin))))
                 (kill-buffer (current-buffer)))
            when (not ignored)
            collect udat))
    (set-buffer (find-file-noselect (expand-file-name eicq-world-rc-filename)))
    (goto-char (point-max))
    (insert "\n\n==== These entries were imported from your LICQ configuration.\n")
    (loop for udat in user-alist
      do (insert (format ":icq %s %s :licq\n" (car udat) (cdr udat))))
    (save-buffer (current-buffer))
    (kill-buffer (current-buffer))))

;;;###autoload
(defun eicq-import-from-micq ()
  "Import ICQ contact data from a .micqrc file."
  (interactive)
  (let (user-alist
        unode me)
    (set-buffer (find-file-noselect 
		 (or (expand-file-name "micqrc"
				       (file-name-as-directory
					(expand-file-name ".micq"
							  (getenv "HOME"))))
		     (expand-file-name ".micqrc" (getenv "HOME")))))
    (goto-char (point-min))
    (if (re-search-forward "^UIN \\([0-9]+\\)$" nil t)
        (setq me (match-string 1)))
    (if (re-search-forward "^Contacts$\\|^\\[Contacts\\]$" nil t)
        (setq user-alist
              (loop while (re-search-forward 
			   "^[ \t]*\\*?\\([0-9]+\\)[ \t]+\\(.*\\)$" nil t)
                do (setq unode (cons (match-string 1) (match-string 2)))
                collect unode)))
    (kill-buffer (current-buffer))
    (set-buffer (find-file-noselect (expand-file-name eicq-world-rc-filename)))
    (goto-char (point-max))
    (insert "\n\n==== These entries were imported from your MICQ configuration.\n")
    (if me
        (insert (format ":icq %s me\n\n" me)))
    (loop for unode in user-alist
      do (insert (format ":icq %s %s :micq\n" (car unode) (cdr unode))))
    (save-buffer (current-buffer))
    (kill-buffer (current-buffer))))

;; End of file.

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
