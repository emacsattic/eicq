;;; eicq-convert.el --- Utilities to convert other ICQ configurations to EICQ

;; Copyright (C) 2001 Steve Youngs, Erik Arneson

;; RCS: $Id$
;; OriginalAuthor: Erik Arneson <erik@aarg.net>
;; Maintainer: Erik Arneson <erik@aarg.net>
;; Created: Aug 06, 2001
;; Last-Modified: <2001-9-15 20:12:47 (steve)>
;; Homepage: http://eicq.sf.net/
;; Keywords: comm ICQ

;; Eicq is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; Eicq is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


(require 'eicq)

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
    (set-buffer (find-file-noselect (expand-file-name "~/.micqrc")))
    (goto-char (point-min))
    (if (re-search-forward "^UIN \\([0-9]+\\)$" nil t)
        (setq me (match-string 1)))
    (if (re-search-forward "^Contacts$" nil t)
        (setq user-alist
              (loop while (re-search-forward "^[ \t]*\\*?\\([0-9]+\\)[ \t]+\\(.*\\)$" nil t)
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
;time-stamp-format: "%4y-%m-%d %02H:%02M:%02S (%u)"
;End: 
