;;; eicq-world.el --- Eicq contact list management

;; Copyright (C) 2002, Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-10-01
;; Last-Modified: <2002-10-03 12:29:12 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

;; This file is part of Eicq.

;; Eicq is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; Eicq is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;;


(eval-when-compile
  (require 'eicq-meta)
  (require 'eicq-menu)
  (autoload 'eicq-search-by-uin "eicq" nil t)
  (autoload 'eicq-buddy-show-buffer "eicq-buddy" nil t)
  (autoload 'eicq-send "eicq")
  (autoload 'eicq-pack "eicq")
  (autoload 'eicq-uin-bin "eicq")
  (autoload 'eicq-process-alias-input "eicq")
  (autoload 'eicq-buddy-update-face "eicq-buddy")
  (autoload 'eicq-completing-aliases "eicq")
  (autoload 'eicq-valid-uin-p "eicq")
  (autoload 'eicq-alias-bin "eicq"))

(defvar eicq-world-rc-filename "~/.eicq/world"
  "*Filename for resource file.")

;;; Internal variables

(defcustom eicq-user-alias "me"
  "*Your alias in `eicq-world'.
Run `eicq-world-update' after modifying this variable."
  :group 'eicq-info)

(defvar eicq-user-bin nil
  "User alias in binary string.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defvar eicq-all-uin nil
  "All uin in `eicq-world'.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defvar eicq-world nil
  "List of alias, uin, and plist.")

(defvar eicq-all-aliases nil
  "All aliases in `eicq-world'.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defvar eicq-add-user-p nil)
(defvar eicq-new-buddy nil)
(defvar eicq-new-uin nil)
(defvar fix-nick)

(defun eicq-add-user (uin)
  (interactive "sUIN: ")
  (setq eicq-new-uin uin)
  (setq eicq-add-user-p t)
  (eicq-search-by-uin uin))

(defvar eicq-world-rc-regexp
  "^:icq[ \t]+\\([0-9]+\\)[ \t]+\\([^:]+?\\)[ \t]?\\( :.*\\)*$"
  "*Regular expression for rc file.
Format: :icq uin alias group
Group is prefixed by a colon :.  Anything between uin and group including
white spaces is alias.  For example,

:icq 409533 fire :linux :eicq
:icq 123456 the hatter :unreal")

(defun eicq-add-new-user-to-buddy-buffer ()
  "Push the nick name from `eicq-add-user' into the buddy buffer.
Sort of a cut-down version or `eicq-world-update'"
  (add-to-list (symbol-value 'eicq-buddy-view) eicq-new-buddy)
  (set-extent-properties
   (make-extent 0 (length eicq-new-buddy) eicq-new-buddy)
   `(highlight t duplicable t start-open t keymap ,eicq-alias-map))
  (save-excursion
    (set-buffer (find-file-noselect eicq-world-rc-filename))
    (goto-char (point-max))
    (search-backward-regexp eicq-world-rc-regexp nil t)
    (let* ((buddy (list eicq-new-buddy eicq-new-uin 'rc-index (point))))
      (push buddy eicq-world)))
  (setq eicq-all-aliases (mapcar 'first eicq-world))
  (setq eicq-all-uin (mapcar 'second eicq-world))
  (eicq-buddy-show-buffer 'new 'no-select)
  (eicq-send
   (eicq-pack "\x3c\x05"
	      (eicq-uin-bin (car eicq-all-uin))))
  (setq eicq-add-user-p nil)
  (setq eicq-new-buddy nil)
  (setq eicq-new-uin nil))

;;; Code - group:

(defun eicq-group-put (group name)
  "Put something into GROUP.
NAME can be either an alias or another group name."
  (let ((list (assoc group eicq-world)))
    (cond
     (list
      (setcdr list (list (pushnew name (cadr list) :test 'equal))))
     (t
      (push (list group (list name)) eicq-world)))))

(defun eicq-group-get (group)
  "Get members from GROUP."
  (cadr (assoc group eicq-world)))

(defun eicq-group-get-all-aliases (group)
  "Recursively get all aliases from GROUP."
  (loop for x in (eicq-group-get group)
    as expanded-x = (eicq-group-get x)
    if (atom expanded-x) collect x
    else append (eicq-group-get-all-aliases x)))

(defun eicq-group-select-aliases (state &rest aliases)
  "Select aliases and update buddy buffer.
Nil STATE means deselect, 'toggle means invert current state, and other
non-nil means select.

See `eicq-process-alias-input'."
  (interactive '(select))
  (eicq-process-alias-input 'aliases)
  (loop for x in aliases
    do (if (eq state 'toggle)
             (setq state (not (eicq-world-getf x 'selected))))
         (eicq-world-putf x 'selected state)
         (eicq-buddy-update-face x)))

(defun eicq-world-getf (alias tag)
  "For ALIAS get property of TAG.
If TAG is 'all, return the plist."
  (let ((plist (cddr (assoc alias eicq-world))))
    (if (eq tag 'all)
        plist
      (getf plist tag))))

(defun eicq-world-putf (alias tag value)
  "For ALIAS put property of TAG with VALUE."
  (let* ((buddy (assoc alias eicq-world))
         (plist (cddr buddy)))
    (if buddy (setcdr (cdr buddy) (putf plist tag value)))))

(defun eicq-alias-uin (alias)
  "Return an uin from an ALIAS in `eicq-world'.
Return uin if ALIAS is already an uin.
Return 0 if no corresponding uin or invalid uin.
If called interactively, display and push uin into `kill-ring'."
  (interactive (eicq-completing-aliases "uin from alias: " 'single))
  (let ((uin (second (assoc alias eicq-world))))
    (setq uin
          (cond
           ((eicq-valid-uin-p alias) alias)
           ((eicq-valid-uin-p uin) uin)
           (t "0")))
    (when (interactive-p)
      (message uin)
      (kill-new uin))
    uin))

(defun eicq-uin-alias (uin)
  "Return an alias from an UIN in `eicq-world'.
Return UIN if no corresponding ALIAS.
If called interactively, display and push alias into `kill-ring'."
  (interactive (list (read-string "alias from uin: ")))
  (let ((alias (or (first (find uin eicq-world :key 'second :test 'string=))
                   ;; not found, return uin
                   uin)))
    (when (interactive-p)
      (message alias)
      (kill-new alias))
    alias))

;;;###autoload
(defun eicq-world-update ()
  "Read `eicq-world-rc-filename' and update various user variables.
Need to call this whenever RC is modified and to be updated.
RC file is not closed if it is the buffer of current window or it is modified."
  (interactive)
  (save-excursion
    (let (no-killing-at-last)
      (setq eicq-world nil)
      (set-buffer (find-file-noselect eicq-world-rc-filename))
      ;; don't kill if rc file is buffer in current window
      (setq no-killing-at-last
            (or (buffer-modified-p)
                (eq (window-buffer) (current-buffer))))
      (goto-char (point-min))
      (while (search-forward-regexp eicq-world-rc-regexp nil t)
        (let* ((uin (match-string 1))
               (alias (match-string 2))
               (group (match-string 3))
               buddy)

          ;; idea from Erik Arneson <erik@starseed.com>
          (set-extent-properties
           ;; We may consider moving to eicq-uin-alias or somewhere else, if
           ;; we don't want to waste enourmous unused extents.
           (make-extent 0 (length alias) alias)
           `(highlight t duplicable t start-open t keymap ,eicq-alias-map))

          (setq buddy (list alias uin 'rc-index (point)))

          ;; group stuff not used yet
          (if group
              (setq buddy
                    (append buddy (read (format "(group (%s))" group)))))
          (push buddy eicq-world)))
      (unless no-killing-at-last (kill-buffer (current-buffer)))))

  (setq eicq-all-aliases (mapcar 'first eicq-world))
  (setq eicq-all-uin (mapcar 'second eicq-world))
  (setq eicq-user-bin (eicq-alias-bin eicq-user-alias)))

(defun eicq-world-info (alias)
  "Return local info of buddy ALIAS."
  ;; TODO
  (assoc alias eicq-world))

(defun world-mode ()
  "eicq resource file mode.
Quick hack for font-lock. Each record is separated by \"==== \" at the
beginning of the line."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "world")
  (setq major-mode 'world-mode)
  (setq fill-column 100)
  (auto-fill-mode 1)
  ;; hiding details for privacy
  (outline-minor-mode)
  (set (make-local-variable 'outline-regexp)
       "==== ")

  ;; highlight this month and next month
  ;; format example: "^Aug 08, 1979$"
  (let* ((this-month (string-to-number (format-time-string "%m")))
         (this-month-name (aref eicq-monthnames this-month))
         (next-month-name (aref eicq-monthnames (1+ this-month)))
         (month-regexp (format "^%s.*\\|^%s.*"
                               this-month-name next-month-name)))
    (setq font-lock-keywords
          ;; highlight separator
          `(("^==== " (0 'font-lock-warning-face t))
            ;; highlight date
            (,month-regexp (0 'highlight t))
            ;; highlight keyword prefixed with :
            (":\\(\\w\\|-\\)+" 0 font-lock-reference-face t))))
  (font-lock-mode 1))

(defun world-sort ()
  (interactive)
  (beginning-of-buffer)
  (sort-subr nil 'world-next-friend 'world-end-friend))

(defun world-next-friend ()
  (interactive)
  (let ((result (search-forward "====" nil t)))
    ;; go back before ====
    (if result (backward-char 4)
      ;; required by sort-subr
      (end-of-buffer))))

(defun world-end-friend ()
  (interactive)
  ;; skip current friend
  (forward-char 1)
  (let ((result (search-forward "====" nil t)))
    ;; go back before ====
    (if result (backward-char 5)
      (end-of-buffer))))

(defun world-find (alias)
  "Goto a friend record of ALIAS in `eicq-world-rc-filename'.
Prefix argument means do not use (load) eicq completing alias feature."
  (interactive
   (if current-prefix-arg
       (list (read-string "find: "))
     (progn
       (require 'eicq)
       (eicq-completing-aliases "find: " 'single))))
  (find-file eicq-world-rc-filename)
  (goto-char (point-min))
  (re-search-forward
   (concat "^:icq.*?"
           (regexp-quote alias)
           "\\b.*$")))

(provide 'eicq-world)

;;; eicq-world.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
