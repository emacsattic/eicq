;;; eicq-buddy.el --- "Buddy" code for Eicq

;; Copyright (C) 2002, Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-10-01
;; Last-Modified: <2002-10-01 13:22:38 (steve)>
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
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defgroup eicq-buddy nil
  "Contact list preferences."
  :group 'eicq)

(defcustom eicq-buddy-window-width 20
  "*Width of window for `eicq-buddy-buffer'."
  :group 'eicq-interface)

(defcustom eicq-buddy-status-color-hint-flag t
  "*Non-nil means put status color hints."
  :type 'boolean
  :group 'eicq-buddy)

(defcustom eicq-buddy-view 
  'eicq-connected-aliases
  "*View of buddy buffer.
It determines what aliases to be display in buddy buffer.  For example,
\(eicq-connected-aliases) means display all connected aliases.

See `eicq-buddy-view-all', `eicq-buddy-view-connected', and
`eicq-buddy-view-active'."
  :group 'eicq-buddy
  :type '(choice (item eicq-all-aliases)
                 (item eicq-connected-aliases)
                 (item eicq-active-aliases))
  :initialize 'custom-initialize-default)

(defface eicq-face-online
  '((((background dark))
     (:foreground "green"))
    (((background light))
     (:foreground "green")))
  "Face for ONLINE status."
  :group 'eicq-buddy)

(defface eicq-face-away
  '((((background dark))
     (:foreground "red"))
    (((background light))
     (:foreground "red")))
  "Face for AWAY status."
  :group 'eicq-buddy)

(defface eicq-face-occ
  '((((background dark))
     (:foreground "orange"))
    (((background light))
     (:foreground "orange")))
  "Face for OCCUPIED status."
  :group 'eicq-buddy)

(defface eicq-face-dnd
  '((((background dark))
     (:foreground "lightblue"))
    (((background light))
     (:foreground "lightblue")))
  "Face for DO NOT DISTURB status."
  :group 'eicq-buddy)

(defface eicq-face-ffc
  '((((background dark))
     (:foreground "yellow"))
    (((background light))
     (:foreground "yellow")))
  "Face for FREE FOR CHAT status."
  :group 'eicq-buddy)

(defface eicq-face-na
  '((((background dark))
     (:foreground "pink"))
    (((background light))
     (:foreground "pink")))
  "Face for NOT AVAILABLE status."
  :group 'eicq-buddy)

(defface eicq-face-offline
  '((((background dark))
     (:foreground "grey"))
    (((background light))
     (:foreground "grey")))
  "Face for OFFLINE status."
  :group 'eicq-buddy)

(defface eicq-face-selected
  '((((background dark))
     (:foreground "darkblue" :background "yellow"))
    (((background light))
     (:foreground "darkblue" :background "yellow")))
  "Face for OFFLINE status."
  :group 'eicq-buddy)

;;; Internal variables

(defvar eicq-buddy-buffer nil
  "Buffer for contact list.")

(defun eicq-switch-to-buddy-buffer ()
  "Switches from the log buffer to the buddy buffer.
Needed so we can by-pass the status buffer."
  (interactive)
  (other-window 2))

(defadvice display-buffer
  (after eicq-buddy-avoid-window-select last activate)
  "Avoid setting buffer to small vertical window `eicq-buddy-buffer'.
When switching buffer in buddy window, other window in the frame is
used instead of buddy window, unless buddy window is the only window
in the frame.

To set buffer in buddy window explicitly, make buddy window the only window
in the frame by `one-window'."
  (unless (= (frame-width)
             (window-width (get-buffer-window buffer)))
    (delete-other-windows (get-buffer-window buffer))))

(defun eicq-buddy-mode ()
  "Major mode for contact list in eicq.
Commands: \\{eicq-buddy-mode-map}

Turning on `eicq-buddy-mode' runs the hook `eicq-buddy-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eicq-buddy-mode-map)
  (setq mode-name "eicq-buddy")
  (setq major-mode 'eicq-buddy-mode)
  ;; put easy-menu-add after set mode-name
  (easy-menu-add eicq-main-easymenu)
  (easy-menu-add eicq-buddy-menu)
  (easy-menu-add eicq-log-menu)
  (setq modeline-format "%b")

  (run-hooks 'eicq-buddy-mode-hook))

(defun eicq-buddy-view-set (&optional symbol value)
  "Set `eicq-buddy-view'."
  (set-default symbol value)
  (eicq-buddy-show-buffer 'new 'no-select))

(defun eicq-buddy-show-buffer (&optional new no-select)
  "Switch to `eicq-buddy-buffer'.
Create buffer if buffer does not exists already or
NEW is non-nil.
Don't select buddy window if NO-SELECT is non-nil.
See `eicq-buddy-view' and `eicq-buddy-status-color-hint-flag'."
  (interactive)
  (when (or (not (buffer-live-p eicq-buddy-buffer))
            new)
    (setq eicq-buddy-buffer (get-buffer-create "*eicq buddy*"))
    (set-buffer eicq-buddy-buffer)
    (erase-buffer)
    (loop for alias in (symbol-value eicq-buddy-view)
      as status = (eicq-world-getf alias 'status)
      as face = (eicq-status-face status)
      do (insert-face (concat alias "\n") face))
    (eicq-buddy-mode))
  (unless no-select
    (switch-to-buffer eicq-buddy-buffer)))

(defun eicq-buddy-view-all ()
  "Display all aliases in `eicq-world'.
See `eicq-buddy-view'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-all-aliases))

(defun eicq-buddy-view-connected ()
  "Display all connected aliases.
See `eicq-buddy-view' and `eicq-connected-aliases'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-connected-aliases))

(defun eicq-buddy-view-active ()
  "Display all active aliases.
See `eicq-buddy-view' and `eicq-active-aliases'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-active-aliases))

(defun eicq-buddy-update-status (alias status)
  "Update ALIAS with new STATUS."
  ;; update alias variables
  (unless (member status (mapcar 'second eicq-statuses))
    (push (cons 'unknown-status eicq-recent-packet)
          eicq-error-packets)
    (eicq-log-error "Unknown status: %s" status)
    (setq status "online"))             ; assumed online

  (unless (equal status (eicq-world-getf alias 'status))
    (eicq-log-buddy-status alias "*** %s" status)
    (eicq-world-putf alias 'status status)
    (if (string= status "offline")
	(if (member alias eicq-connected-aliases)
	    (setq eicq-connected-aliases
		  (delete alias eicq-connected-aliases))
	  (eicq-log-buddy-status alias "*** has been invisible"))
      ;; if not offline
      (add-to-list 'eicq-connected-aliases alias))

    ;; update buffer

    ;; view != all + offline -> delete
    ;; view = all + offline -> offline-face
    (if (and (string= status "offline")
	     (not (eq eicq-buddy-view 'eicq-all-aliases)))
	(eicq-buddy-update-face alias 'delete)
      (if (or (member alias (symbol-value eicq-buddy-view))
	      (string= status "offline"))
	  (eicq-buddy-update-face alias)))))

(defun eicq-buddy-update-face (alias &optional delete)
  "Update face of ALIAS.
Non-nil DELETE means delete alias from buffer."
  (save-excursion
    (when (buffer-live-p eicq-buddy-buffer)
      (set-buffer eicq-buddy-buffer)
      (goto-char (point-min))

      (if (search-forward-regexp
           ;; use "^" alias "$" so searching "foo" will not get "foobar"
           (concat "^"
                   ;; to allow funny characters in alias
                   (regexp-quote alias)
                   "$")
           nil t)
          ;; old alias
          (if delete
              (delete-region
               (point-at-bol)
               ;; take care of last line
               (min (1+ (point-at-eol)) (point-max))))
        ;; new alias
        (unless delete
          (insert alias "\n")
          (forward-line -1)))

      (unless delete
        (put-text-property
         ;; from the end of last line to the beginning of next line instead
         ;; of just bol and eol of current line, to make sure covering
         ;; highlight by selected; start-open and end-open do not seem to fix
         (max (1- (point-at-bol)) (point-min))
         ;; take care of last line
         (min (1+ (point-at-eol)) (point-max))
         'face (eicq-status-face (eicq-world-getf alias 'status)))

        (when (eicq-world-getf alias 'selected)
          ;; highlight first char
          (put-text-property
           (+ 0 (point-at-bol)) (+ 1 (point-at-bol))
           'face 'eicq-face-selected))))))

(defun eicq-buddy-select-all-in-view (state &optional predicate)
  "Select all aliases in current view.
See `eicq-group-select-aliases' for STATE.
PREDICATE accepts an alias as an argument and limits the application.
Current view is `eicq-buddy-view'."
  (loop for x in (symbol-value eicq-buddy-view)
    if (or (null predicate)
           (funcall predicate x))
    do (eicq-group-select-aliases state x)))

(defun eicq-buddy-select-all-in-view-by-status (status)
  "Toggle selections of all aliases with STATUS in current view."
  (interactive
   (list (eicq-completing-read "status: " eicq-valid-statuses)))
  (eicq-buddy-select-all-in-view
   'toggle
   (lambda (x)
     (equal (eicq-world-getf x 'status) status))))

(defun eicq-buddy-select-all-in-view-by-regexp (regexp)
  "Toggle selections of all aliases matching REGEXP in current view."
  ;; checked my screenshots? know why i use a symbol prefix now?
  (interactive "sregexp: ")
  (eicq-buddy-select-all-in-view
   'toggle
   (lambda (x)
     (string-match regexp x))))

(defun eicq-buddy-selected-in-view ()
  "Return a list of all selected aliases in current view.
Selected means an alias has non-nil 'selected property.
Current view is `eicq-buddy-view'."
  (loop for x in (symbol-value eicq-buddy-view)
    if (eicq-world-getf x 'selected)
    collect x))

(provide 'eicq-buddy)

;;; eicq-buddy.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
