;;; eicq-log.el --- Logging code for Eicq.

;; Copyright (C) 2002,03,04 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <steve@youngs.au.com>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Created:       2002-10-01
;; Last-Modified: <2004-05-30 11:13:42 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

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

;;; Commentary:
;;
;; Logs incoming and outgoing events in Eicq.

(eval-and-compile
  (require 'eicq-menu)
  (require 'outline))

(eval-when-compile
  (require 'smiley)
  (require 'goto-addr))

(defgroup eicq-log nil
  "Message logging preferences."
  :group 'eicq)

(defcustom eicq-log-fill-column 50
  "Fill column for `eicq-log-buffer'.
Log in buffer is auto-filled, that is, word-wrapped upto this column.
Normally frame width is 80 and window width of `eicq-buddy-buffer' is 20,
therefore default value 50 will be nice."
  :group 'eicq-log)

(defcustom eicq-log-filename "~/.eicq/log"
  "*Pathname and filename for storing eicq log.
Automatically created if the directory is non-existent."
  :group 'eicq-log)

(defcustom eicq-log-buffer-position-flag 'tail
  "*Non-nil means automatically updating buffer position.
Nil means no automatic update, 'tail means keeping the bottom of the buffer 
visible, other non-nil means keeping the top of the buffer visible."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))  

(defcustom eicq-log-info-flag 'tail
  "*Non-nil means log misc info.
These include any info from ICQ server other than buddy messages, status
change notice, and query results.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-buddy-status-flag 'tail
  "*Non-nil means log buddy status change notice.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-buddy-message-flag 'tail
  "*Non-nil means log buddy messages from ICQ server.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-outgoing-flag 'tail
  "*Non-nil means log outgoing messages to ICQ server.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-error-flag 'tail
  "*Non-nil means log critical error messages.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-debug-flag nil
  "*Non-nil means log verbose debugging messages.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-system-flag 'tail
  "*Non-nil means log system messages.
These include network status, login status, and others.
Nil means no log, 'tail means putting new log at the end of the log
buffer, other non-nil means putting new log at the beginning."
  :group 'eicq-log
  :type '(choice (item t) (item tail) (item nil)))

(defcustom eicq-log-info-mark nil
  "*Non-nil means mark unread.
These include any info from ICQ server other than buddy messages, 
status change notice, and query results.
Nil means mark read."
  :group 'eicq-log
  :type 'boolean)

(defcustom eicq-log-buddy-status-mark nil
  "*Non-nil means mark buddy status change notice unread.
Nil means mark read."
  :type 'boolean
  :group 'eicq-log)

(defcustom eicq-log-buddy-message-mark t
  "*Non-nil means mark buddy messages unread.
Nil means mark read."
  :type 'boolean
  :group 'eicq-log)

(defcustom eicq-log-outgoing-mark nil
  "*Non-nil means mark outgoing messages unread.
Nil means mark read."
  :group 'eicq-log
  :type 'boolean)

(defcustom eicq-log-error-mark t
  "*Non-nil means mark critical error messages unread.
Nil means mark read."
  :group 'eicq-log
  :type 'boolean)

(defcustom eicq-log-debug-mark t
  "*Non-nil means mark verbose debugging messages unread.
Nil means mark read."
  :group 'eicq-log
  :type 'boolean)

(defcustom eicq-log-system-mark nil
  "*Non-nil means mark system messages unread.
Nil means mark read."
  :group 'eicq-log
  :type 'boolean)

(defcustom eicq-save-log-on-exit-p t
  "*Non-nil means the log file will be automatically saved when exiting."
  :group 'eicq-log
  :type 'boolean
  :tag "Save log on exit")

(defcustom eicq-smiley nil
  "*Non-nil means smileys are enabled."
  :group 'eicq-interface
  :type '(choice (item t) (item nil)))

;;; Internal variables

(defvar eicq-log-buffer nil
  "Buffer for log.")

(defvar eicq-log-outline-regexp "^...:.. "
  "Regexp for log header.
See `outline-regexp'.")

(defun eicq-log-mode ()
  "Major mode for logging messages in eicq.
Commands: \\{eicq-log-mode-map}

Turning on `eicq-log-mode' runs the hook `eicq-log-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eicq-log-mode-map)
  (setq mode-name "eicq-log")
  (setq major-mode 'eicq-log-mode)
  ;; put easy-menu-add after set mode-name
  (easy-menu-add eicq-main-easymenu)
  (easy-menu-add eicq-buddy-menu)
  (easy-menu-add eicq-log-menu)
  (setq fill-column eicq-log-fill-column)

  ;; HACK no menubar from outline mode
  (let ((features (remove 'menubar features)))
    (outline-minor-mode))

  (set (make-local-variable 'outline-regexp)
       eicq-log-outline-regexp)         ; any better?

  (setq modeline-format
        (list "[" 'eicq-user-alias
              "] *** " 'eicq-user-status))

  (run-hooks 'eicq-log-mode-hook))

;;;###autoload
(defun eicq-log-show-buffer (&optional new no-select)
  "Switch to `eicq-log-buffer'.
Create buffer with log file if buffer does not exists already.
Non-nil NEW means rotate and create a new log file.
Non-nil NO-SELECT means don't select log window.
See `eicq-log-filename'."
  (interactive)
  (when new
    ;; save and close current log first if any
    (setq eicq-log-buffer
          (find-buffer-visiting eicq-log-filename))

    (if eicq-log-buffer
        (with-current-buffer eicq-log-buffer
          (save-buffer)
          (kill-buffer nil)))

    ;; rename old log in disk
    (if (file-exists-p eicq-log-filename)
        (rename-file
         eicq-log-filename
         (concat eicq-log-filename
                 ;; in case you do something stupid with it
                 (format-time-string "-%Y-%b%d-%H%M-%S")))))

  (unless (buffer-live-p eicq-log-buffer)
    (setq eicq-log-buffer (find-file-noselect eicq-log-filename))
    (with-current-buffer eicq-log-buffer
      (eicq-log-mode)
      (if (zerop (buffer-size))
          (insert "=======================================\n"
		  "Welcome to Eicq - The XEmacs ICQ Client\n\n"
		  "If you experience problems, please use:\n"
		  "  \'M-x eicq-report-bug RET\'\n"
		  "=======================================\n\n"))))
  (unless no-select
    (switch-to-buffer eicq-log-buffer)))

(defun eicq-log-new-file ()
  "Rotate and create a new log file."
  (interactive)
  (eicq-log-show-buffer 'new))

(defconst eicq-log-entry-re "^[SMTWRFA][0-9][0-9]:[0-9][0-9]"
  "Regular expression matching the beginning of a log entry.")

(defun eicq-log (id message option mark-unread)
  "Log message under ID.
Put MESSAGE at the end of log buffer if OPTION is non-nil.
Mark MESSAGE unread if MARK-UNREAD is non-nil"
  (if (and option (buffer-live-p eicq-log-buffer))
      (with-current-buffer eicq-log-buffer
        (save-excursion
          (let ((start-point (if (eq option 'tail)
                                 (point-max) (point-min)))
                (weekday ["S" "M" "T" "W" "R" "F" "A"])
                ;; to fill messages correctly
                (paragraph-start ""))
            (goto-char start-point)
            (insert
             (aref weekday (string-to-number (format-time-string "%w")))
             (format-time-string "%R ")
             ;; use concat instead of format for extent
             (concat "[" id "] " message "\n\n"))
            (fill-region start-point (point))
            (goto-char start-point)
	    (if mark-unread
		(eicq-log-mark-unread)
	      (eicq-log-mark-read))))
	(if eicq-log-buffer-position-flag
	    (if (eq eicq-log-buffer-position-flag 'tail)
		(progn
		  (goto-char (point-max))
		  (re-search-backward eicq-log-entry-re))
	      (progn 
		(goto-char (point-min))
		(re-search-forward eicq-log-entry-re)))))))

(defun eicq-log-info (&rest messages)
  "See `eicq-log-info-flag'.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log "!info" (apply 'format messages) eicq-log-info-flag eicq-log-info-mark))

(defun eicq-log-buddy-status (alias &rest messages)
  "See `eicq-log-buddy-status-flag'.
ALIAS is an id to be logged under.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log alias 
	    (apply 'format messages) 
	    eicq-log-buddy-status-flag eicq-log-buddy-status-mark))

(defun eicq-log-buddy-message (alias &rest messages)
  "See `eicq-log-buddy-message-flag'.
ALIAS is an id to be logged under.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log alias 
	    (apply 'format messages) 
	    eicq-log-buddy-message-flag eicq-log-buddy-message-mark)
  (save-excursion
    (set-buffer eicq-log-buffer)
    (if eicq-smiley
        (smiley-buffer))
    (goto-address)))

(defun eicq-log-buddy-url (alias message url)
  "See `eicq-log-buddy-message-flag'.
ALIAS is an id MESSAGE to be logged under.
URL will be highlighted."
  ;; idea from Erik Arneson <erik@starseed.com>
  (set-extent-properties
   (make-extent 0 (length url) url)
   `(highlight t duplicable t keymap ,eicq-url-map))
  (eicq-log alias (concat message "\nURL: " url)
            eicq-log-buddy-message-flag eicq-log-buddy-message-mark))

(defun eicq-log-outgoing (alias &rest messages)
  "See `eicq-log-outgoing-flag'.
ALIAS is an id to be logged under.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log alias 
	    (apply 'format messages) eicq-log-outgoing-flag eicq-log-outgoing-mark))

(defun eicq-log-error (&rest messages)
  "See `eicq-log-error-flag'.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log "!error" 
	    (apply 'format messages) eicq-log-error-flag eicq-log-error-mark))

(defun eicq-log-debug (&rest messages)
  "See `eicq-log-debug-flag'.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log "!debug" 
	    (apply 'format messages) eicq-log-debug-flag eicq-log-debug-mark))

(defun eicq-log-system (&rest messages)
    "See `eicq-log-system-flag'.
MESSAGES is an argument list for `format' to be inserted."
  (eicq-log "!system" 
	    (apply 'format messages) eicq-log-system-flag eicq-log-system-mark))

(defface eicq-face-log-unread
  '((((background dark))
     (:foreground "red"))
    (((background light))
     (:foreground "red")))
  "Face for unread log messages."
  :group 'eicq-log)

(defface eicq-face-log-read
  '((((background dark))
     (:foreground "turquoise"))
    (((background light))
     (:foreground "turquoise")))
  "Face for read log messages."
  :group 'eicq-log)

(defvar eicq-log-mark-alist
  '((unread . eicq-face-log-unread)
    (read . eicq-face-log-read))
  "Alist of log message marks and their colors.")

(defun eicq-log-mark (&optional mark)
  "Mark log message around point using MARK.
Possible MARK: 'read, 'unread, 'toggle.
Nil MARK means 'read.
See `eicq-face-log-unread' and `eicq-face-log-read'."
  (save-excursion
    ;; so that we can mark current line even at bol
    (end-of-line)
    (let ((len (length eicq-log-outline-regexp))
          (p (search-backward-regexp eicq-log-outline-regexp nil t))
          (face (cdr (assoc mark eicq-log-mark-alist))))
      (if p (add-text-properties
             p (+ len p -2)
             (list 'face face 'start-open t))))))

(defun eicq-log-mark-region (start end &optional mark)
  "Mark all log messages in the region.
MARK is any mark in `eicq-log-mark'."
  (interactive "r")
  (save-excursion
    (goto-char start)
    ;; Due to bad design of outline.el, we use condition-case to guard
    ;; against error when advancing at the end of buffer.
    (condition-case nil
        (while (<= (point) end)
          (eicq-log-mark mark)
          (eicq-log-next 1))
      (error nil))))

(defun eicq-log-mark-unread (&optional mark-region)
  "Mark log message around point as unread.
Non-nil MARK-REGION or prefix argument means marks all log in the region."
  (interactive "P")
  (if mark-region
      (eicq-log-mark-region (region-beginning) (region-end) 'unread)
    (eicq-log-mark 'unread)))

(defun eicq-log-mark-read (&optional mark-region)
  "Mark log message around point as read.
Non-nil MARK-REGION or prefix argument means marks all log in the region."
  (interactive "P")
  (if mark-region
      (eicq-log-mark-region (region-beginning) (region-end) 'read)
    (eicq-log-mark 'read))
  (if (interactive-p)
      (run-hooks 'eicq-read-message-hook)))

(defun eicq-log-around ()
  "Return the log message around.
If called interactively, display and push log into `kill-ring'."
  (interactive)
  (save-excursion
    (let ((log (buffer-substring
                (progn (outline-back-to-heading)
                       (search-forward "] " nil t)
                       (point))
                (progn (eicq-log-next 1)
                       (point)))))
      (when (interactive-p)
        (message log)
        (kill-new log))
      log)))

(defalias 'eicq-log-contract 'hide-subtree)
(defalias 'eicq-log-expand 'show-subtree)
(defalias 'eicq-log-previous 'outline-backward-same-level)
(defalias 'eicq-log-next 'outline-forward-same-level)

(defun eicq-log-next-unread ()
  "Moves point to the next unread message.  
Does nothing if there are no unread messages after point."
  (interactive)
  (let ((here (point)))
    (goto-char 
     (catch 'where
       (progn 
	 (while (not (eq here (point-max))) ; mildly bogus target
	   (let ((next (next-single-property-change here 'face)))
	     (unless next
	       (throw 'where (point)))
	     (if (eq (get-text-property next 'face)
		     (cdr (assoc 'unread eicq-log-mark-alist)))
		 (throw 'where next)
	       (setq here next)))))))))

(defun eicq-log-previous-unread ()
  "Moves point to the previous unread message.  
Does nothing if there are no unread messages after point."
  (interactive)
  (let ((here (point)))
    (goto-char 
     (catch 'where
       (progn 
	 (while (not (eq here (point-max))) ; mildly bogus target
	   (let ((prev (previous-single-property-change here 'face)))
	     (unless prev
	       (throw 'where (point)))
	     (if (eq (get-text-property prev 'face)
		     (cdr (assoc 'unread eicq-log-mark-alist)))
		 (throw 'where prev)
	       (setq here prev)))))))))

(provide 'eicq-log)

;;; eicq-log.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
