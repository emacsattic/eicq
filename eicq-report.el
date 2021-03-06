;;; eicq-report.el --- Generate a bug report   -*-Emacs-Lisp-*-

;; Copyright (C) 2001,03,04 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <steve@youngs.au.com>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Last-Modified: <2004-05-30 11:14:42 (steve)>
;; Keywords:      bug-report

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
;; To submit a bug report use: M-x eicq-report-bug
;; To send general comments/questions use: M-x eicq-email-author

;;; Code:

(autoload 'eicq-version "eicq" 
  "Version of eicq you are currently using." t nil)
(require 'sendmail)
(require 'shadow)
(require 'gnus-msg)
(require 'gnus-util)
(require 'message)

;; To keep the byte-compiler from spewing out warnings.
(eval-when-compile
  (defvar after-sep-pos)
  (defvar final-resting-place)
  (defvar eicq-version)
  (require 'font-lock)
  (require 'yow)
  (require 'pp))


;;; Variables

(defcustom eicq-report-bug-send-init nil
  "*If non-nil, include the user's init.el file in the bug report."
  :group 'eicq-option
  :type 'boolean)

;;; Internal variables

(defconst eicq-report-salutations
  ["Dear bug team:"
   "Ciao bug team:"
   "Salut bug team:"
   "Guten Tag bug team:"
   "To whom it may concern:"
   "Fellow Eicq'ers:"
   "Yo bug team:"
   "G'day bug team:"
   "Greetings Earthlings:"]
  "A list of salutations used for `eicq-report-bug'.")

(defvar eicq-bug-address
  "Eicq Bugs <eicq-bugs@lists.sf.net>"
  "The address used for submitting bug reports.")

(defvar eicq-report-blurb nil)

;;; Functions

(defun eicq-report-pre-hook ()
  "Pre hook run by report-submit-bug-report."
  (message-goto-subject)
  (insert
   (format "Eicq v%s bug: " eicq-version))
  (if eicq-report-blurb
      (progn
	(mail-text)
	(insert "\n" eicq-report-blurb "\n"))))

(defun eicq-report-post-hook ()
  "Post hook run by report-submit-bug-report."
  (save-excursion
    (message-goto-subject)
    (font-lock-fontify-buffer)
    (let ((subj (read-string "Subject header: ")))
      (if (string-equal subj "")
	  (subst-char-in-region
	   (point)
	   (progn
	     (insert
	      (if (or (fboundp 'yow) (load "yow" t t)) (yow) ""))
	     (point))
	   ?\n ?\ )
	(insert subj)))))

;; Stolen from Gnus.
(defun eicq-report-debug ()
  "Go through the Eicq source files and report what variables have been changed.
The source file has to be in the load path."
  (interactive)
  (let ((files '("eicq-buddy.el" "eicq-comm.el" "eicq-log.el" 
		 "eicq-menu.el" "eicq-meta.el" "eicq-report.el" 
		 "eicq-status.el" "eicq-toolbar.el" "eicq-wharf.el" 
		 "eicq-world.el" "eicq.el"))
	(point (point))
	file expr olist sym)
    (message "Please wait while we snoop your variables...")
    (sit-for 0)
    ;; Go through all the files looking for non-default values for variables.
    (save-excursion
      (set-buffer (get-buffer-create " *eicq bug info*"))
      (while files
	(erase-buffer)
	(when (and (setq file (locate-library (pop files)))
		   (file-exists-p file))
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (if (not (re-search-forward "^;;* *Internal variables" nil t))
	      (message "Malformed sources in file %s" file)
	    (narrow-to-region (point-min) (point))
	    (goto-char (point-min))
	    (while (setq expr (ignore-errors (read (current-buffer))))
	      (ignore-errors
		(and (or (eq (car expr) 'defvar)
			 (eq (car expr) 'defcustom))
		     (stringp (nth 3 expr))
		     (or (not (boundp (nth 1 expr)))
			 (not (equal (eval (nth 2 expr))
				     (symbol-value (nth 1 expr)))))
		     (push (nth 1 expr) olist)))))))
      (kill-buffer (current-buffer)))
    (when (setq olist (nreverse olist))
      (insert "\n"))
    (while olist
      (if (boundp (car olist))
	  (condition-case ()
	      (pp `(setq ,(car olist)
			 ,(if (or (consp (setq sym (symbol-value (car olist))))
				  (and (symbolp sym)
				       (not (or (eq sym nil)
						(eq sym t)))))
			      (list 'quote (symbol-value (car olist)))
			    (symbol-value (car olist))))
		  (current-buffer))
	    (error
	     (format "(setq %s 'whatever)\n" (car olist))))
	(insert ";; (makeunbound '" (symbol-name (car olist)) ")\n"))
      (setq olist (cdr olist)))
    (insert "\n\n\n")
    ;; Remove any control chars - they seem to cause trouble for some
    ;; mailers.  (Byte-compiled output from the stuff above.)
    (goto-char point)
    (while (re-search-forward "[\000-\010\013-\037\200-\237]" nil t)
      (replace-match (format "\\%03o" (string-to-char (match-string 0)))
		     t t))))

(defun eicq-prepare-report ()
  "Grabs the variables, features to include in bug report.
Then put it all into a mail buffer, nicely formatted."
  (message-goto-to)
  (insert eicq-bug-address)
  (message-goto-body)
  (forward-line 1)
  (setq after-sep-pos (point))
  (setq final-resting-place (point-marker))
  (insert 
   "\n\n"
   "===============================================================\n"
   "System info to help the Eicq boys and girls try to fix your bug:\n"
   "==============================================================="
   "\n\n")
  (eicq-version 1)
  ;; Insert the output of 'describe-installation'.
  (insert "\n\n"
	  (symbol-value 'Installation-string))
  ;; Load-path shadows can cause some grief.
  (flet ((append-message
	   (&rest args) ())
	 (clear-message
	   (&optional label frame stdout-p no-restore)
	   ()))
    (insert "\n\nLoad-Path Lisp Shadows:\n"
	    "----------------------\n")
    (let ((before-shadows (point)))
      (insert
	(format "%s"
		(find-emacs-lisp-shadows load-path)))
      (save-restriction
	(narrow-to-region before-shadows (point))
	(fill-paragraph t)
	(insert "\n"))))
  ;; Insert a list of installed packages.
  (insert "\n\nInstalled XEmacs Packages:\n"
	  "-------------------------\n")
  (cl-prettyprint
   (symbol-value 'packages-package-list))
  (insert "\n")
  ;; Insert a list of loaded features
  (let ((before-features (point)))
    (insert
     (format "\n\nFeatures:\n--------\n\n%s" (symbol-value 'features)))
    (save-restriction
      (narrow-to-region before-features (point))
      (fill-paragraph t)
      (insert "\n\n")))
  ;; Insert the contents of the user's init file if it exists 
  ;; and the user wants it sent.
  (if eicq-report-bug-send-init
      (if (file-readable-p user-init-file)
	  (save-excursion
	    (message-goto-signature)
	    (forward-line -3)
	    (beginning-of-line)
	    (insert "\n\nUser Init File:\n--------------\n\n")
	    (insert-file-contents user-init-file))))
  ;; Insert all the Eicq vars that have been changed from default
  (save-excursion
    (message-goto-signature)
    (forward-line -3)
    (beginning-of-line)
    (insert "\n\nEicq variables of note:\n----------------------\n")
    (eicq-report-debug))
  (eicq-report-pre-hook)
  (eicq-report-post-hook)
  (mail-text)
  (insert
   (aref eicq-report-salutations
	 (% (+ (% (random) 1000) 1000)
	    (length eicq-report-salutations))) "\n")
  (goto-char final-resting-place)
  (set-marker final-resting-place nil)
  (message "Please enter your report.  Type C-c C-c to send, C-x k to abort."))

;;;###autoload
(defun eicq-report-bug (&optional blurb no-confirm)
  "Submit a bug report for eicq.
Optional argument BLURB is a string that adds a preamble to the bug report.
Optional argument NO-CONFIRM if 't' will not ask for confirmation.

If you have Gnus it will be used, otherwise the standard XEmacs mail
command is used.

Yes, it's all part of a secret plot to make more people use 
the MUA of Gods.  Bwahahaha."
  (interactive)
  (if (or no-confirm
	  (y-or-n-p "Do you want to submit a bug report on Eicq? "))
      (progn
	(setq eicq-report-blurb blurb)
	(if (featurep 'gnus)
	    (progn
	      (unless (gnus-alive-p)
		(gnus))
	      (gnus-group-mail 1)
	      (eicq-prepare-report))
	  (mail)
	  (eicq-prepare-report)))))

;;; email-author code

(defconst eicq-email-salutations
  ["Dear Steve,"
   "Ciao Steve,"
   "Guten Tag Steve,"
   "To whom it may concern:"
   "Bonjour Steve,"
   "Yo! Eicq Dude!"
   "G'day Steve,"
   "Hey Man,"
   "Greetings Earthlings:"]
  "A list of salutations used for `eicq-email-author'.")

(defun eicq-prepare-email-author ()
  "Prepare the mail buffer for `eicq-email-author'."
  (message-goto-to)
  (insert "Steve Youngs <steve@youngs.au.com>")
  (message-goto-cc)
  (insert "Eicq-Users <eicq-users@lists.sourceforge.net>")
  (message-goto-subject)
  (insert
   (format "Eicq v%s: " eicq-version))
  (let ((subj (read-string "Subject header: ")))
    (if (string-equal subj "")
	(subst-char-in-region
	 (point)
	 (progn
	   (insert
	    (if (or (fboundp 'yow) (load "yow" t t)) (yow) ""))
	   (point))
	 ?\n ?\ )
      (insert subj)))
  (message-goto-body)
  (insert
   ";; Bug reports, feature requests, patches, thank-you cards... are all welcome.")
  (insert "\n;; Flammage is automagically > /dev/null.\n")
  (insert ";; *** I don't need to read these 3 lines, please delete them. ***\n\n")
  (insert
   (aref eicq-email-salutations
	 (% (+ (% (random) 1000) 1000)
	    (length eicq-email-salutations))) "\n\n\n")
  (forward-line -1))

;;;###autoload
(defun eicq-email-author ()
  "Email comments or money to author.

Uses Gnus if available, otherwise standard mail command."
  (interactive)
  (if (y-or-n-p "Do you want to send comments to the Eicq author? ")
      (progn
	(if (featurep 'gnus)
	    (progn
	      (unless (gnus-alive-p)
		(gnus))
	      (gnus-group-mail 1)
	      (eicq-prepare-email-author))
	  (mail)
	  (eicq-prepare-email-author)))))

(provide 'eicq-report)

;;; eicq-report.el ends here


;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
