;;; eicq-report.el --- Generate a bug report   -*-Emacs-Lisp-*-

;; Copyright (C) 2001 Steve Youngs

;; RCS: $Id$
;; Author: Steve Youngs <youngs@xemacs.org>
;; Maintainer: Steve Youngs <youngs@xemacs.org>
;; Last-Modified: <2001-4-24 16:06:29 (steve)>
;; Keywords: bug-report

;; Eicq is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Eicq is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA


;;; Commentary:
;; To submit a bug report use: M-x eicq-report-bug
;; To send general comments/questions use: M-x eicq-email-author

;;; Code:
(require 'eicq)
(require 'gnus)

;; To keep the byte-compiler from spewing out warnings.
(defvar after-sep-pos)
(defvar final-resting-place)

;;; Variables

(defconst eicq-report-salutations
  ["Dear bug team:"
   "Ciao bug team:"
   "Salut bug team:"
   "Guten Tag bug team:"
   "To whom it may concern:"
   "Fellow eicq'ers:"
   "Yo bug team:"
   "G'day bug team:"
   "Greetings earthlings:"]
  "A list of salutations used for `eicq-report-bug'.")

(defvar eicq-bug-address
  "Eicq Bugs <eicq-bugs@lists.sourceforge.net>"
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

(defun eicq-prepare-report ()
  "Grabs the variables, features to include in bug report.
Then put it all into a mail buffer, nicely formatted."
  (message-goto-to)
  (insert eicq-bug-address)
  (message-goto-body)
  (forward-line 1)
  (setq after-sep-pos (point))
  (setq final-resting-place (point-marker))
  (insert "\n\n"
	  (emacs-version) "\n\n"
	  "Current State:\n==============\n"
	  (format "(setq\n eicq-version %s\n" eicq-version)
	  (format " eicq-world-rc-filename %s\n" (symbol-value
						  'eicq-world-rc-filename))
	  (format " eicq-log-filename %s\n" (symbol-value
					     'eicq-log-filename))
	  (format " eicq-sound-directory %s\n" (symbol-value 
						'eicq-sound-directory))
	  (format " eicq-bridge-filename %s\n" (symbol-value
						'eicq-bridge-filename))
	  (format " eicq-bridge-hostname %s\n" (symbol-value
						'eicq-bridge-hostname))
	  (format " eicq-bridge-port %s\n" (symbol-value 
					    'eicq-bridge-port))
	  (format " eicq-server-hostname %s\n" (symbol-value 
						'eicq-server-hostname))
	  (format " eicq-server-port %s\n" (symbol-value 
					    'eicq-server-port))
	  (format " eicq-dropped-packet-counter %s\n" (symbol-value
						       'eicq-dropped-packet-counter))
	  (format " eicq-resend-packet-counter %s\n" (symbol-value
						      'eicq-resend-packet-counter))
	  (format " eicq-trimmed-packet-counter %s\n" (symbol-value
						       'eicq-trimmed-packet-counter))
	  (format " eicq-error-packets %s\n" (symbol-value
					      'eicq-error-packets))
	  (format " eicq-buddy-view %s\n" (symbol-value
					   'eicq-buddy-view))
	  (format " eicq-delete-offline-messages-flag %s)\n" 
		  (symbol-value
		   'eicq-delete-offline-messages-flag))
	  (format "\nFeatures:\n\t%s" (symbol-value 'features)))
  (message "Formatting output so the bug team can read it.  Please wait...")
  (fill-paragraph t)
  (insert "\n\n")
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
  (insert "Steve Youngs <youngs@xemacs.org>")
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
;time-stamp-format: "%4y-%m-%d %02H:%02M:%02S (%u)"
;End: 
