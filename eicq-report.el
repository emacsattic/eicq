;;; eicq-report.el --- Generate a bug report   -*-Emacs-Lisp-*-

;; Copyright (C) 2001 Steve Youngs


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
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
(require 'gnus-util)
(require 'reporter)

(defvar reporter-version) ; For the byte-compiler

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

(defconst eicq-report-other-vars
  '(reporter-version
    features)
  "A list of variables that aren't generated below.")

(defconst eicq-report-avoid-vars
  '(eicq-user-password
    eicq-auto-away-timeout
    eicq-world-rc-regexp
    eicq-global-key-prefix)
  "A list of variables we don't want to see in the bug report.")

(defvar eicq-report-blurb nil)

;;; Functions

(defun eicq-report-get-versions ()
  "Return a list of eicq versions variables."
  (mapcar
   'intern
   (sort
    (let (completion-ignore-case)
      (all-completions
       "eicq-" obarray
       (function
	(lambda (sym)
	  (and (boundp sym)
	       (let ((name (symbol-name sym)))
		 (and (>= (length name) 8)
		      (string-equal (substring name -8) "-version"))))))))
    'string-lessp)))

(defun eicq-report-get-user-vars ()
  "Return a list of eicq user variables."
  (mapcar
   'intern
   (sort
    (let (completion-ignore-case)
      (all-completions "eicq-" obarray 'user-variable-p))
    'string-lessp)))

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

(defun eicq-gnus-submit-report ()
  "Mimic `reporter-submit-bug-report', but use Gnus."
  (unless (gnus-alive-p)
    (gnus))
  (gnus-group-mail 1)
  (message-goto-to)
  (insert eicq-bug-address)
  (message-goto-body)
  (forward-line 1)
  (setq after-sep-pos (point))
  (setq final-resting-place (point-marker))
  (insert "\n\n")
  (reporter-dump-state
   "eicq"
   vars
   nil nil)
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
Optional argument NO-CONFIRM if 't' will not ask for confirmation."
  (interactive)
  (let ((reporter-confirm-p nil)
	(reporter-prompt-for-summary-p nil)
	(reporter-package-abbrev "eicq"))
    ;; Look out for old reporter versions.
    (or (boundp 'reporter-version)
	(setq reporter-version
	      "Your version of reporter is obsolete.  Please upgrade."))
    (if (or no-confirm
	    (y-or-n-p "Do you want to submit a bug report on Eicq? "))
	(let ((eicq-report-blurb blurb)
	      (vars (nconc (eicq-report-get-versions)
			   (eicq-report-get-user-vars)
			   eicq-report-other-vars))
	      (avoids eicq-report-avoid-vars))
	  (while avoids
	    (setq vars (delq (car avoids) vars))
	    (setq avoids (cdr avoids)))
	  (if (featurep 'gnus)
	      (eicq-gnus-submit-report)
	    (reporter-submit-bug-report
	     eicq-bug-address
	     "eicq"
	     vars
	     (function eicq-report-pre-hook)
	     (function eicq-report-post-hook)
	     (aref eicq-report-salutations
		   (% (+ (% (random) 1000) 1000)
		      (length eicq-report-salutations)))))))))

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
  "Email comments or money to author."
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
