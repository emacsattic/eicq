;;; eicq-status.el --- Status code for Eicq

;; Copyright (C) 2002,03 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <sryoungs@bigpond.net.au>
;; Maintainer:    Steve Youngs <sryoungs@bigpond.net.au>
;; Created:       2002-10-02
;; Last-Modified: <2003-10-15 09:27:33 (steve)>
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

(eval-and-compile
  (require 'eicq-comm)
  (require 'eicq-log)
  (require 'eicq-world)
  (require 'eicq-meta)
  (require 'eicq)
  (require 'wid-edit))

(eval-when-compile
  (defvar eicq-buddy-view))

(autoload 'eicq-buddy-update-face "eicq-buddy")


(defcustom eicq-buddy-status-color-hint-flag t
  "*Non-nil means put status color hints."
  :type 'boolean
  :group 'eicq-buddy)

;;;###autoload
(defcustom eicq-status-window-height 9
  "*Height of window for `eicq-status-buffer'."
  :group 'eicq-interface)

;;;###autoload
(defvar eicq-valid-statuses
  '("online" "away" "occ" "dnd" "ffc" "na" "invisible")
  "All statuses valid for selection.
Used by `eicq-change-status' and in `eicq-buddy-buffer'.")

;;;###autoload
(defcustom eicq-user-initial-status "invisible"
  "*Initial user status when login."
  :group 'eicq-option
  :type
  (cons 'choice
        (mapcar
         (lambda (x) (list 'item x))
         eicq-valid-statuses)))

(defcustom eicq-status-update-hook nil
  "*Hooks to run when a buddy change his status.
Dynamically ALIAS and STATUS are binded to be used in hooks."
  :group 'eicq-option
  :type 'hook)

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

;;; Internal variables

(defvar eicq-statuses
  ;; basically status is only ONE byte (except for invisible?)
  ;; byte after status byte is random
  '(("\x00" "online" eicq-face-online)
    ("\x01" "away" eicq-face-away eicq-auto-reply-away)
    ("\x04" "na99" eicq-face-na)
    ("\x05" "na" eicq-face-na eicq-auto-reply-na)
    ("\x10" "occ-mac" eicq-face-occ)
    ("\x11" "occ" eicq-face-occ eicq-auto-reply-occ)
    ("\x13" "dnd" eicq-face-dnd eicq-auto-reply-dnd)
    ("\x20" "ffc" eicq-face-ffc)
    ("\xff" "offline" eicq-face-offline)
    ("\x00\x01" "invisible" nil))       ; 2 bytes?
  "Status info: hex code, text code, face, auto-reply.")

(defun eicq-status-face (name)
  "Return the face of status from its NAME."
  (caddar
   (member* name eicq-statuses
            :key 'second
            :test 'string=)))

(defun eicq-status-bin (name)
  "Return the binary string of status from its NAME.
Zero-Padded to make it 4 byte-long."
  (substring
   (concat
    (caar
     (member* name eicq-statuses
              :key 'second
              :test 'string=))
    (if eicq-user-meta-web-aware
	"\x00\x01\x00"
      "\x00\x00\x00"))
   0 4))

(defun eicq-status-auto-reply (name)
  "Return the symbol of auto-reply of status from its NAME."
  (fourth (car
           (member* name eicq-statuses
            :key 'second
            :test 'string=))))

(defun eicq-status-idle-reply (name)
  "Return the symbol of auto-reply of status from its NAME."
  (fourth (car
           (member* name eicq-statuses
            :key 'second
            :test 'string=))))

(defun eicq-status-name (bin)
  "Return the name of status from its the binary string BIN."
  (cadr (assoc bin eicq-statuses)))

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

;;;###autoload
(defvar eicq-user-status "offline"
  "Current user status.")

(defun eicq-pack-status-change (status)
  "Pack STATUS change packet 04d8."
  (eicq-pack
   "\xd8\x04"
   (eicq-status-bin status)))

(defun eicq-do-status-update (packet)
  "Handle server command 01a4 in PACKET."
  (let ((alias (eicq-bin-alias packet 21))
        (status (eicq-status-name (substring packet 25 26))))
    (run-hooks 'eicq-status-update-hook)
    (eicq-buddy-update-status alias status)))

(defun eicq-change-status (status &optional no-network)
  "Change to new STATUS.
Non-nil NO-NETWORK means not to send any network packet, only update
variable and modeline."
  (interactive
   (list (eicq-completing-read "status: " eicq-valid-statuses nil t)))
  (unless (equal status eicq-user-status)
    (if eicq-user-auto-away-p
	(setq eicq-user-auto-away-p nil))
    (eicq-log-system "Changed status to %s" status)
    (setq eicq-user-status status)
    (redraw-modeline 'all)
    (unless no-network (eicq-send (eicq-pack-status-change status)))))

;;;###autoload
(defvar eicq-status-buffer nil
  "Buffer for statuses.")

;;;###autoload
(defun eicq-status-show-buffer (&optional new no-select)
  "Switch to `eicq-status-buffer'.
Create buffer if buffer does not exists already or
NEW is non-nil.
Don't select status window if NO-SELECT is non-nil."
  (interactive)
  (when (or (not (buffer-live-p eicq-status-buffer))
            new)
    (setq eicq-status-buffer (get-buffer-create "*Status*"))
    (set-buffer eicq-status-buffer)
    (and (fboundp 'set-specifier)
	 (set-specifier horizontal-scrollbar-visible-p nil 
			(cons (current-buffer) nil)))
    (and (fboundp 'set-specifier)
	 (set-specifier vertical-scrollbar-visible-p nil
			(cons (current-buffer) nil)))
    (erase-buffer)
    (set (make-local-variable 'widget-button-face) 'eicq-face-online)
    (widget-create 'link
		   :help-echo "Change status to \"Online\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "online"))
		   "Online")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'eicq-face-away)
    (widget-create 'link
		   :help-echo "Change status to \"Away\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "away"))
		   "Away")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'eicq-face-occ)
    (widget-create 'link
		   :help-echo "Change status to \"Occupied\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "occ"))
		   "Occupied")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'eicq-face-dnd)
    (widget-create 'link
		   :help-echo "Change status to \"Do Not Disturb\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "dnd"))
		   "Do Not Disturb")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'eicq-face-na)
    (widget-create 'link
		   :help-echo "Change status to \"Not Available\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "na"))
		   "Not Available")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'eicq-face-ffc)
    (widget-create 'link
		   :help-echo "Change status to \"Free For Chat\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "ffc"))
		   "Free For Chat")
    (widget-insert "\n")
    (set (make-local-variable 'widget-button-face) 'default)
    (widget-create 'link
		   :help-echo "Change status to \"Invisible\""
		   :action (lambda (&rest ignore)
			     (eicq-change-status "invisible"))
		   "Invisible")
    (toggle-read-only 1)
    (setq modeline-format "%b")
    (unless no-select
      (switch-to-buffer eicq-status-buffer))))



(provide 'eicq-status)

;;; eicq-status.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
