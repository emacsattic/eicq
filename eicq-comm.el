;; eicq-comm.el --- Handle ICQ communications.

;; Copyright (C) 2002 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-04-10
;; Last-Modified: <2003-09-06 14:10:37 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

;; This file is part of Eicq.

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

(eval-and-compile
  (require 'eicq-log)
  (require 'eicq-menu)
  (require 'eicq-v8proto))

(defgroup eicq-network nil
  "All things networkish."
  :group 'eicq)

(defcustom eicq-network-buffer "*eicq-network*"
  "Buffer for `eicq-network'.
Nil means no associated buffer, or no debug info."
  :group 'eicq-network)

;(defcustom eicq-server-hostname "login.icq.com"
;  "*Hostname or IP address of Mirabilis ICQ server."
;  :group 'eicq-option)

(defcustom eicq-server-hostname "localhost"
  "*Hostname or IP address of Mirabilis ICQ server."
  :group 'eicq-option)

(defcustom eicq-server-port 5190
  "*Port of Mirabilis ICQ server."
  :group 'eicq-option)

(defvar eicq-dropped-packet-counter 0
  "For debug purpose only.")

(defvar eicq-resend-packet-counter 0
  "For debug purpose only.")

(defvar eicq-recent-packet nil
  "The most recent incoming packet.
For debug only.")

(defvar eicq-trimmed-packet-counter 0
  "For debug purpose only.")

(defvar eicq-error-packets nil
  "A list of error incoming packets.
For debug only.")

;;; Internal variables
;;; Code - network:

(defcustom eicq-user-password nil
  "*Password for your ICQ account.
Nil means prompt for entering password every time you login."
 :group 'eicq-info)

(defconst eicq-network "eicq-network"
  "TCP network between XEmacs and ICQ.")

(defconst eicq-pass-xor-table
  (let ((hex-to-int 
	 (lambda (hex-symbol)
	   (string-to-int (substring (format "%s" hex-symbol) 2 4) 16))))
    (mapcar hex-to-int
	    '(0xF3 0x26 0x81 0xC4 0x39 0x86 0xDB 0x92 
		   0x71 0xA3 0xB9 0xE6 0x53 0x7A 0x95 0x7C)))
  "This table is used to \"encrypt\" the login password.

The encryption is done by xor'ing `eicq-user-password' against this
table.  Interestingly, this is the only thing in ICQv8 protocol that
gets encrypted.  Everything else is sent in clear text.")

(defvar eicq-encrypted-password nil
  "The encrypted version of `eicq-user-password'.")

(autoload 'eicq-int-byte "eicq")

;;; FIXME: This is wrong.  It is giving a binary string, but what we
;;; really want is a list of hex numbers, something like '(0xAE 0x5F
;;; 0x55 0x2E4 0xAE 0x5F 0x55 0x2E4)'
(defun eicq-encrypt-password ()
  "Encrypt `eicq-user-password' for login."
  (let ((pass (or (string-to-list eicq-user-password)
		  (string-to-list (read-passwd "Password: "))))
	(table eicq-pass-xor-table)
	encrypted-pass secret)
    (while pass
      (setq encrypted-pass
	    (push (logxor (char-to-int (car pass)) (car table))
		  encrypted-pass))
      (setq table (cdr table))
      (setq pass (cdr pass)))
    (setq encrypted-pass (nreverse encrypted-pass))
    (setq secret nil)
    (while encrypted-pass
      (setq secret (concat secret (eicq-int-byte (car encrypted-pass))))
      (setq encrypted-pass (cdr encrypted-pass)))
    (setq eicq-encrypted-password secret)))


(autoload 'eicq-logout "eicq")

(defun eicq-network-mode ()
  "Major mode for network debug output.
Commands: \\{eicq-main-mode}"
  (make-local-variable 'kill-buffer-hook)
  (kill-all-local-variables)
  (use-local-map eicq-main-map)
  (setq mode-name "eicq-network")
  (setq major-mode 'eicq-network-mode)
  (easy-menu-add eicq-main-easymenu)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list
   'kill-buffer-query-functions
   (lambda ()
     (eicq-logout 'kill)))
  (make-local-variable 'kill-buffer-hook)
  (add-hook
   'kill-buffer-hook
   (lambda () "Kill network buffer."
     (eicq-network-kill))))

(defun eicq-network-kill (&optional process change)
  "Kill `eicq-network'.
PROCESS and CHANGE is for `set-process-sentinel'."
  (if (processp eicq-network) 
      (delete-process eicq-network))
  (setq eicq-network nil))

(defun eicq-network-show-buffer ()
  "Switch to `eicq-bridge-buffer' for network dump info."
  (interactive)
  (switch-to-buffer eicq-network-buffer))

(defun eicq-connected-p ()
  "Return non-nil if the network is ready for sending string."
  (and (processp eicq-network)
       (or (eq (process-status eicq-network) 'open)
	   (eq (process-status eicq-network) 'run))))

(defmacro eicq-binary-process (&rest body)
  `(let (selective-display
	 (coding-system-for-read  'binary)
	 (coding-system-for-write 'binary))
     ,@body))

(defun eicq-connect ()
  "Make a connection to ICQ server.
It needs to make `eicq-bridge' and to make `eicq-network'.

A bridge can be running either internally or externally.  If it is running
external, `eicq-network' will connect to `eicq-bridge-hostname' at
`eicq-bridge-port'.  If it is running internally, `eicq-bridge-port' should
be set to nil; then `eicq-bridge-hostname' will be set to \"127.0.0.1\" and
`eicq-bridge-port' will be assigned a randomly port.

Running externally means no convenient debug network dump inside Emacs, but
this may allow central bridge servers in future."
  ;(setq eicq-network-buffer (get-buffer-create eicq-network-buffer))
  (eicq-binary-process
   (open-network-stream eicq-network
			eicq-network-buffer
			eicq-server-hostname
			(number-to-string eicq-server-port) 'tcp)) 
  (message "Starting up network..."))
;  (cond
;   ((and (eicq-connected-p)
;         eicq-local-network-p)
;    (set-process-sentinel eicq-network 'eicq-network-kill)
;    (set-process-filter eicq-network 'eicq-network-filter)
;   (with-current-buffer eicq-network-buffer
;     (eicq-network-mode))
  ;(set-process-buffer eicq-network eicq-network-buffer))
;   ((and (eicq-connected-p)
;         (not eicq-local-network-p))
;    (set-process-sentinel eicq-network 'eicq-network-kill)
;    (set-process-filter eicq-network 'eicq-network-filter))
;   (t
;    (eicq-log-system "....connection failed"))))


(provide 'eicq-comm)
;;; eicq-comm.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
