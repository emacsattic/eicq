;; eicq-comm.el --- Handle ICQ communications.

;; Copyright (C) 2002 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-04-10
;; Last-Modified: <2002-05-12 02:27:37 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

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

(defgroup eicq-network nil
  "All things networkish."
  :group 'eicq)

(defcustom eicq-network-buffer nil
  "Buffer for `eicq-network'.
Nil means no associated buffer, or no debug info."
  :group 'eicq-network)

(defcustom eicq-network-hostname "127.0.0.1"
  "*IP address of `eicq-network'.
See `eicq-connect'."
  :group 'eicq-network)

(defcustom eicq-network-port
  ;; plant random seed
  (progn (random t) nil)
  "*Port of `eicq-network'.
See `eicq-connect'."
  :group 'eicq-network)

(defcustom eicq-local-network-p t
  "If non-NIL, Eicq will look for a bridge running on a remote host defined
by `eicq-network-hostname' and `eicq-network-port'."
  :group 'eicq-network
  :type 'boolean)

(defcustom eicq-server-hostname "login.icq.com"
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

(defvar eicq-network nil
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

(defun eicq-encrypt-password ()
  "Encrypt `eicq-user-password' for login."
  (let ((pass (or (string-to-list eicq-user-password)
		  (string-to-list (read-passwd "Password: "))))
	(table eicq-pass-xor-table)
	(encrypted-pass))
    (while pass
      (setq encrypted-pass
	    (push (logxor (char-to-int (car pass)) (car table))
		  encrypted-pass))
      (setq table (cdr table))
      (setq pass (cdr pass)))
    (setq eicq-encrypted-password (nreverse encrypted-pass))))


(defun eicq-network-show-buffer ()
  "Switch to `eicq-bridge-buffer' for network dump info."
  (interactive)
  (switch-to-buffer eicq-network-buffer))

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
  (unless (or (eq (process-status eicq-network) 'open)
	      (not eicq-local-network-p))  ; remote bridge
    (setq eicq-network-hostname "127.0.0.1")
    (setq eicq-network-port (+ 4000 (random 1000)))
    (eicq-log-system
     "Trying to setup the connection to %s on port %s..."
     eicq-network-hostname eicq-network-port)
    (setq eicq-network-buffer (get-buffer-create "*eicq-network*"))
    (setq eicq-network
          (open-network-stream
           "eicq network"
           "*eicq-network*"
           eicq-server-hostname
           (number-to-string eicq-server-port)))
    (message "Starting up network...")
    ;; Wait for the connection.  I know 25 seconds is a long time, but
    ;; I'm on a dialup and sometimes it can be horrendously slow.
    (accept-process-output eicq-network 25))

;; Remote connections.  When you need to connect to ICQ through another
;; box.  Commented out for now because I have no way to test it at the
;; moment.  Any takers? :-)
;;
;;   (unless (and eicq-local-network-p
;;                (eicq-connected-p))
;;     (eicq-log-system
;;      "Trying to connect to the network at %s on port %s..."
;;      eicq-network-hostname eicq-network-port)
;;     (setq eicq-network
;;           (condition-case nil
;;               (open-network-stream
;;                "eicq network" nil eicq-network-hostname eicq-network-port)
;;             (file-error nil))))          ; eicq-network = nil if fails

  (cond
   ((and (eicq-connected-p)
         eicq-local-network-p)
    (set-process-sentinel eicq-network 'eicq-network-kill)
    (set-process-filter eicq-network 'eicq-network-filter)
    (with-current-buffer eicq-network-buffer
      (eicq-network-mode)))
   ((and (eicq-connected-p)
         (not eicq-local-network-p))
    (set-process-sentinel eicq-network 'eicq-network-kill)
    (set-process-filter eicq-network 'eicq-network-filter))
   (t
    (eicq-log-system "....connection failed"))))


(provide 'eicq-comm)
;;; eicq-comm.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
