;; eicq-toolbar.el   -*-Emacs-Lisp-*-
;; $Id$
;; Copyright (C) 2000 Steve Youngs


;; Author: Steve Youngs <youngs@xemacs.org>
;;
;; Keywords: eicq, toolbar, comm

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:

;;            A toolbar for eicq

;; Set some variables for the icon directory and files
(eval-when-compile
  (require 'eicq))

(defvar eicq-icon-directory 
  "/usr/local/lib/xemacs/xemacs-packages/etc/eicq/"
  "The directory where the icon data files for eicq are installed.")

(defvar eicq-password-icon 
  (toolbar-make-button-list 
   (expand-file-name "password.xpm" eicq-icon-directory))
  "A password toolbar icon.")

(defvar eicq-show-log-icon 
  (toolbar-make-button-list 
   (expand-file-name "show.xpm" eicq-icon-directory))
  "A show-window toolbar icon.")

(defvar eicq-hide-log-icon 
  (toolbar-make-button-list 
   (expand-file-name "hide.xpm" eicq-icon-directory))
  "A hide log toolbar icon.")

(defvar eicq-send-message-here-icon 
  (toolbar-make-button-list 
   (expand-file-name "send-message-here.xpm" eicq-icon-directory))
  "A send message toolbar icon.")

(defvar eicq-send-message-around-icon 
  (toolbar-make-button-list 
   (expand-file-name "send-message-around.xpm" eicq-icon-directory))
  "A send message toolbar icon.")

(defvar eicq-send-url-here-icon 
  (toolbar-make-button-list 
   (expand-file-name "url-here.xpm" eicq-icon-directory))
  "A send URL toolbar icon.")

(defvar eicq-send-url-around-icon 
  (toolbar-make-button-list 
   (expand-file-name "url-around.xpm" eicq-icon-directory))
  "A send URL toolbar icon.")

(defvar eicq-query-info-here-icon 
  (toolbar-make-button-list 
   (expand-file-name "query-here.xpm" eicq-icon-directory))
  "A query info here toolbar icon.")

(defvar eicq-query-info-around-icon 
  (toolbar-make-button-list 
   (expand-file-name "query-around.xpm" eicq-icon-directory))
  "A query info here toolbar icon.")

(defvar eicq-update-info-icon 
  (toolbar-make-button-list 
   (expand-file-name "update-info.xpm" eicq-icon-directory))
  "A update info toolbar icon.")

(defvar eicq-search-icon 
  (toolbar-make-button-list 
   (expand-file-name "search.xpm" eicq-icon-directory))
  "A search toolbar icon.")

(defvar eicq-authorize-here-icon 
  (toolbar-make-button-list 
   (expand-file-name "authorize-here.xpm" eicq-icon-directory))
  "A authorize toolbar icon.")

(defvar eicq-authorize-around-icon 
  (toolbar-make-button-list 
   (expand-file-name "authorize-around.xpm" eicq-icon-directory))
  "A authorize toolbar icon.")

(defvar eicq-login-icon
  (toolbar-make-button-list
   (expand-file-name "login.xpm" eicq-icon-directory))
  "A login toolbar icon")

(defvar eicq-logout-icon
  (toolbar-make-button-list
   (expand-file-name "logout.xpm" eicq-icon-directory))
  "A logout toolbar icon")

(defvar eicq-disconnect-icon
  (toolbar-make-button-list
   (expand-file-name "disconnect.xpm" eicq-icon-directory))
  "A disconnect toolbar icon")

(defvar eicq-mark-log-read-icon
  (toolbar-make-button-list
   (expand-file-name "read.xpm" eicq-icon-directory))
  "Mark log read toolbar icon")

(defvar eicq-mark-log-unread-icon
  (toolbar-make-button-list
   (expand-file-name "unread.xpm" eicq-icon-directory))
  "Mark log unread toolbar icon")

(defvar eicq-next-log-icon
  (toolbar-make-button-list
   (expand-file-name "next.xpm" eicq-icon-directory))
  "Next log item toolbar icon")

(defvar eicq-previous-log-icon
  (toolbar-make-button-list
   (expand-file-name "previous.xpm" eicq-icon-directory))
  "Previous log item toolbar icon")

(defvar eicq-new-log-icon
  (toolbar-make-button-list
   (expand-file-name "new-log.xpm" eicq-icon-directory))
  "New log file toolbar icon")

(defvar eicq-contract-log-icon
  (toolbar-make-button-list
   (expand-file-name "contract.xpm" eicq-icon-directory))
  "Contract the log toolbar icon")

(defvar eicq-expand-log-icon
  (toolbar-make-button-list
   (expand-file-name "expand.xpm" eicq-icon-directory))
  "Expand the log toolbar icon")

;; Define the functions for the toolbar

(defun eicq-toolbar-change-password (password)
  "Change password from the toolbar."
  (interactive (list (read-passwd "Password: " 'confirm)))
  (eicq-change-password password))

(defun eicq-toolbar-show-log ()
  "Show the log window from the toolbar"
  (interactive)
  (eicq-show-window))

(defun eicq-toolbar-hide-log ()
  "Hide the log window from the toolbar"
  (interactive)
  (eicq-hide-window))

(defun eicq-toolbar-send-message-here ()
  "Send message from toolbar"
  (interactive)
  (eicq-send-message-alias-here))

(defun eicq-toolbar-send-message-around ()
  "Send message from toolbar"
  (interactive)
  (eicq-send-message))

(defun eicq-toolbar-send-url-here ()
  "Send URL from the toolbar"
  (interactive)
  (eicq-send-url-alias-here))

(defun eicq-toolbar-send-url-around ()
  "Send URL from the toolbar"
  (interactive)
  (eicq-send-url))

(defun eicq-toolbar-query-info-here ()
  "Query info from the toolbar"
  (interactive)
  (eicq-query-info-alias-here))

(defun eicq-toolbar-query-info-around ()
  "Query info from the toolbar"
  (interactive)
  (eicq-query-info))

(defun eicq-toolbar-update-info ()
  "Update meta info from the toolbar"
  (interactive)
  (eicq-update-meta-info))

(defun eicq-toolbar-search (nick-name first-name last-name email)
  "Search from the toolbar"
  (interactive "sNick-name: \nsFirst-name: \nsLast-name: \nsEmail: \n")
  (eicq-search nick-name first-name last-name email))

(defun eicq-toolbar-authorize-here ()
  "Authorize from the toolbar"
  (interactive)
  (eicq-authorize-alias-here))

(defun eicq-toolbar-authorize-around ()
  "Authorize from the toolbar"
  (interactive)
  (eicq-authorize))

(defun eicq-toolbar-login ()
  "Login from the toolbar"
  (interactive)
  (eicq-login))

(defun eicq-toolbar-logout ()
  "Logout from the toolbar"
  (interactive)
  (eicq-logout))

(defun eicq-toolbar-disconnect ()
  "Disconnect from the toolbar"
  (interactive)
  (eicq-disconnect))

(defun eicq-toolbar-log-read ()
  "Mark log item read from the toolbar"
  (interactive)
  (eicq-log-mark-read))

(defun eicq-toolbar-log-unread ()
  "Mark log item unread from the toolbar"
  (interactive)
  (eicq-log-mark-unread))

(defun eicq-toolbar-next-log ()
  "Next log item from the toolbar"
  (interactive)
  (eicq-log-next 1))

(defun eicq-toolbar-previous-log ()
  "Previous log item from the toolbar"
  (interactive)
  (eicq-log-previous 1))

(defun eicq-toolbar-new-log ()
  "New log file from the toolbar"
  (interactive)
  (eicq-log-new-file))

(defun eicq-toolbar-contract-log ()
  "Contract the log from the toolbar"
  (interactive)
  (eicq-log-contract))

(defun eicq-toolbar-expand-log ()
  "Expand the log from the toolbar"
  (interactive)
  (eicq-log-expand))

;; Now define the toolbar

(defvar eicq-buddy-toolbar
  '([eicq-password-icon 
     eicq-toolbar-change-password t "Change password"]
    [eicq-show-log-icon 
     eicq-toolbar-show-log t "Show log"]
    [eicq-hide-log-icon 
     eicq-toolbar-hide-log t "Hide log"]
    [eicq-send-message-here-icon 
     eicq-toolbar-send-message-here t "Send message here"]
    [eicq-send-message-around-icon
     eicq-toolbar-send-message-around t "Send message..."]
    [eicq-send-url-here-icon 
     eicq-toolbar-send-url-here t "Send URL here"]
    [eicq-send-url-around-icon 
     eicq-toolbar-send-url-around t "Send URL..."]
    [eicq-query-info-here-icon 
     eicq-toolbar-query-info-here t "Query info here"]
    [eicq-query-info-around-icon
     eicq-toolbar-query-info-around t "Query info..."]
    [eicq-update-info-icon 
     eicq-toolbar-update-info t "Update info"]
    [eicq-search-icon 
     eicq-toolbar-search t "Search"]
    [eicq-authorize-here-icon 
     eicq-toolbar-authorize-here t "Authorize here"]
    [eicq-authorize-around-icon
     eicq-toolbar-authorize-around t "Authorize..."]
    [eicq-login-icon
     eicq-toolbar-login t "Login"]
    [eicq-logout-icon
     eicq-toolbar-logout t "Logout"]
    [eicq-disconnect-icon
     eicq-toolbar-disconnect t "Disconnect"])
  "The clickety click eicq buddy toolbar")

(defvar eicq-log-toolbar
  '([eicq-send-message-here-icon 
     eicq-toolbar-send-message-here t "Send message here"]
    [eicq-send-message-around-icon
     eicq-toolbar-send-message-around t "Send message..."]
    [eicq-send-url-here-icon 
     eicq-toolbar-send-url-here t "Send URL here"]
    [eicq-send-url-around-icon 
     eicq-toolbar-send-url-around t "Send URL..."]
    [eicq-query-info-here-icon 
     eicq-toolbar-query-info-here t "Query info here"]
    [eicq-query-info-around-icon
     eicq-toolbar-query-info-around t "Query info..."]
    [eicq-authorize-here-icon 
     eicq-toolbar-authorize-here t "Authorize here"]
    [eicq-authorize-around-icon
     eicq-toolbar-authorize-around t "Authorize..."]
    [eicq-mark-log-read-icon
     eicq-toolbar-log-read t "Mark read"]
    [eicq-mark-log-unread-icon
     eicq-toolbar-log-unread t "Mark unread"]
    [eicq-next-log-icon
     eicq-toolbar-next-log t "Next"]
    [eicq-previous-log-icon
     eicq-toolbar-previous-log t "Previous"]
    [eicq-new-log-icon
     eicq-toolbar-new-log t "New log file"]
    [eicq-contract-log-icon
     eicq-toolbar-contract-log t "Contract log"]
    [eicq-expand-log-icon
     eicq-toolbar-expand-log t "Expand log"]
    [eicq-login-icon
     eicq-toolbar-login t "Login"]
    [eicq-logout-icon
     eicq-toolbar-logout t "Logout"]
    [eicq-disconnect-icon
     eicq-toolbar-disconnect t "Disconnect"])
  "A clickety click eicq log buffer toolbar")

(defcustom eicq-use-toolbar (if (featurep 'toolbar)
				'default-toolbar
			      nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five valid values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar'."
  :type '(choice (const default-toolbar)
		 (const top-toolbar) (const bottom-toolbar)
		 (const left-toolbar) (const right-toolbar)
		 (const :tag "no toolbar" nil))
  :group 'eicq-option)

(defun eicq-install-buddy-toolbar ()
  (and eicq-use-toolbar
       (set-specifier (symbol-value eicq-use-toolbar)
		      (cons 
		       (current-buffer) eicq-buddy-toolbar))))

(defun eicq-install-log-toolbar ()
  (and eicq-use-toolbar
       (set-specifier (symbol-value eicq-use-toolbar)
		      (cons 
		       (current-buffer) eicq-log-toolbar))))

(provide 'eicq-toolbar)


