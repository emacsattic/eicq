;;; eicq-toolbar.el --- A toolbar for Eicq   -*-Emacs-Lisp-*-

;; Copyright (C) 2000, 2001 Steve Youngs

;; RCS: $Id$
;; Author: Steve Youngs <youngs@xemacs.org>
;; Maintainer: Steve Youngs <youngs@xemacs.org>
;; Last-Modified: <2001-8-17 20:01:35 (steve)>
;; Keywords: eicq, toolbar, comm

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

;;; Commentary:
;; 
;;            A toolbar for eicq.
;;
(require 'eicq)

;;; Code:
(defcustom eicq-icon-directory
  (locate-data-directory "eicq")
  "*The directory where the icon files for Eicq are installed."
  :type 'directory
  :tag "Toolbar Directory"
  :group 'eicq-interface)

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
  :group 'eicq-interface)

;;; Internal variables

(defvar eicq-password-icon
  (toolbar-make-button-list
   (expand-file-name "password.xpm" eicq-icon-directory))
  "A password toolbar icon.")

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

(defvar eicq-login-icon
  (toolbar-make-button-list
   (expand-file-name "login.xpm" eicq-icon-directory))
  "A login toolbar icon.")

(defvar eicq-logout-icon
  (toolbar-make-button-list
   (expand-file-name "logout.xpm" eicq-icon-directory))
  "A logout toolbar icon.")

(defvar eicq-disconnect-icon
  (toolbar-make-button-list
   (expand-file-name "disconnect.xpm" eicq-icon-directory))
  "A disconnect toolbar icon.")

(defvar eicq-new-log-icon
  (toolbar-make-button-list
   (expand-file-name "new-log.xpm" eicq-icon-directory))
  "New log file toolbar icon.")

(defvar eicq-help-icon
  (toolbar-make-button-list
   (expand-file-name "help.xpm" eicq-icon-directory))
  "A help toolbar icon.")

;; Define the functions for the toolbar

(defun eicq-toolbar-change-password (password)
  "Change PASSWORD from the toolbar."
  (interactive (list (read-passwd "Password: " 'confirm)))
  (eicq-change-password password))

(defun eicq-toolbar-send-message-here ()
  "Send message from toolbar."
  (interactive)
  (eicq-send-message-alias-here))

(defun eicq-toolbar-send-message-around ()
  "Send message from toolbar."
  (interactive)
  (eicq-send-message))

(defun eicq-toolbar-send-url-here ()
  "Send URL from the toolbar."
  (interactive)
  (eicq-send-url-alias-here))

(defun eicq-toolbar-send-url-around ()
  "Send URL from the toolbar."
  (interactive)
  (eicq-send-url))

(defun eicq-toolbar-query-info-here ()
  "Query info from the toolbar."
  (interactive)
  (eicq-query-info-alias-here))

(defun eicq-toolbar-query-info-around ()
  "Query info from the toolbar."
  (interactive)
  (eicq-query-info))

(defun eicq-toolbar-update-info ()
  "Update meta info from the toolbar."
  (interactive)
  (eicq-update-meta-info))

(defun eicq-toolbar-search (nick-name first-name last-name email)
  "Search from the toolbar.
Argument NICK-NAME is the ICQ name of the person you're searching for.
Argument FIRST-NAME is the first name of the person you're searching for.
Argument LAST-NAME is the last name of the person you are searching for.
Argument EMAIL is the email address of the person you're searching for."
  (interactive "sNick-name: \nsFirst-name: \nsLast-name: \nsEmail: \n")
  (eicq-search nick-name first-name last-name email))

(defun eicq-toolbar-authorize-here ()
  "Authorize from the toolbar."
  (interactive)
  (eicq-authorize-alias-here))

(defun eicq-toolbar-login ()
  "Login from the toolbar."
  (interactive)
  (eicq-login))

(defun eicq-toolbar-logout ()
  "Logout from the toolbar."
  (interactive)
  (eicq-logout))

(defun eicq-toolbar-disconnect ()
  "Disconnect from the toolbar."
  (interactive)
  (eicq-logout)
  (eicq-disconnect))

(defun eicq-toolbar-new-log ()
  "New log file from the toolbar."
  (interactive)
  (eicq-log-new-file))

(defun eicq-toolbar-help ()
  "Display the Eicq info documentation."
  (interactive)
  (Info-goto-node "(eicq.info)Top"))

;; Now define the toolbar
(defvar eicq-log-toolbar
  '([eicq-password-icon
     eicq-toolbar-change-password t "Change password"]
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
    [eicq-search-icon
     eicq-toolbar-search t "Search"]
    [eicq-authorize-here-icon
     eicq-toolbar-authorize-here t "Authorize here"]
    [eicq-new-log-icon
     eicq-toolbar-new-log t "New log file"]
    [eicq-login-icon
     eicq-toolbar-login t "Login"]
    [eicq-logout-icon
     eicq-toolbar-logout t "Logout"]
    [eicq-disconnect-icon
     eicq-toolbar-disconnect t "Disconnect"]
    [eicq-help-icon
     eicq-toolbar-help t "Help"])
  "A clickety click Eicq log buffer toolbar.")

(defun eicq-install-buddy-toolbar ()
  "Install the toolbar for `eicq-buddy-mode' in Eicq."
  (and eicq-use-toolbar
       (set-specifier (symbol-value eicq-use-toolbar)
		      (cons
		       (current-buffer) eicq-log-toolbar))))

(defun eicq-install-log-toolbar ()
  "Install the toolbar for `eicq-log-mode' in Eicq."
  (and eicq-use-toolbar
       (set-specifier (symbol-value eicq-use-toolbar)
		      (cons
		       (current-buffer) eicq-log-toolbar))))

(provide 'eicq-toolbar)

;;; eicq-toolbar.el ends here


;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%m-%d %02H:%02M:%02S (%u)"
;End: 
