;;; eicq-menu.el --- Menus and keymaps for Eicq.

;; Copyright (C) 2002,03,04 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <steve@youngs.au.com>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Created:       2002-10-01
;; Last-Modified: <2004-05-30 11:14:00 (steve)>
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

;;;###autoload
(defvar eicq-main-map
  (let ((map (make-keymap 'eicq-main-map)))
    (suppress-keymap map)
    (define-key map [X] nil)            ; BUG?
    (define-key map [X i] 'eicq-login)
    (define-key map [X o] 'eicq-logout)
    (define-key map [X s] 'eicq-change-status)
    (define-key map [S] 'eicq-group-select-aliases)
    (define-key map [s] 'eicq-group-select-aliases)
    (define-key map [w] 'eicq-show-window)
    (define-key map [h] 'eicq-hide-window)
    (define-key map [M] 'eicq-send-message)
    (define-key map [m] 'eicq-send-message)
    (define-key map [U] 'eicq-send-url)
    (define-key map [u] 'eicq-send-url)
    (define-key map [A] 'eicq-authorize)
    (define-key map [a] 'eicq-authorize)
    (define-key map [i] 'eicq-query-info)
    (define-key map [I] 'eicq-query-info)
    (define-key map [f] 'world-find)
    (define-key map [V] nil)
    (define-key map [V c] 'eicq-buddy-view-connected)
    (define-key map [V v] 'eicq-buddy-view-active)
    (define-key map [V a] 'eicq-buddy-view-all)
    (define-key map [?1] 'eicq-buddy-show-buffer)
    (define-key map [?2] 'eicq-log-show-buffer)
    (define-key map [?4] 'eicq-network-show-buffer)
    map)
  "Keyboard map common for `eicq-log-mode-map' and `eicq-buddy-mode-map'.")

;;;###autoload
(defun eicq-global-map-set (&optional symbol value)
  "Set `eicq-global-key-prefix'.
WARNING: Bindings with old prefix is not deleted.  Fixable?"
  (define-key global-map value eicq-main-map))

;;;###autoload
(defcustom eicq-global-key-prefix [(meta \`)]
  "*Prefix for all key macros in global."
  :group 'eicq-option
  :set 'eicq-global-map-set)

;;; Internal variables

(defvar eicq-main-menu
  '("Eicq"
    ["Show Window" eicq-show-window t]
    ["Hide Window" eicq-hide-window t]
    ["Register New UIN" eicq-register-new-user t]
    ["Change Password" eicq-change-password t]
    ["Login" eicq-login t]
    ["Logout" eicq-logout t]
    ["Disconnect" eicq-disconnect t]
    "---"
    ["Select" eicq-group-select-aliases t]
    ["Send Message" eicq-send-message t]
    ["Send URL" eicq-send-url t]
    ["Authorize" eicq-authorize t]
    ["Change Status" eicq-change-status t]
    ["Search" eicq-search t]
    ["Update Meta Info" eicq-update-meta-info t]
    "---"
    ["alias -> uin" eicq-alias-uin t]
    ["uin -> alias" eicq-uin-alias t]
    ["Redo Packet" eicq-redo-hex t]
    ["Resend Contact List" eicq-send-contact-list t]
    ["Buddy Buffer" eicq-buddy-show-buffer t]
    ["Log Buffer" eicq-log-show-buffer t]
    ["Network Buffer" eicq-network-show-buffer t]
    "---"
    ["Email Author" eicq-email-author t]
    ["Submit Bug Report" (eicq-report-bug eicq-blurb) t]
    ["Customize" eicq-customize t])
  "Menu for both `eicq-log-mode' and `eicq-buddy-mode'.")

(easy-menu-define
 eicq-main-easymenu nil "Eicq Main" eicq-main-menu)

(defvar eicq-log-menu
  '("Eicq-log"
    ["Select Around" eicq-select-alias-around t]
    ["Send Message Around" eicq-send-message-alias-around t]
    ["Send URL Around" eicq-send-url-alias-around t]
    ["Authorize Around" eicq-authorize-alias-around t]
    ["Forward Message" eicq-forward-message-around t]
    ["Query Around" eicq-query-info-alias-around t]
    "---"
    ["New Log File" eicq-log-new-file t]
    ["Contract Log" eicq-log-contract t]
    ["Expand Log" eicq-log-expand t]
    ["Previous Log" eicq-log-previous t]
    ["Next Log" eicq-log-next t]
    "---"
    ["Mark Read" eicq-log-mark-read t]
    ["Mark Unread" eicq-log-mark-unread t])
  "Menu for `eicq-log-mode'.")

(easy-menu-define
 eicq-buddy-easymenu nil "Eicq Buddy" eicq-log-menu)

;;;###autoload
(defvar eicq-log-mode-map
  (let ((map (make-sparse-keymap 'eicq-log-mode-map)))
    (set-keymap-parents map (list eicq-main-map))
    (define-key map [delete] 'eicq-log-contract)
    (define-key map [insert] 'eicq-log-expand)
    (define-key map [(control up)] 'eicq-log-previous)
    (define-key map [(control down)] 'eicq-log-next)
    (define-key map [v] 'eicq-log-mark-unread)
    (define-key map [c] 'eicq-log-mark-read)
    (define-key map [W] 'eicq-alias-around)
    (define-key map [s] 'eicq-select-alias-around)
    (define-key map [m] 'eicq-send-message-alias-around)
    (define-key map [u] 'eicq-send-url-alias-around)
    (define-key map [a] 'eicq-authorize-alias-around)
    (define-key map [i] 'eicq-query-info-alias-around)
    (define-key map [f] 'eicq-forward-message-around)
    (define-key map [n] 'eicq-log-next-unread)
    (define-key map [N] 'eicq-log-next)
    (define-key map [o] 'eicq-switch-to-buddy-buffer)
    (define-key map [p] 'eicq-log-previous-unread)
    (define-key map [P] 'eicq-log-previous)
    map)
  "Keymap for `eicq-log-mode'.")

(defvar eicq-buddy-menu
  '("Eicq-Buddy"
    ["Select Here" eicq-select-alias-here t]
    ["Select By Status" eicq-buddy-select-all-in-view-by-status t]
    ["Select By Regexp" eicq-buddy-select-all-in-view-by-regexp t]
    ["Send Message Here" eicq-send-message-alias-here t]
    ["Send URL Here" eicq-send-url-alias-here t]
    ["Authorize Here" eicq-authorize-alias-here t]
    ["Query Info Here" eicq-query-info-alias-here t]
    "---"
    ["View Connected" eicq-buddy-view-connected t]
    ["View Active" eicq-buddy-view-active t]
    ["View All" eicq-buddy-view-all t])
  "Menu for `eicq-buddy-mode'.")

(easy-menu-define
 eicq-log-easymenu nil "Eicq Log" eicq-buddy-menu)

;;;###autoload
(defvar eicq-alias-map
  (let ((map (make-sparse-keymap 'eicq-alias-map)))
    (define-key map [button2] 'eicq-send-message-via-mouse)
    map)
  "Keymap for alias extent.")

;;;###autoload
(defvar eicq-url-map
  (let ((map (make-sparse-keymap 'eicq-url-map)))
    (define-key map [button2] 'browse-url-at-mouse)
    (define-key map [B] 'browse-url-at-point)
    (define-key map [return] 'browse-url-at-point)
    map)
  "Keymap for URL extent.")

;;;###autoload
(defvar eicq-buddy-mode-map
  (let ((map (make-sparse-keymap 'eicq-buddy-mode-map)))
    (set-keymap-parents map (list eicq-main-map))
    (define-key map [W] 'eicq-alias-here)
    (define-key map [s] 'eicq-select-alias-here)
    (define-key map [m] 'eicq-send-message-alias-here)
    (define-key map [u] 'eicq-send-url-alias-here)
    (define-key map [a] 'eicq-authorize-alias-here)
    (define-key map [i] 'eicq-query-info-alias-here)
    (define-key map [n] 'next-line)
    (define-key map [o] 'other-window)
    (define-key map [p] 'previous-line)
    map)
  "Keymap for `eicq-buddy-mode'.")




(provide 'eicq-menu)
;;; eicq-menu.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
