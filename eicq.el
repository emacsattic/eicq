;;; eicq.el --- ICQ client for Emacs

;; Copyright (C) 1999 by Stephen Tse
;; Copyright (C) 2000, 2001 Steve Youngs

;; RCS: $Id$
;; OriginalAuthor: Stephen Tse <stephent@sfu.ca>
;; Maintainer: Steve Youngs <youngs@xemacs.org>
;; Created: Aug 08, 1998
;; Last-Modified: <2001-9-2 15:57:59 (steve)>
;; Version: 0.2.16pre3
;; Homepage: http://eicq.sf.net/
;; Keywords: comm ICQ

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
;; Clone of Mirabilis ICQ communication client.
;;
;; Entry points:
;;   eicq-login
;;   eicq-show-window
;;   eicq-customize
;;
;; See README & INSTALL which come with this package
;;
;; This project is done without the consent of Mirabilis.
;;

;;; Code:

(require 'browse-url)
(require 'outline)
(require 'timezone)
(require 'wid-edit)
(require 'goto-addr)
(require 'smiley)

(defconst eicq-version "0.2.16pre3"
  "Version of eicq you are currently using.")

;; Customize Groups.

(defgroup eicq nil
  "Mirabilis ICQ communication client."
  :group 'comm)

(defgroup eicq-info nil
  "Essential account info."
  :group 'eicq)

(defgroup eicq-meta nil
  "User info stored in ICQ server.
Run `eicq-update-meta-info' after changing any of these variables."
  :group 'eicq)

(defgroup eicq-option nil
  "System settings and general preferences."
  :group 'eicq)

(defgroup eicq-log nil
  "Message logging preferences."
  :group 'eicq)

(defgroup eicq-buddy nil
  "Contact list preferences."
  :group 'eicq)

(defgroup eicq-sound nil
  "Sound preferences."
  :group 'eicq)

(defgroup eicq-interface nil
  "Change the look and \"feel\"."
  :group 'eicq)

;; Customize.

(defcustom eicq-user-meta-nickname "e-i-c-q"
  "*Your nickname stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-firstname "XEmacs"
  "*Your first name stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-lastname "Linux"
  "*Your last name stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-primary-email "emacs@home.com"
  "*Your primary email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-secondary-email "eicq@home.com"
  "*Your secondary email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-old-email "eicq@home.com"
  "*Your old email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-city nil
  "*Your city stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-state nil
  "*Your state stored on the ICQ server.
We're talking 'address' here, not online state.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-phone nil
  "*Your phone number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-fax nil
  "*Your fax number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-street nil
  "*Your street name and number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-cell-phone nil
  "*Your cell phone number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-zipcode nil
  "*Your zip code/postal code stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-country nil
  "*Your country stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-web-aware t
  "*Set this to non-nil if you want your presence know on the web.
Run `eicq-update-meta-info' after modifying this variable."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-hide-ip nil
  "*Set to non-nil if you want to hide your IP.
Run `eicq-update-meta-info' after modifying this variable."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-authorization t
  "*Authorization needed to add you to others' contact lists..
Run `eicq-update-meta-info' after modifying this variable."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-about
  "Give a man a new Emacs command,
  and he can hack for a night;
Teach a man to make new Emacs commands,
  and he can hack for a life time."
  "*User 'about' info stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-homepage 
  "http://eicq.sf.net/"
  "*User homepage stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-age 65535
  "*Your age stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
65535 = not entered."
  :group 'eicq-meta)

(defcustom eicq-user-meta-sex 'not-entered
  "*Your sex stored on the ICQ server.
No, it's not an invitation.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta
  :type '(choice (item male) (item female)
                 (item not-entered)))

(defcustom eicq-user-meta-birth-year nil
  "*Your birth year (YY) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-birth-month nil
  "*Your birth month (MM) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-birth-day nil
  "*Your birth day (DD) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-1 0
  "*User 1st language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-2 0
  "*User 2nd language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-3 0
  "*User 3rd language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-city nil
  "*User work city stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-state nil
  "*User work state stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-phone nil
  "*User work phone number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-fax nil
  "*User work fax number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-address nil
  "*User work address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-company nil
  "*User company stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-department nil
  "*User work department stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-position nil
  "*User work position stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-homepage "http://eicq.sf.net/"
  "*User work homepage stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-use-sound-flag nil
  "*Whether to use sound or not.
SOUND-CARD - use the sound card.
PC-SPEAKER - use the PC speaker.
NIL - don't use sound.

This is not yet fully implemented and setting this to anything
other than SOUND-CARD will turn the sound off."
  :group 'eicq-sound
  :type '(choice
	  (item sound-card) 
	  (item pc-speaker) 
	  (item nil)) 
  :tag "Use Sound")

(defcustom eicq-sound-directory 
  (locate-data-directory "sounds")
  "*Directory where sound files are kept."
  :group 'eicq-sound
  :type 'directory 
  :tag "eicq-sound-directory")

(defcustom eicq-sound-alist
  '((message-sound . nil)
    (chat-sound . nil)
    (url-sound . nil)
    (buddy-sound . nil)
    (auth-sound . nil)
    (emailx-sound . nil)
    (pager-sound . nil))
  "*Sound event to sound file alist.
The possible sound events are:
      \"message-sound\" - Incoming message sound.
      \"chat-sound\"    - Incoming chat request sound.
      \"url-sound\"     - Incoming url sound.
      \"buddy-sound\"   - Online notify sound.
      \"auth-sound\"    - Authorise sound.
      \"emailx-sound\"  - Email express sound.
      \"pager-sound\"   - Pager sound."
  :group 'eicq-sound
  :type '(repeat 
	  (cons (sexp :tag "Sound Event") 
		(sexp :tag "Sound File")))
  :tag "Sounds")

(defcustom eicq-coding-system nil
  "*Coding for incoming and outgoing messages.
This feature is supported only in Emacs with MULE.
Nil means not to use any codings.
See `list-coding-systems'."
  :group 'eicq-option
  :type
  (append '(choice (item nil))
          (if (fboundp 'coding-system-list)
              (mapcar
               (lambda (x) (list 'item x))
               (coding-system-list)))))

(defcustom eicq-bridge-filename 
  "icq2tcp"
  "*Filename for `eicq-bridge' program."
  :group 'eicq-option)

(defcustom eicq-bridge-buffer nil
  "Buffer for `eicq-bridge'.
Nil means no associated buffer, or no debug info."
  :group 'eicq-option)

(defcustom eicq-bridge-hostname "127.0.0.1"
  "*IP address of `eicq-bridge'.
See `eicq-connect'."
  :group 'eicq-option)

(defcustom eicq-bridge-port
  ;; plant random seed
  (progn (random t) nil)
  "*Port of `eicq-bridge'.
See `eicq-connect'."
  :group 'eicq-option)

(defcustom eicq-local-bridge-p t
  "If non-NIL, Eicq will look for a bridge running on a remote host defined
by `eicq-bridge-hostname' and `eicq-bridge-port'."
  :group 'eicq-option
  :type 'boolean)

(defcustom eicq-server-hostname "icq1.mirabilis.com"
  "*Hostname or IP address of Mirabilis ICQ server."
  :group 'eicq-option)

(defcustom eicq-server-port 4000
  "*Port of Mirabilis ICQ server."
  :group 'eicq-option)

(defvar eicq-valid-statuses
  '("online" "away" "occ" "dnd" "ffc" "na" "invisible")
  "All statuses valid for selection.
Used by `eicq-change-status' and in `eicq-buddy-buffer'.")

(defcustom eicq-user-initial-status "invisible"
  "*Initial user status when login."
  :group 'eicq-option
  :type
  (cons 'choice
        (mapcar
         (lambda (x) (list 'item x))
         eicq-valid-statuses)))

(defcustom eicq-auto-response-messages-p t
  "Set this to non-NIL to send automatic messages.
The automatic messages are those that are sent when somebody
sends you a message while you are 'away', 'na', 'dnd', or 'occ'."
  :tag "Send auto-response messages."
  :type 'boolean
  :group 'eicq-option)

(defcustom eicq-auto-reply-away
  "I am currently away from ICQ.
Please leave me messages,
I'll get back to you asap.

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you are away."
  :group 'eicq-option)

(defcustom eicq-auto-reply-occ
  "I am currently occupied.
Please leave me messages,
I'll get back to you when I can.

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you are occupied."
  :group 'eicq-option)

(defcustom eicq-auto-reply-dnd
  "Hey, the sign on the door says \"Do Not Disturb\"!

Leave me a message, if you feel you must.
I might get back to you.

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you want to leave alone."
  :group 'eicq-option)

(defcustom eicq-auto-reply-na
  "I am currently not available.
Please leave me messages,
I'll get back to you when I get a chance.

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you are not available."
  :group 'eicq-option)

;; FIXME: How can I make this display the value of eicq-auto-away-timeout
(defcustom eicq-idle-reply-away
   "I must be too busy to talk because I have
been idle now for at least...seconds

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you have idled away."
  :group 'eicq-option)

;; FIXME: How can I make this display the value of eicq-auto-away-timeout
(defcustom eicq-idle-reply-na
   "I must be too busy to talk because I have
been idle now for at least...seconds

This message has been automatically sent to you
by the XEmacs ICQ client \"Eicq\".
<http://eicq.sf.net/>"
  "Auto reply with this when you have idled to na."
  :group 'eicq-option)

(defcustom eicq-delete-offline-messages-flag 'ask
  "*Non-nil means delete all offline messages from server.
'ask means to ask user every time.
Nil means leave messages on server and you will receive the same offline
messages again next time you login."
  :group 'eicq-option
  :type '(choice (item t) (item ask) (item nil)))

(defcustom eicq-buddy-window-width 20
  "*Width of window for `eicq-buddy-buffer'."
  :group 'eicq-interface)

(defcustom eicq-status-window-height 9
  "*Height of window for `eicq-status-buffer'."
  :group 'eicq-interface)

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

(defcustom eicq-buddy-status-color-hint-flag t
  "*Non-nil means put status color hints."
  :type 'boolean
  :group 'eicq-buddy)

(defcustom eicq-buddy-view 
  'eicq-connected-aliases
  "*View of buddy buffer.
It determines what aliases to be display in buddy buffer.  For example,
\(eicq-connected-aliases) means display all connected aliases.

See `eicq-buddy-view-all', `eicq-buddy-view-connected', and
`eicq-buddy-view-active'."
  :group 'eicq-buddy
  :type '(choice (item eicq-all-aliases)
                 (item eicq-connected-aliases)
                 (item eicq-active-aliases))
  :initialize 'custom-initialize-default)

(defcustom eicq-start-in-new-frame nil
  "*If non-NIL, Eicq will start in its own frame."
  :group 'eicq-interface
  :type 'boolean)

(defvar eicq-user-status "offline"
  "Current user status.")

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

(defvar eicq-world-rc-filename "~/.eicq/world"
  "*Filename for resource file.")

(defvar eicq-new-message-hook nil
  "*Hooks to run when there is an incoming message.
Dynamically ALIAS and MESSAGE are binded to be used in hooks.")

(defvar eicq-status-update-hook nil
  "*Hooks to run when a buddy change his status.
Dynamically ALIAS and STATUS are binded to be used in hooks.")

(defvar eicq-read-message-hook nil
  "*Hooks run when a message is marked as \"read\".")

(defvar eicq-system-message-hook nil
  "*Hooks run when a \"system\" message is received.")


;;; Internal variables

(defcustom eicq-user-alias "me"
  "*Your alias in `eicq-world'.
Run `eicq-world-update' after modifying this variable."
  :group 'eicq-info)

(defcustom eicq-user-password nil
  "*Password for your ICQ account.
Nil means prompt for entering password every time you login."
 :group 'eicq-info)

;;;###autoload
(defun eicq-version (&optional arg)
  "Return the version of eicq you are currently using.
If ARG, insert version string at point."
  (interactive "P")
  (if arg
      (insert (message "Eicq version %s" eicq-version))
    (message "Eicq version %s" eicq-version)))

;;; Code - compatibility:

;; MULE compatibility functions for 20.4

(defun-when-void encode-coding-string (string encoding-system)
  "Stub for compatibility with MULE."
  string)

(defun-when-void decode-coding-string (string encoding-system)
  "Stub for compatibility with MULE."
  string)

;; Load the toolbar
(add-hook 'eicq-buddy-mode-hook 'eicq-install-buddy-toolbar)
(add-hook 'eicq-log-mode-hook 'eicq-install-log-toolbar)

;;; Code - data conversion:

;; hex  string based 16 (for debugging)
;; bin  string based 2 (for network packet)
;; uin  string based 10
;; int  integer based 10
;;
;; Note 1: Special care should be taken in regard of endian-ness of `bin'.
;;
;; Note 2: `uin' is stored as a string because it is an 32-bit integer in
;; network packets and it is larger than the integer representation in
;; Emacs. Internally eicq uses `float' to do the conversion.

(defun eicq-hex-bin (hex)
  "Return a binary string from a hex string HEX.
It ignores all non-lower letter hex characters, which makes reading string
from `eicq-bin-pretty-hex' and network debug convenient.  If the length of
HEX is odd, ?0 is appended to its end."
  (interactive "sHex: ")
  (setq hex (downcase hex))
  ;; stole from hexl.el
  (while (string-match "[^a-f0-9]" hex)
    (setq hex (replace-match "" t t hex)))

  (let (hex-list high low bin)
    (setq hex-list
          (mapcar
           (lambda (x)
             (if (and (>= x ?0) (<= x ?9))
                 (- x ?0) (+ 10 (- x ?a))))
           hex))
    (while hex-list
      (setq high (pop hex-list))
      (setq low (or (pop hex-list) 0))
      (setq bin (concat bin (char-to-string
                             (+ (* high 16) low)))))
    (if (interactive-p) (kill-new bin))
    bin))

(defun eicq-bin-hex (bin)
  "Return a hex string from a binary string BIN."
  (mapconcat
   (lambda (x) (format "%02x" x))
   bin nil))

(defun eicq-bin-pretty-hex (bin)
  "Return a pretty-printed hex string from a binary string BIN."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format
        ;; insert a space every 4 hex = 2 bin
        (if (evenp (incf i)) "%02x " "%02x")
        x))
     bin nil)))

(defun eicq-pretty-hex (hex)
  "Return a pretty-printed hex string from a HEX string."
  (eicq-bin-pretty-hex (eicq-hex-bin hex)))

(defun eicq-int-bin (int)
  "Return a 2-byte binary string from an integer INT."
  ;; stole from hexl.el
  (let ((low (logand int 255))
        (high (ash int -8)))
    (concat (char-to-string low) (char-to-string high))))

(defun eicq-bin-int (bin &optional from)
  "Return an integer from a binary string BIN.
Consider only the first two characters from FROM to FROM+1 in BIN.  Useful
in parsing packet.  Nil FROM means 0."
  ;; stole from hexl.el
  (let* ((from (or from 0))
         (low (aref bin from))
         (high (aref bin (1+ from))))
    (+ low (ash high 8))))

(defun eicq-byte-int (bin &optional at)
  "Return an integer from a character byte in a binary string BIN.
Consider the character byte at AT in BIN.  Useful in parsing packet.  Nil
AT means 0."
  (char-to-int (aref bin (or at 0))))

(defun eicq-int-byte (int)
  "Return a binary string of one character byte from an integer INT."
  (if (< int 256)
      (char-to-string (int-to-char int))))

(defun eicq-uin-bin (uin)
  "Return a binary string for an UIN.
Return \"00000000\" in binary string if conversion fails."
  ;; stole from Eric's xmath.el
  (let (x y)
    (setq x (condition-case nil
                (eval (read (format "(float %s.0)" uin)))
              (error 0)))

    ;; least value byte
    (setq y (char-to-string (logand (truncate (mod x 65536)) 255)))
    ;; shift by 8 bit = 1 byte
    (setq x (/ x 256))
    (setq y (concat y (char-to-string
                       (logand (truncate (mod x 65536)) 255))))
    (setq x (/ x 256))
    (setq y (concat y (char-to-string
                       (logand (truncate (mod x 65536)) 255))))
    (setq x (/ x 256))
    (setq y (concat y (char-to-string
                       (logand (truncate (mod x 65536)) 255))))))

(defun eicq-bin-uin (bin &optional from)
  "Return an uin from a binary string BIN.
Consider only the first four characters from FROM to FROM+3 in BIN.
Nil FROM means 0."
  ;; stole from Eric's xmath.el
  (let ((from (or from 0)))
    (format "%.f"
            (+ (float (char-to-int (aref bin from)))
               (* (float (char-to-int (aref bin (+ from 1)))) 256)
               (* (float (char-to-int (aref bin (+ from 2)))) 256 256)
               (* (float (char-to-int (aref bin (+ from 3)))) 256 256
                  256)))))

(defun eicq-bin-ip (bin from)
  "Return an IP address from a binary string BIN.
Consider only the first four characters from FROM to FROM+3 in BIN."
  (mapconcat
   (lambda (x)
     (int-to-string (char-to-int x)))
   (substring bin from (+ from 4)) "."))

;;; Code - utilities:

(defun eicq-completing-read
  (prompt table &optional predicate require-match initial-contents history)
  "Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-CONTENTS, HISTORY.
Same as `completing-read' but accepts strings as well as obarray."
  (completing-read
   prompt
   (if (vectorp table)
       table (mapcar 'list table))
   predicate require-match initial-contents history))

;;;###autoload
(defun eicq-customize ()
  "Interactively customize settings and preferences."
  (interactive)
  (customize-group 'eicq))

;;;###autoload
(defun eicq-browse-homepage ()
  "Browse eicq homepage for news and files."
  (interactive)
  (browse-url "http://eicq.sf.net/"))

(defun eicq-encode-string (string)
  "Return a encoded string from STRING with DOS stuff added.
Encode string with `eicq-coding-system'."
  (encode-coding-string
   ;; add DOS stuff
   ;; "0d" instead to avoid use of ^M
   ;; which messes up with outline mode
   (replace-in-string string "\x0a" "\x0d\x0a")
   eicq-coding-system))

(defun eicq-decode-string (string)
  "Return a decoded string from STRING with DOS stuff removed.
It also quote character % to make `format' happy in `eicq-log'.
Decode string with `eicq-coding-system'."
  (decode-coding-string
   ;; quote %
   (replace-in-string
    ;; remove DOS stuff
    ;; "0d0a" instead to avoid use of ^M
    ;; which messes up with outline mode
    (replace-in-string string "\x0d\x0a" "\x0a")
    "%" "%%")
   eicq-coding-system))

(defconst eicq-message-max-size 350
  "Maximum size of message that ICQ will accept.
Set it to small because size expands after `eicq-encode-string'.")

(defun eicq-spliter (x)
  "Split a long message X into parts of maximum length `eicq-message-max-size'.
Only split at whitespace."
  (loop
    with i = eicq-message-max-size
    while (> (length x) i)
    do (while (and (not (memq (aref x (incf i -1)) '(?  ?\t)))
                   ;; at least half, to safe guard
                   (> i (/ eicq-message-max-size 2))))
    collect (substring x 0 i) into parts
    do (setq x (substring x i))
    finally return (nconc parts (list x))))

(defvar eicq-monthnames
  ["0" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defvar eicq-country-code
  '((65535 . "not entered")
    (93 . "Afghanistan")
    (355 . "Albania")
    (213 . "Algeria")
    (376 . "Andorra")
    (244 . "Angola")
    (672 . "Antarctic Aus Territory")
    (599 . "Antilles")
    (54 . "Argentina")
    (374 . "Armenia")
    (297 . "Aruba")
    (247 . "Ascension Island")
    (61 . "Australia")
    (43 . "Austria")
    (994 . "Azerbaijan")
    (351 . "Azores")
    (973 . "Bahrain")
    (890 . "Bangladesh")
    (809 . "Barbados")
    (257 . "Barundi")
    (375 . "Belarus")
    (32 . "Belgium")
    (229 . "Belize")
    (501 . "Belize")
    (975 . "Bhutan")
    (591 . "Bolivia")
    (387 . "Bosnia Hercegovina")
    (267 . "Botswana")
    (55 . "Brazil")
    (673 . "Brunei Darussalam")
    (226 . "Bukina Faso")
    (359 . "Bulgaria")
    (855 . "Cambodia")
    (237 . "Cameroon")
    (107 . "Canada")
    (238 . "Cape Verde Islands")
    (236 . "Central African Republic")
    (235 . "Chad")
    (56 . "Chile")
    (86 . "China")
    (672 . "Christmas Island")
    (672 . "Cocos Island")
    (57 . "Columbia")
    (269 . "Comoros")
    (242 . "Congo")
    (682 . "Cook Islands")
    (506 . "Costa Rica")
    (225 . "Cote d'Ivorie")
    (385 . "Croatia")
    (53 . "Cuba")
    (357 . "Cyprus")
    (42 . "Czech Republic")
    (45 . "Denmark")
    (253 . "Djibouti")
    (593 . "Ecuador")
    (20 . "Egypt")
    (503 . "El Salvador")
    (240 . "Equatorial Guinea")
    (291 . "Eritrea")
    (372 . "Estonia")
    (251 . "Ethiopia")
    (500 . "Falkland Islands")
    (298 . "Faroe Islands")
    (679 . "Fiji")
    (358 . "Finland")
    (33 . "France")
    (594 . "French Guiana")
    (689 . "French Polynesia")
    (241 . "Gabon")
    (220 . "Gambia")
    (49 . "Germany")
    (233 . "Ghana")
    (350 . "Gibraltar")
    (30 . "Greece")
    (299 . "Greenland")
    (590 . "Guadeloupe")
    (671 . "Guam")
    (502 . "Guatemala")
    (245 . "Guinea - Bissau")
    (224 . "Guinea")
    (592 . "Guyana")
    (509 . "Haiti")
    (504 . "Honduras")
    (852 . "Hong Kong")
    (36 . "Hungary")
    (354 . "Iceland")
    (91 . "India")
    (62 . "Indonesia")
    (98 . "Iran")
    (964 . "Iraq")
    (353 . "Ireland Republic of")
    (972 . "Israel")
    (39 . "Italy")
    (225 . "Ivory Coast see Cote d'Ivorie")
    (81 . "Japan")
    (962 . "Jordan")
    (7 . "Kazakhstan")
    (254 . "Kenya")
    (7 . "Kirghizstan")
    (686 . "Kiribati")
    (850 . "North Korea")
    (82 . "South Korea")
    (965 . "Kuwait")
    (856 . "Laos")
    (371 . "Latvia")
    (961 . "Lebanon")
    (266 . "Lesotho")
    (231 . "Liberia")
    (370 . "Lithuania")
    (352 . "Luxembourg")
    (218 . "Lybia")
    (853 . "Macao")
    (389 . "Macedonia")
    (261 . "Madagascar")
    (265 . "Malawi")
    (60 . "Malaysia")
    (960 . "Maldives")
    (223 . "Mali")
    (356 . "Malta")
    (692 . "Marshall Islands")
    (596 . "Martinique")
    (222 . "Mauritania")
    (230 . "Mauritius")
    (269 . "Mayotte")
    (52 . "Mexico")
    (691 . "Micronesia")
    (373 . "Moldovia")
    (976 . "Mongolia")
    (212 . "Morocco")
    (258 . "Mozanbique")
    (95 . "Myanmar (Burma)")
    (264 . "Namibia")
    (977 . "Napal")
    (674 . "Nauru")
    (31 . "Netherlands (Holland)")
    (599 . "Netherlands Antilles")
    (687 . "New Caledonia")
    (505 . "Nicaragua")
    (227 . "Niger")
    (234 . "Nigeria")
    (47 . "Norway")
    (968 . "Oman")
    (92 . "Pakistan")
    (507 . "Panama")
    (675 . "Papua New Guinea")
    (595 . "Paraguay")
    (51 . "Peru")
    (63 . "Philippines")
    (649 . "Pitcain Island")
    (48 . "Poland")
    (361 . "Portugal")
    (974 . "Qatar")
    (40 . "Romania")
    (7 . "Russia")
    (250 . "Rwanda")
    (685 . "Samoa (USA)")
    (685 . "Samoa Western")
    (378 . "San Marino")
    (966 . "Saudi Arabia")
    (221 . "Senegal")
    (248 . "Seychelles")
    (232 . "Sierra Leone")
    (65 . "Singapore")
    (42 . "Slovakia")
    (386 . "Slovenia")
    (677 . "Solom Islands")
    (252 . "Somalia")
    (27 . "South Africa")
    (34 . "Spain")
    (94 . "Sri Lanka")
    (290 . "St Helena")
    (249 . "Sudan")
    (597 . "Surinam")
    (268 . "Swaziland")
    (46 . "Sweden")
    (41 . "Switzerland")
    (963 . "Syria")
    (886 . "Taiwan")
    (7 . "Tajikistan")
    (255 . "Tanzania")
    (66 . "Thailand")
    (228 . "Togo")
    (676 . "Tongo")
    (216 . "Tunisia")
    (90 . "Turkey")
    (7 . "Turkmenistan")
    (688 . "Tuvalu")
    (1 . "USA")
    (256 . "Uganda")
    (380 . "Ukraine")
    (971 . "United Arab Emirates")
    (44 . "United Kingdom")
    (598 . "Uraguay")
    (7 . "Uzbekistan")
    (678 . "Vanuatu")
    (58 . "Venezuela")
    (84 . "Vietnam")
    (967 . "Yemen")
    (381 . "Yugoslavia")
    (243 . "Zaire")
    (260 . "Zambia")
    (263 . "Zimbabwe"))
  "ISO 3166 country codes.")

(defvar eicq-outgoing-queue nil
  "Lists of outgoing queue to be sent.
Each queue consists of the binary string and the resend counter.")

(defun eicq-send (bin &optional count)
  "Same as `eicq-send-internal' except it uses a queue to slow down sending.
ICQ v5 server (not v2 server) responses slow and does not acknowledge all
packets if they are sent too quickly.  Now uses a queue to store all
outgoing packets, and send them out one by one in short but distinct
intervals.
BIN is the string to send.
COUNT means how many time this packets has been resent. Default is 0."
  (setq eicq-outgoing-queue
        (append eicq-outgoing-queue
                (list (list
                       (eicq-bin-int bin 16) ; seq-no
                       bin
                       (or count 0))))))

(defun eicq-send-queue-start ()
  "Start sending outgoing queue."
  (eicq-send-queue-stop)
  (start-itimer
   "eicq send-queue"
   (lambda ()
     (if eicq-outgoing-queue
         (let ((x (pop eicq-outgoing-queue)))
           (if (> (third x) 60)         ; exceed resend limit = 5
               (incf eicq-dropped-packet-counter)
             (if (zerop (mod (third x) 10))
                 (if (>= (third x) 10)
                     (incf eicq-resend-packet-counter))
                 (eicq-send-internal (second x)))
             ;; cycle the queue
             (eicq-send (second x) (1+ (third x)))))))
   0.5 0.5))

(defun eicq-send-queue-stop ()
  "Stop sending outgoing queue.."
  (setq eicq-outgoing-queue nil)
  (delete-itimer "eicq send-queue"))

;;; Code - network:

(defvar eicq-network nil
  "TCP network between Emacs and `icq2tcp'.")

(defvar eicq-bridge nil
  "Process `icq2tcp'.
It bridges UDP (ICQ server) and TCP (Emacs).")

(defun eicq-bridge-show-buffer ()
  "Switch to `eicq-bridge-buffer' for network dump info."
  (interactive)
  (switch-to-buffer eicq-bridge-buffer))

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

  (unless (or (and (processp eicq-bridge) ; running already
                   (eq (process-status eicq-network) 'run))
              (not eicq-local-bridge-p)   ; remote bridge
              eicq-bridge-port)           ; remote bridge
    (setq eicq-bridge-hostname "127.0.0.1")
    (setq eicq-bridge-port (+ 4000 (random 1000)))
    (eicq-log-system
     "Trying to run a local bridge %s at port %s..."
     eicq-bridge-hostname eicq-bridge-port)
    (setq eicq-bridge-buffer (get-buffer-create "*eicq bridge*"))
    (setq eicq-bridge
          (start-process
           "eicq bridge"
           "*eicq bridge*"
           eicq-bridge-filename
           eicq-server-hostname
           (number-to-string eicq-server-port)
           (number-to-string eicq-bridge-port)))

    ;; should we implement using filter and accept-process-output?

    (message "Starting up bridge...")
    ;; wait for `icq2tcp' to execute
    (accept-process-output eicq-bridge 5)
    (message "Starting up network...")
    ;; wait for `icq2tcp' to setup sockets
    (sleep-for 2))

  (unless (and eicq-local-bridge-p
               (eicq-connected-p))
    (eicq-log-system
     "Trying to connect to the bridge %s at port %s..."
     eicq-bridge-hostname eicq-bridge-port)
    (setq eicq-network
          (condition-case nil
              (open-network-stream
               "eicq network" nil eicq-bridge-hostname eicq-bridge-port)
            (file-error nil))))          ; eicq-network = nil if fails

  (cond
   ((and (eicq-connected-p)
         eicq-local-bridge-p)
    (set-process-sentinel eicq-bridge 'eicq-bridge-kill)
    (set-process-sentinel eicq-network 'eicq-network-kill)
    (set-process-filter eicq-network 'eicq-network-filter)
    (with-current-buffer eicq-bridge-buffer
      (eicq-bridge-mode)))
   ((and (eicq-connected-p)
         (not eicq-local-bridge-p))
    (set-process-sentinel eicq-network 'eicq-network-kill)
    (set-process-filter eicq-network 'eicq-network-filter))
   (t
    (eicq-log-system "....connection failed"))))



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
    (define-key map [?4] 'eicq-bridge-show-buffer)
    map)
  "Keyboard map common for `eicq-log-mode-map' and `eicq-buddy-mode-map'.")

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
    ["Bridge Buffer" eicq-bridge-show-buffer t]
    "---"
    ["Email Author" eicq-email-author t]
    ["Submit Bug Report" (eicq-report-bug eicq-blurb) t]
    ["Customize" eicq-customize t])
  "Menu for both `eicq-log-mode' and `eicq-buddy-mode'.")

(easy-menu-define
 eicq-main-easymenu nil "Eicq Main" eicq-main-menu)

(defun eicq-bridge-mode ()
  "Major mode for bridge debug output.
Commands: \\{eicq-main-mode}"
  (make-local-variable 'kill-buffer-hook)
  (kill-all-local-variables)
  (use-local-map eicq-main-map)
  (setq mode-name "eicq-bridge")
  (setq major-mode 'eicq-bridge-mode)
  (easy-menu-add eicq-main-easymenu)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list
   'kill-buffer-query-functions
   (lambda ()
     (if (y-or-n-p "Terminate ICQ? ")
         (eicq-logout 'kill))))
  (make-local-variable 'kill-buffer-hook)
  (add-hook
   'kill-buffer-hook
   (lambda () "Kill bridge buffer."
     (eicq-network-kill)
     (eicq-bridge-kill))))

(defun eicq-network-kill (&optional process change)
  "Kill `eicq-network'.
PROCESS and CHANGE is for `set-process-sentinel'."
  (if (processp eicq-network) (delete-process eicq-network))
  (setq eicq-network nil))

(defun eicq-bridge-kill  (&optional process change)
  "Kill `eicq-network'.
PROCESS and CHANGE is for `set-process-sentinel'."
  (if (processp eicq-bridge) (delete-process eicq-bridge))
  (setq eicq-bridge nil)
  (if (string= eicq-bridge-hostname "127.0.0.1")
      (setq eicq-bridge-port nil)))

(defvar eicq-frame nil
  "The frame where EICQ is displayed.")

(defvar eicq-wharf-frame)

(defun eicq-disconnect ()
  "Log out of ICQ and close all Eicq buffers."
  (interactive)
  (eicq-logout 'kill)
  (eicq-network-kill)
  (eicq-bridge-kill)
  (loop for each in '(eicq-log-buffer 
		      eicq-buddy-buffer 
		      eicq-status-buffer
		      eicq-bridge-buffer)
    do (kill-buffer (symbol-value each)))
  (delete-other-windows)
  (if eicq-start-in-new-frame
      (delete-frame eicq-frame))
  (setq eicq-frame nil)
  (if (frame-live-p eicq-wharf-frame)
      (delete-frame eicq-wharf-frame))
  (setq eicq-wharf-frame nil))

(defun eicq-connected-p ()
  "Return non-nil if the network is ready for sending string."
  (and (processp eicq-network)
       (eq (process-status eicq-network) 'open)))

(defun eicq-send-internal (bin)
  "Send a binary string BIN to `eicq-network'.
`process-send-string' restricts the length of BIN to 500 or less."
  (if (> (length bin) (+ eicq-message-max-size 50))
      (eicq-log-error
       "You try to send a packet of length %s.\n
It is sent anyway but it may not go through.\n"
       (length bin)
       eicq-message-max-size))

  (if (eicq-connected-p)
      (process-send-string
       eicq-network
       (concat (eicq-int-bin (length bin))
               bin))
    (eicq-log-error
     "Network is not connected when it tries to send a packet")
    (eicq-logout 'kill)))

(defun eicq-network-filter (process bin)
  "Handle a binary string from `eicq-network'.
PROCESS is the process which invokes this filter.
BIN is a binary string from process."
  (setq eicq-recent-packet bin)
  (let ((bin-list (eicq-network-separator bin)))
    (loop for each in bin-list
      do (eicq-do each))))

(defvar eicq-trimmed-packet nil
  "*Last incomplete packet.
Due to limited buffer size of Emacs network buffer, packets can be trimmed
and attached at the beginning of next callback.  Use this in
`eicq-network-separator' to concatenate a packet across two callbacks.
Usually only one per 1000 packets needs this.")

(defun eicq-network-separator (bin)
  "Separate multiple packets BIN into a list of packet.
See `eicq-network-filter'."
  ;; there can be as many as 15 packets or as less as 1 in bin
  (let (len packets)
    (setq bin (concat eicq-trimmed-packet bin))
    (while (not (zerop (length bin)))
      (setq len (eicq-bin-int bin))
      (cond
       ((> (+ 2 len) (length bin))
        ;; last trimmed packet
        (incf eicq-trimmed-packet-counter)
        (setq eicq-trimmed-packet bin)
        (setq bin nil))                 ; to quit while loop
       (t
        (push (substring bin 2 (+ 2 len)) packets)
        (setq bin (substring bin (+ 2 len)))
        (if (zerop (length bin))
            (setq eicq-trimmed-packet nil)))))
    packets))

;;; Code - client to server packets:

(defvar eicq-session-id
  (concat
   (eicq-int-bin (random 65535))
   (eicq-int-bin (random 65535)))
  "Random number used by ICQ client to anti-spoof.")

(defvar eicq-current-seq-num 1
  "Current sequence number in packet.")

(defvar eicq-user-bin nil
  "User alias in binary string.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defun eicq-pack (command &rest parameters)
  "Pack a ICQ protocol version 5 packet.
COMMAND is a hex string.
PARAMETERS is a binary string.
Return a binary string.
Use `eicq-send' to send the string."
  (concat "\x05\x00"
          "\x00\x00\x00\x00"
          eicq-user-bin
          eicq-session-id
          command
          (eicq-int-bin (incf eicq-current-seq-num))
          (eicq-int-bin eicq-current-seq-num)
          "\x00\x00\x00\x00"            ; checkcode
          (if parameters
              (apply 'concat parameters)
            "\x00\x00\x00\x00")))       ; must have at least 4 bytes

(defun eicq-pack-login ()
  "Pack login packet 03e8."
  (let* ((password (or eicq-user-password
                       (read-passwd "password: ")))
         (password-len (eicq-int-bin (1+ (length password))))
         (status (eicq-status-bin eicq-user-initial-status)))
    (eicq-pack
     "\xe8\x03"
     ;; "\x5f\x71\x69\x37"                      ; time
     "\x00\x00\x00\x00"                 ; time
     "\x00\x00\x00\x00"                 ; port (auto-detect)
     password-len
     password "\x00"
     "\x72\x00\x04\x00"                 ; x1
     "\x00\x00\x00\x00"                 ; ip (auto-detect)
     "\x06"                             ; x2 _d95_
     status                             ; initial status
     ;; FIXME invisible has 2 bytes while others only one
     ;; must be \x00 so that initial status works
     "\x00\x00\x00\x00"                 ; x3 _d95_
     "\x02\x00"                         ; seq num, WHY 2?
     "\x00\x00\x00\x00"                 ; x4
     "\x04\x00\x72\x00")))

(defun eicq-pack-keep-alive ()
  "Pack keep alive packet 042e."
  (eicq-pack "\x2e\x04"))

(defun eicq-pack-logout ()
  "Pack logout packet using 0438."
  ;; also used to send text code to server
  (eicq-pack
   "\x38\x04"
   "\x14\x00B_USER_DISCONNECTED\x00\x05\x00"))

(defun eicq-pack-delete-offline-messages ()
  "Pack delete offline message packet 0442."
  (eicq-pack "\x42\x04"))

(defvar eicq-all-uin nil
  "All uin in `eicq-world'.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defun eicq-pack-contact-list ()
  "Pack contact list packet 0406."
  ;; It first makes a large binary string of all uin, then chops into
  ;; chuncks and send them separately with `eicq-send-contact-list'.
  ;; each uin = 4, 4 * max 50 = 200 bytes, to avoid large packets
  ;; Packets < 400 can go through but not readily acknowledged (need
  ;; resending). Small-sized packets are good.
  (let* ((all-uin-bin  (mapconcat 'eicq-uin-bin eicq-all-uin nil))
         (n (length all-uin-bin))
         (i 0)
         packets)
    (while (< i n)
      (let* ((bin (substring all-uin-bin i (min (incf i 200) n)))
             ;; since server does not do bound check,
             ;; wrong count will pop up with random uin
             (count (/ (length bin) 4)))
        (push
         (eicq-pack "\x06\x04" (eicq-int-byte count) bin)
         packets)))
    packets))

(defun eicq-pack-invisible-list ()      ; TODO
  "Pack visible list packet 06a4."
  ;; format same as eicq-pack-contact-list
  )

(defun eicq-pack-visible-list ()        ; TODO
  "Pack visible list packet 06ae."
  ;; format same as eicq-pack-contact-list
  )

(defvar eicq-message-types
  '(("\x01" . normal)
    ("\x02" . chat-request)
    ("\x04" . url)
    ("\x06" . request-auth)
    ("\x08" . authorize)
    ("\x0b" . request-auth)
    ("\x0c" . added)
    ("\x0d" . pager)
    ("\x0e" . email-express)
    ("\x0f" . email-check)
    ("\x1a" . card)
    ("\x13" . contact-list))
  "See `eicq-pack-send-message'.")

(defun eicq-pack-send-message (alias message type)
  "Pack send message of any kinds packet 010e.
ALIAS is the alias/uin to send to.
MESSAGE is the message body.
TYPE is the message type.
Possible TYPE: `eicq-send-message-types'."
  (eicq-pack
   "\x0e\x01"
   (eicq-alias-bin alias)
   (car (rassoc type eicq-message-types))
   "\x00"
   (eicq-int-bin (1+ (length message)))
   message "\x00"))

(defun eicq-pack-status-change (status)
  "Pack STATUS change packet 04d8."
  (eicq-pack
   "\xd8\x04"
   (eicq-status-bin status)))

(defun eicq-pack-register-new-user (password)
  "Pack register new user packet 03fc."
  (eicq-pack
   "\xfc\x03"
   (eicq-int-bin (length password))
   password
   "\xa0\x00\x00\x00"
   "\x24\x61\x00\x00"
   "\x00\x00\x00\x00"))

(defun eicq-pack-meta-user-change-password (password)
  "Pack meta user change password packet 064a:042e.
ALIAS is the alias/uin to query info for."
  (eicq-pack
   "\x4a\x06"
   "\x2e\x04"
   (eicq-int-bin (length password))
   password))

(defun eicq-pack-meta-user-query (alias)
  "Pack meta user query packet 064a:04b0.
ALIAS is the alias/uin to query info for."
  (eicq-pack
   "\x4a\x06"
   "\xb0\x04"
   (eicq-alias-bin alias)))

(defun eicq-pack-meta-user-update-general ()
  "Pack meta user packet 064a:03e8.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xe8\x03"
   (eicq-int-bin (length eicq-user-meta-nickname))
   eicq-user-meta-nickname
   (eicq-int-bin (length eicq-user-meta-firstname))
   eicq-user-meta-firstname
   (eicq-int-bin (length eicq-user-meta-lastname))
   eicq-user-meta-lastname
   (eicq-int-bin (length eicq-user-meta-primary-email))
   eicq-user-meta-primary-email
   (eicq-int-bin (length eicq-user-meta-secondary-email))
   eicq-user-meta-secondary-email
   (eicq-int-bin (length eicq-user-meta-old-email))
   eicq-user-meta-old-email
   (eicq-int-bin (length eicq-user-meta-city))
   eicq-user-meta-city
   (eicq-int-bin (length eicq-user-meta-state))
   eicq-user-meta-state
   (eicq-int-bin (length eicq-user-meta-phone))
   eicq-user-meta-phone
   (eicq-int-bin (length eicq-user-meta-fax))
   eicq-user-meta-fax
   (eicq-int-bin (length eicq-user-meta-street))
   eicq-user-meta-street
   (eicq-int-bin (length eicq-user-meta-cell-phone))
   eicq-user-meta-cell-phone
   ;; zip code? only accept valid values
   "\x00\x00\x00\x00"
   (eicq-int-bin (car (rassoc eicq-user-meta-country eicq-country-code)))))

(defun eicq-pack-meta-user-security ()
  "Pack meta user packet 064a:0424."
  (eicq-pack
   "\x4a\x06"
   "\x24\x04"
   (if eicq-user-meta-authorization
       "\x00"
     "\x01")
   (if eicq-user-meta-web-aware
       "\x01" 
     "\x00")
   (if eicq-user-meta-hide-ip 
       "\x00" 
     "\x01")))

(defun eicq-pack-meta-user-update-work ()
  "Pack meta user packet 064a:03f2.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xf2\x03"
   (eicq-int-bin (length eicq-user-meta-work-city))
   eicq-user-meta-work-city
   (eicq-int-bin (length eicq-user-meta-work-state))
   eicq-user-meta-work-state
   (eicq-int-bin (length eicq-user-meta-work-phone))
   eicq-user-meta-work-phone
   (eicq-int-bin (length eicq-user-meta-work-fax))
   eicq-user-meta-work-fax
   (eicq-int-bin (length eicq-user-meta-work-address))
   eicq-user-meta-work-address
   ;; unknown
   "\x00\x00\x00\x00\x00\x00"
   (eicq-int-bin (length eicq-user-meta-work-company))
   eicq-user-meta-work-company
   (eicq-int-bin (length eicq-user-meta-work-department))
   eicq-user-meta-work-department
   (eicq-int-bin (length eicq-user-meta-work-position))
   eicq-user-meta-work-position
   ;; unknown
   "\x00\x00"
   (eicq-int-bin (length eicq-user-meta-work-homepage))
   eicq-user-meta-work-homepage))

(defun eicq-pack-meta-user-update-more ()
  "Pack meta user packet 064a:03fc.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xfc\x03"
   (eicq-int-bin eicq-user-meta-age)
   (case eicq-user-meta-sex
     (male "\x02")
     (female "\x01")
     (otherwise "\x00"))
   (eicq-int-bin (length eicq-user-meta-homepage))
   eicq-user-meta-homepage
   (eicq-int-byte eicq-user-meta-birth-year)
   (eicq-int-byte eicq-user-meta-birth-month)
   (eicq-int-byte eicq-user-meta-birth-day)
   (eicq-int-byte eicq-user-meta-language-1)
   (eicq-int-byte eicq-user-meta-language-2)
   (eicq-int-byte eicq-user-meta-language-3)))

(defun eicq-pack-meta-user-update-about ()
  "Pack meta user packet 064a:0406.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\x06\x04"
   (eicq-int-bin (length eicq-user-meta-about))
   eicq-user-meta-about))

(defun eicq-pack-search-by-uin (uin) ; TODO
  "Pack search by UIN packet 041a."
  (eicq-pack "\x1a\x04"))

(defun eicq-pack-search (nick-name first-name last-name email)
  "Pack search packet 0424."
  ;; a) 1+ (length nickname) and nickname appended with \x00
  ;; b) (length nickname) and nickname _without \x00
  ;; BOTH are correct! C dude!
  (eicq-pack
   "\x24\x04"
   (eicq-int-bin (1+ (length nick-name)))
   nick-name "\x00"
   (eicq-int-bin (1+ (length first-name)))
   first-name "\x00"
   (eicq-int-bin (1+ (length last-name)))
   last-name "\x00"
   (eicq-int-bin (1+ (length email)))
   email "\x00"))

; (defun eicq-pack-update-authorization ()
;   "Pack update authorization packet 0514"
;   (eicq-pack
;    "\x14\x05"
;    (if eicq-user-meta-authorization
;        "\x00\x00\x00\x00"
;      "\x01\x00\x00\x00")))

(defun eicq-pack-add-user-to-contact-list ()
  "Pack add packet 053c."
  (eicq-pack "\x3c\x05"))

(defvar eicq-random-groups
  '(("general" . "\x01\x00\x00\x00")
    ("romance" . "\x02\x00\x00\x00")
    ("games" . "\x03\x00\x00\x00")
    ("students" . "\x04\x00\x00\x00")
    ("age-20" . "\x06\x00\x00\x00")
    ("age-30" . "\x07\x00\x00\x00")
    ("age-40" . "\x08\x00\x00\x00")
    ("age-50+" . "\x09\x00\x00\x00")
    ("women-wanted" . "\x0a\x00\x00\x00")
    ("man-wanted" . "\x0b\x00\x00\x00"))
  "Random user groups.")

(defun eicq-pack-set-random-group (group)
  "Pack set random group 0564."
  (eicq-pack
   "\x64\x05"
   (cdr (assoc group eicq-random-groups))))

(defun eicq-pack-search-random-user (group)
  "Pack search random user 056e."
  (eicq-pack
   "\x6e\x05"
   (cdr (assoc group eicq-random-groups))))

(defun eicq-pack-request-authorization ()
  "Pack request authorization packet 0456."
  (eicq-pack "\x56\x04"))

(defun eicq-pack-query-servers ()
  "Pack query servers packet 04ba."
  (eicq-pack "\xba\x04"))

(defun eicq-pack-query-addons ()
  "Pack query addons packet 04c4."
  (eicq-pack "\xc4\x04"))

(defun eicq-pack-new-account-permission ()
  "Pack new account permission packet 04ec."
  (eicq-pack "\xec\x04"))

(defun eicq-pack-new-account-register ()
  "Pack register new account packet 03fc."
  (eicq-pack "\xfc\x03"))

(defun eicq-pack-cmd-x1 ()
  "Pack unknown command x1 packet 0442."
  (eicq-pack "\x42\x04"))

(defun eicq-pack-send-message-to-foreigner ()
  "Pack send message to foreigner packet 0456.
Foreigner is someone not on my contact list.
Also used to request permission to add someone
with 'authorized' status to my contact list."
  (eicq-pack "\x56\x04"))

;; protocol v2-specific (obsoleted)
;; - update info 04a6 \xa6\x04
;; - update extended info 04b0 \xb0\x04
;; - login 1 044c \x4c\x04
;; - login 2 0528 \x28\x05
;; - change password 049c \x04\x9c

;;; Code - server to client packets:

(defun eicq-redo-hex (hex)
  "Debug packet HEX.
Copy hex debug output from `icq2tcp' and use this function to re-do the
packet to simulate a server reply."
  (interactive "sHex: ")
  (let ((bin (eicq-hex-bin hex)))
    (eicq-network-filter
     nil
     (concat (eicq-int-bin (length bin)) bin))))

(defvar eicq-do-alist
  '(("\x0a\x00" . eicq-do-ack)
    ("\x1e\x00" . eicq-do-unknown)      ; invalid command?
    ("\x28\x00" . eicq-do-kicked-out)
    ("\x46\x00" . eicq-do-new-account-uin)
    ("\x5a\x00" . eicq-do-login-confirm)
    ("\x64\x00" . eicq-do-wrong-password)
    ("\x6e\x00" . eicq-do-online)
    ("\x78\x00" . eicq-do-offline)
    ("\x82\x00" . eicq-do-query-servers-reply)
    ("\x8c\x00" . eicq-do-search-found)
    ("\xa0\x00" . eicq-do-search-end)
    ("\xb4\x00" . eicq-do-unknown)      ; for v2
    ("\xc8\x00" . eicq-do-update-info-ext-confirm) ; for v2 extended update succeeded
    ("\xd2\x00" . eicq-do-update-info-ext-fail) ; for v2 extended update failed
    ("\xdc\x00" . eicq-do-offline-message)
    ("\xe6\x00" . eicq-do-offline-message-complete)
    ("\xf0\x00" . eicq-do-kicked-out)
    ("\xfa\x00" . eicq-do-already-logged-in)
    ("\x18\x01" . eicq-do-info)
    ("\x22\x01" . eicq-do-info-ext)
    ("\x2c\x01" . eicq-do-info-not-available)
    ("\x36\x01" . eicq-do-info-ext-not-available)
    ("\xa4\x01" . eicq-do-status-update)
    ("\xc2\x01" . eicq-do-system-message)
    ("\xc8\x01" . eicq-do-update-info-ext-confirm)
    ("\xe0\x01" . eicq-do-update-info-confirm)
    ("\xea\x01" . eicq-do-update-info-fail)
    ("\xf4\x01" . eicq-do-update-authorization-confirm)
    ("\xfe\x01" . eicq-do-update-authorization-fail)
    ("\x04\x01" . eicq-do-instant-message)
    ("\x12\x02" . eicq-do-multi)
    ("\x1c\x02" . 0)                    ; contact list confirm
    ("\x4e\x02" . eicq-do-search-random-user-found)
    ("\x58\x02" . eicq-do-search-random-user-found) ; random search failed?
    ("\xde\x03" . eicq-do-meta-user))
  "Handlers for server packet.
Each handler accepts one parameter: PACKET.
PACKET is raw network packet from server in binary string.
0 means to ignore the packet.
See `eicq-do-*'")

(defvar seq-num-bin)
(defvar seq-num)
(defvar user-bin)

(defun eicq-do (packet &optional no-ack)
  "Dispatch server packet to registered handler.
See `eicq-do-alist'.
PACKET is a binary string.
Non-nil NO-ACK means no acknowledgement packet will be sent."
  (let* ((void-ack-commands '("\x0a\x00" "\xf0\x00" "\x28\x00"))
         (version (if (> (length packet) 2) (substring packet 0 2)))
         command seq-num handler)
    (cond
     ((equal version "\x05\x00")
      (setq command (substring packet 7 9)
            seq-num-bin (substring packet 9 13)
            user-bin (substring packet 13 17)
            handler (cdr (assoc command eicq-do-alist)))
      ;; acknowledge
      (unless (or no-ack (member command void-ack-commands))
        ;; use internal to ack fast
        (eicq-send-internal
         (concat "\x05\x00"
                 "\x00\x00\x00\x00"
                 ;; use user-bin from packet instead of eicq-user-bin to
                 ;; handle special case of registering new user
                 user-bin
                 eicq-session-id
                 "\x0a\x00"             ; ack command
                 seq-num-bin
                 "\x00\x00\x00\x00"     ; checkcode
                 "\x00\x00\x00\x00")))  ; stub
      ;; do handler
      (cond
       ((and (numberp handler) (zerop handler)) nil) ; ignored packet
       ((and handler (fboundp handler)) ; registered and valid handler
        (funcall handler packet))
       (t (eicq-do-unknown packet))))   ; unknown packets
     ('invalid-packets
      (push (cons 'invalid-packet eicq-recent-packet) eicq-error-packets)
      ;; hack: error in concatenating trimmed packet may cause invalid
      ;; packets, reset eicq-trimmed-packet
      (setq eicq-trimmed-packet nil)))))

(defun eicq-do-ack (packet)
  "Handle server command 00a0 in PACKET.
Remove acknowledged packets from `eicq-outgoing-queue'."
  (let ((seq-num (eicq-bin-int packet 9)))
    (setq eicq-outgoing-queue
          (delete* seq-num eicq-outgoing-queue :key 'car))))

(defun eicq-do-unknown (packet)
  "Handle any unknown PACKET."
  (push (cons 'unknown-command eicq-recent-packet)
        eicq-error-packets)
  (eicq-log-error
   "Unknown command: %s"
   (eicq-bin-hex (substring packet 7 9))))

(defun eicq-do-wrong-password (packet)
  ;; not authorized?
  "Handle server command 0064 in PACKET."
  (eicq-log-error "Your password is invalid"))

(defun eicq-do-kicked-out (packet)
  "Handle server command 0028 or 00f0 in PACKET."
  (eicq-log-error "You are kicked out of ICQ server")
  (eicq-logout 'kill))

(defun eicq-do-already-logged-in (packet)
  "Handle server command 00fa PACKET."
  (eicq-log-error "You are already logged in."))

(defun eicq-do-offline-message-complete (packet)
  "Handle server command 00e6 in PACKET."
  (eicq-delete-offline-messages))

(defun eicq-do-instant-message (packet)
  "Handle server command 0104 in PACKET."
  (eicq-do-message-helper
   (substring packet 21 25)
   (substring packet 29 -1)
   (substring packet 25 26)))

(defvar local-year)

(defun eicq-do-offline-message (packet)
  "Handle server command 00dc in PACKET."
  (let* ((year (eicq-bin-int packet 25))
         (month (eicq-byte-int packet 27))
         (day (eicq-byte-int packet 28))
         (hour (1- (eicq-byte-int packet 29)))
         (min (eicq-byte-int packet 30))
         (monthname (aref eicq-monthnames month))
         (local-time
          (timezone-fix-time
           (format "%s %s %s:%s %s"
                   monthname day hour min year)
           nil nil))
         (local-year (aref local-time 0))
         (local-monthname (aref eicq-monthnames
                                (aref local-time 1)))
         (local-day (aref local-time 2))
         (local-hour (aref local-time 3))
         (local-min (aref local-time 4)))

    (eicq-do-message-helper
     (substring packet 21 25)
     (format "(%s %02s) %02s:%02s\n%s"
             local-monthname local-day local-hour local-min
             (substring packet 35 -1))
     (substring packet 31 32))))

(defvar eicq-auto-reply-p nil
  "If non-nil Eicq will not automatically set your state to online.

It is used in `eicq-do-message-helper' and `eicq-send-message-helper'.")

(defvar eicq-user-auto-away-p nil
  "This variable is set when the auto-away timer expires, 
and it is reset in eicq-send-message-helper and eicq-change-status.")

(defun eicq-do-message-helper (uin-bin message type-bin)
  "Helper for handling offline and online messages.
UIN-BIN is uin of message sender in binary string.
MESSAGE is message body of any type.
TYPE-BIN is type of message in binary string.
Possible type: `eicq-message-types'."
  (let ((alias (eicq-bin-alias uin-bin))
        type url)
    ;; do not decode here; may contain delimiter \xfe
    (setq type (cdr (assoc type-bin eicq-message-types)))
    (add-to-list 'eicq-active-aliases alias)
    (when (eq type 'url)
      (setq url (split-string message "\xfe"))
      ;; message = description part
      ;; url = hyperlink part
      (setq message (eicq-decode-string (first url)))
      (setq url (eicq-decode-string (second url))))

    (if eicq-auto-response-messages-p
	(when (member eicq-user-status '("away" "na" "dnd" "occ"))
	  (if eicq-user-auto-away-p
	      (progn
		(setq eicq-auto-reply-p t)
		(eicq-idle-reply alias))
	    (eicq-auto-reply alias))))

    (run-hooks 'eicq-new-message-hook)
    
    (case type
      (normal 
       (eicq-log-buddy-message 
	alias (eicq-decode-string message))
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat 
	     eicq-sound-directory 
	     (cdr (assoc 'message-sound eicq-sound-alist))))))
      (chat-request 
       (eicq-log-buddy-message 
	alias "Request chat")
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat 
	     eicq-sound-directory 
	     (cdr (assoc 'chat-sound eicq-sound-alist))))))
      (url 
       (eicq-log-buddy-url 
	alias message url)
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat 
	     eicq-sound-directory 
	     (cdr (assoc 'url-sound eicq-sound-alist))))))
      (authorize 
       (eicq-log-buddy-message 
	alias "You are authorized")
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat
	     eicq-sound-directory 
	     (cdr (assoc 'auth-sound eicq-sound-alist))))))
      (request-auth
       (eicq-log-buddy-message
        alias "Request authorization =\n%s"
        (eicq-decode-string
         (replace-in-string message "\xfe" "\n"))))
      (added (eicq-log-buddy-message alias "You are added"))
      (pager 
       (eicq-log-buddy-message 
	alias "Pager = %s"
	(eicq-decode-string
	 (replace-in-string message "[\xfe]+" "\n")))
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat
	     eicq-sound-directory 
	     (cdr (assoc 'pager-sound eicq-sound-alist))))))
      (email-express
       (eicq-log-buddy-message
        alias "Email express = %s"
        (eicq-decode-string
         (replace-in-string message "[\xfe]+" "\n")))
       (if (string= eicq-use-sound-flag "sound-card")
	   (play-sound-file
	    (concat
	     eicq-sound-directory 
	     (cdr (assoc 'emailx-sound eicq-sound-alist))))))
      (email-check (eicq-log-buddy-message alias "Email check"))
      (card
       (eicq-log-buddy-message
        alias "Card = %s" (eicq-decode-string message)))
      (contact-list
       (eicq-log-buddy-message
        alias "Contact list = %s"
        (eicq-decode-string
         (replace-in-string message "\xfe" "\n"))))
      (otherwise (push (cons 'unknown-message-types
                             eicq-recent-packet)
                       eicq-error-packets)
                 (eicq-log-error "Unknown message type: %s"
                                 (eicq-bin-hex type-bin))))))

(defun eicq-auto-reply (alias)
  "Auto-reply to ALIAS/uin depending on `eicq-user-status'.
Called by `eicq-do-message-heler'."
  (let ((message (symbol-value (eicq-status-auto-reply eicq-user-status))))
    (if message
        (eicq-send-message message alias))))

(defun eicq-idle-reply (alias)
  "Auto-reply to ALIAS/uin depending on `eicq-user-status'.
Called by `eicq-do-message-heler'."
  (let ((message (symbol-value (eicq-status-idle-reply eicq-user-status))))
    (if message
        (eicq-send-message message alias))))

(defun eicq-do-status-update (packet)
  "Handle server command 01a4 in PACKET."
  (let ((alias (eicq-bin-alias packet 21))
        (status (eicq-status-name (substring packet 25 26))))
    (run-hooks 'eicq-status-update-hook)
    (eicq-buddy-update-status alias status)))

(defun eicq-do-online (packet)
  "Handle server command 006e in PACKET."
  (let ((alias (eicq-bin-alias packet 21))
        (status (eicq-status-name (substring packet 38 39)))
        (ip (eicq-bin-ip packet 25))
        (port (eicq-bin-uin packet 29))
        (real-ip (eicq-bin-ip packet 33)))
    (if (eicq-valid-uin-p alias)
        (push (cons 'unknown-alias eicq-recent-packet)
              eicq-error-packets))
    (eicq-buddy-update-status alias status)
    (if (string= eicq-use-sound-flag "sound-card")
	(play-sound-file
	 (concat 
	  eicq-sound-directory 
	  (cdr (assoc 'buddy-sound eicq-sound-alist)))))
    (eicq-world-putf alias 'ip ip)
    (eicq-world-putf alias 'port port)
    (eicq-world-putf alias 'real-ip real-ip)))

(defun eicq-do-offline (packet)
  "Handle server command 0078 in PACKET."
  (let ((alias (eicq-bin-alias packet 21)))
    (eicq-buddy-update-status alias "offline")))

(defun eicq-do-login-confirm (packet)
  "Handle server command 005a in PACKET."
  (eicq-log-debug "Successfully logged in to ICQ server")
  (eicq-change-status eicq-user-initial-status 'no-network)
  ;; not used by v5
  ;; (eicq-send (eicq-pack-login-1))
  ;; (eicq-send (eicq-pack-login-2))
  (eicq-keep-alive-start)
  (eicq-send-contact-list)
  (message "Welcome to Eicq...")
  (eicq-show-window))

(defun eicq-do-system-message (packet)  ; TODO
  "Handle server command 01c2 in PACKET."
  (run-hooks 'eicq-system-message-hook))

(defun eicq-do-info (packet)
  "Handle server command 0118 in PACKET.
Server response to `eicq-pack-info-request'."
  (let* ((uin (eicq-bin-uin packet 21))
         (alias (eicq-uin-alias uin))
         (i 25)
         (nick-name-len (eicq-bin-int packet i))
         (nick-name (substring packet
                               (incf i 2) (1- (incf i nick-name-len))))
         (first-name-len (eicq-bin-int packet i))
         (first-name (substring packet
                                (incf i 2) (1- (incf i first-name-len))))
         (last-name-len (eicq-bin-int packet i))
         (last-name (substring packet
                               (incf i 2) (1- (incf i last-name-len))))
         (email-len (eicq-bin-int packet i))
         (email (substring packet (incf i 2) (1- (incf i email-len))))
         (authorization (eicq-byte-int packet i)))
    (eicq-log-info
     (eicq-decode-string
      (format
       "Query result =

          uin: %s
  Local alias: %s
    Nick name: %s
   First name: %s
    Last name: %s
        Email: %s
Authorization: %s"
       uin alias nick-name first-name last-name email
       (if (= authorization 0) "Needed" "Not Needed"))))))

(defvar country-status)

(defun eicq-do-info-ext (packet)
  "Handle server command 0122 in PACKET.
Server response to `eicq-pack-info-ext-request'."
  (let* ((uin (eicq-bin-uin packet 21))
         (alias (eicq-uin-alias uin))
         (i 25)
         (city-len (eicq-bin-int packet i))
         (city (substring packet (incf i 2) (1- (incf i city-len))))
         (country (eicq-bin-int packet i))
         (country-status (substring packet (incf i 2) (incf i)))
         (state-len (eicq-bin-int packet i))
         (state (substring packet (incf i 2) (1- (incf i state-len))))
         (age (eicq-bin-int packet i))
         (sex (substring packet (incf i 2) (incf i)))
         (phone-len (eicq-bin-int packet i))
         (phone (substring packet (incf i 2) (1- (incf i phone-len))))
         (homepage-len (eicq-bin-int packet i))
         (homepage (substring packet (incf i 2) (1- (incf i homepage-len))))
         (about-len (eicq-bin-int packet i))
         (about (substring packet (incf i 2) (1- (incf i about-len)))))
    (eicq-log-info
     (eicq-decode-string
      (format
       "Extended query result =

        uin: %s
Local alias: %s
    Country: %s
      State: %s
       City: %s
        Age: %s
        Sex: %s
      Phone: %s
   Homepage: %s
      About:

%s"
       uin alias
       (cdr (assoc country eicq-country-code))
       state city
       (if (= age 65535) "not entered" age)
       (cond
        ((string= sex "\x00") "not entered")
        ((string= sex "\x01") "female")
        ((string= sex "\x02") "male"))
       phone homepage about)))))

(defun eicq-do-info-not-available (packet)
  "Handle server command 012c in PACKET."
  (eicq-log-info "No info available."))

(defun eicq-do-info-ext-not-available (packet)
  "Handle server command 0136 in PACKET."
  (eicq-log-info "No extended info available."))

(defun eicq-do-search-found (packet)
  "Handle server command 018c in PACKET."
  (eicq-do-info packet))

(defun eicq-do-search-end (packet)
  "Handle server command 00a0 in PACKET."
  (let ((result (eicq-byte-int packet 8)))
    (case result
      (0 (eicq-log-info "All search results returned"))
      (1 (eicq-log-info "Too many seach results")))))

(defun eicq-do-update-info-confirm (packet)
  "Handle server command 01e0 in PACKET."
  (eicq-log-info "Update info succeeded"))

(defun eicq-do-update-info-fail (packet)
  "Handle server command 01ea in PACKET."
  (eicq-log-info "Update info failed"))

(defun eicq-do-update-authorization-confirm (packet)
  "Handle server command 01f4 in PACKET."
  (eicq-log-info "Update authorization succeeded"))

(defun eicq-do-update-authorization-fail (packet)
  "Handle server command 01fe in PACKET."
  (eicq-log-info "Update authorization failed"))

(defun eicq-do-update-info-ext-confirm (packet)
  "Handle server command 01c8 in PACKET."
  (eicq-log-info "Update extended info succeeded"))

(defun eicq-do-new-account-uin (packet)
  "Handle server command 0046 in PACKET."
  (eicq-log-info
   "New uin: %s"
   (eicq-bin-uin packet 13)))

(defun eicq-do-query-servers-reply (packet) ; TODO
  "Handle server command 0082 in PACKET."
  )

(defun eicq-do-search-random-user-found (packet)
  "Handle server command 0258 in PACKET."
  (if (< (length packet) 30)
      (eicq-log-info "Random user search failed")
    (eicq-query-info (eicq-bin-uin packet 21))))

(defun eicq-do-multi (packet)
  "Handle server command 0212 in PACKET."
  ;; don't use eicq-network-separator or screws up eicq-trimmed-packet
  (let (len bin)
    (setq bin (substring packet 22))
    (while (not (zerop (length bin)))
      (setq len (eicq-bin-int bin))
      (cond
       ((> (+ 2 len) (length bin))
        (eicq-log-error "Error in processing multi packet")
        (setq bin nil))                 ; to quit while loop
       (t
        (eicq-do (substring bin 2 (+ 2 len)) 'no-ack)
        (setq bin (substring bin (+ 2 len))))))))

(defvar eicq-do-meta-alist
  '(("\x64\x00" . eicq-do-meta-user-update-general-confirm)
    ("\x6e\x00" . eicq-do-meta-user-update-work-confirm)
    ("\x78\x00" . eicq-do-meta-user-update-more-confirm)
    ("\x82\x00" . eicq-do-meta-user-update-about-confirm)
    ("\xaa\x00" . eicq-do-meta-user-password)
    ("\xc8\x00" . eicq-do-meta-user-general)
    ("\xd2\x00" . eicq-do-meta-user-work)
    ("\xdc\x00" . eicq-do-meta-user-more)
    ("\xe6\x00" . eicq-do-meta-user-about)
    ("\xf0\x00" . eicq-do-meta-user-interest)
    ("\xfa\x00" . eicq-do-meta-user-background)
    ("\x0e\x01" . eicq-do-meta-user-picture)
    ("\x9a\x01" . eicq-do-meta-user-found))
  "Handlers for server packet meta user subcommands.")

(defun eicq-do-meta-user (packet)
  "Handle server command 03de in PACKET."
  (let* ((subcommand (substring packet 21 23))
         (result (substring packet 23 24))
         (data (substring packet 24))
         (handler (cdr (assoc subcommand eicq-do-meta-alist))))
    (if (not (equal result "\x0a"))
        (eicq-log-info "meta user command failed")
      (if (fboundp handler)
          (funcall handler data)
        (eicq-do-meta-user-unknown packet)))))

(defun eicq-do-meta-user-unknown (packet)
  "Handle server command 03de unknown subcommands in PACKET."
  (eicq-log-debug
   "meta user subcommand %s unhandled = %s"
   (eicq-bin-hex (substring packet 21 23))
   (eicq-bin-pretty-hex (substring packet 24))))

(defun eicq-do-meta-user-general (data)
  "Handle server command 03de subcommand 00c8 in DATA."
  (let* ((i 0)
         (nickname-len (eicq-bin-int data i))
         (nickname (substring data (incf i 2) (1- (incf i nickname-len))))
         (first-name-len (eicq-bin-int data i))
         (first-name
          (substring data (incf i 2) (1- (incf i first-name-len))))
         (last-name-len (eicq-bin-int data i))
         (last-name
          (substring data (incf i 2) (1- (incf i last-name-len))))
         (primary-email-len (eicq-bin-int data i))
         (primary-email
          (substring data (incf i 2) (1- (incf i primary-email-len))))
         (secondary-email-len (eicq-bin-int data i))
         (secondary-email
          (substring data (incf i 2) (1- (incf i secondary-email-len))))
         (old-email-len (eicq-bin-int data i))
         (old-email
          (substring data (incf i 2) (1- (incf i old-email-len))))
         (city-len (eicq-bin-int data i))
         (city (substring data (incf i 2) (1- (incf i city-len))))
         (state-len (eicq-bin-int data i))
         (state (substring data (incf i 2) (1- (incf i state-len))))
         (phone-len (eicq-bin-int data i))
         (phone (substring data (incf i 2) (1- (incf i phone-len))))
         (fax-len (eicq-bin-int data i))
         (fax (substring data (incf i 2) (1- (incf i fax-len))))
         (street-len (eicq-bin-int data i))
         (street (substring data (incf i 2) (1- (incf i street-len))))
         (cellphone-len (eicq-bin-int data i))
         (cellphone
          (substring data (incf i 2) (1- (incf i cellphone-len))))
         (zipcode (substring data i (incf i 4)))
         (country (eicq-bin-int data i))
         (timezone (substring data (incf i 2) (incf i)))
         (authorization (eicq-byte-int data i))
         (web-aware (substring data (incf i) (incf i)))
         (hide-ip (substring data i (incf i))))
    (eicq-log-info
     (eicq-decode-string
      (format
       "meta user general info =
Nickname: %s
Firstname: %s
Lastname: %s
Primary email: %s
Secondary email: %s
Old email: %s
City: %s
State: %s
Phone: %s
Fax: %s
Street: %s
Cellphone: %s
Zipcode: %s
Country: %s
Timezone: %s
Authorization: %s
Web-aware: %s
Hide-IP: %s"
       nickname first-name last-name primary-email secondary-email
       old-email city state phone fax street cellphone
       (eicq-bin-hex zipcode)
       (cdr (assoc country eicq-country-code))
       (eicq-bin-hex timezone)
       (if (= authorization 0) 
	   "Needed" 
	 "Not Needed")
       (eicq-bin-hex web-aware)
       (eicq-bin-hex hide-ip))))))

(defvar unknown)
(defvar unknown-2)

(defun eicq-do-meta-user-work (data)
  "Handle server command 03de subcommand 00d2 in DATA."
  (let* ((i 0)
         (city-len (eicq-bin-int data i))
         (city (substring data (incf i 2) (1- (incf i city-len))))
         (state-len (eicq-bin-int data i))
         (state (substring data (incf i 2) (1- (incf i state-len))))
         (phone-len (eicq-bin-int data i))
         (phone (substring data (incf i 2) (1- (incf i phone-len))))
         (fax-len (eicq-bin-int data i))
         (fax (substring data (incf i 2) (1- (incf i fax-len))))
         (address-len (eicq-bin-int data i))
         (address (substring data (incf i 2) (1- (incf i address-len))))
         (unknown (incf i 6))
         (company-len (eicq-bin-int data i))
         (company (substring data (incf i 2) (1- (incf i company-len))))
         (department-len (eicq-bin-int data i))
         (department (substring data (incf i 2) (1- (incf i department-len))))
         (position-len (eicq-bin-int data i))
         (position (substring data (incf i 2) (1- (incf i position-len))))
         (unknown-2 (incf i 2))
         (homepage-len (eicq-bin-int data i))
         (homepage
          (substring data (incf i 2) (1- (incf i homepage-len)))))
    ;; ignore empty data
    (when (> (length data) 35)
      (eicq-log-info
       (eicq-decode-string
        (format
         "meta user work info =
City: %s
State: %s
Phone: %s
Fax: %s
Address: %s
Company: %s
Department: %s
Position: %s
Homepage: %s"
         city state phone fax address company department position homepage))))))

(defun eicq-do-meta-user-more (data)
  "Handle server command 03de subcommand 00dc in DATA."
  (let* ((i 0)
         (age (eicq-bin-int data i))
         (sex (substring data (incf i 2) (incf i)))
         (homepage-len (eicq-bin-int data i))
         (homepage
          (substring data (incf i 2) (1- (incf i homepage-len))))
         (birth-year (eicq-byte-int data i))
         (birth-month (eicq-byte-int data (incf i)))
         (birth-day (eicq-byte-int data (incf i)))
         (language-1 (eicq-byte-int data (incf i)))
         (language-2 (eicq-byte-int data (incf i)))
         (language-3 (eicq-byte-int data (incf i))))
    ;; ignore empty data
    (when (> (length data) 12)
      (eicq-log-info
       (eicq-decode-string
        (format
         "meta user more info =
Age: %s
Sex: %s
Homepage: %s
Birthday: %s %s, 19%02s
Language-1: %s
Language-2: %s
Language-3: %s"
         (if (= age 65535) "not entered" age)
         (cond
          ((string= sex "\x00") "not entered")
          ((string= sex "\x01") "female")
          ((string= sex "\x02") "male"))
         homepage
         (aref eicq-monthnames birth-month)
         birth-day birth-year
         language-1 language-2 language-3))))))

(defun eicq-do-meta-user-about (data)
  "Handle server command 03de subcommand 00e6 in DATA."
  ;; ignore empty data
  (when (> (eicq-bin-int data) 1)
    (eicq-log-info
     "meta user about info =\n%s"
     (eicq-decode-string (substring data 2 -1)))))

(defun eicq-do-meta-user-interest (data)
  "Handle server command 03de subcommand 00f0 in DATA."
  ;; ignore empty data
  (when (> (length data) 1)
    (eicq-log-info "meta user interest info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-background (data)
  "Handle server command 03de subcommand 00fa in DATA."
  ;; ignore empty data
  (when (> (eicq-bin-int data) 1)
    (eicq-log-info "meta user background info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-picture (data)
  "Handle server command 03de subcommand 00fa in DATA."
  ;; ignore empty data
  (when (> (length data) 1)
    (eicq-log-info "meta user picture info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-found (data)
  "Handle server command 03de subcommand 019a in DATA."
  (eicq-log-info "meta user found\n%s"
                 (eicq-bin-pretty-hex data)))

(defun eicq-do-meta-user-update-general-confirm (data)
  "Handle server command 03de subcommand 0064 in DATA."
  (eicq-log-info "meta user update general info succeeded"))

(defun eicq-do-meta-user-update-work-confirm (data)
  "Handle server command 03de subcommand 006e in DATA."
  (eicq-log-info "meta user update work info succeeded"))

(defun eicq-do-meta-user-update-more-confirm (data)
  "Handle server command 03de subcommand 0078 in DATA."
  (eicq-log-info "meta user update more info succeeded"))

(defun eicq-do-meta-user-update-about-confirm (data)
  "Handle server command 03de subcommand 0082 in DATA."
  (eicq-log-info "meta user update about info succeeded"))

(defun eicq-do-meta-user-update-security-confirm (data)
  "Handle server command 03de subcommand a000 in DATA."
  (eicq-log-info "meta user update security info succeeded"))

(defun eicq-do-meta-user-password (data)
  "Handle server command 03de subcommand 00aa in DATA."
  (eicq-log-info "meta user password change succeeded"))

;;; Code - world:

;; Currently eicq supports only external resource file method of storing
;; buddy info (uin, alias, and properties). Certainly it can be extended
;; but keep all the interface variables and functions in this section. See
;; section "world by rc"

(defvar eicq-world nil
  "List of alias, uin, and plist.")

(defvar eicq-all-aliases nil
  "All aliases in `eicq-world'.
The mere purpose is to speed up operations.
Updated by `eicq-world-update'.")

(defun eicq-world-getf (alias tag)
  "For ALIAS get property of TAG.
If TAG is 'all, return the plist."
  (let ((plist (cddr (assoc alias eicq-world))))
    (if (eq tag 'all)
        plist
      (getf plist tag))))

(defun eicq-world-putf (alias tag value)
  "For ALIAS put property of TAG with VALUE."
  (let* ((buddy (assoc alias eicq-world))
         (plist (cddr buddy)))
    (if buddy (setcdr (cdr buddy) (putf plist tag value)))))

(defun eicq-alias-uin (alias)
  "Return an uin from an ALIAS in `eicq-world'.
Return uin if ALIAS is already an uin.
Return 0 if no corresponding uin or invalid uin.
If called interactively, display and push uin into `kill-ring'."
  (interactive (eicq-completing-aliases "uin from alias: " 'single))
  (let ((uin (second (assoc alias eicq-world))))
    (setq uin
          (cond
           ((eicq-valid-uin-p alias) alias)
           ((eicq-valid-uin-p uin) uin)
           (t "0")))
    (when (interactive-p)
      (message uin)
      (kill-new uin))
    uin))

(defun eicq-uin-alias (uin)
  "Return an alias from an UIN in `eicq-world'.
Return UIN if no corresponding ALIAS.
If called interactively, display and push alias into `kill-ring'."
  (interactive (list (read-string "alias from uin: ")))
  (let ((alias (or (first (find uin eicq-world :key 'second :test 'string=))
                   ;; not found, return uin
                   uin)))
    (when (interactive-p)
      (message alias)
      (kill-new alias))
    alias))

;;; Code - world by rc:

(defvar eicq-world-rc-regexp
  "^:icq[ \t]+\\([0-9]+\\)[ \t]+\\([^:]+?\\)\\( :.*\\)*$"
  "*Regular expression for rc file.
Format: :icq uin alias group
Group is prefixed by a colon :.  Anything between uin and group including
white spaces is alias.  For example,

:icq 409533 fire :linux :eicq
:icq 123456 the hatter :unreal")

(defvar eicq-alias-map
  (let ((map (make-sparse-keymap 'eicq-alias-map)))
    (define-key map [button2] 'eicq-send-message-via-mouse)
    map)
  "Keymap for alias extent.")

(defun eicq-world-update ()
  "Read `eicq-world-rc-filename' and update various user variables.
Need to call this whenever RC is modified and to be updated.
RC file is not closed if it is the buffer of current window or it is modified."
  (interactive)
  (save-excursion
    (let (no-killing-at-last)
      (setq eicq-world nil)
      (set-buffer (find-file-noselect eicq-world-rc-filename))
      ;; don't kill if rc file is buffer in current window
      (setq no-killing-at-last
            (or (buffer-modified-p)
                (eq (window-buffer) (current-buffer))))
      (goto-char (point-min))
      (while (search-forward-regexp eicq-world-rc-regexp nil t)
        (let* ((uin (match-string 1))
               (alias (match-string 2))
               (group (match-string 3))
               buddy)

          ;; idea from Erik Arneson <erik@starseed.com>
          (set-extent-properties
           ;; We may consider moving to eicq-uin-alias or somewhere else, if
           ;; we don't want to waste enourmous unused extents.
           (make-extent 0 (length alias) alias)
           `(highlight t duplicable t start-open t keymap ,eicq-alias-map))

          (setq buddy (list alias uin 'rc-index (point)))

          ;; group stuff not used yet
          (if group
              (setq buddy
                    (append buddy (read (format "(group (%s))" group)))))
          (push buddy eicq-world)))
      (unless no-killing-at-last (kill-buffer (current-buffer)))))

  (setq eicq-all-aliases (mapcar 'first eicq-world))
  (setq eicq-all-uin (mapcar 'second eicq-world))
  (setq eicq-user-bin (eicq-alias-bin eicq-user-alias)))

(defun eicq-world-info (alias)
  "Return local info of buddy ALIAS."
  ;; TODO
  (assoc alias eicq-world))

;; world mode

(defun world-mode ()
  "eicq resource file mode.
Quick hack for font-lock. Each record is separated by \"==== \" at the
beginning of the line."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "world")
  (setq major-mode 'world-mode)
  (setq fill-column 100)
  (auto-fill-mode 1)
  ;; hiding details for privacy
  (outline-minor-mode)
  (set (make-local-variable 'outline-regexp)
       "==== ")

  ;; highlight this month and next month
  ;; format example: "^Aug 08, 1979$"
  (let* ((this-month (string-to-number (format-time-string "%m")))
         (this-month-name (aref eicq-monthnames this-month))
         (next-month-name (aref eicq-monthnames (1+ this-month)))
         (month-regexp (format "^%s.*\\|^%s.*"
                               this-month-name next-month-name)))
    (setq font-lock-keywords
          ;; highlight separator
          `(("^==== " (0 'font-lock-warning-face t))
            ;; highlight date
            (,month-regexp (0 'highlight t))
            ;; highlight keyword prefixed with :
            (":\\(\\w\\|-\\)+" 0 font-lock-reference-face t))))
  (font-lock-mode 1))

(defun world-sort ()
  (interactive)
  (beginning-of-buffer)
  (sort-subr nil 'world-next-friend 'world-end-friend))

(defun world-next-friend ()
  (interactive)
  (let ((result (search-forward "====" nil t)))
    ;; go back before ====
    (if result (backward-char 4)
      ;; required by sort-subr
      (end-of-buffer))))

(defun world-end-friend ()
  (interactive)
  ;; skip current friend
  (forward-char 1)
  (let ((result (search-forward "====" nil t)))
    ;; go back before ====
    (if result (backward-char 5)
      (end-of-buffer))))

(defun world-find (alias)
  "Goto a friend record of ALIAS in `eicq-world-rc-filename'.
Prefix argument means do not use (load) eicq completing alias feature."
  (interactive
   (if current-prefix-arg
       (list (read-string "find: "))
     (progn
       (require 'eicq)
       (eicq-completing-aliases "find: " 'single))))
  (find-file eicq-world-rc-filename)
  (goto-char (point-min))
  (re-search-forward
   (concat "^:icq.*?"
           (regexp-quote alias)
           "\\b.*$")))

;;; Code - group:

(defun eicq-group-put (group name)
  "Put something into GROUP.
NAME can be either an alias or another group name."
  (let ((list (assoc group eicq-world)))
    (cond
     (list
      (setcdr list (list (pushnew name (cadr list) :test 'equal))))
     (t
      (push (list group (list name)) eicq-world)))))

(defun eicq-group-get (group)
  "Get members from GROUP."
  (cadr (assoc group eicq-world)))

(defun eicq-group-get-all-aliases (group)
  "Recursively get all aliases from GROUP."
  (loop for x in (eicq-group-get group)
    as expanded-x = (eicq-group-get x)
    if (atom expanded-x) collect x
    else append (eicq-group-get-all-aliases x)))

(defun eicq-group-select-aliases (state &rest aliases)
  "Select aliases and update buddy buffer.
Nil STATE means deselect, 'toggle means invert current state, and other
non-nil means select.

See `eicq-process-alias-input'."
  (interactive '(select))
  (eicq-process-alias-input 'aliases)
  (loop for x in aliases
    do (if (eq state 'toggle)
             (setq state (not (eicq-world-getf x 'selected))))
         (eicq-world-putf x 'selected state)
         (eicq-buddy-update-face x)))

;;; Code - alias and uin:

(defun eicq-bin-alias (bin &optional from)
  "Return an alias from a binary string BIN.
Return UIN if no corresponding ALIAS in `eicq-world'.
Returns \"0\" if conversion fails.
Consider only the first four characters from FROM to FROM+3 in BIN."
  (eicq-uin-alias (eicq-bin-uin bin from)))

(defun eicq-alias-bin (alias)
  "From an ALIAS return a binary string.
Return \"00000000\" in binary string if no corresponding uin
or invalid uin in `eicq-world'.
or if conversion fails."
  (eicq-uin-bin (eicq-alias-uin alias)))

(defun eicq-valid-uin-p (uin)
  "Return non-nil if UIN is a valid uin."
  (not (string= (eicq-uin-bin uin) "\x00\x00\x00\x00")))

(defvar eicq-alias-history nil
  "History of aliases in `eicq-completing-aliases'.")

(defvar eicq-alias-list-history nil
  "History of aliases in `eicq-send-message-helper'.
For sending messages of any kind to a single alias, it records the same
thing as `eicq-alias-history' does, while sending to multiple aliases, this
records a list of aliases instead of one by one.  This faciliates
re-sending to a list of aliases in future version.")

(defvar eicq-connected-aliases nil
  "Aliases that are in any statuses except 'invisible'.")

(defvar eicq-active-aliases nil
  "Aliases which we have exchanged messages with.")

(defun eicq-completing-aliases (prompt &optional single)
  "Completing-read aliases/uin.
PROMPT is the prompt for reading.
SINGLE means read only one alias/uin.

Must at least complete one alias, use RET (empty string) to finish
entering. It first completing-reads from the union of `eicq-active-aliases'
and `eicq-connected-aliases'. If you hit RET and the input string is not in
the union it the completing-reads from `eicq-all-aliases'.

Tips: You can also enter an uin in place of an alias."
  (let ((aliases
         ;; a must for first one
         (cons (eicq-completing-alias prompt 'required)
               (unless single
                 (loop collect (eicq-completing-alias prompt nil)
                       into aliases
                       ;; empty string means abort
                       until (string= (car (last aliases)) "")
                       finally return (nbutlast aliases))))))
    (delete-duplicates aliases :test 'string=)))

(defun eicq-completing-alias (prompt required)
  "Completing only one alias/uin.
PROMPT is the prompt for reading.
REQUIRED means cannot abort.
Used by `eicq-completing-aliases'.
No abortion when `** ' is in prompt.
Abort by RET (empty string) when `++ ' is in prompt."
  (let ((all eicq-all-aliases)
        (alias
          (eicq-completing-read
           prompt
           (union eicq-active-aliases
                  eicq-connected-aliases)
           nil nil nil 'eicq-alias-history)))
    (unless (or
             ;; valid alias
             (member alias all)
             ;; valid uin
             (eicq-valid-uin-p alias)
             ;; abort
             (and (string= alias "")
                  (not required)))
      (loop do
        (setq alias
              (eicq-completing-read
               (concat (if required "** " "++ ") prompt)
               all nil t alias 'eicq-alias-history))
        while (and (string= alias "") required)))
    alias))

(defun eicq-process-alias-input (symbol)
  "Input alias as selected or from completing.
SYMBOL is the symbol of variable (`alias') to be processed.

Non-nil SYMBOL means no processing.
Negative argument (press \\[negative-argument] before this command) means
taking all selected alias in buddy buffer as input.
Prefix argument (press \\[universal-argument] before this command) means
completing-read multi aliases from minibuffer.
Otherwise, completing-read one alias from minibuffer.

See `eicq-completing-aliases'."
  (or (symbol-value symbol)
      (set symbol
           (if (eq '- current-prefix-arg)
               (eicq-buddy-selected-in-view)
             (eicq-completing-aliases "to: " (not current-prefix-arg))))))

;;; Code - status:

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
    "\x00\x00\x00")
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

(defface eicq-face-selected
  '((((background dark))
     (:foreground "darkblue" :background "yellow"))
    (((background light))
     (:foreground "darkblue" :background "yellow")))
  "Face for OFFLINE status."
  :group 'eicq-buddy)

;;; Code - system main:

(defvar eicq-blurb 
  "As succinctly as possible, tell us:-\n
\tWhat happened.
\tWhat you thought should happen.
\tAnything else that you think is relevant.\n
*** Please delete these instructions before submitting the report. ***
======================================================================\n"
  "Preamble to the bug report.")

;;;###autoload
(defun eicq-login ()
  "Login to ICQ server.
Make connection to server and network if necessary."
  (interactive)
  (when (equal eicq-user-status "offline")
    (or (eicq-valid-uin-p (eicq-bin-uin eicq-user-bin))
        (error "Invalid user uin"))
    (eicq-send-queue-start)
    (setq eicq-trimmed-packet nil)      ; hack
    (setq eicq-current-seq-num 0)
    (eicq-log-show-buffer nil 'no-select)
    (eicq-connect)
    (when (eicq-connected-p)
      (message "Logging on the ICQ server...")
      (eicq-send (eicq-pack-login)))))

(defun eicq-logout (&optional kill)
  "Logout ICQ server.
Remain connected to network and server.
Don't send logout packet if KILL is non-nil,
useful for emergency logout when being kicked out by server."
  (interactive)
  (eicq-log-debug "Logging out ICQ server.")
  ;; use internal since the queue is clean after logged out
  (unless kill (eicq-send-internal (eicq-pack-logout)))
  (setq eicq-connected-aliases nil)
  (eicq-buddy-show-buffer 'new 'no-select)
  (eicq-change-status "offline" 'no-network)
  (eicq-keep-alive-stop)
  (eicq-send-queue-stop))

(defvar eicq-contact-list-packets nil
  "Lists of remaining contact list packets to be sent.
For experimental purpose only.")

(defun eicq-send-contact-list ()
  ;; v2 allows resend
  ;; v5 does not
  "Send the whole contact list.
You can resend contact list after `eicq-world-update'."
  (interactive)
  (setq eicq-connected-aliases nil)
  (eicq-buddy-show-buffer 'new 'no-select)
  (mapc 'eicq-send (eicq-pack-contact-list)))

(defun eicq-keep-alive-start ()
  "Start keeping alive."
  (eicq-keep-alive-stop)
  (start-itimer
   "eicq keep-alive"
   (lambda ()
     (eicq-send (eicq-pack-keep-alive)))
   ;; sending faster won't hurt
   60 60))

(defun eicq-keep-alive-stop ()
  "Stop keeping alive."
  (delete-itimer "eicq keep-alive"))

(defun eicq-change-user (alias password)
  "Change user to ALIAS with PASSWORD.
Need to relogin afterwards."
  (interactive
   (append (eicq-completing-aliases "change to: " 'single)
           (list (read-passwd "password: "))))
  (setq eicq-user-alias alias)
  (setq eicq-user-bin (eicq-alias-bin alias))
  (setq eicq-user-password
   (if (zerop (length password))
       nil
     password)))

(defun eicq-auto-away-timeout-set (&optional symbol value)
  "Set timer for auto-away.  See 'eicq-auto-away-timeout'."
  (delete-itimer "eicq auto-away")      ; delete previous
  (start-itimer
   "eicq auto-away"
   (lambda ()
     ;; auto away for first idle
     (when (member eicq-user-status '("online" "ffc"))
       (eicq-log-system "Auto away.")
       (eicq-change-status "away")
       (setq eicq-user-auto-away-p t)))
   value value
   'is-idle)
  (delete-itimer "eicq auto-na")
  (start-itimer
   "eicq auto-na"
   (lambda ()
     ;; auto na for second idle
     (when (and eicq-user-auto-away-p 
		(equal eicq-user-status "away"))
       (eicq-log-system "Auto na.")
       (eicq-change-status "na")
       ;; eicq-change-status resets this flag
       (setq eicq-user-auto-away-p t)))
   (* 2 value) (* 2 value)
   nil))

(defcustom eicq-auto-away-timeout 300
  "*Seconds of inactivity in Emacs before auto-away.
After two times the seconds of auto-away, it goes auto-na.
See `eicq-auto-away'."
  :group 'eicq-option
  :set 'eicq-auto-away-timeout-set)

(defun eicq-send-message-helper (message aliases type log-message)
  "Send message, url, authorization or others.
MESSAGE is the message to send.
ALIASES is a list of aliases/uin to send to.
TYPE is the type of message in `eicq-message-types'.
LOG-MESSAGE is a message to put in log.

See `eicq-send-message', `eicq-send-url' and `eicq-authorize'."
  (if eicq-user-auto-away-p
      (progn
	(if (not eicq-auto-reply-p)
	    (eicq-change-status "online"))))
  (add-to-list 'eicq-alias-list-history aliases)
  (loop for alias in aliases
    do (add-to-list 'eicq-active-aliases alias)
    do (eicq-send (eicq-pack-send-message alias message type))
    do (eicq-log-outgoing alias ">>> %s" log-message))
  (setq eicq-auto-reply-p nil))

(defvar eicq-message-history nil
  "History of `eicq-send-message' for `completing-read'.")

(defun eicq-send-message (&optional message &rest aliases)
  "Send an instant message.
MESSAGE is the message to send.
ALIASES is a list of aliases/uin to send to.

See `eicq-process-alias-input'."
  (interactive "P")
  (let ((prompt
         (concat "message"
                 ;; display alias if given
                 (if (car aliases)
                     (concat " to "
                             (substring (format "%s" aliases) 1 -1)))
                 ": ")))
    (or (stringp message)
        (setq message
              (read-from-minibuffer prompt
               nil nil nil 'eicq-message-history)))

    ;; idea from Erik Arneson <erik@starseed.com>
    ;; confirm sending a blank message
    (unless (and (or (zerop (length message))
                     ;; \\W fails with "=)" or "..."
                     (string-match "^[ \t]+$" message))
                 (not (y-or-n-p "Send a blank message? ")))
      (eicq-process-alias-input 'aliases)

      ;; apply encode only TEXT portion of packet
      (loop for x in (eicq-spliter message)
        do (eicq-send-message-helper
            ;; encoding outgoing but not that to be insert in log buffer
            (eicq-encode-string x) aliases 'normal x)))))

(defvar eicq-buddy-buffer nil
  "Buffer for contact list.")

(defun eicq-send-message-via-mouse (event)
  ;; Erik Arneson <erik@starseed.com> (from VM)
  "`eicq-send-message' via mouse."
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (and (event-point event) (goto-char (event-point event)))
  (if (eq (current-buffer) eicq-buddy-buffer)
      (eicq-send-message-alias-here)
    ;; fall through
    ;; any alias in log-mode format (enclosed by []) can use this
    (eicq-send-message-alias-around)))

(defvar eicq-url-history nil
  "History of `eicq-send-url' for `completing-read'.")

(defun eicq-send-url (&optional url description &rest aliases)
  "Send an url.
URL is any Internet address.
DESCRIPTION is the description of url.
ALIASES is a list of aliases/uin to send to.

See `eicq-process-alias-input'."
  (interactive "P")
  (let ((prompt
         (concat "url"
                 ;; display alias if given
                 (if (car aliases)
                     (concat " to "
                             (substring (format "%s" aliases) 1 -1)))
                 ": ")))
    (or (stringp url)
        (setq url
              (read-from-minibuffer
               prompt nil nil nil 'eicq-url-history)))

    ;; idea from Erik Arneson <erik@starseed.com>
    ;; confirm sending a blank url
    (unless (and (or (zerop (length url))
                     ;; \\W fails with "=)" or "..."
                     (string-match "^[ \t]+$" url))
                 (not (y-or-n-p "Send a blank url? ")))
      (or description
          (setq description
                (read-from-minibuffer
                 "description: " nil nil nil 'eicq-message-history)))
      (eicq-process-alias-input 'aliases)

      (eicq-send-message-helper
       (format "%s\xfe%s"
               ;; encode only to TEXT portions of packet, instead of the whole
               (eicq-encode-string description)
               (eicq-encode-string url))
       aliases 'url (format "%s (%s)" url description)))))

(defun eicq-authorize (&optional alias)
  "Send authorization to allow adding to contact list.
ALIAS is an alias/uin."
  (interactive)
  (if alias
      ;; display alias if given
      (message "%s authorized." alias)
    (setq alias (car (eicq-completing-aliases "authorize: " 'single))))

  (eicq-send-message-helper
   "" (list alias) 'authorize "authorized"))

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

(defun eicq-register-new-user (password)
  "Register a new uin with PASSWORD."
  (interactive (list (read-passwd "Password: " 'confirm)))
  (eicq-send (eicq-pack-register-new-user password)))

(defun eicq-change-password (password)
  "Change PASSWORD."
  (interactive (list (read-passwd "Password: " 'confirm)))
  (eicq-send (eicq-pack-meta-user-change-password password)))

(defun eicq-search (nick-name first-name last-name email)
  "Search ICQ users."
  (interactive "sNick-name: \nsFirst-name: \nsLast-name: \nsEmail: ")
  (eicq-send (eicq-pack-search nick-name first-name last-name email)))

(defun eicq-search-random-user (group)
  "Search random user in GROUP."
  (interactive
   (list (eicq-completing-read
          "Random group: "
          (mapcar 'car eicq-random-groups))))
  (eicq-send (eicq-pack-search-random-user group)))

(defun eicq-set-random-group (group)
  "Set random user GROUP."
  (interactive
   (list (eicq-completing-read
          "Random group: "
          (mapcar 'car eicq-random-groups))))
  (eicq-send (eicq-pack-set-random-group group)))

(defun eicq-update-meta-info ()
  "Update user meta info on the ICQ server.
Run this after changing any meta user info variables."
  (interactive)
  ;(eicq-send (eicq-pack-update-authorization))
  (eicq-send (eicq-pack-meta-user-security))
  (eicq-send (eicq-pack-meta-user-update-general))
  (eicq-send (eicq-pack-meta-user-update-work))
  (eicq-send (eicq-pack-meta-user-update-more))
  (eicq-send (eicq-pack-meta-user-update-about)))

(defun eicq-delete-offline-messages ()
  "Delete offline messages from ICQ server.
See `eicq-delete-offline-messages-flag'."
  (interactive)
  (if (or
       (interactive-p)
       (eq eicq-delete-offline-messages-flag t)
       (and (eq eicq-delete-offline-messages-flag 'ask)
            (y-or-n-p
             "All offline messages are received.  \
Delete them from server? ")))
      (eicq-send (eicq-pack-delete-offline-messages))))

(defun eicq-query-info (&optional alias)
  "Query meta user info.
ALIAS is an alias/uin."
  (interactive)
  (if alias
      ;; display alias if given
      (message "Query %s." alias)
    (setq alias (car (eicq-completing-aliases "Query: " 'single))))
  (let ((local-info (eicq-world-info alias)))
    (if local-info
        (eicq-log-info "Local info:\n%s" local-info)))
  (eicq-send (eicq-pack-meta-user-query alias)))

(defvar eicq-status-buffer nil
  "Buffer for statuses.")

(defvar eicq-log-buffer nil
  "Buffer for log.")

(defvar eicq-wharf-frame-use-p)

;;;###autoload
(defun eicq-show-window ()
  "Show windows of eicq buffers.
Make them if not yet done.
See `eicq-buddy-buffer' and `eicq-log-buffer'."
  (interactive)
  (if (featurep 'eicq-wharf)
      (if eicq-wharf-frame-use-p
	  (eicq-wharf-new-frame)))
  (unless (frame-live-p eicq-frame)
    (setq eicq-frame
          (if eicq-start-in-new-frame
              (new-frame)
            (selected-frame))))
  (select-frame eicq-frame)
  (eicq-buddy-show-buffer)
  (eicq-status-show-buffer)
  (eicq-log-show-buffer)
  (set-window-buffer nil eicq-buddy-buffer)
  (delete-other-windows)
  (set-window-buffer
   (split-window nil eicq-buddy-window-width t) eicq-log-buffer)
  (set-window-buffer nil eicq-status-buffer)
  (set-window-buffer
   (split-window nil eicq-status-window-height) eicq-buddy-buffer)
  (other-window 2))


(defun eicq-hide-window ()
  "Hide windows of eicq buffers."
  (interactive)
  (loop for each in '(eicq-buddy-buffer 
		      eicq-log-buffer 
		      eicq-status-buffer 
		      eicq-bridge-buffer)
    do (delete-windows-on (symbol-value each))))

;;; Code - log:

;; message history buffer

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

(defvar eicq-log-outline-regexp "^...:.. "
  "Regexp for log header.
See `outline-regexp'.")

(defun eicq-switch-to-buddy-buffer ()
  "Switches from the log buffer to the buddy buffer.
Needed so we can by-pass the status buffer."
  (interactive)
  (other-window 2))

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
          (insert "====================\n"))))
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
    (smiley-buffer)
    (goto-address)))

(defvar eicq-url-map
  (let ((map (make-sparse-keymap 'eicq-url-map)))
    (define-key map [button2] 'browse-url-at-mouse)
    (define-key map [B] 'browse-url-at-point)
    (define-key map [return] 'browse-url-at-point)
    map)
  "Keymap for URL extent.")

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

(defun eicq-alias-around ()
  "Return an alias/uin on current line or lines before.
If called interactively, display and push alias into `kill-ring'."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (looking-at "^...:.. \\[\\(.*\\)\\]")
    (let ((alias (match-string 1)))
      (cond
       ((or (member alias eicq-all-aliases)
              (eicq-valid-uin-p alias))
        (when (interactive-p)
          (message alias)
          (kill-new alias))
        alias)
       (t (error "No valid alias/uin found"))))))

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

(defun eicq-forward-message-around (&optional no-header)
  "Forward message around
Non-nil NO-HEADER means avoid prefixing message with original sender's
info.
ALIASES is a list of aliases/uin to send to.

See `eicq-process-alias-input'."
  (interactive "P")
  (let* ((message (eicq-log-around))
         (alias (eicq-alias-around))
         (uin (eicq-alias-uin alias)))
    (eicq-send-message
     (concat
      (if (not no-header)
          (format "%s (ICQ#%s) Wrote:\n" alias uin))
      message))))

(defun eicq-forward-message-around-without-header ()
  "See `eicq-forward-message-around'."
  (interactive)
  (eicq-forward-message-around 'no-header))

(defun eicq-select-alias-around ()
  "See `eicq-group-select-aliases' and `eicq-alias-around'."
  (interactive)
  (eicq-group-select-aliases 'toggle (eicq-alias-around)))

(defun eicq-send-message-alias-around ()
  "See `eicq-send-message' and `eicq-alias-around'."
  (interactive)
  (eicq-send-message nil (eicq-alias-around)))

(defun eicq-send-url-alias-around ()
  "See `eicq-send-url' and `eicq-alias-around'."
  (interactive)
  (eicq-send-url nil nil (eicq-alias-around)))

(defun eicq-authorize-alias-around ()
  "See `eicq-authorize' and `eicq-alias-around'."
  (interactive)
  (eicq-authorize (eicq-alias-around)))

(defun eicq-query-info-alias-around ()
  "See `eicq-query-info' and `eicq-alias-around'."
  (interactive)
  (eicq-query-info (eicq-alias-around)))

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

;;; Code - buddy:

;; contact list (list of aliases) buffer

(easy-menu-define
 eicq-log-easymenu nil "Eicq Log" eicq-buddy-menu)

(defadvice display-buffer
  (after eicq-buddy-avoid-window-select last activate)
  "Avoid setting buffer to small vertical window `eicq-buddy-buffer'.
When switching buffer in buddy window, other window in the frame is
used instead of buddy window, unless buddy window is the only window
in the frame.

To set buffer in buddy window explicitly, make buddy window the only window
in the frame by `one-window'."
  (unless (= (frame-width)
             (window-width (get-buffer-window buffer)))
    (delete-other-windows (get-buffer-window buffer))))

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

(defun eicq-buddy-mode ()
  "Major mode for contact list in eicq.
Commands: \\{eicq-buddy-mode-map}

Turning on `eicq-buddy-mode' runs the hook `eicq-buddy-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eicq-buddy-mode-map)
  (setq mode-name "eicq-buddy")
  (setq major-mode 'eicq-buddy-mode)
  ;; put easy-menu-add after set mode-name
  (easy-menu-add eicq-main-easymenu)
  (easy-menu-add eicq-buddy-menu)
  (easy-menu-add eicq-log-menu)
  (setq modeline-format "%b")

  (run-hooks 'eicq-buddy-mode-hook))

(defun eicq-buddy-view-set (&optional symbol value)
  "Set `eicq-buddy-view'."
  (set-default symbol value)
  (eicq-buddy-show-buffer 'new 'no-select))

(defun eicq-buddy-show-buffer (&optional new no-select)
  "Switch to `eicq-buddy-buffer'.
Create buffer if buffer does not exists already or
NEW is non-nil.
Don't select buddy window if NO-SELECT is non-nil.
See `eicq-buddy-view' and `eicq-buddy-status-color-hint-flag'."
  (interactive)
  (when (or (not (buffer-live-p eicq-buddy-buffer))
            new)
    (setq eicq-buddy-buffer (get-buffer-create "*eicq buddy*"))
    (set-buffer eicq-buddy-buffer)
    (erase-buffer)
    (loop for alias in (symbol-value eicq-buddy-view)
      as status = (eicq-world-getf alias 'status)
      as face = (eicq-status-face status)
      do (insert-face (concat alias "\n") face))
    (eicq-buddy-mode))
  (unless no-select
    (switch-to-buffer eicq-buddy-buffer)))

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

(defun eicq-buddy-view-all ()
  "Display all aliases in `eicq-world'.
See `eicq-buddy-view'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-all-aliases))

(defun eicq-buddy-view-connected ()
  "Display all connected aliases.
See `eicq-buddy-view' and `eicq-connected-aliases'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-connected-aliases))

(defun eicq-buddy-view-active ()
  "Display all active aliases.
See `eicq-buddy-view' and `eicq-active-aliases'."
  (interactive)
  (eicq-buddy-view-set 'eicq-buddy-view 'eicq-active-aliases))

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

(defun eicq-buddy-update-face (alias &optional delete)
  "Update face of ALIAS.
Non-nil DELETE means delete alias from buffer."
  (save-excursion
    (when (buffer-live-p eicq-buddy-buffer)
      (set-buffer eicq-buddy-buffer)
      (goto-char (point-min))

      (if (search-forward-regexp
           ;; use "^" alias "$" so searching "foo" will not get "foobar"
           (concat "^"
                   ;; to allow funny characters in alias
                   (regexp-quote alias)
                   "$")
           nil t)
          ;; old alias
          (if delete
              (delete-region
               (point-at-bol)
               ;; take care of last line
               (min (1+ (point-at-eol)) (point-max))))
        ;; new alias
        (unless delete
          (insert alias "\n")
          (forward-line -1)))

      (unless delete
        (put-text-property
         ;; from the end of last line to the beginning of next line instead
         ;; of just bol and eol of current line, to make sure covering
         ;; highlight by selected; start-open and end-open do not seem to fix
         (max (1- (point-at-bol)) (point-min))
         ;; take care of last line
         (min (1+ (point-at-eol)) (point-max))
         'face (eicq-status-face (eicq-world-getf alias 'status)))

        (when (eicq-world-getf alias 'selected)
          ;; highlight first char
          (put-text-property
           (+ 0 (point-at-bol)) (+ 1 (point-at-bol))
           'face 'eicq-face-selected))))))

(defun eicq-buddy-select-all-in-view (state &optional predicate)
  "Select all aliases in current view.
See `eicq-group-select-aliases' for STATE.
PREDICATE accepts an alias as an argument and limits the application.
Current view is `eicq-buddy-view'."
  (loop for x in (symbol-value eicq-buddy-view)
    if (or (null predicate)
           (funcall predicate x))
    do (eicq-group-select-aliases state x)))

(defun eicq-buddy-select-all-in-view-by-status (status)
  "Toggle selections of all aliases with STATUS in current view."
  (interactive
   (list (eicq-completing-read "status: " eicq-valid-statuses)))
  (eicq-buddy-select-all-in-view
   'toggle
   (lambda (x)
     (equal (eicq-world-getf x 'status) status))))

(defun eicq-buddy-select-all-in-view-by-regexp (regexp)
  "Toggle selections of all aliases matching REGEXP in current view."
  ;; checked my screenshots? know why i use a symbol prefix now?
  (interactive "sregexp: ")
  (eicq-buddy-select-all-in-view
   'toggle
   (lambda (x)
     (string-match regexp x))))

(defun eicq-buddy-selected-in-view ()
  "Return a list of all selected aliases in current view.
Selected means an alias has non-nil 'selected property.
Current view is `eicq-buddy-view'."
  (loop for x in (symbol-value eicq-buddy-view)
    if (eicq-world-getf x 'selected)
    collect x))

(defun eicq-alias-here ()
  "Return an alias/uin on current line.
Leading or trailing whitespace are ignored.
If called interactively, display and push alias into `kill-ring'."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((alias
           (buffer-substring
            (progn
              (beginning-of-line)
              (skip-chars-forward "[ \t]")
              (point))
            (progn
              (end-of-line)
              (skip-chars-backward "[ \t]")
              (point)))))
      (cond
       ((or (member alias eicq-all-aliases)
            (eicq-valid-uin-p alias))
        (when (interactive-p)
          (message alias)
          (kill-new alias))
          alias)
       (t (error "No valid alias/uin found"))))))

(defun eicq-select-alias-here (action)
  "See `eicq-group-select-aliases' and `eicq-alias-here'.
Nil or 'toggle ACTION means toggle selection for alias here.
`numberp' action or digit arguments (press \\[digit-argument] before this
command) means select the number of next/previous aliases.
'toggle-all ACTION or prefix argument (press \\[universal-argument] before this command) means
toggle selections for all aliases in view.
'deselect-all or other non-nil ACTION or negative argument (press
\\[negative-argument] before this command) means deselect for all aliases
in view.

See `eicq-buddy-select-all-in-view'."
  (interactive
   (list (cond
          ((not current-prefix-arg) 'toggle)
          ((eq '- current-prefix-arg) 'deselect-all)
          ((numberp current-prefix-arg) current-prefix-arg)
          (t 'toggle-all))))
  (cond
   ((or (not action) (eq action'toggle))
    (eicq-group-select-aliases 'toggle (eicq-alias-here))
    (forward-line))
   ((and (numberp action) (zerop action))) ; recurrsion done
   ((natnump action)
    (eicq-group-select-aliases 'select (eicq-alias-here))
    (forward-line 1)
    (eicq-select-alias-here (1- action)))
   ((numberp action)                    ; negative digit
    (eicq-group-select-aliases 'select (eicq-alias-here))
    (forward-line -1)
    (eicq-select-alias-here (1+ action)))
   ((eq action 'toggle-all)
    (eicq-buddy-select-all-in-view 'toggle))
   ((eq action 'deselect-all)
    (eicq-buddy-select-all-in-view nil))))

(defun eicq-send-message-alias-here ()
  "See `eicq-send-message' and `eicq-alias-here'."
  (interactive)
  (eicq-send-message nil (eicq-alias-here)))

(defun eicq-send-url-alias-here ()
  "See `eicq-send-url' and `eicq-alias-here'."
  (interactive)
  (eicq-send-url nil nil (eicq-alias-here)))

(defun eicq-authorize-alias-here ()
  "See `eicq-authorize' and `eicq-alias-here'."
  (interactive)
  (eicq-authorize (eicq-alias-here)))

(defun eicq-query-info-alias-here ()
  "See `eicq-query-info' and `eicq-alias-here'."
  (interactive)
  (eicq-query-info (eicq-alias-here)))

;;; Code - keymap:

(defun eicq-global-map-set (&optional symbol value)
  "Set `eicq-global-key-prefix'.
WARNING: Bindings with old prefix is not deleted.  Fixable?"
  (define-key global-map value eicq-main-map))

(defcustom eicq-global-key-prefix [(meta \`)]
  "*Prefix for all key macros in global."
  :group 'eicq-option
  :set 'eicq-global-map-set)

;;; Code - footer:

;; otherwise sending large contact list leads to significant delay
(byte-compile 'eicq-pack-contact-list)

(defvar eicq-load-hook nil
  "*Hooks run after Eicq has loaded everything up.")

(run-hooks 'eicq-load-hook)



(provide 'eicq)

;;; eicq.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%m-%d %02H:%02M:%02S (%u)"
;End: 
