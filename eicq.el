;;; eicq.el --- ICQ client for Emacs

;; Copyright (C) 1999 by Stephen Tse
;; Copyright (C) 2000, 2001, 2002, Steve Youngs

;; RCS: $Id$
;; OriginalAuthor: Stephen Tse <stephent@sfu.ca>
;; Maintainer:     Steve Youngs <youngs@xemacs.org>
;; Created:        Aug 08, 1998
;; Last-Modified:  <2003-09-06 14:28:51 (steve)>
;; Version:        0.5.0pre2
;; Homepage:       http://eicq.sf.net/
;; Keywords:       comm ICQ

;; This file is part of Eicq.

;; Eicq is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; Eicq is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
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

;;;###autoload
(defconst eicq-version "0.5.9"
  "Version of eicq you are currently using.")

(eval-and-compile
  (require 'timezone)
  (require 'outline)
  (require 'eicq-comm)
  (require 'eicq-log)
  (require 'eicq-meta)
  (require 'eicq-world)
  (require 'eicq-v8proto))

(eval-when-compile
  (defvar eicq-user-status)
  (defvar eicq-user-initial-status)
  (defvar eicq-buddy-buffer)
  (defvar eicq-buddy-window-width)
  (defvar eicq-status-buffer)
  (defvar eicq-status-window-height))

(autoload 'eicq-status-auto-reply "eicq-status")
(autoload 'eicq-status-idle-reply "eicq-status")
(autoload 'eicq-status-name "eicq-status")
(autoload 'eicq-change-status "eicq-status" nil t)
(autoload 'eicq-status-show-buffer "eicq-status" nil t)
(autoload 'eicq-buddy-update-status "eicq-status")
(autoload 'eicq-buddy-selected-in-view "eicq-buddy")
(autoload 'eicq-buddy-show-buffer "eicq-buddy" nil t)
(autoload 'eicq-buddy-select-all-in-view "eicq-buddy")

;; Customize Groups.

(defgroup eicq nil
  "Mirabilis ICQ communication client."
  :group 'comm)

(defgroup eicq-info nil
  "Essential account info."
  :group 'eicq)

(defgroup eicq-option nil
  "System settings and general preferences."
  :group 'eicq)

(defgroup eicq-sound nil
  "Sound preferences."
  :group 'eicq)

(defgroup eicq-interface nil
  "Change the look and \"feel\"."
  :group 'eicq)

;; Customize.

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

(defcustom eicq-start-in-new-frame nil
  "*If non-NIL, Eicq will start in its own frame."
  :group 'eicq-interface
  :type 'boolean)

(defcustom eicq-new-message-hook nil
  "*Hooks to run when there is an incoming message.
Dynamically ALIAS and MESSAGE are binded to be used in hooks."
  :group 'eicq-option
  :type 'hook)

(defcustom eicq-read-message-hook nil
  "*Hooks run when a message is marked as \"read\"."
  :group 'eicq-option
  :type 'hook)

(defcustom eicq-system-message-hook nil
  "*Hooks run when a \"system\" message is received."
  :group 'eicq-option
  :type 'hook)

(defcustom eicq-load-hook nil
  "*Hooks run after Eicq has loaded everything up."
  :type 'hook
  :group 'eicq-option)

;;; Internal variables

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

(defvar eicq-frame nil
  "The frame where Eicq is displayed.")

(defvar eicq-wharf-frame)

(defun eicq-disconnect ()
  "Log out of ICQ and close all Eicq buffers."
  (interactive)
  (if eicq-save-log-on-exit-p
      (save-excursion
	(set-buffer eicq-log-buffer)
	(save-buffer)
	(if (file-exists-p eicq-log-filename)
	    (rename-file
	     eicq-log-filename
	     (concat eicq-log-filename
		     ;; in case you do something stupid with it
		     (format-time-string "-%Y-%b%d-%H%M-%S")))))
    (save-excursion
      (set-buffer eicq-log-buffer)
      (save-buffer)
      (if (file-exists-p eicq-log-filename)
	  (delete-file eicq-log-filename))))
  (eicq-logout 'kill)
  (eicq-network-kill)
  (loop for each in '(eicq-log-buffer 
		      eicq-buddy-buffer 
		      eicq-status-buffer
		      eicq-network-buffer)
    do (if (buffer-live-p (symbol-value each))
           (kill-buffer (symbol-value each))))
  (delete-other-windows)
  (if (and eicq-start-in-new-frame
	   (frame-live-p eicq-frame))
      (delete-frame eicq-frame))
  (setq eicq-frame nil)
  (if (and (featurep 'eicq-wharf)
	   (frame-live-p eicq-wharf-frame))
      (delete-frame eicq-wharf-frame))
  (setq eicq-wharf-frame nil))

(defun eicq-send-internal (bin)
  "Send a binary string BIN to `eicq-network'.
`process-send-string' restricts the length of BIN to 500 or less."
  (if (> (length bin) (+ eicq-message-max-size 50))
      (eicq-log-error
       "You try to send a packet of length %s.\n
It is sent anyway but it may not go through.\n"
       (length bin)
       eicq-message-max-size))

;  (if (eicq-connected-p)
      (process-send-string
       eicq-network-buffer
       (concat (eicq-int-bin (length bin))
               bin)))
;    (eicq-log-error
;     "Network is not connected when it tries to send a packet")
;    (eicq-logout 'kill)))

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

; (defun eicq-pack-login ()
;   "Pack login packet 03e8."
;   (let* ((password eicq-encrypted-password)
;          (password-len (eicq-int-bin (1+ (length password))))
;          (status (eicq-status-bin eicq-user-initial-status)))
;     (eicq-pack
;      "\xe8\x03"
;      ;; "\x5f\x71\x69\x37"                      ; time
;      "\x00\x00\x00\x00"                 ; time
;      "\x00\x00\x00\x00"                 ; port (auto-detect)
;      password-len
;      password "\x00"
;      "\x72\x00\x04\x00"                 ; x1
;      "\x00\x00\x00\x00"                 ; ip (auto-detect)
;      "\x06"                             ; x2 _d95_
;      status                             ; initial status
;      ;; FIXME invisible has 2 bytes while others only one
;      ;; must be \x00 so that initial status works
;      "\x00\x00\x00\x00"                 ; x3 _d95_
;      "\x02\x00"                         ; seq num, WHY 2?
;      "\x00\x00\x00\x00"                 ; x4
;      "\x04\x00\x72\x00")))

(defconst eicq-icq-client-id-string
  "ICQ Inc. - Product of ICQ (TM).2002a.5.37.1.3728.85"
  "A string to identify ourselves as a nice well behaved ICQ client.")

(defun eicq-pack-login-a ()
  "Send stage 1 of the 2 stage login process."
  (let* ((hello (cdr (assq 'hello eicq::CLI_IDENT)))
	 (uin (cdr (assq 'uin eicq::CLI_IDENT)))
	 (password (cdr (assq 'password eicq::CLI_IDENT)))
	 (version (cdr (assq 'version eicq::CLI_IDENT)))
	 (unk (cdr (assq 'unk eicq::CLI_IDENT)))
	 (ver-major (cdr (assq 'ver-major eicq::CLI_IDENT)))
	 (ver-minor (cdr (assq 'ver-minor eicq::CLI_IDENT)))
	 (ver-lessor (cdr (assq 'ver-lessor eicq::CLI_IDENT)))
	 (ver-build (cdr (assq 'ver-build eicq::CLI_IDENT)))
	 (ver-subbuild (cdr (assq 'ver-subbuild eicq::CLI_IDENT)))
	 (language (cdr (assq 'language eicq::CLI_IDENT)))
	 (country (cdr (assq 'country eicq::CLI_IDENT)))
	 (login-str (concat hello uin password version
			    unk ver-major ver-minor ver-lessor
			    ver-build ver-subbuild country language)))
    (process-send-string eicq-network login-str)))


(defun eicq-pack-login-b ()
  "Pack stage 2 of the login process."
  ;;todo
  )
     

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

(defun eicq-pack-register-new-user (password)
  "Pack register new user packet 03fc."
  (eicq-pack
   "\xfc\x03"
   (eicq-int-bin (length password))
   password
   "\xa0\x00\x00\x00"
   "\x24\x61\x00\x00"
   "\x00\x00\x00\x00"))

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

(defconst eicq-pack-add-user-to-contact-list "\x3c\x05")

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
  (eicq-logout 'kill)
  (sit-for 2)
  (if (and eicq-user-password
	   (not (string= eicq-delete-offline-messages-flag "ask")))
      (progn
	(eicq-log-system "Attempting auto-reconnect...")
	(eicq-login))))

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
	(progn
	  (add-to-list 'eicq-active-aliases alias)
	  (eicq-send (eicq-pack-send-message alias message 'normal))
	  (eicq-log-system "Automatic response sent.")))))

;; FIXME - This ain't workin.
(defun eicq-idle-reply (alias)
  "Auto-reply to ALIAS/uin depending on `eicq-user-status'.
Called by `eicq-do-message-heler'."
  (let ((message (symbol-value (eicq-status-idle-reply eicq-user-status))))
    (if message
	(progn
	  (add-to-list 'eicq-active-aliases alias)
	  (eicq-send (eicq-pack-send-message alias message 'normal))
	  (eicq-log-system "Automatic response sent (idle)."))))
  (setq eicq-auto-reply-p nil))

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
  (eicq-keep-alive-start)
  (eicq-send-contact-list)
  (message "Welcome to Eicq...")
  (eicq-show-window))

(defun eicq-do-system-message (packet)  ; TODO
  "Handle server command 01c2 in PACKET."
  (run-hooks 'eicq-system-message-hook))

(defvar fix-nick)

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
    ;; Dynamically add a new user to your contact list.
    (if eicq-add-user-p
	(progn
	  ;; Prompt for zero or more group names to add to.
	  (let ((new-group 
		 (read-string 
		  "Add user to group[s] (fmt: :group1 :group2 or RET for none): ")))
	    ;; Test for blank or invalid alias.  Prompt for alternative
	    ;; alias if needed.  Use the UIN if user simply hits RET.
	    (if (or (string-equal nick-name "")
		    (string-match "^:" nick-name))
		(let ((fix-nick (read-string "Blank or invalid Alias.  Enter new Alias (or RET to use UIN): ")))
		  (if (string-equal fix-nick "")
		      (setq nick-name uin)
		    (if (string-match "^:" fix-nick)
			(loop until (string-match "^[^:]" fix-nick)
			  do (setq fix-nick
				   (read-string 
				    "Invalid Alias (can't begin with \":\"): "))
			  do (if (string-equal fix-nick "")
				 (setq nick-name uin
				       fix-nick "valid")
			       (setq nick-name fix-nick)))
		      (setq nick-name fix-nick)))))
	    (setq fix-nick nil)
	    ;; Write the new UIN Alias Group[s] to the world file.
	    (set-buffer
	     (find-file-noselect (expand-file-name eicq-world-rc-filename)))
	    (goto-char (point-max))
	    (insert "\n")
	    (insert
	     (format ":icq %s %s %s\n" uin nick-name new-group))
	    (save-buffer (current-buffer))
	    (kill-buffer (current-buffer))
	    ;; Inform the user in the log.
	    (eicq-log-info
	     (eicq-decode-string
	      (format "Alias: %s, UIN: %s added to contact list."
		      nick-name uin)))
	    (setq eicq-new-buddy nick-name)
	    (setq new-group nil)))
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
	 (if (= authorization 0) "Needed" "Not Needed")))))))


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
      (0 
       (if eicq-add-user-p
	   (eicq-add-new-user-to-buddy-buffer)
	 (eicq-log-info "All search results returned")))
      (1 
       (eicq-log-info "Too many seach results")))))

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
      (eicq-pack-login-a))))


(autoload 'eicq-wharf-change-messages "eicq-wharf")

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
  (eicq-send-queue-stop)
  (eicq-world-update)
  (eicq-send-contact-list)
  (if (and (featurep 'eicq-wharf)
	   (frame-live-p eicq-wharf-frame))
      (progn
	(eicq-wharf-change-messages "New" -9999)
	(eicq-wharf-change-messages "Sys" -9999))))

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

;;;###autoload
(defun eicq-auto-away-timeout-set (&optional symbol value)
  "Set timer for auto-away.  See `eicq-auto-away-timeout'."
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

;;;###autoload
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
         (concat "Message"
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

(defconst eicq-pack-search-by-uin "\x1a\x04")

(defun eicq-search-by-uin (uin)
  (interactive "sUIN: ")
  (eicq-send
   (eicq-pack eicq-pack-search-by-uin
	      (eicq-uin-bin uin))))

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

(defvar eicq-wharf-frame-use-p)

(autoload 'eicq-wharf-new-frame "eicq-wharf")

;;;###autoload
(defun eicq-show-window ()
  "Show windows of eicq buffers.
Make them if not yet done.
See `eicq-buddy-buffer' and `eicq-log-buffer'."
  (interactive)
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
  (other-window 2)
  (save-excursion
    (if eicq-wharf-frame-use-p
	(eicq-wharf-new-frame)))
  (focus-frame eicq-frame))

(defun eicq-hide-window ()
  "Hide windows of eicq buffers."
  (interactive)
  (loop for each in '(eicq-buddy-buffer 
		      eicq-log-buffer 
		      eicq-status-buffer 
		      eicq-network-buffer)
    do (delete-windows-on (symbol-value each))))

;;; Code - log:

;; message history buffer

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

;;; Code - buddy:

;; contact list (list of aliases) buffer

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

;;; Code - footer:

;; otherwise sending large contact list leads to significant delay
(byte-compile 'eicq-pack-contact-list)

(run-hooks 'eicq-load-hook)

(provide 'eicq)

;;; eicq.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
