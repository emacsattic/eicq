;;; eicq-v8proto.el --- ICQ version 8 protocol.

;; Copyright (C) 2002,03 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-10-01
;; Last-Modified: <2003-10-01 22:40:44 (steve)>
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

;;; Commentary:
;;
;;  The various network packets that make up the ICQv8 protocol.
;;
;;  See http://www.stud.uni-karlsruhe.de/~uck4/ICQ/

;;; Todo:
;;
;;   o Make it work
;;   o Finish adding the rest of the ICQ packet types.

;;; Code:

(defcustom eicq-use-firewall nil
  "*Set to non-nil if you are behind a firewall.

This doesn't have anything to do with socks 4/5 firewalls, Eicq doesn't
support socks yet."
  :type 'boolean
  :group 'eicq-network)

(defsubst eicq-hex-to-bin (hex)
  "Convert hexadecimal HEX to binary."
  (let ((hex-to-int
	 (lambda (hex-symbol)
	   (string-to-int (substring (format "%s" hex-symbol) 2 4) 16)))
	int)
    (setq int (mapcar hex-to-int hex))
    (let (result bin)
      (while int
	(push (char-to-string (int-to-char (car int))) result)
	(setq int (cdr int)))
      (setq result (nreverse result))
      (while result
	(setq bin (concat bin (car result)))
	(setq result (cdr result)))
      (symbol-value 'bin))))

(defsubst eicq-put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.

This is a carbon copy of `put-alist' in alist.el, it's here so it can
be redefined as a `defsubst'.  If there is a pair whose car is ITEM,
replace its cdr by VALUE.  If there is not such pair, create new pair
\(ITEM. VALUE\) and return new alist whose car is the new pair and cdr
is ALIST."
  (let ((pair (assoc item alist)))
    (if pair
	(progn
	  (setcdr pair value)
	  alist)
      (cons (cons item value) alist))))

(defsubst eicq-alist-hex-to-bin (alist)
  "Convert the value side of each cons cell in ALIST from hex to a binary string."
  (let* ((tmp-alist (copy-alist alist))
	 key)
    (while tmp-alist
      (setq key (push (caar tmp-alist) key))
      (setq tmp-alist (cdr tmp-alist)))
    (setq key (nreverse key))
    (while key
      (eicq-put-alist (car key) (eicq-hex-to-bin (cdr (assq (car key) alist))) alist)
      (setq key (cdr key)))
    (symbol-value 'alist)))

(defmacro deficq (const alist &optional doc)
  "Defines an ICQ protocol packet.

It is like `defconst' but does some data conversions.  

Argument CONST is the packet name.

Argument ALIST is its initial value.  It is an alist of 1 or more
\(symbol . value\) pairs where:

      symbol == parameter name 
      value  == parameter value \(expressed in hexadecimal\)

Every packet has \"\(type . VALUE\)\" as its first \(or only\) cons cell
which identifies the packet.

Optional Argument DOC is a doc string, and if present, will have
either the string \"This is an outgoing Eicq ICQ protocol packet.\",
or the string \"This is an incoming Eicq ICQ protocol packet.\", or
the string \"This is a peer to peer Eicq ICQ protocol packet.\", or
the string \"This makes up part of the FLAP header in an Eicq ICQ
protocol packet.\" appended to it.

The name of the constant determines whether it represents an outgoing,
incoming, peer to peer, or FLAP header packet.

      eicq::CLI_foo  == outgoing
      eicq::SVR_foo  == incoming
      eicq::P2P_foo  == peer to peer
      eicq::FLAP_foo == FLAP header

Here's an example:

      \(deficq eicq::CLI_foo '\(\(bar . \(0xF3 0x26\)\)
			      \(baz . \(0x0B 0x50\)\)\)
        \"This is eicq::CLI_foo's doc string.\")
       => eicq::CLI_foo

      \(describe-variable 'eicq::CLI_foo\)
       => `eicq::CLI_foo' is a variable declared in Lisp.

          Value: \(\(bar . \"� & \"\) \(baz . \" P \"\)\)

          Documentation:
          This is eicq::CLI_foo's doc string.

          This is an outgoing Eicq ICQ protocol packet.

As you can probably surmise from that example, the data conversion is
hexadecimal to binary string on the value side of each cons cell of
the alist."
  (if doc
      (cond ((string= (substring (symbol-name const) 0 9) "eicq::CLI")
	     `(defconst ,const
		(quote ,(eicq-alist-hex-to-bin (cadr alist)))
		,(concat doc "\n\nThis is an outgoing Eicq ICQ protocol packet.")))
	    ((string= (substring (symbol-name const) 0 9) "eicq::SVR")
	     `(defconst ,const 
		(quote ,(eicq-alist-hex-to-bin (cadr alist)))
		,(concat doc "\n\nThis is an incoming Eicq ICQ protocol packet.")))
	    ((string= (substring (symbol-name const) 0 9) "eicq::P2P")
	     `(defconst ,const 
		(quote ,(eicq-alist-hex-to-bin (cadr alist)))
		,(concat doc "\n\nThis is a peer to peer Eicq ICQ protocol packet.")))
	    ((string= (substring (symbol-name const) 0 10) "eicq::FLAP")
	     `(defconst ,const 
		(quote ,(eicq-alist-hex-to-bin (cadr alist)))
		,(concat doc "\n\nThis makes up part of the FLAP header in an Eicq ICQ protocol packet.")))
	    (t
	     (error
	      (substitute-command-keys
	       "Invalid packet name, see \\[describe-function] deficq"))))
    (if (or (string= (substring (symbol-name const) 0 9) "eicq::CLI")
	    (string= (substring (symbol-name const) 0 9) "eicq::SVR")
	    (string= (substring (symbol-name const) 0 9) "eicq::P2P")
	    (string= (substring (symbol-name const) 0 10) "eicq::FLAP"))
	`(defconst ,const (quote ,(eicq-alist-hex-to-bin (cadr alist))))
      (error
       (substitute-command-keys
	"Invalid packet name, see \\[describe-function] deficq")))))

(put 'deficq 'lisp-indent-function 1)
(put 'deficq 'edebug-form-spec '(const init &optional doc))

;;; General packets
(deficq eicq::FLAP_header
  '((id . (0x2a)))
  "This is prepended to the packets.

Add to this, an element from `eicq::FLAP_channels', a \"sequence\" number
which is randomly generated before each connection and incremented with
each packet, the length of the packet, and the actual packet and its
data, and Bob's yer Uncle, you've got yourself an ICQ packet.")

(deficq eicq::FLAP_channels
  '((login      . (0x01))
    (snac       . (0x02))
    (error      . (0x03))
    (disconnect . (0x04))
    (ping       . (0x05)))
  "This is the second half of a FLAP header.

See `eicq::FLAP_header'.")

(deficq eicq::CLI_bytefiller
  '((extra-bytes . (0x00 0x00 0x00 0x00 0x00 0x00)))
  "Used to fill each packet with the correct number of bytes.

Each packet's \"type\" parameter always ends in 6 bytes containing
\"0x00\".  So instead of defining them in each and every packet, we do
it once here.  Just imagine all those nano seconds we're saving in
processing time.")

;;; Login packets
(deficq eicq::CLI_IDENT
  '((hello        . (0x00 0x00 0x00 0x01))
    (cookie       . (0x00 0x06)) ; + length cookie + cookie string
    (uinhdr       . (0x00 0x01 0x00))
    ;; `eicq-length-hex' hasn't been written yet, like `length', but
    ;; return the answer in hex.
    (uinlength    . `(eicq-length-hex uin))
    ;; `eicq-uin-login' hasn't been written yet, it turns a UIN into
    ;; something we can convert into what the ICQ server wants to see.
    ;; eg. '34307457' -> '0x33 0x34 0x33 0x30 0x37 0x34 0x35 0x37'
    (uin          . `(eicq-uin-login eicq-user-alias))
    ;; my own password hardcoded for testing purposes
    (passwdhdr    . (0x00 0x02 0x00))
    (passwdlen    . `(eicq-length-hex passwd))
    ;; I think `eicq-encrypt-password' is wrong.  See `eicq-comm.el'.
    (passwd       . `(eicq-encrypt-password))
    ;; The client version we are pretending to be "ICQ Inc. - Product
    ;; of ICQ (TM).2002a.5.37.1.3728.85"
    (version      . (0x00 0x03 0x00 0x33 
			  0x49 0x43 0x51 0x20 0x49 0x6E 0x63 0x2E 
			  0x20 0x2D 0x20 0x50 0x72 0x6F 0x64 0x75
			  0x63 0x74 0x20 0x6F 0x66 0x20 0x49 0x43 
			  0x51 0x20 0x28 0x54 0x4D 0x29 0x2E 0x32
			  0x30 0x30 0x32 0x61 0x2E 0x35 0x2E 0x33 
			  0x37 0x2E 0x31 0x2E 0x33 0x37 0x32 0x38
			  0x2E 0x38 0x35))
    (unk          . (0x00 0x16 0x00 0x02 0x01 0x0a)) ; unknown
    (ver-major    . (0x00 0x17 0x00 0x02 0x00 0x05))
    (ver-minor    . (0x00 0x18 0x00 0x02 0x00 0x25))
    (ver-lessor   . (0x00 0x19 0x00 0x02 0x00 0x01))
    (ver-build    . (0x00 0x1a 0x00 0x02 0x0e 0x90))
    (ver-subbuild . (0x00 0x14 0x00 0x04 0x00 0x00 0x00 0x55))
    ;; language and country are hardcoded for now to English "en",
    ;; Australia "au" for testing purposes
    (language     . (0x00 0x0f 0x00 0x02 0x65 0x6e))
    (country      . (0x00 0x0e 0x00 0x02 0x61 0x75)))
  "CLI_IDENT Channel: 1 (login).

The packet sent upon establishing a connection. If the client wants to
login to login.icq.com, it sends all alist elements except 'cookie',
which is for login to the redirected server. To request a
new UIN, just send 'hello'.")

;;; Client to Server packets (no meta)
(deficq eicq::CLI_READY
  '((type     . (0x00 0x01 0x00 0x02))
    (family1  . (0x00 0x01 0x00 0x03 0x01 0x10 0x04 0x7b))
    (family19 . (0x00 0x13 0x00 0x02 0x01 0x10 0x04 0x7b))
    (family2  . (0x00 0x02 0x00 0x01 0x01 0x01 0x04 0x7b))
    (family3  . (0x00 0x03 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family21 . (0x00 0x15 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family4  . (0x00 0x04 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family6  . (0x00 0x06 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family9  . (0x00 0x09 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family10 . (0x00 0x0a 0x00 0x01 0x01 0x10 0x04 0x7b))
    (family11 . (0x00 0x0b 0x00 0x01 0x01 0x10 0x04 0x7b)))
  "CLI_READY Channel: 2 SNAC(1,2).

This packet seems to pass the SNAC Families and their versions along
with some unknown other information back to the server.")

(deficq eicq::CLI_RATESREQUEST
  '((type . (0x00 0x01 0x00 0x06)))
  "CLI_RATESREQUEST Channel: 2 SNAC(1,6).

This packet requests from the server several bits of information most
likely regarding how fast certain packets can be sent to the server
and possibly a maximum packet size as well.")

(deficq eicq::CLI_ACKRATES
  '((type   . (0x00 0x01 0x00 0x08))
    (group1 . (0x00 0x01))
    (group2 . (0x00 0x02))
    (group3 . (0x00 0x03))
    (group4 . (0x00 0x04))
    (group5 . (0x00 0x05)))
  "CLI_ACKRATES Channel: 2 SNAC(1,8).

This packet is sent in response to the SVR_RATES SNAC(1,7), see
`eicq::SVR_RATES'. This packet contains the same group numbers as
was in the SVR_RATES packet and is an acknowledgement of their
receipt.")

(deficq eicq::CLI_REQINFO
  '((type . (0x00 0x01 0x00 0x0E)))
  "CLI_REQINFO Channel: 2 SNAC(1,14).

This command requests from the server certain information about the
client that is stored on the server.")

(deficq eicq::CLI_SNAC1_11
  '((type  . (0x00 0x01 0x00 0x11))
    (param . (0x00 0x00 0x00 0x3c)))
  "CLI_SNAC1_11 Channel: 2 SNAC(1,17).

Sent before CLI_SETSTATUS, see `eicq::CLI_SETSTATUS'.")

(deficq eicq::CLI_FAMILIES
  '((type     . (0x00 0x01 0x00 0x17))
    (family1  . (0x00 0x01 0x00 0x03))
    (family19 . (0x00 0x13 0x00 0x04))
    (family2  . (0x00 0x02 0x00 0x01))
    (family3  . (0x00 0x03 0x00 0x01))
    (family21 . (0x00 0x15 0x00 0x01))
    (family4  . (0x00 0x04 0x00 0x01))
    (family6  . (0x00 0x06 0x00 0x01))
    (family9  . (0x00 0x09 0x00 0x01))
    (family10 . (0x00 0x0a 0x00 0x01))
    (family11 . (0x00 0x0b 0x00 0x01)))
  "CLI_FAMILIES Channel: 2 SNAC(1,23).

This packet is a response to SVR_FAMILIES SNAC(1,3), see
`eicq::SVR_FAMILIES'.  This tells the server which SNAC families and
their corresponding versions which the client understands. This also
seems to identify the client as an ICQ vice AIM client to the server.")

(deficq eicq::CLI_SETSTATUS
  '((type        . (0x00 0x01 0x00 0x1e))
    (status-head . (0x00 0x06 0x00 0x04))  ; + status code
    (error       . (0x00 0x08 0x00 0x02))  ; + error code
    (cli2cli     . (0x00 0x0c))            ; there's more to this... no idea
    (ip          . `(eicq-get-hex-ip))     ; gotta write this
    (dcport      . `(eicq-get-hex-dcport)) ; gotta write this
    (tcpflag-fw  . (0x01))                 ; Behind firewall
    (tcpflag     . (0x04))                 ; Normal connection
    (icqver      . (0x00 0x08))
    (dccookie    . (0x00 0x00 0x00 0x00))
    (unk1        . (0x00 0x00))
    (unk2        . (0x00 0x50))
    (unk3        . (0x00 0x00))
    (count       . (0x00 0x03))
    (clientid    . (0xfe 0xf1 0xfc 0xf0)) ; A time string but abused for client id.
    (time2       . (0x00 0x00 0x00 0x00)) ; Another time string, also abused for client id
    (time3       . (0x00 0x00 0x00 0x00)) ; Another time string
    (unk4        . (0x00 0x00)))
  "CLI_SETSTATUS Channel: 2  SNAC(1,30).

This sets the clients online status code and some other direct client
to client information as well.

This packet is made up of:

    type         --  packet type SNAC(1,30) (x01/x1e)
    status-head  --  prefix to the status code, 
                     see `eicq::CLI_status-codes' and
                     `eicq::SVR_status-codes'
    error        --  prefix to an error code
    cli2cli      --  not sure about this
    ip           --  local IP address (not implemented yet)
    dcport       --  port to use for direct connections
                     (not implemented yet)
    tcpflag-fw   --  flag to set if behind a firewall
    tcpflag      --  as above with no firewall
    icqver       --  highest ICQ protocol version this client uses
    dccookie     --  a server generated per/direct connection cookie
    unk1         --  unknown
    unk2         --  unknown
    unk3         --  unknown
    count        --  the number of timestamps following
    clientid     --  a timestamp but we're using it as a client id
    time2        --  another timestamp, also used for client ids
    time3        --  another timestamp
    unk4         --  unknown")

(deficq eicq::CLI_REQLOCATION 
  '((type . (0x02 0x02)))
  "CLI_REQLOCATION Chennel: 2 SNAC(2,2).

Request rights information for location service. This is from the
OSCAR document.")

(deficq eicq::CLI_SETUSERINFO
  '((type             . (0x00 0x02 0x00 0x04))
    (caps             . (0x00 0x05 0x00 0x40))
    (caps-serverrelay . (0x09 0x46 0x13 0x49 0x4c 0x7f 0x11 0xd1 
			      0x82 0x22 0x44 0x45 0x53 0x54 0x00 0x00))
    (caps-2002        . (0x09 0x46 0x13 0x4e 0x4c 0x7f 0x11 0xd1 
			      0x82 0x22 0x44 0x45 0x53 0x54 0x00 0x00))
    (caps-rtf         . (0x97 0xb1 0x27 0x51 0x24 0x3c 0x43 0x34 
			      0xad 0x22 0xd6 0xab 0xf7 0x3f 0x14 0x92))
    (caps-icq         . (0x09 0x46 0x13 0x44 0x4c 0x7f 0x11 0xd1 
			      0x82 0x22 0x44 0x45 0x53 0x54 0x00 0x00)))
  "CLI_SETUSERINFO Channel: 2 SNAC(2,4).

This packet sends the client's capabilities information to the server.")

(deficq eicq::CLI_REQBUDDY
  '((type . (0x00 0x03 0x00 0x02)))
  "CLI_REQBUDDY Channel: 2 SNAC(3,2).

Request rights information for buddy service. This from the OSCAR
document.")

;; Don't use `eicq::CLI_bytefiller' with this one.
(deficq eicq::CLI_ADDCONTACT
  '((type . (0x00 0x03 0x00 0x04 0x00 0x00 0x00 0x00 0x00 0x04)))
  ;; There are parameters to this: UINs and their lengths.
  ;; eg.  (length . (0x08))
  ;;      (uin    . (0x33 0x34 0x33 0x30 0x37 0x34 0x35 0x37))
  ;; For UIN '34307457'
  "CLI_ADDCONTACT Chennel:2  SNAC(3,4).

This is sent at login and when you add a new user to your contact
list. It contains a list of all the uin's in you're contact list.
The list is length UIN pairs.

    eg. (length . (0x08))
        (uin    . (0x33 0x34 0x33 0x30 0x37 0x34 0x35 0x37))

For UIN '34307457'.")

(deficq eicq::CLI_REMCONTACT
  '((type . (0x00 0x03 0x00 0x05)))
  "CLI_REMCONTACT Channel: 2  SNAC(3,5).

Sent to remove contacts from contact list.  I'm not sure about the
content of this packet, my guess would be that take the same format as
`eicq::CLI_ADDCONTACT'.")

(deficq eicq::CLI_SETICBM
  '((type  . (0x00 0x04 0x00 0x02))
    (param . (0x00 0x00 0x00 0x00 0x00 0x03 0x1f 0x40 
		   0x03 0xe7 0x03 0xe7 0x00 0x00 0x00 0x00)))
  "CLI_SETICBM Channel: 2 SNAC(4,2).

This packet seems to change some of the values passed from the server
in SRV_REPLYICBM SNAC(4,5), see `eicq::SVR_REPLYICBM'.")

(deficq eicq::CLI_REQICBM
  '((type . (0x00 0x04 0x00 0x04)))
  "CLI_REQICBM Channel: 2 SNAC(4,4).

Request rights information for ICBM (instant messages) operations. This
from the OSCAR document.")

;;(deficq eicq::CLI_SENDMSG
;;  '((type . (0x00 0x04 0x00 0x06))
;;    (mid  . (0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)) ; that's probably wrong
;;    (

(deficq eicq::CLI_REQBOS
  '((type . (0x00 0x09 0x00 0x02)))
  "CLI_REQBOS Channel: 2 SNAC(9,2).

Request BOS rights.  This is from the OSCAR document.")



;;; Client to Server packets (meta)

;;; Server to Client packets (no meta)

;;; Server to Client packets (meta)

;;; Peer to Peer packets (client to client)



(provide 'eicq-v8proto)

;;; eicq-v8proto.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
