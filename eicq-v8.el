;;; eicq-v8.el --- ICQ v8 for eicq.

;; Copyright (C) 2004 Steve Youngs, Zajcev Evgeny

;; Author:        Zajcev Evgeny <zevlg@yandex.ru>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Created:       Mon Jun  7 16:20:56 MSD 2004
;; Last-Modified: <2004-06-09 10:46:29 (steve)>
;; Keywords:      eicq ICQ protocol

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

;;  The various network packets that make up the ICQv8 protocol.
;;
;;  See http://www.stud.uni-karlsruhe.de/~uck4/ICQ/

;;; Code:

(eval-when-compile
  (require 'cl))

;;;; Constants

;; FLAP channel ids
(defconst eicq-FLAP-HELLO 1)
(defconst eicq-FLAP-SNAC 2)
(defconst eicq-FLAP-ERRORS 3)
(defconst eicq-FLAP-LOGOFF 4)
(defconst eicq-FLAP-PING 5)
(defconst eicq-FLAP-VER-MAJOR 5)
(defconst eicq-FLAP-VER-MINOR 37)
(defconst eicq-FLAP-VER-LESSER 1)
(defconst eicq-FLAP-VER-BUILD 3828)
(defconst eicq-FLAP-VER-SUBBUILD 85)

(defstruct eicq-ctx
  proc					; eicq network connection
  readingp				; non-nil if we are currently reading from eicq context
  sequence				; current flap sequence

  income-buffer				; where incoming data stored
  income-handlers
  outcome-buffer			; buffer for outgoing messages
  outcome-handles

  userinfo				; plist of userinfo '(uin <num> password <passwd>)
  plist)				; user defined plist

(defun eicq-ctx-get-prop (ectx prop)
  "Get ECTX's property PROP value."
  (plist-get (eicq-ctx-plist ectx) 'prop))

(defun eicq-ctx-put-prop (ectx prop val)
  "Put ECTX's property PROP value VAL."
  (setf (eicq-ctx-plist ectx)
	(plist-put (eicq-ctx-plist ectx) prop val)))

(defun eicq-ctx-rm-prop (ectx prop)
  "Remove ECTX's property PROP."
  (setf (eicq-ctx-plist ectx)
	(plist-remprop (eicq-ctx-plist ectx) prop)))

(defvar eicq-default-ctx nil
  "Internal variable, do not modify.")

(defvar eicq-connections nil
  "List if eicq connections.
Internal variable, do not modify.")

(defun eicq-ctx ()
  "Return default Eicq context."
  eicq-default-ctx)

;; FLAP accessors
(defsubst eicq-flap-cid (flap)
  (nth 0 flap))

(defsubst eicq-flap-seq (flap)
  (nth 1 flap))

(defsubst eicq-flap-data (flap)
  (nth 2 flap))

;;; SNAC accessors
(defsubst eicq-snac-family (snac)
  (nth 0 snac))

(defsubst eicq-snac-subtype (snac)
  (nth 1 snac))

(defsubst eicq-snac-flags0 (snac)
  (nth 2 snac))

(defsubst eicq-snac-flags1 (snac)
  (nth 3 snac))

(defsubst eicq-snacrid (snac)
  (nth 4 snac))

;;; TLV accessors
(defsubst eicq-tlv-type (tlv)
  (nth 0 tlv))

(defsubst eicq-tlv-val (tlv)
  (nth 1 tlv))

(defun eicq-util-encrypt (password)
  (let ((tb "\xf3\x26\x81\xc4\x39\x86\xdb\x92\x71\xa3\xb9\xe6\x53\x7a\x95\x7c")
	(res (make-string (length password) ?\x00))
	(idx 0))
    (while (< idx (length password))
      (aset res idx (logxor (aref password idx) (aref tb (mod idx 16))))
      (setq idx (1+ idx)))
    res))

(defun eicq-connect (&optional server port)
  "Connect to SERVER and PORT."
  (unless server
    (setq server "login.icq.com"))
  (unless port
    (setq port 5190))
  (let* ((proc (open-network-stream (format "eicq-%s:%d" server port) nil
				    server port))
	 (ectx (make-eicq-ctx :sequence (random 65536)
			      :proc proc)))
    (set-process-filter proc 'eicq-proc-filter)
    (set-process-sentinel proc 'eicq-proc-sentinel)
    ;; Store connection in eicq connections list
    (push ectx eicq-connections)
    ectx))

(defun eicq-close (ectx)
  "Close ECTX connection."
  ;; TODO:
  ;;  - write me
  (eicq-proc-sentinel (eicq-ctx-proc ectx)))

(defun eicq-send (ectx msg)
  "Using eicq context ECTX, send message MSG."
  (process-send-string (eicq-ctx-proc ectx) msg))

(put 'eicq-send 'lisp-indent-function 1)

(defun eicq-send-flush (ectx)
  "Flush ECTX's output buffer."
  (when (> (length (eicq-ctx-outcome-buffer ectx)) 0)
    (process-send-string (eicq-ctx-proc ectx) (eicq-ctx-outcome-buffer ectx))))

(defun eicq-send-recv (ectx msg spec)
  "Using eicq context ECTX send MSG and wait for reply."
  (eicq-send ectx msg)

  ;; TODO:
  ;;  - write me
  )

(defun eicq-proc-find (proc)
  "Find eicq context by PROC."
  (let ((ecs eicq-connections))
    (while (and ecs (not (eq (eicq-ctx-proc (car ecs)) proc)))
      (setq ecs (cdr ecs)))
    (car ecs)))

(defun eicq-proc-sentinel (proc &optional event)
  "Sentinel for eicq PROC."
  (let ((ectx (eicq-proc-find proc)))
    (when (eicq-ctx-p ectx)
      (message "eicq: removing procces %S" proc)
      (sit-for 1)
      (delete-process proc)
      (setq eicq-connections (delq ectx eicq-connections)))))

(defun eicq-proc-filter (proc out)
  "Filter for eicq connection."
  (let ((ectx (eicq-proc-find proc)))
    (when (eicq-ctx-p ectx)
      (setf (eicq-ctx-income-buffer ectx)
	    (concat (eicq-ctx-income-buffer ectx) out))
      (eicq-proc-process-incoming ectx))))

(defun eicq-proc-process-incoming (ectx)
  "Process incoming."
  (unless (eicq-ctx-readingp ectx)
    (let* ((flap (eicq-next-flap ectx))
	   (cid (eicq-flap-cid flap)))
      
      (cond ((= cid eicq-FLAP-HELLO)
	     ;; XXX Login channel
	     (let ((cmd (eicq-parse-message nil [4 number])))
	       (cond ((= cmd 1)
		      (eicq-login ectx))

		     (t (error (format "Unknown FLAP CMD(%) in login channel." cmd))))
	       ))
	    
	    ((= cid eicq-FLAP-SNAC)
	     ;; SNAC channel
	     ;; TODO:
	     ;;  - Write me
	     )

	    ((= cid eicq-FLAP-ERRORS)
	     ;; ERRORS channel
	     )

	    ((= cid eicq-FLAP-LOGOFF)
	     ;; LOGOFF channel
	     )

	    ((= cid eicq-FLAP-PING)
	     ;; PING channel
	     )
	    
	    (t (error (format "Unknown FLAP cid(%d)." cid))))
)))


(defun eicq-number->string (size val)
  "Convert number value VAL to string of SIZE."
  (cond ((= size 1)
	 (char-to-string val))
	((= size 2)
	 (concat (char-to-string (ash (mod val 65536) -8))
		 (char-to-string (logand val 255))))
	((= size 4)
	 (mapconcat 'identity
		    (nreverse 
		     (list (char-to-string (int-to-char (% val 256)))
			   (char-to-string (int-to-char (% (setq val (/ val 256)) 256)))
			   (char-to-string (int-to-char (% (setq val (/ val 256)) 256)))
			   (char-to-string (int-to-char (% (setq val (/ val 256)) 256)))))
		    ""))
	(t (error "Invalid SIZE" size))))

(defun eicq-create-message (&rest UNIPREFIX-message-spec)
  "Create eicq message according to SPEC."
  (let (UNIPREFIX-value-spec UNIPREFIX-value)
    (mapconcat (lambda (UNIPREFIX-element)
		 (setq UNIPREFIX-value-spec (eval (aref UNIPREFIX-element 0)))
		 (setq UNIPREFIX-value (eval (aref UNIPREFIX-element 1)))
		 
		 (cond ((numberp UNIPREFIX-value)
			(eicq-number->string UNIPREFIX-value-spec UNIPREFIX-value))
		       ((stringp UNIPREFIX-value)
			(substring  UNIPREFIX-value 0 UNIPREFIX-value-spec))
		       ((null UNIPREFIX-value)
			(make-string UNIPREFIX-value-spec ?\x00))
		       (t (error "Invalid SPEC" UNIPREFIX-value-spec))))
	       UNIPREFIX-message-spec "")))
  
(defun eicq-create-flap (ectx chan-id flap-data)
  "Create FLAP packet of channel id CHAN-ID and DATA."
  (let* ((len (length flap-data))
	 (msg (eicq-create-message [1 #x2A]
				   [1 chan-id]
				   [2 (eicq-ctx-sequence ectx)]
				   (vector 2 len)
				   (vector len flap-data))))
    ;; Increase sequence number
    (incf (eicq-ctx-sequence ectx))
    (when (> (eicq-ctx-sequence ectx) 65535)
      (setf (eicq-ctx-sequence ectx)
	    (% (eicq-ctx-sequence ectx) 65536)))
    msg))

(put 'eicq-create-flap 'lisp-indent-function 2)

(defun eicq-create-snac (ectx family-subtype snac-data &optional rid flags0 flags1)
  "Create SNAC packet."
  (eicq-create-flap ectx 2
    (concat (eicq-create-message
	     [2 (car family-subtype)]
	     [2 (cdr family-subtype)]
	     [1 flags0]
	     [1 flags1]
	     [4 rid])
	    snac-data)))

(put 'eicq-create-snac 'lisp-indent-function 2)

(defconst eicq-tlv-types
  '((uin . 1)
    (password . 2)
    (version . 3)))

(defun eicq-tlv-type-value (type)
  "Return TLV type as number.
TYPE is one of 'uin, 'password, 'version .."
  (cdr (assq type eicq-tlv-types)))

(defun eicq-create-tlv (type val)
  "Create TLV."
  (let ((len (length val)))
    (eicq-create-message (vector 2 type)
			 (vector 2 len)
			 (vector len val))))

(put 'eicq-create-snac 'lisp-indent-function 1)

(defun eicq-parse-message (msg spec)
  "Parse MSG according to SPEC.
General messages parser.  NOT IMPLEMENTED.

SPEC is one of
 - Vector         specifies one value.
 - List           specifies multiple values.
 - List of Lists  specifies list of lists of multiple values."
  (error 'unimplemented (symbol-name 'eicq-parse-message)))

(put 'eicq-parse-message 'lisp-indent-function 1)

(defun eicq-next-flap (ectx)
  "Get next queued FLAP from eicq context ECTX."
  (let ((oflap (eicq-parse-message nil
		 (list [1 #x2A]
		       [1 number]		; channel-id
		       [2 number]		; sequence
		       [2 eicq-length-1]
		       [eicq-length-1 string]))))
    (when (null oflap)
      (error "Unexpected non-flap message."))
    oflap))

(defun eicq-next-snac (ectx &optional msg-string)
  "Get next SNAC."
  (eicq-parse-message msg-string
    (list [2 number]			;family
	  [2 number]			;subtype
	  [1 number]			;flags0
	  [1 number]			;flags1
	  [4 number])))			;rid

(defun eicq-next-tlv (ectx &optional msg-string)
  "Interpret MSG as TLV."
  (eicq-parse-message msg-string
    (list [2 number]			; type
	  [2 eicq-length-1]		; length
	  [eicq-length-1 string])))	; value


(defun eicq-login (ectx &optional uin password)
  "In ECTX context send UIN/PASSWORD to ICQ server."
  (unless uin
    (setq uin (plist-get (eicq-ctx-userinfo ectx) 'uin)))
  (unless password
    (setq password (plist-get (eicq-ctx-userinfo ectx) 'password)))
    
  (eicq-send ectx
    (eicq-create-flap ectx eicq-FLAP-HELLO
      (concat
       (eicq-create-message [4 #x00000001])
       (eicq-create-tlv 1 uin)
       (eicq-create-tlv 2 (eicq-util-encrypt password))
       (eicq-create-tlv 3 eicq-version)
       (eicq-create-tlv #x16 (eicq-create-message [2 #x010A]))
       (eicq-create-tlv #x17 (eicq-create-message [2 eicq-FLAP-VER-MAJOR]))
       (eicq-create-tlv #x18 (eicq-create-message [2 eicq-FLAP-VER-MINOR]))
       (eicq-create-tlv #x19 (eicq-create-message [2 eicq-FLAP-VER-LESSER]))
       (eicq-create-tlv #x1A (eicq-create-message [2 eicq-FLAP-VER-BUILD]))
       (eicq-create-tlv #x14 (eicq-create-message [4 eicq-FLAP-VER-SUBBUILD]))
       (eicq-create-tlv #x0F "en")
       (eicq-create-tlv #x0E "us")))))


(provide 'eicq-v8)

;;; eicq-v8.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
