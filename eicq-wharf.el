;;; eicq-wharf.el --- DockApp/status window for Eicq

;; Copyright (C) 2001,03,04 Steve Youngs, Erik Arneson

;; RCS: $Id$
;; Author:        Erik Arneson <erik@aarg.net>
;; Maintainer:    Erik Arneson <erik@aarg.net>
;; Created:       Aug 10, 2001
;; Last-Modified: <2004-05-30 11:16:06 (steve)>
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

(defvar eicq-wharf-frame nil
  "Frame in which EicqWharf is running.")

(defvar eicq-wharf-frame-props
  '((name . "EicqWharf")
    (height . 4)
    (width . 8)
    (unsplittable . t)
    (minibuffer . none)
    (menubar-visible-p . nil)
    (has-modeline-p . nil)
    (default-gutter-visible-p . nil)
    (default-toolbar-visible-p . nil)
    (scrollbar-height . 0)
    (scrollbar-width . 0))
  "Frame properties for EicqWharf.")

(defvar eicq-wharf-buf nil
  "Buffer in which EicqWharf is running.")

(defgroup eicq-wharf nil
  "Miniature Eicq status window."
  :prefix "eicq-wharf-"
  :group 'eicq)

(defface eicq-wharf-default-face
  '((((class color))
     (:foreground "Green" :family "fixed" :size "9pt"))
    (t
     (:family "fixed" :size "9pt")))
  "Face used in EicqWharf window.  If you want this to be dockable, make sure
you use a small but readable font."
  :group 'eicq-wharf)

;;;###autoload
(defcustom eicq-wharf-frame-use-p nil
  "If non-NIL, start up the EicqWharf mini-frame."
  :type 'boolean
  :group 'eicq-wharf
  :tag "EicqWharf mini-frame")

;;; Internal variables

;;;###autoload
(defun eicq-wharf-new-frame ()
  "Create new EicqWharf frame."
  (unless (frame-live-p eicq-wharf-frame)
    (setq eicq-wharf-frame (new-frame eicq-wharf-frame-props))
    (select-frame eicq-wharf-frame)
    (unless (buffer-live-p eicq-wharf-buf)
      (setq eicq-wharf-buf (get-buffer-create "*EicqWharf*"))
      (set-buffer-dedicated-frame eicq-wharf-buf eicq-wharf-frame)
      (save-excursion
        (set-buffer eicq-wharf-buf)
        (insert "New 000\nSys 000\nStatus")
        (set-extent-face (make-extent (point-min) (point-max) eicq-wharf-buf)
                         'eicq-wharf-default-face)
        ))
    (if (fboundp 'set-specifier)
        (progn
          (set-specifier horizontal-scrollbar-visible-p nil
                         (cons eicq-wharf-frame nil))
          (set-specifier vertical-scrollbar-visible-p nil
                         (cons eicq-wharf-frame nil))))
    (set-face-font 'default
                   (face-font-name 'eicq-wharf-default-face)
                   eicq-wharf-frame)
    (set-window-buffer nil eicq-wharf-buf)))

(defun eicq-wharf-change-messages (type num)
  (let (oldnum newnum)
    (if eicq-wharf-buf
        (save-excursion
          (set-buffer eicq-wharf-buf)
          (goto-char (point-min))
          (if (re-search-forward (concat "^\\("
                                         type
                                         " *\\([0-9]+\\)\\)$")
                                 nil t)
              (progn
                (setq oldnum (string-to-int (match-string 2))
                      newnum (+ oldnum num))
                (if (> 0 newnum)
                    (setq newnum 0))
                (replace-match (format "%-3s %03d" type newnum))))))))

(defun eicq-wharf-inc-messages ()
  "Increment number of new messages in EicqWharf."
  (eicq-wharf-change-messages "New" 1))

(defun eicq-wharf-dec-messages ()
  "Decrement number of new messages in EicqWharf."
  (eicq-wharf-change-messages "New" -1))

(defun eicq-wharf-inc-system ()
  "Increment number of system messages in EicqWharf."
  (eicq-wharf-change-messages "Sys" 1))

(defun eicq-wharf-dec-system ()
  "Decrement number of system messages in EicqWharf."
  (eicq-wharf-change-messages "Sys" -1))

(provide 'eicq-wharf)
;;; eicq-wharf.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
