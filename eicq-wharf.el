;;; eicq-wharf.el --- DockApp/status window for Eicq

;; Copyright (C) 2001 Steve Youngs, Erik Arneson

;; RCS: $Id$
;; Author: Erik Arneson <erik@aarg.net>
;; Maintainer: Erik Arneson <erik@aarg.net>
;; Created: Aug 10, 2001
;; Last-Modified: <2001-9-20 16:30:10 (erik)>
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

(require 'eicq)

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
;time-stamp-format: "%4y-%m-%d %02H:%02M:%02S (%u)"
;End: 
