;; eicq-setup.el --- Setup user directory and files for Eicq.

;; Copyright (C) 2002,03 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-10-03
;; Last-Modified: <2003-09-19 11:49:59 (steve)>
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

(defconst eicq-user-directory
  (file-name-as-directory
   (expand-file-name ".eicq" (getenv "HOME")))
  "Directory to store rc file and log files.")

(defconst eicq-world-user-file-template
  "
Literally you can put anything you like in this resource file, such as
this introduction. The only thing the eicq reader cares is keyword
\":icq\" at the beginning of a line. For example,

:icq 34307457 eicq
:icq 12345678 me
:icq 88888888 the queen

This way, it defines three buddies: \"eicq\", \"me\", and \"the queen\". It
reads alias name until the end of the line. You can customize this
behaviour with variable `eicq-world-rc-regexp'. Any other lines are
ignored. 

Adding your own UIN:

Just change \"12345678 me\" above to your UIN/alias.  And don't forget
to change the (setq eicq-user-alias \"me\") line in ~/.emacs to match.

BTW, that 1st buddy up there, (34307457 eicq), is a valid UIN, it's
mine. :-)

Remember to M-x eicq-world-update after changing this rc file.

"
  "Template used to create user's initial Eicq rc file.")

;;;###autoload
(defun eicq-setup ()
  "Setup your personal Eicq directory.

This directory, which defaults to '~/.eicq/', holds the resource file
for your ICQ contacts and any log files.  It's also a good place to
put your sound files."
  (interactive)
  (unless (file-directory-p eicq-user-directory)
    (make-directory eicq-user-directory t))
  (unless (file-exists-p (expand-file-name "world" eicq-user-directory))
    (save-excursion
      (find-file (expand-file-name "world" eicq-user-directory))
      (switch-to-buffer "world")
      (insert (symbol-value 'eicq-world-user-file-template))
      (save-buffer "world")
      (kill-buffer "world")))
  (message 
   (format "Don't forget to edit %sworld to your requirements"
	   eicq-user-directory)))
	      

(provide 'eicq-setup)
;;; eicq-setup.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
