;; eicq-setup.el --- Setup user directory and files for Eicq.

;; Copyright (C) 2002 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-10-03
;; Last-Modified: <2002-10-03 09:45:49 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

;; This file is part of Eicq.

;; Eicq is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Eicq is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

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
