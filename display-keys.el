;;; display-keys.el --- Pretty display keys when they are pressed  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: convenience
;; PackageRequires: ((emacs "24.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'subr-x)

(defgroup display-keys nil
  "Customization group for display-keys."
  :prefix "display-keys-"
  :group 'emacs)

(defface display-keys-key 
  '((t
     :height 2.0
     :weight bold
     :background "gray50"
     :foreground "black"
     :box (:line-width 5 :style released-button :color "gray50")))
  "Face used on keys.")

;;; Buffer
(defcustom display-keys-frame-parameters
  '((name . "Keys Display")
    (height . 2)
    (width . 30)
    (top .  80)
    (left . 400)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (menu-bar-lines . nil)
    (minibuffer . nil))
  "Parameters used on the recording frame.
See `make-frame'."
  :type '(alist :key-type symbol :value-type sexp))

(defvar display-keys--buffer " *display-keys*")

(defun display-keys--buffer ()
  "Return the buffer used to display keys."
  (let* ((buf (or (get-buffer display-keys--buffer)
                  (with-current-buffer (get-buffer-create display-keys--buffer)
                    (set-scroll-bar-mode nil)
                    (setq-local mode-line-format nil)
                    (setq-local cursor-type nil)
                    (current-buffer)))))
    (let ((old-frame (selected-frame))
          (frame (if-let ((win (get-buffer-window buf 'all-frames)))
                     (window-frame win)         
                   (with-selected-frame (make-frame display-keys-frame-parameters)
                     (switch-to-buffer display-keys--buffer)
                     (selected-frame)))))
      (raise-frame frame)
      (select-frame old-frame))
    buf))

(defun display-keys--for-command (&rest _)
  "Display the keys that invoked the current command."
  (with-current-buffer (display-keys--buffer)
    (with-selected-window (get-buffer-window (current-buffer) 'all-frames)
      (erase-buffer)
      (insert "\n")
      (mapc (lambda (k) (insert " "
                           (propertize (help-key-description (vector k) nil)
                                       'face 'display-keys-key)))
            (this-command-keys-vector))
      (recenter))))

;;; Commands
(defvar display-keys--command-list nil)

(defun display-keys-for-commands (&rest commands)
  "Turn on keys display for the given COMMANDS."
  (interactive "CCommand name: ")
  (dolist (f commands)
    (cl-pushnew f display-keys--command-list)
    (advice-add f :before #'display-keys--for-command)))

(defun display-keys-disable-for-commands (&rest commands)
  "Turn off keys display for the given COMMANDS."
  (interactive "CCommand name (empty for all): ")
  (when (or (equal commands 'all)
            (equal commands '(##)))
    (setq commands display-keys--command-list)
    (setq display-keys--command-list nil))
  (dolist (f commands)
    (advice-remove f #'display-keys--for-command))
  (when (called-interactively-p 'any)
    (message "display-keys disabled for %s commands"
             (length commands))))

(provide 'display-keys)
;;; display-keys.el ends here
