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
(defvar display-keys--buffer " *display-keys*")

(defcustom display-keys-frame-parameters
  `((name . "Keys Display")
    (unsplittable . t)
    (cursor-type . nil)
    (alpha . 70)
    (height . 6)
    (width . 30)
    (top .  400)
    (left . -300)
    (user-position . t)
    (user-size . t)
    (border-width . 0)
    ;; (internal-border-width . 0)
    (buffer-predicate . ,(lambda (b) (equal (buffer-name b)
                                       display-keys--buffer)))
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (tool-bar-lines . 0)
    (menu-bar-lines . 0)
    (minibuffer . nil))
  "Parameters used on the recording frame.
See `make-frame'."
  :type '(alist :key-type symbol :value-type sexp))

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

(defun display-keys--prettify-prefix (p)
  "Return a list of keys for prefix argument P."
  (cond
   ((and (numberp p) (< p 0))
    (cons ?\C-- (cdr (display-keys--prettify-prefix (- p)))))
   ((numberp p)
    (list ?\C-u (+ p 48)))
   ((consp p)
    (make-list (round (log (car p) 4)) ?\C-u))))

;;; Display logic
(defvar display-keys--timer nil)
(defun display-keys--hide-frame ()
  (ignore-errors
    (when-let ((w (get-buffer-window display-keys--buffer 'all-frames)))
      (iconify-frame (window-frame w)))))

(defcustom display-keys-duration 3
  "Number of seconds the display-keys frame is displayed."
  :type '(choice integer nil))

(defun display-keys--start-timer ()
  (when (timerp display-keys--timer)
    (cancel-timer display-keys--timer))
  (when display-keys-duration
    (setq display-keys--timer (run-at-time display-keys-duration nil #'display-keys--hide-frame))))

(defun display-keys--for-command (&rest _)
  "Display the keys that invoked the current command."
  (with-current-buffer (display-keys--buffer)
    (display-keys--start-timer)
    (with-selected-window (get-buffer-window (current-buffer) 'all-frames)
      (erase-buffer)
      (recenter 1)
      (insert "\n " (symbol-name this-command))
      (insert "\n\n")
      (mapc (lambda (k) (insert " "
                           (propertize (help-key-description (vector k) nil)
                                       'face 'display-keys-key)))
            (append (display-keys--prettify-prefix current-prefix-arg)
                    (this-command-keys-vector)
                    nil)))))

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
