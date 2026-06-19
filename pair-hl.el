;;; pair-hl.el --- Highlight enclosing and adjacent pairs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Dohna <pub@lya.moe>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Dohna <pub@lya.moe>
;; Keywords: faces, convenience
;; URL: https://codeberg.org/dohna/pair-hl.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; pair-hl is a minor mode that dynamically highlights the enclosing and
;; adjacent delimiter pairs as you move point.  Works for parentheses,
;; brackets, braces and string quotes.
;;
;; Usage:
;;
;; To toggle the mode in the current buffer:
;;   M-x pair-hl-mode
;; To start the mode automatically in most programming modes:
;;   (add-hook 'prog-mode-hook #'pair-hl-mode)

;;; Code:

(defgroup pair-hl nil
  "Highlight enclosing and adjacent pairs."
  :group 'convenience)

(defcustom pair-hl-debounce-delay 0.2
  "Idle time in seconds to wait before highlighting pairs.
Setting this prevents calculation spam while typing quickly.
Set to 0 to highlight immediately without debouncing."
  :type 'number
  :group 'pair-hl)

(defcustom pair-hl-highlight-enclosing t
  "When non-nil, highlight the innermost enclosing pair."
  :type 'boolean
  :group 'pair-hl)

(defcustom pair-hl-highlight-adjacent-before t
  "When non-nil, highlight paired delimiters directly before point."
  :type 'boolean
  :group 'pair-hl)

(defcustom pair-hl-highlight-adjacent-after t
  "When non-nil, highlight paired delimiters directly after point."
  :type 'boolean
  :group 'pair-hl)

(defcustom pair-hl-adjacent-max-distance 1
  "Maximum chars to search before/after point for adjacent pairs.
1 means only the character immediately adjacent to point is checked.
Larger values search a wider periphery so you don't need to be
exactly next to a delimiter for it to light up."
  :type 'integer
  :group 'pair-hl)

(defcustom pair-hl-highlight-string-quotes t
  "When non-nil, highlight string-quote delimiters (class 7, e.g. \"...\")."
  :type 'boolean
  :group 'pair-hl)

(defface pair-hl-enclosing-face
  '((t :inherit underline))
  "Face for the inner enclosing pair."
  :group 'pair-hl)

(defface pair-hl-adjacent-before-face
  '((t :background "wheat"))
  "Face for the pair directly before the point."
  :group 'pair-hl)

(defface pair-hl-adjacent-after-face
  '((t :background "turquoise"))
  "Face for the pair directly after the point."
  :group 'pair-hl)

(defface pair-hl-mismatch-face
  '((t :background "salmon"))
  "Face for unmatched/mismatched delimiters."
  :group 'pair-hl)

(defvar-local pair-hl--overlays nil
  "List of currently active overlays.")

(defvar-local pair-hl--last-point nil
  "Cache of the last point position to prevent redundant calculations.")

(defvar-local pair-hl--last-tick nil
  "Cache of `buffer-chars-modified-tick' to prevent redundant updates.")

(defvar-local pair-hl--timer nil
  "Store the current debounce idle timer.")

(defun pair-hl--clear ()
  "Remove all active pair highlights."
  (mapc #'delete-overlay pair-hl--overlays)
  (setq pair-hl--overlays nil))

(defun pair-hl--cancel-timer ()
  "Cancel the pending debounce timer if it exists."
  (when pair-hl--timer
    (cancel-timer pair-hl--timer)
    (setq pair-hl--timer nil)))

(defun pair-hl--get-enclosing-pair (ppss)
  "Return (open-pos . close-pos) for the inner enclosing pair.
Returns (mismatch . pos) if the opening delimiter is unmatched."
  (let* ((string-hl (and (nth 3 ppss)
                         pair-hl-highlight-string-quotes))
         (paren-hl (nth 9 ppss))
         (start (cond (string-hl (nth 8 ppss))
                      (paren-hl (car (last (nth 9 ppss)))))))
    (when start
      (condition-case nil
          (cons start (1- (scan-sexps start 1)))
        (scan-error (cons 'mismatch start))))))

(defun pair-hl--check-adjacent (pos dir max-d)
  "Check positions near POS for matching delimiters in direction DIR.
Returns a single descriptor for the nearest match/mismatch,
or nil if none found within MAX-D."
  (let* ((beg (point-min))
         (end (point-max))
         (backward (< dir 0))
         (target-class (if backward 5 4)))
    (catch 'ret
      (dotimes (i max-d)
        (when-let* ((current-pos (+ pos (* i dir)))
                    ((if backward (>= current-pos beg) (< current-pos end)))
                    (syn (syntax-after current-pos))
                    (class (syntax-class syn))
                    ((or (= class target-class)
                         (and (= class 7)
                              pair-hl-highlight-string-quotes))))
          (condition-case nil
              (when-let* ((start-pos (if backward (1+ current-pos) current-pos))
                          (match-pos (scan-sexps start-pos dir)))
                (throw 'ret
                       (if backward
                           `(before ,match-pos ,current-pos)
                         `(after ,current-pos ,(1- match-pos)))))
            (scan-error
             (throw 'ret
                    (if backward
                        `(mismatch-before ,current-pos)
                      `(mismatch-after ,current-pos)))))))
      nil)))

(defun pair-hl--get-adjacent-pairs (ppss)
  (unless (or (nth 3 ppss) (nth 4 ppss))
    (let ((p (point))
          (max-d (max 1 (or pair-hl-adjacent-max-distance 1))))
      (delq nil (list (pair-hl--check-adjacent (1- p) -1 max-d)
                      (pair-hl--check-adjacent p 1 max-d))))))

(defun pair-hl--render-overlays (specs)
  "Reuse existing overlays and hide unused ones based on SPECS.
SPECS is a list of (pos . face)."
  (let ((old-ovs pair-hl--overlays)
        (new-ovs nil)
        (win (selected-window)))
    (dolist (spec specs)
      (let* ((pos (car spec))
             (face (cdr spec))
             (ov (if old-ovs
                     (pop old-ovs)
                   (let ((new (make-overlay pos (1+ pos) nil t nil)))
                     (overlay-put new 'priority 100)
                     (overlay-put new 'evaporate t)
                     (overlay-put new 'window win)
                     new))))
        (move-overlay ov pos (1+ pos))
        (overlay-put ov 'face face)
        (push ov new-ovs)))
    (mapc #'delete-overlay old-ovs)
    (setq pair-hl--overlays new-ovs)))

(defun pair-hl--apply-highlights (buf p)
  "Calculate and apply highlights in BUF at position P."
  (when (and (buffer-live-p buf)
             (eq (current-buffer) buf)
             (eq (point) p))
    (let ((desired
           (while-no-input
             (save-match-data
               (let* ((ppss (syntax-ppss))
                      (hl-list nil)
                      (add-hl (lambda (pos face)
                                (push (cons pos face) hl-list))))

                 (when pair-hl-highlight-enclosing
                   (pcase (pair-hl--get-enclosing-pair ppss)
                     (`(mismatch . ,pos)
                      (funcall add-hl pos 'pair-hl-mismatch-face))
                     (`(,open . ,close)
                      (funcall add-hl open 'pair-hl-enclosing-face)
                      (funcall add-hl close 'pair-hl-enclosing-face))))

                 (dolist (adj (pair-hl--get-adjacent-pairs ppss))
                   (pcase adj
                     (`(mismatch-before ,pos)
                      (when pair-hl-highlight-adjacent-before
                        (funcall add-hl pos 'pair-hl-mismatch-face)))
                     (`(mismatch-after ,pos)
                      (when pair-hl-highlight-adjacent-after
                        (funcall add-hl pos 'pair-hl-mismatch-face)))
                     (`(before ,open ,close)
                      (when pair-hl-highlight-adjacent-before
                        (funcall add-hl open 'pair-hl-adjacent-before-face)
                        (funcall add-hl close 'pair-hl-adjacent-before-face)))
                     (`(after ,open ,close)
                      (when pair-hl-highlight-adjacent-after
                        (funcall add-hl open 'pair-hl-adjacent-after-face)
                        (funcall add-hl close 'pair-hl-adjacent-after-face)))))
                 hl-list)))))
      (unless (eq desired t)
        (pair-hl--render-overlays desired)))))

(defun pair-hl--post-command ()
  "Hook run after every command to trigger debounce logic."
  (let ((pt (point))
        (tick (buffer-chars-modified-tick)))
    (when (or (not (eq pt pair-hl--last-point))
              (not (eq tick pair-hl--last-tick)))
      (setq pair-hl--last-point pt
            pair-hl--last-tick tick)
      (pair-hl--cancel-timer)
      (if (and (numberp pair-hl-debounce-delay)
               (> pair-hl-debounce-delay 0))
          (setq pair-hl--timer
                (run-with-idle-timer pair-hl-debounce-delay nil
                                     #'pair-hl--apply-highlights
                                     (current-buffer) pt))
        (pair-hl--apply-highlights (current-buffer) pt)))))

;;;###autoload
(define-minor-mode pair-hl-mode
  "Minor mode to dynamically highlight enclosing and adjacent pairs."
  :init-value nil
  :lighter " PairHL"
  (if pair-hl-mode
      (progn
        (add-hook 'post-command-hook #'pair-hl--post-command nil t)
        (setq pair-hl--last-point (point)
              pair-hl--last-tick (buffer-chars-modified-tick))
        (pair-hl--apply-highlights (current-buffer) (point)))
    (remove-hook 'post-command-hook #'pair-hl--post-command t)
    (pair-hl--cancel-timer)
    (pair-hl--clear)
    (setq pair-hl--last-point nil
          pair-hl--last-tick nil)))

(provide 'pair-hl)
;;; pair-hl.el ends here
