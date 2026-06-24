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

(defcustom pair-hl-show-pair-context-when-offscreen 'adjacent
  "What off-screen pair context to show in the echo area.
When nil, never show pair context.
When `adjacent', show only adjacent pairs that are off-screen.
When t, show all highlighted pairs (enclosing and adjacent)."
  :type '(choice
          (const :tag "Off" nil)
          (const :tag "Adjacent pairs only" adjacent)
          (const :tag "All highlighted pairs" t))
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

(defvar pair-hl--offscreen-shown nil
  "Non-nil when the offscreen pair context is currently shown.")

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
  "Return a spec (TYPE START END FACE) for the innermost enclosing pair.
TYPE is `enclosing' or `mismatch'.  END is nil for mismatches."
  (let* ((string-hl (and (nth 3 ppss)
                         pair-hl-highlight-string-quotes))
         (paren-hl (nth 9 ppss))
         (start (cond (string-hl (nth 8 ppss))
                      (paren-hl (car (last (nth 9 ppss)))))))
    (when start
      (condition-case nil
          (list 'enclosing start
                (1- (scan-sexps start 1))
                'pair-hl-enclosing-face)
        (scan-error (list 'mismatch start nil
                          'pair-hl-mismatch-face))))))

(defun pair-hl--scan-pair-at (pos dir max-d)
  "Return a spec (TYPE START END FACE) by scanning from POS in direction DIR,
or nil if none found within MAX-D.  TYPE is `before', `after',
or `mismatch'.  END is nil for mismatches."
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
                           (list 'before match-pos current-pos
                                 'pair-hl-adjacent-before-face)
                         (list 'after current-pos (1- match-pos)
                               'pair-hl-adjacent-after-face))))
            (scan-error
             (throw 'ret (list 'mismatch current-pos nil
                               'pair-hl-mismatch-face))))))
      nil)))

(defun pair-hl--get-before-pair (pos max-d)
  "Return a spec for the paired delimiter directly before POS.
See `pair-hl--scan-pair-at' for the return format."
  (pair-hl--scan-pair-at (1- pos) -1 max-d))

(defun pair-hl--get-after-pair (pos max-d)
  "Return a spec for the paired delimiter directly after POS.
See `pair-hl--scan-pair-at' for the return format."
  (pair-hl--scan-pair-at pos 1 max-d))

(defun pair-hl--render-overlays (specs)
  "Reuse existing overlays and hide unused ones based on SPECS.
SPECS is a list of (TYPE START END FACE) specs."
  (let ((old-ovs pair-hl--overlays)
        (new-ovs nil)
        (win (selected-window)))
    (dolist (spec specs)
      (let* ((start (nth 1 spec))
             (end (nth 2 spec))
             (face (nth 3 spec))
             (positions (if end (list start end) (list start))))
        (dolist (pos positions)
          (let ((ov (if old-ovs
                        (pop old-ovs)
                      (let ((new (make-overlay pos (1+ pos) nil t nil)))
                        (overlay-put new 'priority 100)
                        (overlay-put new 'evaporate t)
                        (overlay-put new 'window win)
                        new))))
            (move-overlay ov pos (1+ pos))
            (overlay-put ov 'face face)
            (push ov new-ovs)))))
    (mapc #'delete-overlay old-ovs)
    (setq pair-hl--overlays new-ovs)))

(defun pair-hl--fontified-line-for-positions (line-number line-pos-pairs)
  "Return LINE-NUMBER and fontified LINE-POS-PAIRS as a string.
LINE-POS-PAIRS is a list of (POS . FACE) cons cells."
  (save-excursion
    (goto-char (caar line-pos-pairs))
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (when font-lock-mode
        (font-lock-ensure bol eol))

      (let ((str (buffer-substring bol eol)))
        (dolist (pair line-pos-pairs)
          (let ((p (car pair))
                (face (cdr pair)))
            (add-face-text-property (- p bol) (1+ (- p bol)) face nil str)))
        (concat (propertize (format "%d: " line-number) 'face 'line-number)
                str)))))

(defun pair-hl--collect-offscreen-lines (specs)
  "Return fontified line strings for off-screen positions in SPECS.
SPECS is a list of (TYPE START END FACE) specs."
  (let ((win (selected-window))
        grouped-alist)
    (dolist (spec specs)
      (let* ((start (nth 1 spec))
             (end (nth 2 spec))
             (face (nth 3 spec))
             (positions (if end (list start end) (list start))))
        (dolist (pos positions)
          (unless (pos-visible-in-window-p pos win)
            (let* ((line-num (line-number-at-pos pos))
                   (existing-group (assq line-num grouped-alist)))
              (if existing-group
                  (push (cons pos face) (cdr existing-group))
                (push (cons line-num (list (cons pos face)))
                      grouped-alist)))))))
    (nreverse
     (mapcar
      (lambda (group)
        (let ((line-num (car group))
              (line-entries (cdr group)))
          (pair-hl--fontified-line-for-positions
           line-num line-entries)))
      grouped-alist))))

(defun pair-hl--show-offscreen-context (lines)
  "Show echo-area context for off-screen pair positions.
LINES is a list of fontified strings."
  (setq pair-hl--offscreen-shown t)
  (message "%s" (string-join lines "\n")))

(defun pair-hl--hide-offscreen-context ()
  "Clear the offscreen pair context from the echo area."
  (when pair-hl--offscreen-shown
    (setq pair-hl--offscreen-shown nil)
    (message nil)))

(defun pair-hl--collect-highlights ()
  "Collect the list of highlights if not interrupted by input.
Returns a list of (TYPE START END FACE) specs, or t if interrupted.
The enclosing spec, if present, is always the first element."
  (while-no-input
    (save-match-data
      (let* ((ppss (syntax-ppss))
             (not-in-string-or-comment (not (or (nth 3 ppss) (nth 4 ppss))))
             (pos (point))
             (max-d (max 1 (or pair-hl-adjacent-max-distance 1)))
             (enc (when pair-hl-highlight-enclosing
                    (pair-hl--get-enclosing-pair ppss)))
             (bef (when (and not-in-string-or-comment
                             pair-hl-highlight-adjacent-before)
                    (pair-hl--get-before-pair pos max-d)))
             (aft (when (and not-in-string-or-comment
                             pair-hl-highlight-adjacent-after)
                    (pair-hl--get-after-pair pos max-d))))
        (delq nil (list enc bef aft))))))

(defun pair-hl--apply-highlights (buf p)
  "Calculate, render highlights in BUF at P, and update offscreen context.
When `pair-hl-show-pair-context-when-offscreen' is `adjacent',
the enclosing pair is excluded from the offscreen display."
  (when (and (buffer-live-p buf)
             (eq (current-buffer) buf)
             (eq (point) p))
    (let ((hls (pair-hl--collect-highlights)))
      (unless (eq hls t)
        (pair-hl--render-overlays hls)
        (if-let* ((pair-hl-show-pair-context-when-offscreen)
                  ((eq buf (window-buffer (selected-window))))
                  (lines (pair-hl--collect-offscreen-lines
                          (if (and (eq pair-hl-show-pair-context-when-offscreen
                                       'adjacent)
                                   (eq (caar hls) 'enclosing))
                              (cdr hls)
                            hls))))
            (pair-hl--show-offscreen-context lines)
          (pair-hl--hide-offscreen-context))))))

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
  "Minor mode to dynamically highlight enclosing and adjacent pairs.
When enabled, highlights the innermost enclosing delimiter pair and
any adjacent pairs around point.  Can optionally show off-screen
pair context in the echo area (see `pair-hl-show-pair-context-when-offscreen')."
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
    (when pair-hl--offscreen-shown
      (setq pair-hl--offscreen-shown nil)
      (message nil))
    (setq pair-hl--last-point nil
          pair-hl--last-tick nil)))

(provide 'pair-hl)
;;; pair-hl.el ends here
