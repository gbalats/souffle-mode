;;; souffle-core.el -- Core functionality

;; Copyright (C) 2021, George Balatsouras
;;
;; Author: George Balatsouras <gbalats(at)gmail(dot)com>
;; Keywords: convenience, languages
;;
;; This file is NOT part of Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


;;; Commentary:

;; This file provides the core functionality of this package, such as
;; movement commands for various elements.

;; Please see souffle-mode.el for more commentary.

;;; Code:


(defgroup souffle nil
  "Major mode `souffle-mode' for editing Souffle Lang code."
  :prefix "souffle-"
  :group  'languages)

;;----------------------------
;; Bypass comments
;;----------------------------

(defun souffle-forward-comment (&optional direction)
  "Souffle mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start "//")
        (factor (if (< (or direction 0) 0) -99999 99999)))
    (forward-comment factor)))


;;----------------------------
;; Movement by clauses
;;----------------------------

(defun souffle-backward-clause (&optional arg)
  "Move backward to previous clause.
With ARG, repeat.  See `souffle-forward-clause'."
  (interactive "^p")
  (or arg (setq arg 1))
  (souffle-forward-clause (- arg)))

(defun souffle-forward-clause (&optional arg)
  "Move forward to the next clause.
With ARG, repeat.  With negative argument, move ARG times
backward to previous clause."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; moving forward
  (while (> arg 0)
    (souffle-forward-comment 1)      ; bypass comment
    (while                              ; search for dot
        (progn
          (re-search-forward "\\." nil t 1)
          (nth 8 (syntax-ppss))))       ; while ignoring those inside
                                        ; comments or strings
    (setq arg (1- arg)))
  ;; moving backwards
  (while (< arg 0)
    (souffle-forward-comment -1)     ; bypass backward comment
    (backward-char)
    (while                              ; search for previous dot
        (progn
          (re-search-backward "\\`\\|\\." nil t 1)
          (nth 8 (syntax-ppss))))       ; while ignoring those inside
                                        ; comments or strings
    (if (= (char-after) ?\.)
        (forward-char))
    (skip-chars-forward "[:space:]")    ; skip spaces
    (souffle-forward-comment 1)      ; skip comments
    (setq arg (1+ arg))))


;;----------------------------
;; Movement by atoms
;;----------------------------


(defun souffle--skip-delim-forward ()
  "Move forward past one atom delimiter."
  (cond ((eq (char-after) ?\.) (forward-char))
        ((eq (char-after) ?\,) (forward-char))
        ((eq (char-after) ?\;) (forward-char))
        ((looking-at ":-")     (forward-char 2))))

(defun souffle--skip-delim-backward ()
  "Move backwards before one atom delimiter."
  (cond ((eq (char-before) ?\.) (backward-char))
        ((eq (char-before) ?\,) (backward-char))
        ((eq (char-before) ?\;) (backward-char))
        ((looking-back ":-" 2)  (backward-char 2))))


(defun souffle-backward-atom (&optional arg)
  "Move backward to previous atom.
With ARG, repeat.  See `souffle-forward-atom'."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; moving backwards
  (catch 'break
    (while (> arg 0)
      ;; Move backwards to bypass the previous delimiter
      (souffle-forward-comment -1)     ; bypass backward comment
      (skip-chars-backward "[:space:]")   ; skip spaces
      (souffle--skip-delim-backward)   ; skip delimiter
      (while
          (progn
            ;; search for previous atom delimiter
            (unless (re-search-backward ",\\|;\\|\\.\\|:-" nil 0 1)
              (souffle-forward-comment 1)
              (throw 'break nil))
            ;; check parser state
            (let ((parser-state (syntax-ppss))) ; break unless
              (or (nth 8  parser-state)         ; inside comments or strings
                  (> (car parser-state) 0)))))  ; inside parenthesized group
      ;; Move forward to reach atom beginning
      (souffle--skip-delim-forward)      ; skip atom delimiter
      (skip-chars-forward "[:space:]\\|\n") ; skip spaces
      (souffle-forward-comment 1)        ; skip comment
      (setq arg (1- arg))))
  ;; negative arg means move forward instead
  (when (< arg 0)
    (souffle-forward-atom (- arg))))


(defun souffle-forward-atom (&optional arg)
  "Move forward to the next atom.
With ARG, repeat.  With negative argument, move ARG times
backward to previous atom."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; moving forward
  (catch 'break
    (while (> arg 0)
      ;; Move forward to bypass the next delimiter
      (souffle-forward-comment 1)      ; skip any comment
      (skip-chars-forward "[:space:]")    ; skip spaces
      (souffle--skip-delim-forward)    ; skip atom delimiter
      (while
          (progn
            ;; search for next atom delimiter
            (unless (re-search-forward ",\\|;\\|\\.\\|:-" nil 0 1)
              (throw 'break nil))
            ;; check parser state
            (let ((parser-state (syntax-ppss))) ; break unless
              (or (nth 8  parser-state)         ; inside comments or strings
                  (> (car parser-state) 0)))))  ; inside parenthesized group
      ;; Move backwards to reach the end of the atom
      (souffle--skip-delim-backward)      ; skip delimiter
      (skip-chars-backward "[:space:]\\|\n") ; skip spaces
      (souffle-forward-comment -1)        ; skip any comment
      (setq arg (1- arg))))
  ;; negative arg means move backwards instead
  (when (< arg 0)
    (souffle-backward-atom (- arg))))

(defun souffle-bounds-of-clause-at-point ()
  "Determine the start and end buffer locations for the clause at point.

Return a cons cell (START . END) giving the start and end
positions of the clause found."
  (let ((orig-point (point))
        (start nil)
        (end nil))
    (save-excursion
      (forward-char 1)
      (souffle-backward-clause 1)
      (unless (> (point) orig-point)
        (setq start (point)))
      (souffle-forward-clause 1)
      (unless (< (point) orig-point)
        (setq end (point))))
    (when (and start end)
      (cons start end))))


(provide 'souffle-core)

;;; souffle-core.el ends here
