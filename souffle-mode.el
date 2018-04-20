;;; souffle-mode.el --- Major mode for editing Souffle Datalog code

;; Copyright (C) 2014, George Balatsouras
;;
;; Author: George Balatsouras <gbalats(at)gmail(dot)com>
;; Maintainer: George Balatsouras <gbalats(at)gmail(dot)com>
;; Created: 26 Aug 2014
;; Version: 0.2
;; Keywords: convenience, languages
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; This is a major mode for the souffle language.  It provides syntax
;; highlighting, movement functions, and expansions.
;;
;; Put this file into your load-path and the following into your
;; `~/.emacs':
;;
;; (require 'souffle-mode)


;;; Code:

;;----------------------------
;; Prerequisites
;;----------------------------

(require 'dash)
(require 'f)
(require 'font-lock)
(require 'multiple-cursors)
(require 'newcomment)
(require 's)
(require 'smie)
(eval-when-compile
  (require 'compile)
  (require 'regexp-opt))


(require 'souffle-core)
;; (require 'souffle-mode-expansions)

;;----------------------------
;; Local variables
;;----------------------------

(defcustom souffle-default-face 'default
  "Default face in `souffle-mode' buffers."
  :type  'face
  :group 'souffle)

(defcustom souffle-predicate-face 'font-lock-function-name-face
  "Face for predicate names in `souffle-mode' buffers."
  :type  'face
  :group 'souffle)

(defcustom souffle-mode-hook nil
  "List of functions to be executed on entry to `souffle-mode'."
  :type  'hook
  :group 'souffle)

(defcustom souffle-indent-width 3
  "Level of indentation in `souffle-mode' buffers."
  :type 'integer
  :group 'souffle)

(defvar souffle-electric-newline-p t
  "*Non-nil means automatically indent the next line when the user types RET.")



;;----------------------------
;; Keywords
;;----------------------------

(defconst souffle-dot-keywords-regexp
  (eval-when-compile
    (concat "\\."
            "\\("
            (regexp-opt '("decl" "printsize" "input" "output" "override" "comp"
                          "init" "type" "number-type" "symbol-type")
                        'symbols)
            "\\)"))
  "Regular expression for souffle lang keywords that are prefixed by dot.")

(defconst souffle-aggregate-keywords-regexp
  (eval-when-compile
    (regexp-opt '("min")))
  "Regular expression for souffle lang aggregate keywords.")

(defconst souffle-misc-keywords-regexp
  (eval-when-compile
    (regexp-opt '("overridable")))
  "Regular expression for miscellaneous souffle lang keywords.")

(defconst souffle-types-regexp
  (eval-when-compile
    (regexp-opt '("symbol" "number" "txt")
                'symbols))
  "Regular expression for souffle lang types.")

(defconst souffle-number-regexp
  (concat "\\<[[:digit:]]+"
          "\\(?:\\.[[:digit:]]+\\)?"
          "\\(?:[eE][+-]?[[:digit:]]+\\)?\\>")
  "Regular expression for souffle lang numbers.")

(defconst souffle-string-operations-regexp
  (eval-when-compile
    (regexp-opt '("cat" "contains" "match" "ord")
                'symbols))
  "Regular expression for souffle lang string operations.")

(defvar souffle-font-lock-keywords
  (let* ((variable-regexp "[_?[:alpha:]][_[:word:]]*")
         (predicate-name-regexp "[_[:alpha:]][_[:word:]]*")
         (predicate-regexp
          (concat "\\(" predicate-name-regexp "\\)"
                  "\\s(.*?\\s)")))       ; non-greedy *? operator inside paren
    `((,souffle-types-regexp . font-lock-type-face)
      (,souffle-dot-keywords-regexp 1 font-lock-keyword-face)
      (,souffle-misc-keywords-regexp . font-lock-keyword-face)
      (,souffle-aggregate-keywords-regexp . font-lock-keyword-face)
      (,souffle-string-operations-regexp . font-lock-builtin-face)
      (,predicate-regexp 1 souffle-predicate-face)
      (,souffle-number-regexp . font-lock-warning-face)
      (,variable-regexp . font-lock-variable-name-face)))
  "Font-lock keywords for `souffle-mode'.")


;;----------------------------
;; Indentation
;;----------------------------

(defvar souffle-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (clause (atoms ":-" atoms ".")
              (".decl" pred "(" decls ")")
              (".type" type)
              (".type" type "=" types)
              (atoms "."))
      (type (id))
      (types (type)
             (types "|" types))
      (atom (pred "(" exps ")")
            (exp "=" exp)
            (exp "!=" exp)
            (exp "<=" exp)
            (exp ">=" exp)
            (exp "<" exp)
            (exp ">" exp))
      (pred (id))
      (atoms (atoms "," atoms)
             (atoms ";" atoms)
             ("!" atom)
             (atom))
      (decl (id ":" id))
      (decls (decls "," decls)
             (decl))
      (exp (id)
           (exp "+" exp)
           (exp "-" exp)
           (exp "*" exp)
           (exp "/" exp))
      (exps (exps "," exps) (exp)))
    '((assoc ",") (assoc ";"))
    '((nonassoc ":") (assoc "|"))
    '((assoc "+") (assoc "-") (assoc "*") (assoc "/")))))

(defvar souffle-smie-keywords-regexp
  (regexp-opt '("." "," ";" "|" ":" ":-" "!" "=" "!=" "<" "<=" ">" ">="
                "+" "-" "*"  "/" "{" "}" "(" ")")))

(defun souffle-smie-forward-token ()
  "Souffle lang SMIE lexer function to forward token."
  (forward-comment (point-max))
  (cond
   ((looking-at "\\.\\w+")
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at souffle-smie-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))

(defun souffle-smie-backward-token ()
  "Souffle lang SMIE lexer function to go one token backwards."
  (forward-comment (- (point)))
  (cond
   ((looking-back "\\.\\w+" (min 12 (- (point) 2)) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   ((looking-back souffle-smie-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))

(defun souffle-smie-rules (kind token)
  "Souffle lang rules for indenting some KIND of TOKEN."
  (pcase (cons kind token)
    (`(:elem . basic) '(column . 0))
    ;; (`(:before . ".") (smie-rule-parent 0))
    (`(:after . ".") '(column . 0)) ; To work around smie-closer-alist.
    (`(:after . ")")
     (smie-rule-parent 0))
    (`(:before . ",")
     (when (smie-rule-bolp)
       (cond ((smie-rule-parent-p ":-" "->") (smie-rule-parent 0))
             ((not (smie-rule-sibling-p)) (smie-rule-parent 1)))))
    (`(,_ . ,(or `"," `";" `"|")) (smie-rule-separator kind))
    (`(:before . ":-")
     (when (smie-rule-bolp) (smie-rule-parent 1)))
    (`(:after . ,(or `":-" `"->"))
     (if (smie-rule-hanging-p) souffle-indent-width
       (1- souffle-indent-width)))))


;;----------------------------
;; Comment-specific commands
;;----------------------------

(defun souffle-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For details (including the use of ARG), see `comment-dwim'."
  (interactive "*P")
  (let ((comment-start "//") (comment-end ""))
    (comment-dwim arg)))


;;----------------------------
;; refactorings
;;----------------------------

(defun souffle-narrow-to-clause ()
  (save-excursion
    (souffle-forward-clause 1)       ; move to clause ending
    (set-mark (point))
    (souffle-backward-clause 1)      ; move to clause beginning
    (deactivate-mark)
    (narrow-to-region (point) (mark))))

;;;###autoload
(defun souffle-rename-symbol ()
  (interactive)
  (save-restriction
    (souffle-narrow-to-clause)
    (mc/mark-all-symbols-like-this)))

(add-to-list 'mc--default-cmds-to-run-once
             'souffle-rename-symbol)


;;----------------------------
;; syntax table
;;----------------------------

(defvar souffle-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++ style comment `//' ..."
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?? "_" st)
    st)
  "Syntax table for `souffle-mode'.")


;;----------------------------
;; keymap
;;----------------------------

(defvar souffle-mode-map
  (let ((map (make-sparse-keymap)))
    ;; modify the keymap
    (define-key map (kbd "C-c :") 'souffle-rename-symbol)
    (define-key map (kbd "C-c C-c") 'souffle-connect)
    (define-key map (kbd "C-c C-q") 'souffle-query)
    (define-key map (kbd "C-c C-a") 'souffle-add-block)
    (define-key map (kbd "C-c C-p") 'souffle-pop-count)
    (define-key map (kbd "C-c C-l") 'souffle-pred-print)
    (define-key map (kbd "C-c i")   'souffle-pred-info)
    (define-key map (kbd "C-M-e") 'souffle-forward-clause)
    (define-key map (kbd "C-M-a") 'souffle-backward-clause)
    (define-key map (kbd "M-e") 'souffle-forward-atom)
    (define-key map (kbd "M-a") 'souffle-backward-atom)
    (define-key map [remap comment-dwim] 'souffle-comment-dwim)
    (when souffle-electric-newline-p
      (define-key map "\r" 'reindent-then-newline-and-indent))
    map)
  "Keymap for `souffle-mode'.")


;;----------------------------
;; backwards compatibility
;;----------------------------

(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))


;;----------------------------
;; define the mode
;;----------------------------

;;;###autoload
(define-derived-mode souffle-mode prog-mode "souffle mode"
  "Major mode for editing Souffle Datalog ..."
  :group 'souffle

  (kill-all-local-variables)

  ;; Select the mode's keymap.
  (use-local-map souffle-mode-map)

  ;; Comments start with `//'.
  (set (make-local-variable 'comment-start) "//")

  ;; code for syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(souffle-font-lock-keywords))

  ;; syntax table
  (set-syntax-table souffle-syntax-table)

  ;; smie setup
  (smie-setup souffle-smie-grammar #'souffle-smie-rules
              :forward-token #'souffle-smie-forward-token
              :backward-token #'souffle-smie-backward-token)

  ;; major mode name
  (setq mode-name "Souffle")
  (setq major-mode 'souffle-mode)

  ;; permit the user to customize the mode with a hook
  (run-mode-hooks 'souffle-mode-hook))


;;----------------------------
;; Add file association
;;----------------------------

(add-to-list 'auto-mode-alist '("\\.dl$" . souffle-mode))


(provide 'souffle-mode)

;;; souffle-mode.el ends here
