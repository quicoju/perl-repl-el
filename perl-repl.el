;;; perl-repl.el

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;; WARNING: This is a work in progress.
;; The goal is to build simple comint derived mode to run a Perl REPL.
;; So far I've only used it with re.pl although in shouldn't be
;; difficult to run it with Reply or perl -de 0

(require 'comint)
(require 'cperl-mode)
(require 'rx)

(defvar perl-repl-file-path "re.pl"
  "Path to the program used by `perl-repl'")

(defvar perl-repl-arguments '())

(defvar perl-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    map)
  "Basic mode map for `perl-repl")

(defvar perl-repl-prompt-regexp (rx (regexp "^[^$\n]*") "\ufeff$ ")
  "Prompt for `perl-repl'.")

(defconst perl--repl-buffer-name/raw
  "Perl-REPL")

(defconst perl--repl-buffer-name
  (concat "*" perl--repl-buffer-name/raw "*"))

(defun perl-repl ()
  "Run an inferior instance of `re.pl' inside Emacs."
  (interactive)
  (let* ((perl-repl-program perl-repl-file-path)
	 (buffer (get-buffer-create perl--repl-buffer-name)))
    (unless (comint-check-proc buffer)
      (apply 'make-comint-in-buffer perl--repl-buffer-name/raw
	     buffer perl-repl-program perl-repl-arguments))
    (save-selected-window
      (display-buffer buffer)
      (select-window (get-buffer-window buffer))
      (perl-repl-mode))))

(define-derived-mode perl-repl-mode comint-mode perl--repl-buffer-name/raw
  "Major mode for `perl-repl'.
\\{perl-repl-mode-map}"
  nil perl--repl-buffer-name/raw
  (setq comint-prompt-regexp perl-repl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'perl-repl-font-lock-keywords)
       (cperl-load-font-lock-keywords))
  (set (make-local-variable 'font-lock-defaults)
       '(perl-repl-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) perl-repl-prompt-regexp)
  (perl--repl-hook-cperl-keys))

(defun perl--repl-hook-cperl-keys ()
  (cperl-define-key (kbd "C-c C-c") 'perl-send-expression)
  (cperl-define-key (kbd "C-c C-r") 'perl-send-region)
  (cperl-define-key (kbd "C-c C-l") 'perl-send-line))

(defun perl-send-region (start end)
  "Send the current region (if any) to the Perl-REPL."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (perl--send-region-to-repl start end))

(defun perl-send-line ()
  "Send the current line to the Perl-REPL."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (let ((end (point)))
      (move-beginning-of-line nil)
      (perl--send-region-to-repl (point) end))))

(defun perl-send-expression ()
  "Send the current expression to the Perl-REPL."
  (interactive)
  (save-excursion
    (perl-repl--forward-to-end-of-expr)
    (if (> (current-column) 1)
	(backward-char))
    (let ((end (point)))
      (perl-repl--backward-to-start-of-expr)
      (perl--send-region-to-repl (point) end))))

;; Helpers
;; =======
(defun perl--get-repl-buffer-process ()
  (get-buffer-process perl--repl-buffer-name))

(defun perl-repl--initialize ()
  "Helper function to initialize perl-repl"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(defun perl--repl-show-and-move-to-end ()
  "Make the Perl REPL visible, and move point to end.
Keep original window selected."
  (display-buffer perl--repl-buffer-name)
  (save-selected-window
    (select-window (get-buffer-window perl--repl-buffer-name))
    (comint-show-maximum-output)))

(defun perl--repl-eval (expression)
  "Eval EXPRESSION in the *Perl-REPL* buffer.
Allow Racket process output to be displayed and show the window."
  (perl-repl)
  (comint-send-string (perl--get-repl-buffer-process) expression)
  (perl--repl-show-and-move-to-end))

(defun perl--send-region-to-repl (start end)
  "Internal function to send the region to the Perl-REPL.

Before sending the region, call `perl-repl'. Also insert
a ?\n at the process mark so that output goes on a fresh
line, not on the same line as the prompt.

Afterwards call `perl--repl-show-and-move-to-end'."
  (when (and start end)
    (perl-repl)
    (let ((proc (perl--get-repl-buffer-process)))
      (with-current-buffer perl--repl-buffer-name
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert ?\n)
	  (set-marker (process-mark proc) (point))))
      (comint-send-region proc start end)
      (comint-send-string proc "\n"))
    (perl--repl-show-and-move-to-end)))

(defun perl-repl--point-at-expr-boundary-p ()
  (memq 59 (cperl-after-expr-p)))

;; Is the point at an expression boundary
;; and proceeded by semicolon
(defun perl-repl--end-of-expression-p ()
  (and (perl-repl--point-at-expr-boundary-p)
       (eq (char-before) ?\;)))

(defun perl-repl--backward-to-start-of-expr (&optional dir)
  (condition-case nil
      (progn
	(while (or (not (perl-repl--point-at-expr-boundary-p))
		   (nth 3 (syntax-ppss)))
	  (backward-char dir)
	  (skip-chars-backward "$@%#")))
    (error nil)))

(defun perl-repl--forward-to-end-of-expr (&optional dir)
  (condition-case nil
      (progn
	(while (or (not (perl-repl--end-of-expression-p))
		   (nth 3 (syntax-ppss)))
	  (forward-char)
	  (skip-chars-forward "$@%#")))
    (error nil)))

(provide 'perl-repl)
