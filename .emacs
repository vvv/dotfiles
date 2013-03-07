;;; -*- coding: utf-8-unix -*-

(server-start)

(setq inhibit-startup-message t) ; don't show startup screen

;;; Remove toolbar, scrollbar, and menu bar
;;; (see http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html)
(dolist (f '(menu-bar-mode scroll-bar-mode tool-bar-mode)) (funcall f -1))

(transient-mark-mode t) ; highlight marked region

(progn
  (iswitchb-mode t) ; enable switching between buffers using substrings
  (setq iswitchb-prompt-newbuffer nil)) ; create a buffer silently
(icomplete-mode t)  ; enable incremental minibuffer completion

(progn (require 'uniquify) (setq uniquify-buffer-name-style 'forward))

(setq backup-directory-alist '((".*" . "~/.backups"))) ; backups location

(setq require-final-newline t default-indicate-empty-lines t)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq enable-local-variables :safe)

(setq c-default-style '((c-mode . "linux") (awk-mode . "awk") (other . "gnu")))

;;; GNU global
(when (file-readable-p "/opt/local/share/gtags/gtags.el")
  (autoload 'gtags-mode "/opt/local/share/gtags/gtags.el" nil t)
  (setq gtags-suggested-key-mapping t gtags-pop-delete t)
  (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
  ;; Don't let gtags overwrite some key sequences.
  (eval-after-load "gtags"
    '(progn
       (define-key gtags-mode-map "\C-ct" nil)   ; `toggle-truncate-lines'
       (define-key gtags-mode-map "\C-t" nil)))) ; `transpose-chars'

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (kill-new (filter-buffer-substring (point-min) (point-max)))
  (when (interactive-p)
    (message "Buffer saved")))
(global-set-key "\M-W" 'copy-buffer-as-kill)

;;; --------------------------------------------------------------------
;;; Semi-automatic rstripping

(defvar trailing-whitespace-allowed nil
  "If file name matches this regexp,
`delete-trailing-whitespace-if-confirmed' will skip it.")

(defun delete-trailing-whitespace-if-confirmed ()
  "Delete all the trailing whitespace across the current buffer,
asking user for confirmation."
  (unless (and trailing-whitespace-allowed
	       (string-match trailing-whitespace-allowed (buffer-file-name)))
    (when (and (save-excursion (goto-char (point-min))
			       (re-search-forward "[[:blank:]]$" nil t))
	       (y-or-n-p (format "Delete trailing whitespace from %s? "
				 (buffer-name))))
      (delete-trailing-whitespace))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-if-confirmed)
;;; --------------------------------------------------------------------

(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(global-set-key (kbd "C-c v f") 'view-file)
(global-set-key (kbd "C-c v v") 'view-mode)
;; Don't let gtags overwrite `C-c v'
(eval-after-load "gtags" '(define-key gtags-mode-map "\C-cv" nil))

(setq calendar-week-start-day 1) ; weeks should begin on Monday

;;; Org Mode
(eval-after-load "org"
  '(progn
    (setq org-hide-leading-stars t
	  ;; see http://doc.norang.ca/org-mode.html#HeadingLevelsOddEven
	  org-odd-levels-only nil
	  org-directory "~/.org"
	  org-default-notes-file (concat org-directory "/refile.org")
	  org-agenda-files '("~/life.org" "~/work.org" "~/overhead.org"))
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cb" 'org-iswitchb)
    (global-set-key "\C-ca" 'org-agenda)))
(eval-after-load "org-clock"
  '(progn
     (setq org-clock-out-remove-zero-time-clocks t
	   org-clock-into-drawer 2
	   org-clock-persist 'history
	   org-clock-idle-time 10)
     (org-clock-persistence-insinuate)
     (global-set-key "\C-cc" 'org-clock-goto)))
(eval-after-load "org-archive"
  '(org-defkey org-mode-map "\C-c\C-x\C-a"
	       'org-archive-subtree-default-with-confirmation))
(global-set-key (kbd "C-M-r") 'org-capture)

;;; Spell check
(global-set-key (kbd "C-c ib") 'ispell-buffer)
(global-set-key (kbd "C-c ir") 'ispell-region)
;XXX; (add-hook 'c-mode-hook 'flyspell-prog-mode)

(eval-after-load "ps-print"
  '(setq ps-paper-type 'a4
	 ps-print-header nil
	 ps-multibyte-buffer 'bdf-font-except-latin))

(setq vc-follow-symlinks t)

(define-key help-map "A" 'apropos-variable)

(progn ;; Unset unwelcome key bindings.
  (global-unset-key (kbd "s-q"))  ; `save-buffers-kill-emacs'
  (global-unset-key (kbd "s-w"))  ; `delete-frame'
  (global-unset-key "\C-z")       ; `suspend-frame'
  (global-unset-key (kbd "s-:"))  ; `ispell'
  (global-unset-key (kbd "s-g"))  ; `isearch-repeat-forward'
  (global-unset-key (kbd "s-z"))) ; `undo'

(setq confirm-kill-emacs 'yes-or-no-p) ; say "no" to accidental terminations

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((height . 46) (width . 80) (top . 0))))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control))))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Menlo")))))
