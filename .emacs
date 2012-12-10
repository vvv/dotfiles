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
(global-set-key (kbd "C-c v m") 'view-mode)
;; Don't let gtags overwrite `C-c v'
(eval-after-load "gtags" '(define-key gtags-mode-map "\C-cv" nil))

;;; Org Mode
(eval-after-load "org"
  '(progn
    (setq org-hide-leading-stars t
	  ;; see http://doc.norang.ca/org-mode.html#HeadingLevelsOddEven
	  org-odd-levels-only nil
	  org-directory "~/.org"
	  org-default-notes-file (concat org-directory "/refile.org")
	  org-agenda-files '("~/life.org" "~/work.org" "~/work-overhead.org"))
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

;; I tend to `suspend-frame' accidentally.
(global-unset-key "\C-z")

;; `Cmd-q' (bound to `save-buffers-kill-emacs') is even more dangerous,
;; as it is located near harmless `M-q' (`fill-paragraph').
(global-unset-key (kbd "s-q"))

;XXX; ;;; --------------------------------------------------------------------
;XXX; ;;; Aquamacs settings
;XXX;
;XXX; (eval-after-load "aquamacs"
;XXX;   '(progn (setq aquamacs-scratch-file nil)   ; don't restore *scratch*
;XXX; 	  (setq special-display-regexps nil) ; don't "pop-up" buffers
;XXX; 	  (aquamacs-autoface-mode -1)        ; no mode-specific faces
;XXX; 	  (setq inhibit-startup-echo-area-message t)
;XXX; 	  ;; don't highlight matching parentheses
;XXX; 	  (show-paren-mode -1)
;XXX; 	  ;; show modeline in Monaco
;XXX; 	  (set-face-attribute 'mode-line nil :inherit 'unspecified)
;XXX; 	  ;; show echo area in Monaco
;XXX; 	  (set-face-attribute 'echo-area nil :family 'unspecified)))
;XXX;
;XXX; ;; Disable saving file places
;XXX; (eval-after-load "saveplace"
;XXX;   '(progn (remove-hook 'find-file-hook 'save-place-find-file-hook)
;XXX; 	  (remove-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)
;XXX; 	  (remove-hook 'kill-buffer-hook 'save-place-to-alist)))
;XXX;
;XXX; ;;; --------------------------------------------------------------------
;XXX; ;;; Cyrillic key bindings
;XXX;
;XXX; (let ((convert (if (>= emacs-major-version 23)
;XXX; 		   (lambda (c) c)  ; no conversion
;XXX; 		 (lambda (c) (+ (- c (make-char 'mule-unicode-0100-24ff 40))
;XXX; 				(make-char 'cyrillic-iso8859-5))))))
;XXX;   (mapc
;XXX;    (lambda (pair)
;XXX;      (global-set-key
;XXX;       (vector (funcall convert (car pair))) (cdr pair)))
;XXX;    '((?\C-а . forward-char)  (?\M-а . forward-word)
;XXX;      (?\C-и . backward-char) (?\M-и . backward-word)
;XXX;      (?\C-т . next-line)     (?\C-з . previous-line)
;XXX;      (?\C-в . delete-char)   (?\M-в . kill-word)
;XXX;      (?\C-ф . move-beginning-of-line) (?\C-у . move-end-of-line)
;XXX;      (?\C-о . newline-and-indent)     (?\C-щ . open-line)
;XXX;      (?\C-ц . kill-region)     (?\M-ц . kill-ring-save) (?\C-н . yank)
;XXX;      (?\M-с . capitalize-word) (?\M-г . upcase-word) (?\M-д . downcase-word)
;XXX;      (?\M-Б . beginning-of-buffer) (?\M-Ю . end-of-buffer)
;XXX;      (?\C-ы . isearch-forward) (?\C-к . isearch-backward)
;XXX;      (?\M-й . fill-paragraph)  (?\C-л . kill-line) (?\M-я . zap-to-char)
;XXX;      (?\C-е . transpose-chars) (?\M-е . transpose-words)
;XXX;      (?\C-п . keyboard-quit)   (?\C-д . recenter)))
;XXX;   (global-set-key (apply 'vector (mapcar convert [?\C-ч ?л])) 'kill-buffer)
;XXX;   (global-set-key (apply 'vector (mapcar convert [?\C-\M-з])) 'backward-list)
;XXX;   (global-set-key (apply 'vector (mapcar convert [?\C-\M-т])) 'forward-list)
;XXX;   (global-set-key (apply 'vector (mapcar convert [?\C-ч ?\C-ы])) 'save-buffer)
;XXX;   (global-set-key (apply 'vector (mapcar convert [?\C-ч ?щ])) 'other-window))
;XXX; ;; ^ http://community.livejournal.com/ru_emacs/20743.html#3
;XXX; ;; XXX See also:
;XXX; ;;   * `ps-mule-encode-ucs2' function definition.
;XXX; ;;   * (info "(elisp)Translation Keymaps")
;XXX;
;XXX; (defun insert-logt-entry ()
;XXX;   "Append new entry to ~/.logt file."
;XXX;   (interactive)
;XXX;   (find-file "~/.logt")
;XXX;   (goto-char (point-min))
;XXX;   (open-line 1)
;XXX;   (insert (format-time-string "%a %Y-%m-%d %T> ")))
;XXX; (global-set-key (kbd "C-c l") 'insert-logt-entry)
;XXX;
;XXX; ;;; Dictionary
;XXX; ;; (try `M-x dictionary-match-words')
;XXX; (when (file-accessible-directory-p "~/lib/emacs/dictionary/")
;XXX;   (setq load-path (cons "~/lib/emacs/dictionary" load-path))
;XXX;   (autoload 'dictionary-search "dictionary" nil t)
;XXX;   (global-set-key (kbd "C-c d") 'dictionary-search)
;XXX;   (eval-after-load "dictionary"
;XXX;     '(define-key dictionary-mode-map (kbd "DEL") 'scroll-down)))
;XXX;
;XXX; (autoload 'work-log-mode "~/lib/emacs/work-log/work-log.el" nil t)
;XXX;
;XXX; (setq visible-bell t) ; flash a frame instead of beeping
;XXX; (setq sort-fold-case t)   ; sorting functions should ignore case
;XXX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(put 'scroll-left 'disabled nil)
