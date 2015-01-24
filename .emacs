;;; -*- coding: utf-8-unix -*-

(server-start)

(setq inhibit-startup-message t) ; don't show startup screen

;;; Remove toolbar, scrollbar, and menu bar
;;; (see http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html)
(menu-bar-mode -1)
(when window-system
  (dolist (f '(scroll-bar-mode tool-bar-mode)) (funcall f -1)))

(transient-mark-mode t) ; highlight marked region

(icomplete-mode t) ; enable incremental minibuffer completion
(if (string< emacs-version "24.4")
    (progn
      (iswitchb-mode t) ; enable switching between buffers using substrings
      (setq iswitchb-prompt-newbuffer nil)) ; create a buffer silently
  (electric-indent-mode -1) ; let `C-j' indent the line
  (ido-mode t))

(progn (require 'uniquify) (setq uniquify-buffer-name-style 'forward))

(setq backup-directory-alist '((".*" . "~/.backups"))) ; backups location

(setq require-final-newline t default-indicate-empty-lines t)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq enable-local-variables :safe)

(setq c-default-style '((c-mode . "linux") (awk-mode . "awk") (other . "gnu")))
(global-set-key (kbd "M-g f") 'ff-find-other-file)

;;; GNU global
(let ((fn (locate-file "gtags/gtags.el"
		       '("/opt/local/share" "/usr/local/share"))))
  (when (file-readable-p fn)
    (autoload 'gtags-mode fn nil t)
    (setq gtags-suggested-key-mapping t gtags-pop-delete t)
    (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))))
(eval-after-load "gtags"
  '(progn
     (setq gtags-ignore-case nil)
     ;; Don't let gtags overwrite some key sequences.
     (define-key gtags-mode-map "\C-ct" nil)   ; `toggle-truncate-lines'
     (define-key gtags-mode-map "\C-t" nil)    ; `transpose-chars'
     (define-key gtags-mode-map "\C-ch" nil)   ; `hide-region-hide'
     (define-key gtags-mode-map "\C-cf" nil))) ; `toggle-frame-fullscreen'

(add-hook 'sh-mode-hook
	  (lambda () (setq indent-tabs-mode t sh-basic-offset 8)))

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (kill-new (filter-buffer-substring (point-min) (point-max)))
  (when (interactive-p)
    (message "Buffer saved")))
(global-set-key "\M-W" 'copy-buffer-as-kill)

;;; --------------------------------------------------------------------
;;; Semi-automatic rstripping

(defvar trailing-whitespace-allowed "\.diff$"
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
(global-set-key (kbd "C-c u") 'untabify)
(global-set-key (kbd "C-c T") 'tabify)

(global-set-key (kbd "C-c v f") 'view-file)
(global-set-key (kbd "C-c v v") 'view-mode)
;; Don't let gtags overwrite `C-c v'
(eval-after-load "gtags" '(define-key gtags-mode-map "\C-cv" nil))

(setq calendar-week-start-day 1) ; weeks should begin on Monday

;;; --------------------------------------------------------------------
;;; Org Mode

; http://orgmode.org/worg/org-faq.html#keeping-current-with-Org-mode-development
(let ((d "~/lib/emacs/org-mode/lisp"))
  (when (file-exists-p d) (push d load-path)))

(eval-after-load "org"
  '(progn
    (setq org-startup-indented t
	  org-directory "~/.org"
	  org-default-notes-file "~/.org/refile.org"
	  org-agenda-files '("~/.org/todo.org" "~/.org/XRTX.org"
			     "~/.org/Infopulse.org" "~/.org/kolbas.org"))

    ;; http://doc.norang.ca/org-mode.html#TodoKeywords
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
	  org-treat-S-cursor-todo-selection-as-state-change nil
	  org-enforce-todo-dependencies t)
    (setq org-todo-keyword-faces
	  '(("TODO" :foreground "red" :weight bold)
	    ("NEXT" :foreground "blue" :weight bold)
	    ("DONE" :foreground "forest green" :weight bold)
	    ("WAITING" :foreground "orange" :weight bold)
	    ("HOLD" :foreground "magenta" :weight bold)
	    ("CANCELLED" :foreground "forest green" :weight bold)))
    (setq org-todo-state-tags-triggers
	  '(("CANCELLED" ("CANCELLED" . t))
	    ("WAITING" ("WAITING" . t))
	    ("HOLD" ("HOLD" . t) ("WAITING")) ; set :HOLD:, unset :WAITING:
	    ("TODO" ("WAITING") ("HOLD") ("CANCELLED"))
	    ("NEXT" ("WAITING") ("HOLD") ("CANCELLED"))
	    ("DONE" ("WAITING") ("HOLD") ("CANCELLED"))
	    (done ("WAITING") ("HOLD"))))

    ;; http://doc.norang.ca/org-mode.html#NextTasks
    (defun bh/mark-next-parent-tasks-todo ()
      "Visit each parent task and change NEXT states to TODO"
      (when (nth 2 (org-heading-components))
	(save-excursion
	  (while (org-up-heading-safe)
	    (when (member (nth 2 (org-heading-components)) (list "NEXT"))
	      (org-todo "TODO"))))))
    (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo
	      'append)
    (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)))

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cb" 'org-iswitchb)
    (global-set-key "\C-ca" 'org-agenda)))

(eval-after-load "org-clock"
  '(progn
     (setq org-clock-out-remove-zero-time-clocks t
	   org-clock-into-drawer 2
	   org-clock-idle-time 10)
     (global-set-key "\C-cc" 'org-clock-goto)))

(eval-after-load "org-archive"
  '(org-defkey org-mode-map "\C-c\C-x\C-a"
	       'org-archive-subtree-default-with-confirmation))
;; (global-set-key (kbd "C-M-r") 'org-capture)
;;; --------------------------------------------------------------------

;;; Spell check
(global-set-key (kbd "C-c ib") 'ispell-buffer)
(global-set-key (kbd "C-c ir") 'ispell-region)
;XXX; (add-hook 'c-mode-hook 'flyspell-prog-mode)

(eval-after-load "ps-print"
  '(setq ps-paper-type 'a4
	 ps-print-header nil
	 ps-multibyte-buffer 'bdf-font-except-latin))

;;; https://github.com/winterTTr/ace-jump-mode
(let ((fn "~/lib/emacs/ace-jump-mode/ace-jump-mode.el"))
  (when (file-readable-p fn)
    (autoload 'ace-jump-mode fn nil t)
    (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
    (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
    (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
    ;; Don't let org-mode grab the binding.
    (add-hook 'org-mode-hook
	      '(lambda () (define-key org-mode-map (kbd "C-c SPC") nil)))))

;;; http://www.emacswiki.org/emacs/FullScreen
(if (string< emacs-version "24.4")
    (progn
      (defun toggle-fullscreen ()
	(interactive)
	(set-frame-parameter
	 nil 'fullscreen
	 (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
      (global-set-key (kbd "C-c f") 'toggle-fullscreen))
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen))

(setq vc-follow-symlinks t)

;; Audio bells annoy me.  Setting `visible-bell' to `t' brings no
;; relief: it results in an ugly white rectangle appearing in the
;; middle of the screen (Emacs version 24.3.1).  That's why I redefine
;; `ring-bell-function':
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(define-key help-map "A" 'apropos-variable)

(progn ;; Unset unwelcome key bindings.
  (global-unset-key (kbd "s-q"))      ; `save-buffers-kill-emacs'
  (global-unset-key (kbd "s-w"))      ; `delete-frame'
  (global-unset-key "\C-z")           ; `suspend-frame'
  (global-unset-key (kbd "C-x C-z"))  ; `suspend-frame'
  (global-unset-key (kbd "s-:"))      ; `ispell'
  (global-unset-key (kbd "s-g"))      ; `isearch-repeat-forward'
  (global-unset-key (kbd "s-n"))      ; `ns-new-frame'
  (global-unset-key (kbd "s-z"))      ; `undo'
  (global-unset-key "\C-z"))          ; `undo'

(setq confirm-kill-emacs 'yes-or-no-p) ; say "no" to accidental terminations

(when (eq system-type 'darwin)
  (quail-define-package
   "russian-mac" "Russian" "RU" nil
   "ЙЦУКЕН Russian Mac layout"
   nil t t t t nil nil nil nil nil t)

  ;; >< 1! 2" 3№ 4% 5: 6, 7. 8; 9( 0) -_ =+
  ;;     Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ъ
  ;;      Ф  Ы  В  А  П  Р  О  Л  Д  Ж  Э  Ё
  ;;   ][  Я  Ч  С  М  И  Т  Ь  Б  Ю  /?

  (quail-define-rules
   ; row 1
   ("§" ?>)
   ; row 2
   ("q" ?й) ("w" ?ц) ("e" ?у) ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш)
   ("o" ?щ) ("p" ?з) ("[" ?х) ("]" ?ъ)
   ; row 3
   ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о) ("k" ?л)
   ("l" ?д) (";" ?ж) ("'" ?э) ("\\" ?ё)
   ; row 4
   ("`" ?\]) ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?и) ("n" ?т) ("m" ?ь)
   ("," ?б) ("." ?ю)
   ; Shift row 1
   ("±" ?<) ("@" ?\") ("#" ?№) ("$" ?%) ("%" ?:) ("^" ?,) ("&" ?.)
   ("*" ?\;)
   ; Shift row 2
   ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш)
   ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ъ)
   ; Shift row 3
   ("A" ?Ф) ("S" ?Ы) ("D" ?В) ("F" ?А) ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л)
   ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?Ё)
   ; Shift row 4
   ("~" ?\[) ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И) ("N" ?Т) ("M" ?Ь)
   ("<" ?Б) (">" ?Ю))

  (setq default-input-method "russian-mac")

  (when window-system
    ;; Stop falling back to rasterized Unicode characters.
    ;; Source: http://j.mp/1jxmsM8 (stackoverflow.com)
    (set-fontset-font "fontset-default" 'unicode '("Menlo" . "iso10646-1"))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((height . 44) (width . 80) (top . 0))))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control))))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Menlo")))))
