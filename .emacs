;;; -*- coding: utf-8-unix -*-

(server-start)

(setenv "LANG" "en_US.UTF-8")

(setq inhibit-startup-message t) ; don't show startup screen

;;; Remove toolbar, scrollbar, and menu bar
;;; (see http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html)
(menu-bar-mode -1)
(when window-system
  (dolist (f '(scroll-bar-mode tool-bar-mode)) (funcall f -1)))

;; Always perform yes-or-no prompts using the echo area and keyboard input.
(setq use-dialog-box nil)

(transient-mark-mode t) ; highlight marked region

(icomplete-mode t) ; enable incremental minibuffer completion
(if (string< emacs-version "24.4")
    (progn
      (iswitchb-mode t) ; enable switching between buffers using substrings
      (setq iswitchb-prompt-newbuffer nil) ; create a buffer silently
      (defmacro with-eval-after-load (file &rest body)
	"Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
	(declare (indent 1) (debug t))
	`(eval-after-load ,file (lambda () ,@body))))
  ;; emacs-version >= 24.4
  (electric-indent-mode -1) ; let `C-j' indent the line
  (unless (locate-library "ivy") (ido-mode 1)))

(progn (require 'uniquify) (setq uniquify-buffer-name-style 'forward))

(setq backup-directory-alist '((".*" . "~/.backups"))) ; backups location

(setq require-final-newline t default-indicate-empty-lines t)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq display-time-day-and-date nil display-time-24hr-format nil)
(display-time)

(setq enable-local-variables :safe)

(setq c-default-style '((c-mode . "linux") (awk-mode . "awk") (other . "gnu")))
(global-set-key (kbd "M-g f") 'ff-find-other-file)

(add-hook 'sh-mode-hook
	  (lambda () (setq indent-tabs-mode nil sh-basic-offset 4)))
(add-hook 'java-mode-hook
	  (lambda () (setq c-basic-offset 8 tab-width 8 indent-tabs-mode t)))
(add-hook 'html-mode-hook
	  (lambda ()
	    (set (make-local-variable 'sgml-basic-offset) 4)
	    (setq indent-tabs-mode nil)))
(add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-hook 'octave-mode-hook
	  (lambda () (setq octave-block-offset 4)))

;;; GNU global
;;; https://github.com/leoliu/ggtags
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1))))
(with-eval-after-load "ggtags"
  (define-key ggtags-navigation-map "\M-*" 'ggtags-navigation-mode-abort)
  (define-key ggtags-mode-map "\M-*" 'ggtags-find-tag-continue))
;; In case Emacs is unable to find `global executable, the following
;; command might help (OSX >= 10.10.3):
;;
;;     sudo launchctl config system path "$PATH"
;;
;; [via https://twitter.com/launchderp/status/585874100939137024]

(dolist
    (m '(c-mode python-mode sh-mode rust-mode html-mode js-mode haskell-mode
		markdown-mode yaml-mode ruby-mode org-mode emacs-lisp-mode))
  (font-lock-add-keywords m
   ; Fontify "XXX", even in comments.
   '(("\\<\\(XXX\\)" 1 'font-lock-warning-face prepend))))

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (kill-new (filter-buffer-substring (point-min) (point-max)))
  (when (interactive-p)
    (message "Buffer saved")))
(global-set-key "\M-W" 'copy-buffer-as-kill)

(global-set-key (kbd "C-M-5") 'replace-string)

;;; --------------------------------------------------------------------
;;; Semi-automatic rstripping

(defvar trailing-whitespace-allowed
  (list "\.diff$"
	(concat "^" (expand-file-name "~/\.emacs\.d/elpa/")))
  "If file name matches any regexp from this list,
`delete-trailing-whitespace-if-confirmed' will skip it.")

(defun delete-trailing-whitespace-if-confirmed ()
  "Delete all the trailing whitespace across the current buffer,
asking user for confirmation."
  (unless (and trailing-whitespace-allowed
	       (cl-some (lambda (regexp)
			  (string-match-p regexp (buffer-file-name)))
			trailing-whitespace-allowed))
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

(global-set-key (kbd "C-c v") 'view-mode)

(setq calendar-week-start-day 1) ; weeks should begin on Monday

;;; --------------------------------------------------------------------
;;; Org Mode

(with-eval-after-load "org"
  (setq org-startup-indented t)
  (setq org-directory "~/.org")

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
  (global-set-key "\C-cb" 'org-switchb)

  (setq org-outline-path-complete-in-steps nil))

(with-eval-after-load "org-clock"
  (setq org-clock-out-remove-zero-time-clocks t
	org-clock-into-drawer 2
	org-clock-idle-time 10)
  (global-set-key "\C-cC" 'org-clock-goto))

(eval-after-load "org-archive"
  '(org-defkey org-mode-map "\C-c\C-x\C-a"
	       'org-archive-subtree-default-with-confirmation))

;;; --------------------------------------------------------------------
;;; Haskell

;; https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md#indentation
(with-eval-after-load "haskell-indentation"
  (setq haskell-indentation-layout-offset 2
	haskell-indentation-starter-offset 4
	haskell-indentation-left-offset 4
	haskell-indentation-where-pre-offset 2
	haskell-indentation-where-post-offset 2))

;;; --------------------------------------------------------------------

;;; Spell check
(global-set-key (kbd "C-c Ib") 'ispell-buffer)
(global-set-key (kbd "C-c Ir") 'ispell-region)

(eval-after-load "ps-print"
  '(setq ps-paper-type 'a4
	 ps-print-header nil
	 ps-multibyte-buffer 'bdf-font-except-latin))

;;; http://www.emacswiki.org/emacs/HideRegion
(let ((fn "~/lib/emacs/hide-region/hide-region.el"))
  (when (file-readable-p fn)
    (autoload 'hide-region-hide   fn nil t)
    (autoload 'hide-region-unhide fn nil t)
    (global-set-key (kbd "C-c h r") 'hide-region-hide)
    (global-set-key (kbd "C-c h u") 'hide-region-unhide)))

;;; http://www.emacswiki.org/emacs/FullScreen
(if (string< emacs-version "24.4")
    (progn
      (defun toggle-fullscreen ()
	(interactive)
	(set-frame-parameter
	 nil 'fullscreen
	 (unless (frame-parameter nil 'fullscreen) 'fullboth)))
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

;; Unset unwelcome key bindings.
(dolist
    (b (list
	(kbd "s-q")       ; `save-buffers-kill-emacs'
	(kbd "s-w")       ; `delete-frame'
	"\C-z"            ; `suspend-frame'
	(kbd "C-x C-z")   ; `suspend-frame'
	(kbd "s-:")       ; `ispell'
	(kbd "s-g")       ; `isearch-repeat-forward'
	(kbd "s-n")       ; `ns-new-frame'
	(kbd "s-z")       ; `undo'
	"\C-z"            ; `undo'
	(kbd "C-x C-p"))) ; `mark-page'
  (global-unset-key b))

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

  (quail-define-package
   "ukrainian-mac" "Ukrainian" "UK" nil
   "ЙЦУКЕН Ukrainian Mac layout"
   nil t t t t nil nil nil nil nil t)
  ;; >< 1! 2" 3№ 4% 5: 6, 7. 8; 9( 0) -_ =+
  ;;     Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ї
  ;;      Ф  И  В  А  П  Р  О  Л  Д  Ж  Є  Ґ
  ;;   '~  Я  Ч  С  М  І  Т  Ь  Б  Ю  /?
  (quail-define-rules
   ; row 1
   ("§" ?>)
   ; row 2
   ("q" ?й) ("w" ?ц) ("e" ?у) ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш)
   ("o" ?щ) ("p" ?з) ("[" ?х) ("]" ?ї)
   ; row 3
   ("a" ?ф) ("s" ?и) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о) ("k" ?л)
   ("l" ?д) (";" ?ж) ("'" ?є) ("\\" ?ґ)
   ; row 4
   ("`" ?\') ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?і) ("n" ?т) ("m" ?ь)
   ("," ?б) ("." ?ю)
   ; Shift row 1
   ("±" ?<) ("@" ?\") ("#" ?№) ("$" ?%) ("%" ?:) ("^" ?,) ("&" ?.)
   ("*" ?\;)
   ; Shift row 2
   ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш)
   ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ї)
   ; Shift row 3
   ("A" ?Ф) ("S" ?И) ("D" ?В) ("F" ?А) ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л)
   ("L" ?Д) (":" ?Ж) ("\"" ?Є) ("|" ?Ґ)
   ; Shift row 4
   ("~" ?\~) ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?І) ("N" ?Т) ("M" ?Ь)
   ("<" ?Б) (">" ?Ю))

  (when window-system
    ;; Stop falling back to rasterized Unicode characters.
    ;; Source: http://j.mp/1jxmsM8 (stackoverflow.com)
    (set-fontset-font "fontset-default" 'unicode '("Menlo" . "iso10646-1"))))

;;; ----------------------------------------------------------------------
;;; Make `M-s o' (`M-x occur') use the word at point as the default value.
;;; (The elements of `regexp-history' are also accessible using `M-n'.)
;;;
;;; See also
;;; http://lists.gnu.org/archive/html/emacs-devel/2005-08/msg01087.html

(defun current-word-and-regexp-history ()
  (let ((nearest (current-word t)))
    (if nearest
	(cons nearest regexp-history)
      regexp-history)))

;; Overwrite the original `occur-read-primary-args', defined in `replace.el'.
(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
	 (w (current-word t))
	 (defaults (if w (cons w regexp-history) regexp-history))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              'current-word-and-regexp-history)))
    (list regexp
	  (if perform-collect
	      ;; Perform collect operation
	      (if (zerop (regexp-opt-depth regexp))
		  ;; No subexpression so collect the entire match.
		  "\\&"
		;; Get the regexp for collection pattern.
		(let ((default (car occur-collect-regexp-history)))
		  (read-regexp
		   (format "Regexp to collect (default %s): " default)
		   default 'occur-collect-regexp-history)))
	    ;; Otherwise normal occur takes numerical prefix argument.
	    (when current-prefix-arg
	      (prefix-numeric-value current-prefix-arg))))))
;;; ----------------------------------------------------------------------

(when (file-readable-p "~/lib/emacs/htmlize.el")
  (autoload 'htmlize-buffer "~/lib/emacs/htmlize.el" nil t))

(when (file-readable-p "~/lib/emacs/local.el")
  (load "~/lib/emacs/local.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; ----------------------------------------------------------------------
;;; https://github.com/abo-abo

(when (require 'ivy nil 'noerror)
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-S-s") 'isearch-forward)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c j") 'counsel-git)
  (global-set-key (kbd "C-c g") 'counsel-git-grep)
  (global-set-key (kbd "C-c i") 'counsel-imenu)
  (global-set-key (kbd "C-c p") 'ivy-push-view)
  (global-set-key (kbd "C-c P") 'ivy-pop-view))

(with-eval-after-load "counsel"
  ;; XXX Workaround for https://github.com/abo-abo/swiper/issues/866
  (defun counsel-git-grep-function (string &optional _pred &rest _unused)
    "Grep in the current git repository for STRING."
    (if (and (or counsel-git-grep-skip-counting-lines
		 (> counsel--git-grep-count 20000))
	     (< (length string) 3))
	(counsel-more-chars 3)
      (let* ((string (car (split-string string "\n" t))) ; XXX Modify `string'
	     (default-directory counsel--git-dir)
	     (cmd (format counsel-git-grep-cmd
			  (setq ivy--old-re (ivy--regex string t)))))
	(if (and (not counsel-git-grep-skip-counting-lines)
		 (<= counsel--git-grep-count 20000))
	    (split-string (shell-command-to-string cmd) "\n" t)
	  (counsel--gg-candidates (ivy--regex string))
	  nil)))))

(when (require 'avy nil 'noerror)
  (setq avy-all-windows 'all-frames)
  (global-set-key (kbd "C-/") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-?") 'avy-goto-char-timer) ; avy-goto-char-2 ?
  (global-set-key (kbd "C-x SPC") 'avy-pop-mark))

(when (require 'ace-window nil 'noerror)
  (global-set-key (kbd "M-p") 'ace-window)
  (global-set-key (kbd "M-P") 'ace-swap-window)
  (setq aw-ignore-current t)

  ;; Prevent some modules from stealing "M-p" binding.
  (add-hook 'diff-mode-hook
            (lambda () (define-key diff-mode-map (kbd "M-p") nil)))
  (add-hook 'markdown-mode-hook
            (lambda () (define-key markdown-mode-map (kbd "M-p") nil)))
  (add-hook 'compilation-mode-hook
            (lambda () (define-key compilation-mode-map (kbd "M-p") nil)))
  (add-hook 'grep-mode-hook
            (lambda () (define-key grep-mode-map (kbd "M-p") nil))))
;;; ----------------------------------------------------------------------

;;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible there anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))
(advice-add 'which-function-mode :before
	    (lambda (&rest ignore)
	      (setq header-line-format
		    (unless which-function-mode
		      '((which-func-mode ("" which-func-format " ")))))))
(global-set-key (kbd "C-c c") 'which-function-mode)

(defun vvv/insert-date (arg)
  "Insert today's date in \"%Y-%m-%d (%a)\" format.

A prefix argument specifies the number of days to add to today."
  (interactive "P")
  (let ((shift_days (prefix-numeric-value (or arg 0))))
    (insert (format-time-string
	     "%Y-%m-%d (%a)"
	     (time-add (current-time) (* shift_days 24 3600))))))
(define-key global-map (kbd "C-c d") 'vvv/insert-date)

(defun vvv/copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

Adopted from Xah Lee's `http://ergoemacs.org/emacs/emacs_copy_file_path.html'"
  (interactive "P")
  (let ((-fpath
	 (abbreviate-file-name
	  (if (equal major-mode 'dired-mode)
	      (expand-file-name default-directory)
	    (if (buffer-file-name)
		(buffer-file-name)
	      (user-error "Current buffer is not associated with a file!"))))))
    (kill-new
     (if *dir-path-only-p
         (progn
           (message "Directory path copied: %s"
		    (file-name-directory -fpath))
           (file-name-directory -fpath))
       (progn
         (message "File path copied: %s" -fpath) -fpath)))))
(define-key global-map (kbd "C-c 1") 'vvv/copy-file-path)

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If
so, jump to that line number.
If path does not have a file extention, automatically try with
“.el” for elisp files.
This command is similar to `find-file-at-point' but without
prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2017-09-01"
  (interactive)
  (let*
      (($inputStr
	(if (use-region-p)
	    (buffer-substring-no-properties (region-beginning) (region-end))
	  (let
	      ($p0 $p1 $p2
		   ;; chars that are likely to be delimiters of
		   ;; file path or url, e.g. space, tabs,
		   ;; brakets. The colon is a problem. cuz it's in
		   ;; url, but not in file name. Don't want to use
		   ;; just space as delimiter because path or url
		   ;; are often in brackets or quotes as in
		   ;; markdown or html
		   ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
	    (setq $p0 (point))
	    (skip-chars-backward $pathStops)
	    (setq $p1 (point))
	    (goto-char $p0)
	    (skip-chars-forward $pathStops)
	    (setq $p2 (point))
	    (goto-char $p0)
	    (buffer-substring-no-properties $p1 $p2))))
       ($path
	(replace-regexp-in-string
	 "^file:///" "/"
	 (replace-regexp-in-string ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (progn (find-file (xahsite-url-to-filepath $path)))
          (progn (browse-url $path)))
      (progn                            ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
            (progn
              (let (($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (progn
                    (when (y-or-n-p
			   (format "file doesn't exist: 「%s」. Create?"
				   $fpath))
                      (find-file $fpath))))))
          (progn
            (if (file-exists-p $path)
                (find-file $path)
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?"
					$path))
                  (find-file $path ))))))))))
(global-set-key (kbd "C-c o") 'xah-open-file-at-cursor)

;;; - https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;;; - https://stackoverflow.com/a/4459159/136238
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (1+ (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(define-key global-map (kbd "C-c $") 'aj-toggle-fold)

;;; ------------------------------------------------------------------
;;; http://nullprogram.com/blog/2010/10/06/

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))
(global-set-key (kbd "C-c 8") 'set-80-columns)
;;; ------------------------------------------------------------------

(require 'iedit)

(setq compilation-scroll-output t)

(when (file-accessible-directory-p "/opt/local/share/info/")
  ;; # Create a `dir' file, if necessary:
  ;; cd /opt/local/share/info
  ;; if ! [ -f dir ]; then
  ;;     for f in {coreutils,make,gawk,gsed}.info {libc,gcc,cpp,gdb}.info.gz; do
  ;;         sudo install-info $f dir
  ;;     done
  ;; fi
  (add-hook 'Info-mode-hook
	    (lambda ()
	      (setq Info-additional-directory-list
		    '("/opt/local/share/info/")))))

(when (require 'fill-column-indicator nil 'noerror)
  (setq fci-rule-column 80)
  (global-set-key (kbd "C-c F") 'fci-mode))

;;; Solarized color theme
;;; - http://ethanschoonover.com/solarized
;;; - https://github.com/sellout/emacs-color-theme-solarized
(when (require 'color-theme-solarized nil 'noerror)
  (load-theme 'solarized t)
  (defun toggle-solarized-light ()
    "Switch dark/light modes of Solarized color theme."
    (interactive)
    (setq frame-background-mode
	  (if (eq frame-background-mode 'dark) 'light 'dark))
    (load-theme 'solarized t)
    (mapc 'frame-set-background-mode (frame-list)))
  (global-set-key (kbd "C-M-8") 'toggle-solarized-light))

(when (require 'god-mode nil 'noerror)
  (global-set-key (kbd "<escape>") 'god-mode-all)
  (setq god-exempt-major-modes nil god-exempt-predicates nil))

;;; https://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode) (winner-mode))

;;; Use `S-{right,left,up,down}' to move between neighbouring windows
;;; in a frame.
(windmove-default-keybindings)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((height . 46) (width . 80) (top . 0))))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-modules nil)
 '(package-selected-packages
   (quote
    (god-mode color-theme-solarized iedit markdown-mode col-highlight indent-tools lua-mode hide-region counsel command-log-mode multiple-cursors visual-regexp rust-mode fill-column-indicator haskell-mode yaml-mode org ace-window swiper ggtags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :foundry "apple" :family "Menlo")))))
