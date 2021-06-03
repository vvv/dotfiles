;;; -*- coding: utf-8-unix -*-

(eval-when-compile (require 'subr-x))  ; string-empty-p, string-trim-right

(require 'server)
(unless (server-running-p)
  (server-start))

(setenv "LANG" "en_US.UTF-8")

(setq inhibit-startup-message t) ; don't show startup screen

;;; Remove toolbar, scrollbar, and menu bar
;;; (see http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html)
(menu-bar-mode -1)
(when window-system
  (dolist (f '(scroll-bar-mode tool-bar-mode)) (funcall f -1)))

;; Unset unwanted key bindings.
(dolist
    (b (list
        (kbd "s-q")       ; `save-buffers-kill-emacs'
        (kbd "s-w")       ; `delete-frame'
        "\C-z"            ; `suspend-frame'
        (kbd "C-x C-z")   ; `suspend-frame'
        (kbd "s-:")       ; `ispell'
        (kbd "s-g")       ; `isearch-repeat-forward'
        (kbd "s-f")       ; `isearch-forward'
        (kbd "s-l")       ; `goto-line'
        (kbd "s-m")       ; `iconify-frame'
        (kbd "s-n")       ; `ns-new-frame'
        (kbd "s-o")       ; `ns-open-file-using-panel'
        (kbd "s-p")       ; `ns-print-buffer'
        (kbd "s-t")       ; `ns-popup-font-panel'
        (kbd "s-v")       ; `yank'
        (kbd "s-z")       ; `undo'
        "\C-z"            ; `undo'
        (kbd "C-/")       ; `undo'
        (kbd "C-x C-p"))) ; `mark-page'
  (global-unset-key b))

(setq confirm-kill-emacs 'yes-or-no-p) ; say "no" to accidental terminations

(defun find-user-init-file ()
  "Edit `user-init-file'."
  (interactive)
  (find-file user-init-file))
;;; Kudos to @jamiecollinson for posting
;;; https://jamiecollinson.com/blog/my-emacs-config/
(global-set-key (kbd "C-c I") 'find-user-init-file)

(setq-default indent-tabs-mode nil)  ; Use spaces, not tabs.

;;; Adjust font size.
(dolist
    (s (list "s-+" "s-=" "s--" "s-0"))
  (global-set-key (kbd s) 'text-scale-adjust))

(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-g") 'goto-line)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; If `use-package' is not installed, install it.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

;;; Use $PATH and some other environment variables from the user's shell.
;;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "OPENSSL_ROOT_DIR"))
    (exec-path-from-shell-initialize)))

;; Always perform yes-or-no prompts using the echo area and keyboard input.
(setq use-dialog-box nil)

(transient-mark-mode t) ; highlight marked region
(column-number-mode) ; enable column number display in the mode line

(icomplete-mode t) ; enable incremental minibuffer completion
(if (version< emacs-version "24.4")
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

(setq backup-directory-alist '(("." . "~/.backups"))) ; backups location

;;; Add a newline automatically at the end of the file.
(setq require-final-newline t)

;;; Indicate unused lines at the end of the window with a small image
;;; in the left fringe.
(setq-default indicate-empty-lines t)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq display-time-day-and-date nil display-time-24hr-format nil)
(display-time)

(setq enable-local-variables :safe)

(setq c-default-style '((c-mode . "linux") (awk-mode . "awk") (other . "gnu")))
(global-set-key (kbd "M-g f") 'ff-find-other-file)

(add-hook 'c-mode-hook
          (lambda () (setq indent-tabs-mode t)))
(add-hook 'sh-mode-hook
          (lambda () (setq sh-basic-offset 4)))
(add-hook 'java-mode-hook
          (lambda () (setq c-basic-offset 8 tab-width 8 indent-tabs-mode t)))
(add-hook 'html-mode-hook
          (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))
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

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; Add `("REVIEW" . <color>)' to the front of `hl-todo-keyword-faces',
  ;; using the <color> of the "HACK" keyword.
  (let ((color (cdr (assoc "HACK" hl-todo-keyword-faces))))
    (setq hl-todo-keyword-faces
          (cons (cons "REVIEW" color) hl-todo-keyword-faces))))

;; ; XXX Use `hl-todo' package instead?
;; ; [https://github.com/tarsius/hl-todo]
(dolist
    (m '(c-mode dhall-mode elm-mode emacs-lisp-mode haskell-mode
         html-mode js-mode makefile-mode markdown-mode org-mode
         protobuf-mode python-mode ruby-mode rust-mode
         rustic-mode sh-mode sql-mode yaml-mode))
  (font-lock-add-keywords m
   ; Fontify "XXX", even in comments.
   '(("\\<\\(XXX\\|TODO\\|FIXME\\)\\>" 1 'font-lock-warning-face prepend))))

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (kill-new (filter-buffer-substring (point-min) (point-max)))
  (when (interactive-p)
    (message "Buffer saved")))
(global-set-key "\M-W" 'copy-buffer-as-kill)

(global-set-key (kbd "C-M-5") 'replace-string)
(global-set-key (kbd "C-S-K") 'kill-whole-line)
(global-set-key (kbd "C-c l") 'sort-lines)

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
(global-set-key (kbd "s-v") 'view-mode)

(setq calendar-week-start-day 1) ; weeks should begin on Monday

;;; --------------------------------------------------------------------
;;; Org Mode

(with-eval-after-load "org"
  (setq org-startup-indented t
        org-directory "~/.org")

;XXX   ;; capture
;XXX   (setq org-default-notes-file (concat org-directory "/notes.org"))
;XXX   (global-set-key "\C-cc" 'org-capture)

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

  (global-set-key "\C-cL" 'org-store-link)
  (global-set-key "\C-cB" 'org-switchb)
  (global-set-key (kbd "C-c a") 'org-agenda)

  (add-to-list 'org-file-apps '("\\.erb\\'" . emacs))

  ;; https://orgmode.org/manual/Structure-Templates.html
  ;; Enable org-tempo so that `< s <TAB>' is expanded to a code block.
  (require 'org-tempo)

  (setq org-outline-path-complete-in-steps nil))

(with-eval-after-load "org-clock"
  (setq org-clock-out-remove-zero-time-clocks t
        org-clock-into-drawer 2
        org-clock-idle-time 10)
  (global-set-key "\C-cc" 'org-clock-goto))

(with-eval-after-load "org-archive"
  (org-defkey org-mode-map "\C-c\C-x\C-a"
              'org-archive-subtree-default-with-confirmation))

;;; http://www.emacslife.com/baby-steps-org.html
(let ((f "~/.org/todo.org"))
  (when (file-exists-p f)
    (global-set-key "\C-cb"
                    `(lambda () (interactive) (find-file ,f)))
    (setq org-agenda-files `(,f))))
;;; --------------------------------------------------------------------

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode  ; FIXME
  :config
    (editorconfig-mode 1))

;XXX; (use-package treemacs
;XXX;   :ensure t
;XXX;   :config
;XXX;   (global-set-key (kbd "C-c r") 'treemacs)
;XXX;   (global-set-key (kbd "s-r") 'treemacs))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package rustic
  :ensure t
  :diminish rustic-mode)

;;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :hook (
         (dhall-mode . lsp)
         (ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)  ; 1MB
        gc-cons-threshold 100000000)
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-signature-render-documentation nil)
  :commands lsp)

;;; ------------------------------------------------------------------
;;; lsp-rust-analyzer-inlay-hints-mode workaround

(with-eval-after-load "lsp-rust"
  ;; Activate inlay hints ..
  ;; [https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/#inlay-hints]
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; .. but keep them hidden.
  (defvar vvv/hide-inlay-hints t "Whether to keep inlay hints hidden.")

  (defun vvv/lsp-rust-analyzer-inlay-hints-mode (orig-func &optional arg)
    ;; 1) Inlay hints do not work unless
    ;;    `lsp-rust-analyzer-server-display-inlay-hints' is set to `t'.
    ;; `lsp-rust.el' has this piece of code
    ;; ```
    ;;   :after-open-fn (lambda ()
    ;;                    (when lsp-rust-analyzer-server-display-inlay-hints
    ;;                      (lsp-rust-analyzer-inlay-hints-mode)))
    ;; ```
    ;; `(lsp-rust-analyzer-inlay-hints-mode)' enables the mode; this behaviour
    ;; is explained in the documentation of `define-minor-mode'.
    (unless (and (null arg) vvv/hide-inlay-hints)
      (funcall orig-func arg)))
  (advice-add 'lsp-rust-analyzer-inlay-hints-mode :around
              #'vvv/lsp-rust-analyzer-inlay-hints-mode)

  (defun vvv/toggle-lsp-rust-analyzer-inlay-hints ()
    "Show/hide inlay hints."
    (interactive)
    (let ((show vvv/hide-inlay-hints))
      (setq vvv/hide-inlay-hints (not show))
      (lsp-rust-analyzer-inlay-hints-mode (if show 1 -1))
      (message "Inlay hints %sabled" (if show "en" "dis"))))

  (global-set-key (kbd "s-L") #'vvv/toggle-lsp-rust-analyzer-inlay-hints)

  ;; (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t))
;;; ------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(use-package company
  :ensure t
  :diminish company-mode)

(use-package counsel-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (counsel-projectile-mode))

(use-package dhall-mode
  :config
  (remove-hook 'after-change-functions 'dhall-after-change)
  (setq dhall-use-header-line nil
        dhall-format-at-save nil)
  (add-hook 'dhall-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

;; (defun vvv/reload-tags-table (arg)
;;   "A combination of `tags-reset-tags-tables' and `visit-tags-table'."
;;   (interactive "P")
;;   (tags-reset-tags-tables)
;;   (if (null current-prefix-arg)
;;       (let ((tags-file (locate-dominating-file default-directory "TAGS")))
;;         (when tags-file
;;           (visit-tags-table tags-file)
;;           (message "Loaded tags file: %s" tags-file-name)))
;;     (call-interactively 'visit-tags-table)))
;; (global-set-key (kbd "C-c .") 'vvv/reload-tags-table)

;; (with-eval-after-load "ps-print"
;;   (setq ps-paper-type 'a4
;;         ps-print-header nil
;;         ps-multibyte-buffer 'bdf-font-except-latin))

;;; http://www.emacswiki.org/emacs/HideRegion
(let ((fn "~/lib/emacs/hide-region/hide-region.el"))
  (when (file-readable-p fn)
    (autoload 'hide-region-hide   fn nil t)
    (autoload 'hide-region-unhide fn nil t)
    (global-set-key (kbd "C-c h r") 'hide-region-hide)
    (global-set-key (kbd "C-c h u") 'hide-region-unhide)))

;;; http://www.emacswiki.org/emacs/FullScreen
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(setq vc-follow-symlinks t)

;; Audio bells annoy me.  Setting `visible-bell' to `t' brings no
;; relief: it results in an ugly white rectangle appearing in the
;; middle of the screen (Emacs version 24.3.1).  That's why I redefine
;; `ring-bell-function':
(setq ring-bell-function 'ignore)

(global-set-key (kbd "s-o") 'other-window)
(defun vvv/other-window-backwards ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'vvv/other-window-backwards)
(global-set-key (kbd "s-O") 'vvv/other-window-backwards)

(define-key help-map "A" 'apropos-variable)

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

  (setq default-input-method "ukrainian-mac")

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

;;; https://github.com/petere/emacs-eruby-mode.git
(let ((fn "~/lib/emacs/emacs-eruby-mode/eruby-mode.el"))
  (when (file-readable-p fn)
    (autoload 'eruby-mode fn nil t)))

(when (file-readable-p "~/lib/emacs/local.el")
  (load "~/lib/emacs/local.el"))

;;; ----------------------------------------------------------------------
;;; https://github.com/abo-abo

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
      (ivy-mode t)
      ;; Add recent files and bookmarks to ‘ivy-switch-buffer’.
      (setq ivy-use-virtual-buffers t)
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c j"   . counsel-git)
         ("C-c G"   . counsel-git-grep)
         ("C-c i"   . counsel-imenu))
  :defer 0)

(with-eval-after-load "counsel"
  ;; XXX Workaround for https://github.com/abo-abo/swiper/issues/866
  ;;
  ;; `string' parameter of `counsel-git-grep-function' may be equal to
  ;; "\n\n3 chars more". In this case the command
  ;; git --no-pager grep --full-name -n --no-color -i -e \"\(
  ;;
  ;; 3\).*\(chars\).*\(more\)\"
  ;; will be executed, resulting in
  ;; "fatal: -e option, '\(': Unmatched ( or \(" error.
  (defun vvv/first-line (args)
    (let ((string (car args)))
      (list (car (split-string string "[\f\n\r\v]+")))))
  (when (version<= "24.4" emacs-version)
    (advice-add #'counsel-git-grep-function :filter-args #'vvv/first-line)))

(use-package avy
  :ensure t
  :config
    (setq avy-all-windows 'all-frames)
  :bind (("C-."     . avy-goto-word-or-subword-1)
         ("C->"     . avy-goto-char-timer)  ; avy-goto-char-2 ?
         ("C-x SPC" . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-ignore-current t)
  (with-eval-after-load "diff-mode"
    ;; Expropriate the key binding from `diff-mode'.
    (define-key diff-mode-map (kbd "M-o") nil))
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))
;;; ----------------------------------------------------------------------

;;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible there anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))
(when (version<= "24.4" emacs-version)
  (advice-add 'which-function-mode :before
              (lambda (&rest ignore)
                (setq header-line-format
                      (unless which-function-mode
                        '((which-func-mode ("" which-func-format " "))))))))
(global-set-key (kbd "C-c W") 'which-function-mode)

(defun vvv/insert-date (arg)
  "Insert today's date in \"%Y-%m-%d (%a)\" format.

A prefix argument specifies the number of days to add to today."
  (interactive "P")
  (let ((shift_days (prefix-numeric-value (or arg 0))))
    (insert (format-time-string
             "%Y-%m-%d (%a)"
             (time-add (current-time) (* shift_days 24 3600))))))
(global-set-key (kbd "C-c d") 'vvv/insert-date)

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
(global-set-key (kbd "C-c 1") 'vvv/copy-file-path)

(defun vvv/grep (command-args)
  "When in git repository, run git-grep from its top-level directory;
otherwise run ordinary grep."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((git-toplevel (string-trim-right
                           (shell-command-to-string
                            "git rev-parse --show-toplevel 2>/dev/null")))
            (git-p (not (string-empty-p git-toplevel))))
       (list (read-shell-command
              "Run: "
              (if git-p
                  (let ((word (if (null current-prefix-arg)
                                  (current-word)
                                (format "\\<%s\\>" (current-word)))))
                    (format "cd %s; git --no-pager grep -nHE '%s'"
                            git-toplevel word))
                grep-command)
              'grep-history)))))
  (compilation-start command-args 'grep-mode)
  (switch-to-buffer-other-window grep-last-buffer))
(global-set-key (kbd "C-c g") 'vvv/grep)

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
(global-set-key (kbd "C-c $") 'aj-toggle-fold)

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

(use-package iedit
  :ensure t)

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

(global-set-key (kbd "C-c F") 'display-fill-column-indicator-mode)

(defun vvv/download-url (url path &optional sha1)
  "Download a file from URL, check its SHA1 checksum (if provided), save as PATH.
If the destination file exists, it will be overwritten."
  (url-copy-file url path 'overwrite)
  (when sha1
    (let ((checksum)
          (buf (find-file-noselect path nil t)))
      (setq checksum (secure-hash 'sha1 buf))
      (kill-buffer buf)
      (unless (string= checksum sha1)
        (error "%s: checksum doesn't match" path)))))

;;; ------------------------------------------------------------------
;;; Solarized color theme
;;; - http://ethanschoonover.com/solarized
;;; - https://github.com/sellout/emacs-color-theme-solarized

(use-package solarized-theme
  :ensure t
  :config
  (progn
    (defun vvv/toggle-solarized-light ()
      "Switch between dark/light modes of the Solarized color theme."
      (interactive)
      (setq frame-background-mode
            (if (eq frame-background-mode 'dark) 'light 'dark))
      (load-theme
       (intern
        (format "solarized-%s" frame-background-mode))
        ;; (format "solarized-%s-high-contrast" frame-background-mode))
       t)
      (mapc 'frame-set-background-mode (frame-list)))
    (global-set-key (kbd "C-M-8") 'vvv/toggle-solarized-light)
    (vvv/toggle-solarized-light)))
;;; ------------------------------------------------------------------

;;; "Undo" (and "redo") changes in the window configuration with the
;;; key commands `C-c left' and `C-c right'.
;;;
;;; See https://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode))

;;; Use `S-{right,left,up,down}' to move between neighbouring windows
;;; in a frame.
(windmove-default-keybindings)

(global-set-key (kbd "C-c H") 'hl-line-mode)

;;; https://www.emacswiki.org/emacs/VlineMode
(unless (fboundp 'vline-mode)
  (let ((path "~/.emacs.d/vline.el"))
    (unless (file-exists-p path)
      (vvv/download-url "http://www.emacswiki.org/emacs/download/vline.el"
                        "c4f5ea2731d8b89e24eec5e9be98e2652b54bdbc"))
    (load-file path)
    (global-set-key (kbd "C-c h") 'vline-mode)))

(use-package nlinum
  :ensure t
  :bind (("C-c n" . nlinum-mode)
         ("s-n" . nlinum-mode)))

(defun vvv/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "s-k") #'vvv/kill-current-buffer)

;; (use-package org-roam
;;   :bind ("C-c c" . org-roam-find-file)
;;   :init
;;   (add-hook 'after-init-hook #'org-roam-mode)
;;   :config
;;   (let ((d "~/.org-roam"))
;;     (unless (file-directory-p d)
;;       (make-directory d))
;;     (setq org-roam-directory d)))

;;; Set default font.
;;;
;;; See http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin)  ; MacOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo-14" nil t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono-13" nil t))))

;;; Position and size of the initial X window frame.
(setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 53)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-highlight-mode nil)
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(mac-command-modifier 'super)
 '(mac-option-modifier 'meta)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(org-modules nil)
 '(package-selected-packages
   '(solarized-theme color-theme-sanityinc-solarized hl-todo toml-mode just-mode json-mode typescript-mode terraform-mode rustic org-roam org-roam-server counsel-projectile feature-mode flycheck company company-mode which-key protobuf-mode lsp-ivy p editorconfig lsp-treemacs lsp-mode nlinum clojure-mode salt-mode dockerfile-mode dhall-mode flycheck-elm elm-mode ox-reveal exec-path-from-shell use-package-chords outshine iedit htmlize diminish use-package color-theme-solarized markdown-mode col-highlight indent-tools lua-mode hide-region counsel command-log-mode visual-regexp fill-column-indicator yaml-mode org ace-window swiper ggtags))
 '(which-function-mode nil)
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36"))))
 '(which-func ((t (:foreground "dark cyan")))))
