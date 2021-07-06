;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq confirm-kill-emacs 'yes-or-no-p)

(setq uniquify-buffer-name-style 'forward)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Valery V. Vorotyntsev"
      user-mail-address "valery.vv@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(when IS-MAC
  (setq doom-font (font-spec :family "Menlo" :size 14)))

;; Position and size of the initial window frame.
(setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 53)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one)
(setq doom-theme (if IS-MAC 'doom-solarized-dark 'doom-tomorrow-night))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(display-time)  ; show time in the mode line

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :after iedit
      :map doom-leader-search-map :desc "Toggle Iedit mode" "e" #'iedit-mode)

(map! :after evil-easymotion :map evilem-map
      "s" #'avy-goto-word-or-subword-1
      "SPC" #'avy-pop-mark)
(setq avy-all-windows 'all-frames)

;; Don't register jumps between marks in the better-jumper's history.
(setq better-jumper-use-evil-jump-advice nil)
;; Let jump list behave like a backtrace (stack), not an ever-growing list.
(setq better-jumper-add-jump-behavior 'replace)

;; Suppress the pesky warning
;; > There are <num> files in folder <repo-dir> so watching the repo may slow Emacs down.
;; > Do you want to watch all files in <repo-dir>? (y or n) n
;; > LSP :: You can configure this warning with the `lsp-enable-file-watchers' and `lsp-file-watch-threshold' variables
(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        ;; Do NOT display signature documentation in eldoc.
        lsp-signature-render-documentation nil
        ;; Highlighted symbols are invisible in `doom-tomorrow-night' color theme.
        lsp-enable-symbol-highlighting nil))

;; Don't show information of the symbols, flycheck diagnostics, and LSP code actions
;; on the current line; see https://emacs-lsp.github.io/lsp-ui/#lsp-ui-sideline
(after! lsp-ui-sideline (setq lsp-ui-sideline-show-code-actions nil))

(after! lsp-rust
  ;; Activate inlay type hints ..
  ;; [https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/#inlay-hints]
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; .. but keep them hidden.
  (defvar vvv/hide-inlay-hints t "Whether to hide inlay type hints.")

  (defun vvv/lsp-rust-analyzer-inlay-hints-mode (orig-func &optional arg)
    ;; Inlay hints don't work unless
    ;; `lsp-rust-analyzer-server-display-inlay-hints' is set to `t'.
    ;;
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
    "Show/hide inlay type hints."
    (interactive)
    (let ((show vvv/hide-inlay-hints))
      (setq vvv/hide-inlay-hints (not show))
      (lsp-rust-analyzer-inlay-hints-mode (if show 1 -1))
      (message "Inlay hints %sabled" (if show "en" "dis"))))

  (map! (:when (featurep! :tools lsp)
         :mode rustic-mode
         :map doom-leader-toggle-map :desc "Inlay type hints" "t" #'vvv/toggle-lsp-rust-analyzer-inlay-hints))

  ;; Show inlay type hints for method chains.
  (setq lsp-rust-analyzer-display-chaining-hints t))

;; This thing started to cover the code with documentation windows quite aggressively
;; (I kinda know what `let' keyword does, thank you very much).  I am probably
;; missing something useful, but for now it's easier to just disable the whole package.
(after! lsp-ui-doc
  (setq lsp-ui-doc-enable nil))
