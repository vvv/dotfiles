;;; -*- coding: utf-8-unix -*-

;; See http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time))

(require 'cl)

(setq inhibit-startup-message t) ; don't show startup screen

;; Remove toolbar, scrollbar, and menu bar
;; (see http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html)
(dolist (f '(menu-bar-mode scroll-bar-mode tool-bar-mode)) (funcall f -1))

;; Allow this Emacs process to be a server for client processes
(gnuserv-start)
(setq gnuserv-frame t)

;; ---------------------------------------------------------------------
;; internet

;; Opera browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "opera" browse-url-generic-args '("-newpage"))
(global-set-key (kbd "<f9> ww") 'browse-url)

;; w3m browser
(setq w3m-use-cookies t)
(global-set-key (kbd "<f9> w3") 'w3m-browse-url)
(global-set-key (kbd "<f9> wf") 'w3m-find-file)
(global-set-key (kbd "<f9> wi") 'w3m-view-image)

;; IRC server to use (see http://www.haskell.org/haskellwiki/IRC_channel)
(setq erc-server "chat.freenode.net")

(defun yubnub (command)
  "Uses `browse-url' to submit a command to yubnub and opens result
in an external browser defined in `browse-url-browser-function'.

To get started  `M-x yubnub <RET> ls <RET>' will return a list of
all yubnub commands."
  (interactive "syubnub command: ")
  (browse-url
   (concat "http://yubnub.org/parser/parse?command=" command)))
(global-set-key (kbd "<f9> y") 'yubnub)

;; ---------------------------------------------------------------------
;; On-Screen Display alerts

(if (and (display-graphic-p) (file-executable-p "/usr/bin/osd_cat"))
    (defun osd (fmt &rest args)
      "Display message on X screen."
      (let ((opts "-p bottom -A center -l 1 \
-f '-adobe-helvetica-bold-r-*-*-24-*-*-*-*-*-iso10646-1'")
            (msg (apply 'format (concat fmt "\n") args)))
        (start-process "osd" nil shell-file-name shell-command-switch
                       (format "echo %s | osd_cat %s"
                               (shell-quote-argument msg) opts))))
  (defalias 'osd 'message))

;; ---------------------------------------------------------------------
;; Cyrillic key bindings

(let ((convert (lambda (c) (+ (- c (make-char 'mule-unicode-0100-24ff 40))
			      (make-char 'cyrillic-iso8859-5)))))
  (mapc
   (lambda (pair)
     (global-set-key
      (vector (funcall convert (car pair))) (cdr pair)))
   '((?\C-а . forward-char)  (?\M-а . forward-word)
     (?\C-и . backward-char) (?\M-и . backward-word)
     (?\C-т . next-line)     (?\C-з . previous-line)
     (?\C-в . delete-char)   (?\M-в . kill-word)
     (?\C-ф . move-beginning-of-line) (?\C-у . move-end-of-line)
     (?\C-о . newline-and-indent)     (?\C-щ . open-line)
     (?\C-ц . kill-region)     (?\M-ц . kill-ring-save) (?\C-н . yank)
     (?\M-с . capitalize-word) (?\M-г . upcase-word) (?\M-д . downcase-word)
     (?\M-Б . beginning-of-buffer) (?\M-Ю . end-of-buffer)
     (?\C-ы . isearch-forward) (?\C-к . isearch-backward)
     (?\M-й . fill-paragraph)  (?\C-л . kill-line) (?\M-я . zap-to-char)
     (?\C-е . transpose-chars) (?\M-е . transpose-words)
     (?\C-п . keyboard-quit)   (?\C-д . recenter)))
  (global-set-key (apply 'vector (mapcar convert [?\C-ч ?л])) 'kill-buffer)
  (global-set-key (apply 'vector (mapcar convert [?\C-\M-з])) 'backward-list)
  (global-set-key (apply 'vector (mapcar convert [?\C-\M-т])) 'forward-list))
;; ^ http://community.livejournal.com/ru_emacs/20743.html#3
;; XXX See also:
;;   * `ps-mule-encode-ucs2' function definition.
;;   * (info "(elisp)Translation Keymaps")

;; ---------------------------------------------------------------------
;; mail

(global-set-key (kbd "<f12> <f12>") 'gnus) ; see also ~/.gnus.el
(global-set-key (kbd "<f12> m") 'gnus-group-mail)
(global-set-key (kbd "<f9> bb") 'bbdb)
(global-set-key (kbd "<f9> bc") 'bbdb-create)

;; Don't pop-up *BBDB* buffer when completing.
(setq bbdb-completion-display-record nil)

(setq sendmail-program "/usr/bin/msmtp" message-sendmail-f-is-evil nil
      user-mail-address (base64-decode-string "dmFsZXJ5LnZ2QGdtYWlsLmNvbQ=="))

;; ---------------------------------------------------------------------
;; Dayjob-specific stuff

(defun work-lan-p ()
  ; netstat -rn | grep -qE '^(0\.){3}0 +192\.168\.1\.1 '
  (zerop (call-process "/usr/local/bin/work-lan-p.sh")))

(when (work-lan-p)
  (setq user-mail-address (base64-decode-string "dnZ2QG10cy5jb20udWE="))

  (defun insert-dayjob-tag ()
    "Insert fashioned CVS tag (e.g., \"TESTING_17_JUL_2008\")."
    (interactive)
    (insert (upcase (format-time-string "TESTING_%d_%b_%Y"))))
  (global-set-key (kbd "<f9> r") 'insert-dayjob-tag)

  (defun mts-search (word)
    "Search MTS address book for a given word.

XXX [FIXME] Depends on custom Opera search `mts' to be available.
All we need is just to send HTTP POST request."
    (interactive
     (list (let ((default (current-word t)))
	     (read-string (concat "Search MTS for"
				  (when default (format " (%s)" default))
				  ": ")
			  nil nil default))))
    (browse-url (concat "mts " word)))
  (global-set-key (kbd "<f9> m") 'mts-search))

;; ---------------------------------------------------------------------
;; jabber

(eval-after-load "jabber"
  '(progn
     (setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -no_ticket"))

     (setq jabber-account-list
	   (list
	    (append `(,(base64-decode-string "dmFsZXJ5LnZ2QGdtYWlsLmNvbQ==")
		      (:connection-type . ssl))
		    (if (work-lan-p)
			'((:network-server . "127.0.0.1") (:port . 12345))
		      '((:network-server . "talk.google.com"))))))

     (defadvice jabber-process-subscription-request
       (around jabber-ignore-12111 (jc from presence-status) activate)
       "Process an incoming subscription request, ignoring some ICQ users.

icq.com:
  As part of the process of upgrading ICQ users to our newest, most
  advanced version, ICQ6, we have added a new user name to your
  contact list ''ICQ System''. The newly added user is intended to
  improve ICQ's line of communication with our users and assure you
  continue to enjoy talking to everybody, everywhere.
jabber.el:
  Go subscribe yourself."
       (unless
	   (string-match
"^\\(12111\\|176258467\\|278610504\\|279491906\\|86301548\\|869518\\)@icq\\."
	    from) ad-do-it))

     ;; auto-away
     (add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)
     (eval-after-load "jabber-autoaway"
       '(setq jabber-autoaway-method
	      (cond ((getenv "DISPLAY") 'jabber-xprintidle-get-idle-time)
		    ((null window-system) 'jabber-termatime-get-idle-time))))

     ;; Gmail
     (add-hook 'jabber-post-connect-hooks 'jabber-gmail-subscribe)
     (global-set-key (kbd "<f9> g") 'jabber-gmail-query)
     (eval-after-load "jabber-gmail"
       '(defun jabber-gmail-dothreads (threads)
	  (osd "gmail: %d" (length threads))))

     ;; on-screen notifications
     (defun jabber-message-osd (from buffer text proposed-alert)
       "Display a message using the osd_cat program."
       (let ((jid (if (jabber-muc-sender-p from) from (jabber-jid-user from))))
	 (osd (cdr (jabber-activity-lookup-name jid)))))
     (add-hook 'jabber-alert-message-hooks 'jabber-message-osd)

     ;; misc.
     (remove-hook 'jabber-alert-presence-hooks 'jabber-presence-echo) ; quiet!
     (setq jabber-vcard-avatars-retrieve nil
	   jabber-history-enabled t jabber-use-global-history nil
	   jabber-roster-show-bindings nil jabber-show-offline-contacts nil)
     (add-hook 'jabber-post-connect-hooks 'jabber-keepalive-start)))

;; ---------------------------------------------------------------------
;; Semi-automatic rstripping

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
(global-set-key (kbd "<f9> s") 'delete-trailing-whitespace)

(setq default-indicate-empty-lines t require-final-newline t)

;; ---------------------------------------------------------------------
;; color-theme

;; `color-theme.el' bug workaround [XXX Is it still needed?]
(eval-after-load "color-theme"
  '(defun color-theme-face-attr-construct (face frame)
     (if (atom face)
	 (custom-face-attributes-get face frame)
       (if (and (consp face) (eq (car face) 'quote))
	   (custom-face-attributes-get (cadr face) frame)
	 (custom-face-attributes-get (car face) frame)))))

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  (require 'color-theme)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (color-theme-snapshot) ; restore default (light) colors
    ;; create snapshot if necessary
    (when (not (commandp 'color-theme-snapshot))
      (fset 'color-theme-snapshot (color-theme-make-snapshot)))
    (color-theme-dark-laptop)))

(global-set-key (kbd "<f9> n") 'toggle-night-color-theme)
;; ---------------------------------------------------------------------

;; Haskell mode (see http://haskell.org/haskellwiki/Haskell_mode_for_Emacs)
(autoload 'inferior-haskell-load-file "haskell-site-file")
(eval-after-load "inf-haskell"
  '(global-set-key (kbd "<f9> h") 'haskell-hoogle))
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; dictionary-el
(global-set-key (kbd "<f9> d") 'dictionary-search)
(eval-after-load "dictionary"
  '(progn (setq dictionary-server "localhost")
	  (define-key dictionary-mode-map (kbd "DEL") 'scroll-down)))

;; spell check
(setq ispell-program-name "aspell")
(global-set-key (kbd "<f9> ib") 'ispell-buffer)
(global-set-key (kbd "<f9> ir") 'ispell-region)

;; org-mode
(autoload 'org-store-link "org")
(global-set-key (kbd "<f9> o") 'org-store-link)
(eval-after-load "org"
  '(setq org-log-done t
	 org-agenda-files '("~/job/TODO")))

;; Use proper X buffer encoding
;; (see <http://community.livejournal.com/ru_emacs/47287.html>).
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT STRING TEXT))

;; ---------------------------------------------------------------------
;; miscellaneous settings

(setq backup-directory-alist '((".*" . "~/.backups"))) ; backups location

(iswitchb-mode)  ; enable switching between buffers using substrings
(setq iswitchb-prompt-newbuffer nil) ; be quiet
(icomplete-mode) ; enable incremental minibuffer completion

(global-font-lock-mode t) ; enable syntax highlighting
(transient-mark-mode t)   ; highlight marked region
(mouse-wheel-mode t)
(auto-compression-mode 1) ; automatically uncompress files when visiting
(setq sort-fold-case t)   ; sorting functions should ignore case

(setq calendar-week-start-day 1 european-calendar-style t)

(eval-after-load "ps-print"
  '(setq ps-paper-type 'a4
	 ps-print-header nil
	 ps-multibyte-buffer 'bdf-font-except-latin))

;; insert at point regardless of where you click
(setq mouse-yank-at-point t)

(global-set-key (kbd "<f9> t") 'toggle-truncate-lines)
(global-set-key (kbd "<f9> v") 'view-file)

(define-key help-map "A" 'apropos-variable)

(defun other-window-back ()
  "Select previous window on this frame.
The result is equal to evaluating `(other-window -1)'."
  (interactive)
  (other-window -1))
(define-key ctl-x-map "O" 'other-window-back)

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it."
  (interactive)
  (let ((transient-mark-mode nil)) ; don't deactivate the mark
    (copy-region-as-kill (point-min) (point-max)))
  (when (interactive-p)
    (message "Buffer saved")))
(global-set-key "\M-W" 'copy-buffer-as-kill)

(setq vc-follow-symlinks nil)

;; enable some properties
(dolist (s '(downcase-region narrow-to-region scroll-left upcase-region))
  (put s 'disabled nil))

;; I never miss `x-menu-bar-open' but often do F9 key.
(global-set-key [f10] (key-binding [f9]))

;; Set proper encoding for X buffer.
;; (See <http://community.livejournal.com/ru_emacs/47287.html>.)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT STRING TEXT))

;; report loading time
(message "~/.emacs loaded in %d seconds"
	 (destructuring-bind (hi lo ms) (current-time)
	   (- (+ hi lo)
	      (+ (car *emacs-load-start*) (cadr *emacs-load-start*)))))
(unintern '*emacs-load-start*)

(ignore-errors (jabber-connect-all))
