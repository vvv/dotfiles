;;; GETTING STARTED:
;;;   When in `*Group*' buffer, press `B' (gnus-group-browse-foreign-server).
;;;   Choose `nnfolder' backend, leaving address empty.
;;;   Subscribe to group (`mail.misc') with `u' key.

;; (setq mail-sources
;;       '((file)
;; 	(imap :server "hostname" :password "secret")))

(setq gnus-select-method '(nnfolder ""))

(gnus-demon-add-handler 'gnus-demon-scan-mail 2 nil) ; scan every 2 minutes
(setq gnus-verbose-backends 3) ; do not report mail scans

(defun split-with-osd (split)
  (osd split)
  split)
(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      '(| (from "servicedesk@.*" (: split-with-osd "mail.sdesk"))
	  (: split-with-osd "mail.misc")))

(setq gnus-message-archive-group "mail.sent") ; where to store sent messages

(setq nnmail-expiry-target 'nnmail-fancy-expiry-target
      nnmail-fancy-expiry-targets '(("from" ".*" "nnfolder:mail.archive-%y%m"))
      gnus-auto-expirable-newsgroups ".*")

(setq gnus-treat-display-smileys nil) ; don't display smileys
(setq trailing-whitespace-allowed (concat "^" (expand-file-name "~/Mail")))
