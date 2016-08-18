;;;; Emacs for Sys Admins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By: Sean Jones
;; Started: 7/25/16


;;;; Copyright and License ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; © Copyright 2016 Sean Jones
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Welcome to my Emacs configuration.
;;
;; The organization of this config uses a single, large file broken
;; into 'pages'. Each 'page' contains a category or type of
;; configuration.
;;
;; There are two indicators of pages. The first is a comment
;; starting with four semi-colons (;;;;). The second is (^L).
;; The comment line is to dilimit pages visually while the (^L)
;; character is to add functionality to the pages. Below is a brief
;; introduction to using pages.
;;
;; First you may wish to create additional pages or redefine the pages
;; currently in place. To create the (^L) character in a way that is
;; functional use the key chord 'C-q C-l'.
;;
;; Next is how to edit pages without distraction. Place the cursor
;; between two page delimiters and use the key chord 'C-x n p' or
;; evaluate (narrow-to-page). This will take you to a buffer that
;; edits the area between two page delimiters. To leave this special
;; buffer and return to the file in its entirety use the key chord
;; 'C-x n w' or evaluate (widen).
;;
;; You can use these functional delimiters in other useful ways.
;; To jump forward a page you would use 'C-x ]' or (forward-page).
;; To jump backwords you would use 'C-x [' or (backward-page).
;; There are several other useful commands related to pages I will
;; not describe here. Use 'C-h a' "page" <RET> to get a list of
;; functions related to pages.
;;
;; A note on require. In this config file I use the convention of
;; requiring a package directly before the configuration that needs
;; it. This keeps the file understandable. Why is this package
;; required? Look at the line below and there's your answer.


;;;; Systemd Unit Configuration for Emacs Daemon ;;;;;;;;;;;;;;;;;;;;;
;;
;; The Emacs daemon is one of my favorite features. On a system with
;; the traditional linux init system this is straight forward to
;; use. On a machine utilizing Systemd this is still easy but requires
;; the use of a Unit file, which is not provided in Emacs. Because of
;; this I like to keep a usable template in my config so I don't have
;; to hunt one down on the web or try to remember all the things a
;; good Unit file should have.
;;
;; Place this in (~/.config/systemd/user/emacs.service).
;;
;; ------------------------------------------------------------------
;;
;; [Unit]
;; Description=Emacs: the extensible, self-documenting text editor
;;
;; [Service]
;; Type=forking
;; ExecStart=/usr/bin/emacs --daemon
;; ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
;; Environment=SSH_AUTH_DOCK=%t/keyring/ssh
;; Restart=always
;;
;; [Install]
;; WantedBy=default.target
;;
;; ------------------------------------------------------------------
;;
;; You can then use the command:
;;
;;     'systemctl <action> --user emacs.service'
;;
;; In this command '<action>' can be either start, stop, restart,
;; enable, or disable. Once the daemon is running you can launch an
;; emacs client with "emacsclient -c".


;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section is dedicated to declaring and defining my custom
;; variables. The reason for this is to ensure that my configuration
;; is documented in a way that is consistant with the rest of
;; Emacs. Good documentation is the best!

(defvar my-packages nil
  "List of packages that should always be present for my configuration.

This list is not indicative of all packages that are
present. This list only defines what is explicitly installed. It
does not reflect any dependancies or 'built in' packages.")


;;;; Package Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section is for package management. There are four persistant
;; subsections: Repositories, Refreash package content, Package lists
;; and Auto-install packages. The repositories subsection defines
;; additional repos for Emacs to use and then initializes
;; 'package'. The refresh package content subsection gets all of the
;; packages from all of the configured repos and makes them available
;; for install. The Define package list subsection defines a list of
;; packages that should always be present. The Auto-install subsection
;; looks at the list of packages that should always be present and
;; checks that they are. If the package is already present nothing is
;; done, if the package is not present it will automatically be
;; installed.
;;
;; TODO - Package management should be able to fail gracefully. If a
;; repo is down initialization should continue as normal. If packages
;; are missing and cannot be installed the init should adapt.

;; Proxy
;;
;; Put this in (~/.emacs.d/proxy-info.el) and fill in the blanks with
;; your proxy information.
;;
;; ------------------------------------------------------------------
;;
;; (setq url-proxy-service '(("http"  . "")
;;                           ("https" . "")
;; 		             ("ftp"   . "")))
;;
;; ------------------------------------------------------------------
;;
;; Once that is in place uncomment the 'load-file' line below
;;
;; (load-file "~/.emacs.d/proxy-info.el")

;; Repositories
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh package list
(package-refresh-contents)

;; Define package list
(setq my-packages '(auto-complete
		    bbdb
		    gmail2bbdb
		    go-autocomplete
		    go-eldoc
		    go-mode
		    helm
		    hlinum
		    magit
		    markdown-mode
		    moe-theme
		    multiple-cursors
		    org-bullets
		    password-store
		    powerline
		    yaml-mode))

;; Windows packages
(defun windows-packages ()
  "This function adds windows specific packages to the install list"
  (add-to-list 'my-packages 'helm-w32-launcher t))
(if (eq system-type 'windows-nt)
    (windows-packages))

;; Confirm / Install packages
(dolist (package my-packages)
  (if (ignore-errors (require package))
      (message "%s is already installed..." package)
    (package-install package)))


;;;; Look and Feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This page is for configuration that changes either the look of
;; Emacs or the basic functionality. Things such as defining a theme,
;; changing bell behavior, initial screen, etc...

;; Theming
(require 'powerline)
(require 'moe-theme)
(powerline-moe-theme)
(setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(moe-dark)
;; Available Colors: blue, orange, green, magenta, yellow, purple,
;; red, cyan, w/b
;; Test colors interactivly with "moe-theme-select-colors"
(moe-theme-set-color 'blue)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; A more sane 'yes or no' prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; No bell
(setq ring-bell-function 'ignore)

;; Column numbers mode
(column-number-mode 1)

;; Highlight line number
(require 'hlinum)
(hlinum-activate)

;; Initial screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Run some elisp yo\n\n")


;;;; Package dependant configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are configurations that depend on an external package to be
;; installed. Configurations here can be anything from keybindings to
;; variables to functions.

;; Electric Pair
(electric-pair-mode 1)

;; Gnus
;;
;; To monitor a Gnus group for fresh news/mail do the following:
;;
;;     'M-x gnus' then do 'G p' in the group buffer
;;     Add '(modeline-notify t)' to the properties

(setq user-mail-address "sean.d.jones92@gmail.com")
(setq gnus-use-cache t)
(setq gmail2bbdb-bbdb-file "~/.emacs.d/bbdb")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(require 'gnus)
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
(setq send-mail-function 'smtpmail-send-it)

;; Golang
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook 'gofmt-before-save)
(setq gofmt-command "goimports")
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x x") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(if (eq system-type 'windows-nt)
    (global-set-key (kbd "C-c h w") 'helm-w32-launcher))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(helm-autoresize-mode 1)

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-recentf t)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)

(helm-mode 1)

;; iSpell
(setq ispell-dictionary "american")

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Mulitple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Org Mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq diary-file "~/.emacs.d/diary")
(setq org-agenda-include-diary t)
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/.emacs.d/notes.org")
(setq org-agenda-files '("~/.emacs.d/notes.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java       . t)
   (js         . t)
   (sql        . t)
   (emacs-lisp . t)
   (latex      . t)
   (ledger     . t)
   (lisp       . t)
   (org        . t)
   (perl       . t)
   (python     . t)
   (sh         . t)))


;;;; Custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My custom functions

(defun diary-drawer ()
  "Open the Emacs diary in a pop up fashion."
  (interactive)
  (find-file-other-window "~/.emacs.d/diary")
  (diary-mode))

(defun dired-show-only (regexp)
  "Only show files matching the regexp."
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun go-local ()
  "Clean up all remote connections and be a little funny about it."
  (interactive)
  (ignore-errors (tramp-cleanup-all-connections))
  (ignore-errors (tramp-cleanup-all-buffers))
  (message "Don't you know I'm local?!"))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

(defun proxy ()
  "set http_proxy env variable"
  (interactive)
  (if (y-or-n-p "Does this proxy require a login? ")
      (let ((hostname (read-from-minibuffer "What is the hostname? "))
	    (port     (read-from-minibuffer "What is the port? "))
	    (user     (read-from-minibuffer "What is the username? "))
	    (password (read-from-minibuffer "What is the password? ")))
	(setenv "http_proxy" (concat user ":" password "@" hostname ":" port))
	(setenv "https_proxy" (concat user ":" password "@" hostname ":" port))
	(setenv "ftp_proxy" (concat user ":" password "@" hostname ":" port)))
    (let ((hostname (read-from-minibuffer "What is the hostname? "))
	  (port     (read-from-minibuffer "What is the port? ")))
      (setenv "http_proxy" (concat hostname ":" port))
      (setenv "https_proxy" (concat hostname ":" port))
      (setenv "ftp_proxy" (concat hostname ":" port)))))

(defun save-buffer-clean ()
  "Strip the trailing whitespace from a file and save it."
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun smart-buffer-kill ()
  "Kill buffers in a way that makes sense."
  (interactive)
  (if (= (count-windows) 1)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun ssh-clip ()
  "Copy '~/.ssh/id_rsa.pub' to clipboard.
This will first empty the kill-ring (clipboard)"
  (interactive)
(if (= (count-windows) 1)
    (let ((origin (current-buffer)))
      (setq kill-ring nil)
      (find-file "~/.ssh/id_rsa.pub")
      (mark-page)
      (kill-ring-save (point-min) (point-max))
      (kill-buffer)
      (message "Public key copied to clipboard"))
  (let ((origin (current-buffer)))
    (setq kill-ring nil)
    (find-file-other-window "~/.ssh/id_rsa.pub")
    (mark-page)
    (kill-ring-save (point-min) (point-max))
    (kill-buffer)
    (switch-to-buffer-other-window origin)
    (message "Public key copied to clipboard"))))


;;;; Windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section has configurations to make this init file function
;; properly in a windows environment.
;;

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))


;;;; Custom hooks/modes  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'go-mode-hook 'linum-mode)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


;;;; Custom keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The subsection of this page titled 'Enabled bindings' is for
;; keybindings that are disabled by default in emacs that I have
;; chosen to enable.

(global-set-key "\C-cp" 'bbdb)
(global-set-key "\C-cd" 'diary-drawer)
(global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(require 'dired)
(define-key dired-mode-map [?%?h] 'dired-show-only)
(define-key gnus-group-mode-map (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; Enabled bindings
(put 'narrow-to-page 'disabled nil)
