;;;; Emacs for Sys Admins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By: Sean Jones
;; Started: 7/25/16


;;;; TO-DO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1) Get ispell working for windows
;;
;; 2) PDF rendering does not work on windows
;;
;; 3) Configure a section for Helm to replace Ido (at least test it)
;;   a) Try out helm-ispell
;;   b) helm-systemd
;;   c) helm-themes
;;   d) helm-w32-launcher - I think this is a windows thing
;;
;; 4) Package management section needs some love. When on windows
;; initialization hangs on "package-refresh-contents". Added a work
;; around for the time being. This needs to be handled in a better way.


;;;; Instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Welcome to my Emacs configuration.
;;
;; This configuration aims to be a little different from what
;; seems to be a typical Emacs configuration. To me, a typical config
;; is aimed at turning Emacs into a specialized development
;; environment targeted at specific programming languages and
;; development workflows. What I am trying to create here is an
;; Emacs config for Sys Admins. This should turn Emacs into a
;; toolbox that augments a persons ability to interactively administer
;; local and remote systems while remaining unbiased in terms of
;; programming/scripting preferences.
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


;;;; Package Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section is for package management. There are three persistant
;; subsections: Repositories, Refreash package list, and Auto-install
;; packages. The repositories subsection defines additional repos for
;; Emacs to use and then initializes 'package'. The refresh package
;; list subsection gets all of the packages from all of the configured
;; repos and makes them available for install. The Auto-install
;; subsection defines a list of packages that should always be present
;; and checks that they are. If the package is already present nothing
;; is done, if the package is not present it will automatically be
;; installed.

;; Proxy
;;
;; If running Emacs on GNU/Linux this should 'just work' if your proxy
;; information is defined as environment variables. For windows there
;; is some user intervention that needs to take place.
;;
;; I have chosen to configure the proxy this way so that my proxy
;; configurations are not put up on GitHub when I push my config
;; changes.
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
 '("mepla" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; Refresh package list
(if (eq system-type 'windows-nt)	; Ugly fix for ugly problem
    (if (y-or-n-p "Should the package list be refreshed? ")
	(package-refresh-contents))
  (package-refresh-contents))

;; Define package lists
;; Top list is windows, bottom is linux
(if (eq system-type 'windows-nt)
    (setq my-packages '(bbdb
			gmail2bbdb
			helm
			helm-w32-launcher
			ledger-mode
			magit
			markdown-mode
			moe-theme
			multiple-cursors
			password-store
			powerline
			request
			restclient
			yaml-mode))
  (setq my-packages '(bbdb
		      gmail2bbdb
		      helm
		      ledger-mode
		      magit
		      markdown-mode
		      moe-theme
		      multiple-cursors
		      password-store
		      powerline
		      request
		      restclient
		      yaml-mode)))

;; Auto-install packages
;; (dolist (package '(bbdb			; Define a windows and linux list
;; 		   gmail2bbdb
;; 		   helm
;; 		   helm-w32-launcher
;; 		   ledger-mode
;; 		   magit
;; 		   markdown-mode
;; 		   moe-theme
;; 		   multiple-cursors
;; 		   password-store
;; 		   powerline
;; 		   request
;; 		   restclient
;; 		   yaml-mode))
;;   (if (ignore-errors (require package))
;;       (message "%s is already installed..." package)
;;     (package-install package)))
(dolist (package my-packages)
  (if (ignore-errors (require package))
      (message "%s is already installed..." package)
    (package-install package)))


;;;; Look and Feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This page is for configuration that changes either the look of
;; Emacs or the basic functionality. Things such as defining a theme,
;; changing bell behavior, global minor modes, etc.

;; Load my theme
;; (load-theme 'wombat t)
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

;; Enable Electric Pair (auto bracket closing)
(electric-pair-mode 1)

;; Column numbers mode
(column-number-mode 1)

;; Spell check dictionary
(setq ispell-dictionary "american")

;; Enable 'Interactive Do'
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode 1)
; (icomplete-mode 1)

;; Initial screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Run some elisp yo\n\n")
;;(setq initial-major-mode 'org-mode)



;;;; Package dependant configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are configurations that depend on an external package to be
;; installed. Configurations here can be anything from keybindings to
;; variables to functions.

;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
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

;; Mulitple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;;;; Custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; If this page grows too large I may put its content into another
;; file and load it from here instead. This would allow for handling
;; my functions more like an external library.

(defun go-local ()
  "Clean up all remote connections and be a little funny about it."
  (interactive)
  (ignore-errors (tramp-cleanup-all-connections))
  (ignore-errors (tramp-cleanup-all-buffers))
  (message "Don't you know I'm local?!"))

(defun save-buffer-clean ()
  "Strip the trailing whitespace from a file and save it."
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

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

(defun smart-buffer-kill ()
  "Kill buffers in a way that makes sense."
  (interactive)
  (if (= (count-windows) 1)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun diary-drawer ()
  "Open the Emacs diary in a pop up fashion."
  (interactive)
  (find-file-other-window "~/.emacs.d/diary")
  (diary-mode))

(defun init-drawer ()
  "Open '~/.emacs.d/init.el' in a pop up fashion."
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

(defun dired-show-only (regexp)
  "Only show files matching the regexp."
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))


;;;; Windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section has configurations to make this init file function
;; properly in a windows environment.

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))


;;;; Custom hooks/modes  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


;;;; Custom keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These keybindings are for 'baked-in' or custom
;; functions/macros. Nothing defined here should rely on an external
;; package.
;;
;; The subsection of this page titled 'Enabled bindings' is for
;; keybindings that are disabled by default in emacs that I have
;; chosen to enable.

(global-set-key "\C-cp" 'bbdb)
(global-set-key "\C-ci" 'init-drawer)
(global-set-key "\C-cd" 'diary-drawer)
(global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(require 'dired)
(define-key dired-mode-map [?%?h] 'dired-show-only)
;; (define-key gnus-group-mode-map (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; Enabled bindings
(put 'narrow-to-page 'disabled nil)


;;;; Org Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This page contains all things 'org-mode'. Keybindings, variables,
;; everything. It's just too big to spread out. It's easier to have
;; all the org-mode configuration here.

;; UTF-8 Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Variables
(setq diary-file "~/.emacs.d/diary")
(setq org-agenda-include-diary t)
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/.emacs.d/notes.org")
(setq org-agenda-files '("~/.emacs.d/notes.org"))

;; Babel languages
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


;;;; Mail Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section is at the end because I still need to work out some of
;; the finer points. For example, Sometimes on startup there is an
;; error that 'send-mail-function' has a null value or some such
;; nonsense.
;;
;; To monitor a Gnus group for fresh news/mail do the following:
;;
;;     'M-x gnus' then do 'G p' in the group buffer
;;     Add '(modeline-notify t)' to the properties

(setq user-mail-address "sean.d.jones92@gmail.com")
(setq gnus-use-cache t)
(setq gmail2bbdb-bbdb-file "~/.emacs.d/bbdb")
;; (smtpmail-smtp-server "smtp.gmail.com")
;; (smtpmail-smtp-service 587)
;; (gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
;; (send-mail-function 'smtpmail-send-it)
