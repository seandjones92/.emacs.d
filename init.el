;;;; Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;; Systemd Unit Configuration for Emacs Daemon ;;;;;;;;;;;;;;;;;;;;;
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
;; 'systemctl <action> --user emacs.service'

;; Prevent encoding prompt at start
(set-language-environment "UTF-8")

;; Set Windows home
(if (eq system-type 'windows-nt)
    (setq default-directory (concat "C:\\Users\\" (user-login-name) "\\")))

;; check if internet is available
(if (eq system-type 'windows-nt)
    (defun internet-up ()
	(call-process "ping" nil nil nil "-n" "1" "www.google.com"))
  (defun internet-up ()
      (call-process "ping" nil nil nil "-c" "1" "www.google.com")))

;; Define package list
(setq my-packages '(auto-complete
		    helm
		    helm-projectile
		    hlinum
		    magit
		    markdown-mode
		    moe-theme
		    multiple-cursors
		    projectile))

;; Package management
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(defun auto-package-mgmt ()
  "Install my packages"
  (package-initialize)
  (package-refresh-contents)
  (dolist (package my-packages)
    (if (ignore-errors (require package))
	(message "%s is already installed..." package)
      (package-install package))))

;; package management with internet check
(if (internet-up)
    (auto-package-mgmt))

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

;; Electric Pair
(electric-pair-mode 1)

;; Highlight parenthesis
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

;; iSpell
(setq ispell-dictionary "american")

;; Truncate lines
(set-default 'truncate-lines t)

;; Initial screen
(setq inhibit-startup-screen t)


;;;; FUNCTIONS

(defun dired-show-only (regexp)
  "Only show files matching the regexp."
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun go-local ()
  "Clean up all remote connections."
  (interactive)
  (ignore-errors (tramp-cleanup-all-connections))
  (ignore-errors (tramp-cleanup-all-buffers)))

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

;;;; Custom hooks/modes  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)

;;;; Custom keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(require 'dired)
(define-key dired-mode-map [?%?h] 'dired-show-only)

;; Enabled bindings
(put 'narrow-to-page 'disabled nil)

;;;; PACKAGES
;; eval after load

;; Helm
(defun my-helm-setup ()
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
  (setq helm-autoresize-max-height 65)

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

  (helm-mode 1))

(if (require 'helm)
    (my-helm-setup))

;; Magit
(defun my-magit-setup ()
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(if (require 'magit)
    (my-magit-setup))

;; Mulitple cursors
(defun my-multicursor-setup ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(if (require 'multiple-cursors)
    (my-multicursor-setup))

;; Projectile
(if (require 'projectile)
    (projectile-mode))

;; Highlight line number
(if (require 'hlinum)
    (hlinum-activate))

;; Moe theme
(defun my-moetheme-setup ()
  (moe-dark))

(if (require 'moe-theme)
    (my-moetheme-setup))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors moe-theme markdown-mode magit hlinum helm-projectile csv-nav auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
