
(set-language-environment "UTF-8")

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are tangled, and the tangled file is compiled"
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(defun generate-init-readme ()
  "If the current buffer is 'init.org' then 'README.md' is generated"
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks
    (let ((prog-mode-hook nil))
      (org-md-export-to-markdown)
      (rename-file "init.md" "README.md" t))))

(add-hook 'after-save-hook 'generate-init-readme)

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

(defun my-windows-config ()
  (setq default-directory (concat "C:\\Users\\" (user-login-name) "\\"))
  (setq python-shell-interpreter "py.exe"))

(if (eq system-type 'windows-nt)
    (my-windows-config))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(column-number-mode 1)

(electric-pair-mode 1)
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

(setq ispell-dictionary "american")

(set-default 'truncate-lines t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch page\n\n")

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)
   (js . t)
   (sql . t)
   (clojure . t)
   (lisp . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (scheme . t)
   (sh . t)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)

(global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(require 'dired)
(define-key dired-mode-map [?%?h] 'dired-show-only)

(put 'narrow-to-page 'disabled nil)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

(if (eq system-type 'windows-nt)
    (defun internet-up ()
        (call-process "ping" nil nil nil "-n" "1" "www.google.com"))
  (defun internet-up ()
      (call-process "ping" nil nil nil "-c" "1" "www.google.com")))

(setq my-packages '(ag
                    all-the-icons
                    auto-complete
                    cider
                    elpy
                    gist
                    helm
                    helm-ag
                    helm-projectile
                    hlinum
                    magit
                    markdown-mode
                    moe-theme
                    multiple-cursors
                    neotree
                    org-bullets
                    paredit
                    projectile
                    powerline
                    zeal-at-point))

(defun auto-package-mgmt ()
  "Install my packages"
  (package-initialize)
  (package-refresh-contents)
  (dolist (package my-packages)
    (if (ignore-errors (require package))
        (message "%s is already installed..." package)
      (package-install package))))

(if (internet-up)
    (auto-package-mgmt))

(defun my-autocomplete-setup ()
  (ac-config-default)
  (setq-default ac-sources '(ac-source-filename
                             ac-source-functions
                             ac-source-yasnippet
                             ac-source-variables
                             ac-source-symbols
                             ac-source-features
                             ac-source-abbrev
                             ac-source-words-in-same-mode-buffers
                             ac-source-dictionary)))

(if (require 'auto-complete-config)
    (my-autocomplete-setup))

(defun my-elpy-keybindings ()
  (define-key elpy-mode-map (kbd "<f12>") 'elpy-goto-definition-other-window))

(defun my-elpy-setup ()
  (package-initialize)
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'my-elpy-keybindings))

(if (require 'elpy)
    (my-elpy-setup))

(defun my-helm-fuzzy-settings ()
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))

(defun my-helm-keybindings ()
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x x") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

(defun my-helm-misc ()
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-recentf t))

(defun my-helm-sizing ()
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 65))

(defun my-helm-setup ()
  (require 'helm-config)
  (my-helm-fuzzy-settings)
  (my-helm-keybindings)
  (my-helm-misc)
  (my-helm-sizing)
  (helm-mode 1))

(if (require 'helm)
    (my-helm-setup))

(defun my-magit-setup ()
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(if (require 'magit)
    (my-magit-setup))

(defun my-multicursor-setup ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(if (require 'multiple-cursors)
    (my-multicursor-setup))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'eshell-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode            #'enable-paredit-mode)

(defun my-projectile-keybindings ()
  (define-key projectile-mode-map (kbd "C-c a") 'helm-projectile-ag))

(defun my-projectile-setup ()
  (projectile-mode)
  (projectile-discover-projects-in-directory default-directory)
  (add-hook 'projectile-mode-hook 'my-projectile-keybindings))

(if (require 'projectile)
    (my-projectile-setup))

(if (require 'hlinum)
    (hlinum-activate))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(defun my-neotree-setup ()
  (global-set-key (kbd "C-c n") 'neotree-project-dir)
  (if (eq system-type 'windows-nt)
      (setq neo-theme 'arrow)
    (setq neo-theme 'icons))
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-width 30))

(if (require 'neotree)
    (my-neotree-setup))

(defun my-moetheme-setup ()
  (setq moe-theme-highlight-buffer-id t)
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (moe-dark))

(defun my-moetheme-with-powerline ()
  (powerline-moe-theme)
  (setq moe-theme-highlight-buffer-id t)
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.0 1.7 1.5 1.3 1.0 1.0 1.0 1.0 1.0))
  (moe-dark)
  (setq powerline-default-separator 'wave))

(if (require 'powerline)
    (if (require 'moe-theme)
        (my-moetheme-with-powerline))
  (if (require 'moe-theme)
      (my-moetheme-setup)))

(if (require 'org-bullets)
    (add-hook 'org-mode-hook
              (lambda ()
                (org-bullets-mode 1))))

(global-set-key "\C-cd" 'zeal-at-point)

(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files/Zeal"))
