(set-language-environment "UTF-8")

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are tangled, and the tangled file is compiled"
  (interactive)
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

(setq default-directory (concat (getenv "HOME") "/"))

(setenv "PATH"
	(concat
	 (concat (getenv "HOME") "/.local/bin" ":")
	 (getenv "PATH")))

(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(column-number-mode 1)

(display-battery-mode 1)

(electric-pair-mode 1)
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

(setq ispell-dictionary "american")

(set-default 'truncate-lines t)

(setq dired-listing-switches "-lh")

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch page\n\n")

(defun dired-show-only (regexp)
  "Display files in the current directory that match the given
regular expression."
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun go-local ()
  "Destroy all TRAMP connections and kill all associated
buffers. Be aware that this will destroy local sudo/root TRAMP
sessions."
  (interactive)
  (ignore-errors (tramp-cleanup-all-connections))
  (ignore-errors (tramp-cleanup-all-buffers)))

(defun save-buffer-clean ()
  "Strip the trailing whitespace from lines and the end of the
file and save it."
  (interactive)
  (widen)
  (delete-trailing-whitespace)
  (save-buffer))

(defun smart-buffer-kill ()
  "If there is more than one buffer visible in the frame, kill the buffer and
its associated window."
  (interactive)
  (if (= (count-windows) 1)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun become-root (&optional prefix)
  "Elevate persmissions to root using TRAMP. If run without a
prefix, place the user at the root of the file system in
dired. If run with a prefix open the current file with elevated
permissions."
  (interactive "P")
  (if prefix
      (find-file (concat "/sudo:root@localhost:" buffer-file-name))
    (dired "/sudo:root@localhost:/")))

(defun ssh-clip ()
  "Copy '~/.ssh/id_rsa.pub' to clipboard. This will first empty
the kill-ring (clipboard)"
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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun full-frame-shell ()
  "Opens `shell' in a full frame."
  (interactive)
  (shell)
  (delete-other-windows))

(defun toggle-bars (arg)
  "Toggle both horizontal and vertical scroll bars."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (frame-parameter nil 'vertical-scroll-bars) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'vertical-scroll-bars
	       (if (> arg 0)
		   (or scroll-bar-mode default-frame-scroll-bars)))
	 (cons 'horizontal-scroll-bars
	       (when (> arg 0) 'bottom)))))

(defun update-config ()
  "Pull the config from github, load and byte-compile it."
  (interactive)
  (async-shell-command "cd ~/.emacs.d && git pull")
  (find-file (concat user-emacs-directory "init.org"))
  (org-babel-tangle)
  (load-file (concat user-emacs-directory "init.el"))
  (byte-compile-file (concat user-emacs-directory "init.el")))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in a
directory named after the org-buffer and insert a link to this
file."
  (interactive)
  (if (file-directory-p (concat buffer-file-name ".d"))
      (message "Directory already exists")
    (make-directory (concat buffer-file-name ".d")))
  (setq filename ;; do this first, if exit code is non 0 then do not proceed
	(concat
	 (make-temp-name
	  (concat (buffer-file-name)
		  ".d/"
		  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (setq relative-filename
	(concat "./" (mapconcat 'identity
				(nthcdr (- (length (split-string filename "/")) 2)
					(split-string filename "/")) "/")))
  (call-process "gnome-screenshot" nil nil nil "--area" "-f" filename)
  (insert (concat "[[" relative-filename "]]"))
  (org-display-inline-images))

(defcustom list-of-dired-switches
  '("-lh" "-lah")
  "List of ls switches for dired to cycle through.")

(defun cycle-dired-switches ()
  "Cycle through the list `list-of-dired-switches' of swithes for ls"
  (interactive)
  (setq list-of-dired-switches
	(append (cdr list-of-dired-switches)
		(list (car list-of-dired-switches))))
  (dired-sort-other (car list-of-dired-switches)))

(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
	mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
	(setq mb (match-beginning 0))
	(when (re-search-forward "\0338" end-marker t)
	  (ora-apt-progress-message
	   (substring-no-properties
	    (delete-and-extract-region mb (point))
	    2 -2)))))))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))

(defun go-to-scratch ()
  "Quickly go to scratch page."
  (interactive)
  (switch-to-buffer "*scratch*"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (sql . t)
   (perl . t)
   (python . t)
   (shell . t)))

(add-hook 'org-mode-hook 'turn-on-font-lock)

(add-hook 'text-mode-hook 'toggle-truncate-lines)

(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)

(global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(global-set-key (kbd "C-+") 'calc)
(global-set-key (kbd "C-c S") 'toggle-truncate-lines)
(global-set-key (kbd "C-!") 'become-root)
(global-set-key (kbd "C-~") 'eshell)
(global-set-key (kbd "C-`") 'eshell-here)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key [f12] 'toggle-bars)
(global-set-key [f5] 'update-config)
(global-set-key [f1] 'go-to-scratch)
(require 'dired)
(define-key dired-mode-map [?%?h] 'dired-show-only)
(define-key dired-mode-map [?%?G] 'find-grep-dired)
(define-key dired-mode-map [?%?f] 'find-name-dired)
(define-key dired-mode-map ")" 'cycle-dired-switches)
(define-key org-mode-map (kbd "C-}") 'my-org-screenshot)
(require 'flymake)
(define-key flymake-mode-map [f6] 'flymake-show-diagnostics-buffer)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa stable" . 10)
	("melpa"        . 0)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(package-install 'use-package)

(eval-when-compile
  (require 'use-package))

(use-package ag
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package auto-complete
  :ensure t)
