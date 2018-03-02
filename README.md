<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. About</a></li>
<li><a href="#sec-2">2. Configurations (Internal)</a>
<ul>
<li><a href="#sec-2-1">2.1. Meta</a></li>
<li><a href="#sec-2-2">2.2. Windows</a></li>
<li><a href="#sec-2-3">2.3. Base defaults</a></li>
<li><a href="#sec-2-4">2.4. Functions</a></li>
<li><a href="#sec-2-5">2.5. Mode hooks</a></li>
<li><a href="#sec-2-6">2.6. Keybindings</a></li>
</ul>
</li>
<li><a href="#sec-3">3. Configurations (External)</a>
<ul>
<li><a href="#sec-3-1">3.1. Packages</a></li>
<li><a href="#sec-3-2">3.2. Auto Complete</a></li>
<li><a href="#sec-3-3">3.3. Elpy</a></li>
<li><a href="#sec-3-4">3.4. Helm</a></li>
<li><a href="#sec-3-5">3.5. Magit</a></li>
<li><a href="#sec-3-6">3.6. Multiple cursors</a></li>
<li><a href="#sec-3-7">3.7. Paredit</a></li>
<li><a href="#sec-3-8">3.8. Projectile</a></li>
<li><a href="#sec-3-9">3.9. Highlight line number</a></li>
<li><a href="#sec-3-10">3.10. Neotree</a></li>
<li><a href="#sec-3-11">3.11. Themeing</a></li>
</ul>
</li>
<li><a href="#sec-4">4. Systemd unit file</a></li>
<li><a href="#sec-5">5. Licensing</a></li>
</ul>
</div>
</div>


# About<a id="sec-1" name="sec-1"></a>

This configuration is based off of the system shown [here](https://github.com/larstvei/dot-emacs). The idea is
that the configuration serves as it's own plain english
documentation.

Install with:

    git clone git@github.com:seandjones92/Emacs.git ~/.emacs.d

Once the repo is cloned launch Emacs to generate the running
config. After that is done, execute the following command to prevent
the dynamic configuration from being tracked in git:

    git update-index --assume-unchanged init.el

If you want to make changes to the repo-version of init.el start tracking again with:

    git update-index --no-assume-unchanged init.el

# Configurations (Internal)<a id="sec-2" name="sec-2"></a>

This section contains all of the configurations that do not rely on
external packages. If the configuration cannot be accomplished by a
standalone Emacs installation with no internet connection then it does
not belong here.

## Meta<a id="sec-2-1" name="sec-2-1"></a>

All changes to the config should be made to `init.org`, **not** in
`init.el`. The running configuration is generated at first launch and
whenever `init.org` is saved from within Emacs. Any changes made
directly to `init.el` will be lost, it is regenerated regularly.

The initial `init.el` looks like this:

    ;; This file replaces itself with the actual configuration at first run.
    
    ;; We can't tangle without org!
    (require 'org)
    ;; Open the configuration
    (find-file (concat user-emacs-directory "init.org"))
    ;; tangle it
    (org-babel-tangle)
    ;; load it
    (load-file (concat user-emacs-directory "init.el"))
    ;; finally byte-compile it
    (byte-compile-file (concat user-emacs-directory "init.el"))

It tangles the org-file, replacing itself with the actual configuration.

To make sure that the encoding prompt is not shown on launch we start
the init with this line:

    (set-language-environment "UTF-8")

The function defined below generates a new `init.el` each time
`init.org` is saved from within Emacs.

    (defun tangle-init ()
      "If the current buffer is 'init.org' the code-blocks are tangled, and the tangled file is compiled"
      (when (equal (buffer-file-name)
                   (expand-file-name (concat user-emacs-directory "init.org")))
        ;; Avoid running hooks when tangling
        (let ((prog-mode-hook nil))
          (org-babel-tangle)
          (byte-compile-file (concat user-emacs-directory "init.el")))))
    
    (add-hook 'after-save-hook 'tangle-init)

This section will generate `README.md` after each save.

    (defun generate-init-readme ()
      "If the current buffer is 'init.org' then 'README.md' is generated"
      (when (equal (buffer-file-name)
                   (expand-file-name (concat user-emacs-directory "init.org")))
        ;; Avoid running hooks
        (let ((prog-mode-hook nil))
          (org-md-export-to-markdown)
          (rename-file "init.md" "README.md" t))))
    
    (add-hook 'after-save-hook 'generate-init-readme)

If there is anything that should be kept private (not tracked by git)
put it in `private.el`, it will be loaded if it exists.

    (add-hook
     'after-init-hook
     (lambda ()
       (let ((private-file (concat user-emacs-directory "private.el")))
         (when (file-exists-p private-file)
           (load-file private-file)))))

## Windows<a id="sec-2-2" name="sec-2-2"></a>

This section is for defining any behavior needed for the configuration
to work properly on Windows. Unfortunately I have to use Windows at
work so these configurations are needed for me.

    (defun my-windows-config ()
      (setq default-directory (concat "C:\\Users\\" (user-login-name) "\\"))
      (setq python-shell-interpreter "py.exe"))

    (if (eq system-type 'windows-nt)
        (my-windows-config))

## Base defaults<a id="sec-2-3" name="sec-2-3"></a>

The settings defined here do not require anything to be
installed. This is just the base config for emacs.

Remove scrollbars, menu bars, and toolbars:

    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

A quicker 'yes or no' prompt:

    (defalias 'yes-or-no-p 'y-or-n-p)

Disable the system bell:

    (setq ring-bell-function 'ignore)

Enable column numbers:

    (column-number-mode 1)

Enable better handling of parens, quotes, etc.

    (electric-pair-mode 1)
    (require 'paren)
    (setq show-paren-style 'parenthesis)
    (show-paren-mode 1)

Enable spell checking:

    (setq ispell-dictionary "american")

Disable word wrapping:

    (set-default 'truncate-lines t)

Use `*scratch*` as initial screen:

    (setq inhibit-startup-screen t)
    (setq initial-scratch-message ";; Scratch page\n\n")

## Functions<a id="sec-2-4" name="sec-2-4"></a>

This is where I define custom functions.

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

## Mode hooks<a id="sec-2-5" name="sec-2-5"></a>

This is where mode hooks are manipulated.

    (add-hook 'text-mode-hook 'auto-fill-mode)
    (add-hook 'text-mode-hook 'linum-mode)
    (add-hook 'text-mode-hook 'toggle-truncate-lines)
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    (add-hook 'sh-mode-hook 'linum-mode)
    (add-hook 'python-mode-hook 'linum-mode)

## Keybindings<a id="sec-2-6" name="sec-2-6"></a>

This is where I define my custom keybindings.

    (global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
    (global-set-key (kbd "C-x C-s") 'save-buffer-clean)
    (require 'dired)
    (define-key dired-mode-map [?%?h] 'dired-show-only)

Enable keybindings that are disabled by default:

    (put 'narrow-to-page 'disabled nil)

# Configurations (External)<a id="sec-3" name="sec-3"></a>

Configurations after this point rely on external packages. Anything
added from here on out should be designed to fail gracefully in case
the package is not available.

## Packages<a id="sec-3-1" name="sec-3-1"></a>

This section goes over the configuration of package management. To
start this off we need to define a few things. First we will configure
the repositories we wish to use. The `jorgenschaefer.github.io` repo
is only needed for the Elpy package.

    (require 'package)
    (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")
                             ("elpy" . "https://jorgenschaefer.github.io/packages/")))

Next we define a function to determine if we have access to the
internet. We need to wrap this in a check for Windows since `ping`
options behave differently.

    (if (eq system-type 'windows-nt)
        (defun internet-up ()
            (call-process "ping" nil nil nil "-n" "1" "www.google.com"))
      (defun internet-up ()
          (call-process "ping" nil nil nil "-c" "1" "www.google.com")))

Next we define a list containing all of the packages that should be
installed to take full advantage of this configuration. The Silver
Searcher should be installed to use the `ag` and `helm-ag` packages.

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
                        paredit
                        projectile))

The next function defined is to loop through the provided list of
packages and to check if they are present. If not, the package is
installed:

    (defun auto-package-mgmt ()
      "Install my packages"
      (package-initialize)
      (package-refresh-contents)
      (dolist (package my-packages)
        (if (ignore-errors (require package))
            (message "%s is already installed..." package)
          (package-install package))))

To tie it all together we bring in the logic. If we have access to the
internet loop through the list of packages to ensure they are
installed. If we do not have access to the internet nothing is
done. Package dependent configuration is handled gracefully so if
there is no internet there should be no issue.

    (if (internet-up)
        (auto-package-mgmt))

## Auto Complete<a id="sec-3-2" name="sec-3-2"></a>

Here is where auto complete is configured. The `ac-sources` variable
needs to be set or the completion framework won't kick in.

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

## Elpy<a id="sec-3-3" name="sec-3-3"></a>

Elpy is used to get IDE like functionality for Python. To get full use
of this package run `pip install jedi flake8 importmagic autopep8`.

    (defun my-elpy-setup ()
      (package-initialize)
      (elpy-enable))
    
    (if (require 'elpy)
        (my-elpy-setup))

## Helm<a id="sec-3-4" name="sec-3-4"></a>

[Helm](https://github.com/emacs-helm/helm) is an Emacs framework for incremental completions and narrowing
selections. It's a much better way to interact with Emacs. I've broken
it out into smaller chunks so I can better explain what's going on.

This section enables fuzzy finding in almost everything Helm
does. This helps to really speed up interaction with emacs since you
can just type a couple partially completed words to get full phrases
instead of spelling everything out.

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

This part is where keybindings relevant to Helm are defined. The one
I've found to be most useful is `helm-mini` which is activated with
`C-x x`. This will show you currently open buffers and recent files.

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

This section has some more miscellaneous settings. In all honesty I
need to research them a bit more to accuratly describe what each of
these does.

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

This section tells the Helm interface that it should resize itself
depending on how much content it has to display, but should take up no
more than 65 percent of the Emacs interface.

    (defun my-helm-sizing ()
      (helm-autoresize-mode 1)
      (setq helm-autoresize-max-height 65))

Next we tie all of these pieces together in a setup function. It is
important to have the `(require 'helm-config)` on top or else the
configuration will fail.

    (defun my-helm-setup ()
      (require 'helm-config)
      (my-helm-fuzzy-settings)
      (my-helm-keybindings)
      (my-helm-misc)
      (my-helm-sizing)
      (helm-mode 1))

Finally we will check to see if Helm is available before applying any
of these settings.

    (if (require 'helm)
        (my-helm-setup))

## Magit<a id="sec-3-5" name="sec-3-5"></a>

Magit is something that, in my opinion, should be shipped by default
with Emacs. It's the most robust Git interface out there.

    (defun my-magit-setup ()
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))
    
    (if (require 'magit)
        (my-magit-setup))

## Multiple cursors<a id="sec-3-6" name="sec-3-6"></a>

    (defun my-multicursor-setup ()
      (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
    
    (if (require 'multiple-cursors)
        (my-multicursor-setup))

## Paredit<a id="sec-3-7" name="sec-3-7"></a>

This is for better handling of S-expressions in lisp languages

    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'eshell-mode-hook           #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook          #'enable-paredit-mode)

## Projectile<a id="sec-3-8" name="sec-3-8"></a>

    (defun my-projectile-setup ()
      (projectile-mode)
      (projectile-discover-projects-in-directory default-directory)
      (global-set-key (kbd "C-c p C-s") 'helm-projectile-ag)
      (global-set-key (kbd "C-c p C-p") 'helm-projectile-switch-project))
    
    (if (require 'projectile)
        (my-projectile-setup))

## Highlight line number<a id="sec-3-9" name="sec-3-9"></a>

    (if (require 'hlinum)
        (hlinum-activate))

## Neotree<a id="sec-3-10" name="sec-3-10"></a>

In order for this to look right the fonts for `all-the-icons` must be
installed. This is accomplished by `M-x all-the-icons-install-fonts`.

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

## Themeing<a id="sec-3-11" name="sec-3-11"></a>

    (defun my-moetheme-setup ()
      (moe-dark))
    
    (if (require 'moe-theme)
        (my-moetheme-setup))

# Systemd unit file<a id="sec-4" name="sec-4"></a>

Here is an example of a unit file for the emacs daemon. Place this in
`~/.config/systemd/user/emacs.service`.

    [Unit]
    Description=Emacs: the extensible, self-documenting text editor
    
    [Service]
    Type=forking
    ExecStart=/usr/bin/emacs --daemon
    ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
    Environment=SSH_AUTH_DOCK=%t/keyring/ssh
    Restart=always
    
    [Install]
    WantedBy=default.target

Once this is created run `systemctl enable --user emacs.service` to
enable the daemon, and `systemctl start --user emacs.service`

# Licensing<a id="sec-5" name="sec-5"></a>

© Copyright 2016 Sean Jones

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
