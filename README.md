
# Table of Contents

1.  [About](#org860c130)
2.  [Installation](#orgc4fb8c1)
3.  [Configurations (Internal)](#org3f4d281)
    1.  [Meta](#org58e0a9e)
    2.  [Base defaults](#org6cf1692)
    3.  [Functions](#org691bee2)
    4.  [Org Mode](#org69eda9c)
    5.  [Mode hooks](#orgc30f61f)
    6.  [Keybindings](#org9d97b6d)
4.  [Configurations (External)](#org10361f8)
    1.  [Packages](#org18f70b2)
    2.  [All the icons](#org914f12b)
    3.  [Auto Complete](#orgc5e0051)
    4.  [Docker](#org79c114b)
    5.  [Elpy](#org0954fef)
    6.  [Flymake](#org147a32e)
    7.  [Helm](#orgb09d0c4)
    8.  [Helm Tramp](#org7d462d7)
    9.  [Magit](#org69e19ae)
    10. [Paredit](#org37a606d)
    11. [Projectile](#org300432e)
    12. [Neotree](#orgd922965)
    13. [Themeing](#orgd78a859)
5.  [Systemd unit file](#org0b16a52)
6.  [Nautilus Scripts](#org9b9863e)
7.  [Licensing](#orgca81e8c)



<a id="org860c130"></a>

# About

This configuration is based off of the system shown [here](https://github.com/larstvei/dot-emacs). The idea is
that the configuration should serve as it's own plain english
documentation.


<a id="orgc4fb8c1"></a>

# Installation

Clone the repo to `~/.emacs.d`:

    git clone git@github.com:seandjones92/Emacs.git ~/.emacs.d

Once the repo is cloned execute the following commands to prevent the
dynamic configuration from being tracked in git:

    cd ~/.emacs.d
    git update-index --assume-unchanged init.el

If you want to make changes to the repo-version of init.el start tracking again with:

    git update-index --no-assume-unchanged init.el


<a id="org3f4d281"></a>

# Configurations (Internal)

This section contains all of the configurations that do not rely on
external packages. If the configuration cannot be accomplished by a
standalone Emacs installation with no internet connection then it does
not belong here.


<a id="org58e0a9e"></a>

## Meta

All changes to the config should be made to `init.org` **not** to
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
      (interactive)
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

If there is anything that should be kept private (not tracked by git,
and therefore not in this configuration) put it in
`~/.emacs.d/private.el`, it will be loaded if it exists.

    (add-hook
     'after-init-hook
     (lambda ()
       (let ((private-file (concat user-emacs-directory "private.el")))
         (when (file-exists-p private-file)
           (load-file private-file)))))


<a id="org6cf1692"></a>

## Base defaults

Here we define the basic look and feel of Emacs.

Make sure that Emacs always starts in the users home directory.

    (setq default-directory (concat (getenv "HOME") "/"))

Set the `PATH` variable.

    (setenv "PATH"
    	(concat
    	 (concat (getenv "HOME") "/.local/bin" ":")
    	 (getenv "PATH")))

Set the `exec-path` variable.

    (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))

Remove scrollbars, menu bars, and toolbars:

    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

Instead of typeing "yes" or "no" for interactive functions followed by
`<enter>`, all you need to do is press "y" or "n". No `<enter>`
required!

    (defalias 'yes-or-no-p 'y-or-n-p)

Disable the system bell. No flashing, no sounds.

    (setq ring-bell-function 'ignore)

Enable column numbers.

    (column-number-mode 1)

Enable battery mode. This is good for when you set emacs to full screen so your laptop doesn't die.

    (display-battery-mode 1)

For me this allows for better handling of parenthesis and quotes. As
you type `(` a matching `)` is also created. The same goes for
quotes. It also adds some inteligent handling. Further in the
configuration we use `paredit`, which takes things a step further.

    (electric-pair-mode 1)
    (require 'paren)
    (setq show-paren-style 'parenthesis)
    (show-paren-mode 1)

Enable spell checking.

    (setq ispell-dictionary "american")

Disable word wrapping by default, I don't like it.

    (set-default 'truncate-lines t)

This change makes `dired` list files with "human readable" size instead of just bytes.

    (setq dired-listing-switches "-lh")

Use `*scratch*` as initial screen. Also, modify the message at the top
of the buffer.

    (setq inhibit-startup-screen t)
    (setq initial-scratch-message ";; Scratch page\n\n")


<a id="org691bee2"></a>

## Functions

These are my custom functions. I define them all here. If I want them
assigned to a keybinding I do so later in the config.

This function is to be run in `dired`. It prompts for a regular
expression and only shows the files or directories that match the
provided regular expression. This is good for working in directories
with lots of files. Think `ls -al | grep -E <expression>`.

    (defun dired-show-only (regexp)
      "Display files in the current directory that match the given
    regular expression."
      (interactive "sFiles to show (regexp): ")
      (dired-mark-files-regexp regexp)
      (dired-toggle-marks)
      (dired-do-kill-lines))

This function is used to terminate all TRAMP connections and to kill
all buffers associated with TRAMP connections. Sometimes I'll have a
lot going on, machines I'm no longer working on, too many buffers to
sort through and this helps.

    (defun go-local ()
      "Destroy all TRAMP connections and kill all associated
    buffers. Be aware that this will destroy local sudo/root TRAMP
    sessions."
      (interactive)
      (ignore-errors (tramp-cleanup-all-connections))
      (ignore-errors (tramp-cleanup-all-buffers)))

This, in my opinion, is how Emacs should behave by default when saving
files. Strip all white space from the end of the file and the ends of
lines before saving.

    (defun save-buffer-clean ()
      "Strip the trailing whitespace from lines and the end of the
    file and save it."
      (interactive)
      (widen)
      (delete-trailing-whitespace)
      (save-buffer))

Again, another function to get what I would like to be default
behavior. This one handles killing buffers. If there is more than one
buffer and I kill one, kill its window too.

    (defun smart-buffer-kill ()
      "If there is more than one buffer visible in the frame, kill the buffer and
    its associated window."
      (interactive)
      (if (= (count-windows) 1)
          (kill-buffer)
        (kill-buffer-and-window)))

This function allows you to quickly elevate your privileges to
`root`. If called without a prefix you will be placed in dired at `/`,
if you call it with a prefix the current file will be reloaded and
accessed as `root`.

    (defun become-root (&optional prefix)
      "Elevate persmissions to root using TRAMP. If run without a
    prefix, place the user at the root of the file system in
    dired. If run with a prefix open the current file with elevated
    permissions."
      (interactive "P")
      (if prefix
          (find-file (concat "/sudo:root@localhost:" buffer-file-name))
        (dired "/sudo:root@localhost:/")))

This is one I don't use very often but can be useful. Copy the SSH
public key to the clipboard.

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

This function will open an `eshell` buffer named after the current
directory

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

This function will open `shell` using the full frame.

    (defun full-frame-shell ()
      "Opens `shell' in a full frame."
      (interactive)
      (shell)
      (delete-other-windows))

This function will toggle both the vertical and horizontal scroll
bars. Sometimes it's useful when reviewing large log files and using a
mouse to scroll.

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

This function will update the config from my github repository.

    (defun update-config ()
      "Pull the config from github, load and byte-compile it."
      (interactive)
      (async-shell-command "cd ~/.emacs.d && git pull")
      (find-file (concat user-emacs-directory "init.org"))
      (org-babel-tangle)
      (load-file (concat user-emacs-directory "init.el"))
      (byte-compile-file (concat user-emacs-directory "init.el")))

This function will use the gnome-screenshot tool to grab an area
screenshot, create a directory named after the current buffer, save
the screenshot inside that directory, and link to it in the current
buffer.

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

This function changes the options passed to `ls` that are used to generate the `dired` output.

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

This configuration is to help handle progress bars in `eshell`. Shamelessly stolen from [here](https://oremacs.com/2019/03/24/shell-apt/).

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

This function will take you directly to the scratch page.

    (defun go-to-scratch ()
      "Quickly go to scratch page."
      (interactive)
      (switch-to-buffer "*scratch*"))


<a id="org69eda9c"></a>

## Org Mode

Here is my functional configuration of Org Mode.

Enable more babel languages.

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((js . t)
       (sql . t)
       (perl . t)
       (python . t)
       (shell . t)))

Turn font lock on for Org Mode. This makes sure everything looks nice
and pretty.

    (add-hook 'org-mode-hook 'turn-on-font-lock)


<a id="orgc30f61f"></a>

## Mode hooks

This is where mode hooks are manipulated.

For `text-mode` I do want word wrapping enabled and `auto-fill-mode`
enabled. For me this makes sense when thinking about regular old
`*.txt` files.

    (add-hook 'text-mode-hook 'toggle-truncate-lines)

I don't like `global-linum-mode` so I only turn it on for specific
modes.

    (add-hook 'sh-mode-hook 'linum-mode)
    (add-hook 'python-mode-hook 'linum-mode)


<a id="org9d97b6d"></a>

## Keybindings

This is where I define my custom keybindings.

    (global-set-key (kbd "C-x C-k") 'smart-buffer-kill)
    (global-set-key (kbd "C-c k") 'kill-this-buffer)
    (global-set-key (kbd "C-x C-s") 'save-buffer-clean)
    (global-set-key (kbd "C-c p") 'helm-projectile)
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

Enable keybindings that are disabled by default:

    (put 'narrow-to-page 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'narrow-to-defun 'disabled nil)


<a id="org10361f8"></a>

# Configurations (External)

Configurations after this point rely on external packages. Anything
added from here on out should be designed to fail gracefully in case
the package is not available.


<a id="org18f70b2"></a>

## Packages

This section goes over the configuration of package management. To
start this off we need to define a few things. First we will configure
the repositories we wish to use.

    (require 'package)
    (setq package-archives
          '(("gnu" . "https://elpa.gnu.org/packages/")
    	("melpa stable" . "https://stable.melpa.org/packages/")
    	("melpa" . "https://melpa.org/packages/"))
          package-archive-priorities
          '(("melpa stable" . 10)
    	("melpa"        . 0)))

Next we define a function to determine if we have access to the
internet.

    (defun internet-up ()
      (call-process "ping" nil nil nil "-c" "1" "www.google.com"))

Next we define a list containing all of the packages that should be
installed to take full advantage of this configuration. The [Silver
Searcher](https://github.com/ggreer/the_silver_searcher) should be installed to use the `ag` and `helm-ag` packages.

    (setq my-packages '(ag
    		    all-the-icons
    		    auto-complete
    		    decide
    		    docker
    		    docker-compose-mode
    		    docker-tramp
    		    dockerfile-mode
    		    elpy
    		    flymake-shellcheck
    		    gist
    		    helm
    		    helm-ag
    		    helm-projectile
    		    helm-tramp
    		    htmlize
    		    magit
    		    markdown-mode
    		    ;; moe-theme
    		    neotree
    		    nord-theme
    		    org-bullets
    		    paredit
    		    pipenv
    		    projectile
    		    vlf))

The next function defined is to loop through the provided list of
packages and to check if they are present. If not, the package is
installed:

    (defun auto-package-mgmt ()
      "Install my packages"
      (interactive)
      (package-initialize)
      (package-refresh-contents)
      (dolist (package my-packages)
        (if (ignore-errors (require package))
    	(message "%s is already installed..." package)
          (package-install package))))

To tie it all together we bring in the logic. If this is the first
launch of Emacs and we have access to the internet, loop through the
list of packages to ensure they are installed. If we do not have
access to the internet, or if this is not Emacs first launch then
nothing is done. Package dependent configuration is handled gracefully
so if there is no internet there should be no issue.

    (if (file-directory-p (concat user-emacs-directory "elpa"))
        (package-initialize)
      (if (internet-up)
          (auto-package-mgmt)))


<a id="org914f12b"></a>

## All the icons

This package provides icons for neo-tree

    (require 'all-the-icons)


<a id="orgc5e0051"></a>

## Auto Complete

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


<a id="org79c114b"></a>

## Docker

The default configuration for docker is fine for me. I just want to
map the high level menu for easy access.

    (defun my-docker-setup ()
      (global-set-key (kbd "C-c d") 'docker))
    
    (if (require 'docker)
        (my-docker-setup))


<a id="org0954fef"></a>

## Elpy

[Elpy](https://github.com/jorgenschaefer/elpy) provides some python IDE features.

This ensures that Elpy can find virtual environments created by `pipenv`.

    (setenv "WORKON_HOME"
    	(concat (getenv "HOME") "/.local/share/virtualenvs"))

The section below ensures that it uses Python3 by default.

    (defun my-elpy-variables ()
      (setq elpy-rpc-python-command "python3")
      (setq python-shell-interpreter "python3"))

These things need to be enabled for elpy to work properly

    (defun my-elpy-prereqs ()
      (elpy-enable)
      (global-company-mode)
      (yas-global-mode))

Set custom elpy keybindings

    (defun my-elpy-keybindings ()
      (define-key-after elpy-mode-map (kbd "C-.") 'elpy-goto-definition-other-window))

Tie the two functions above together and enable the elpy config if the
package is installed

    (defun my-elpy-setup ()
      (my-elpy-variables)
      (my-elpy-prereqs)
      (my-elpy-keybindings))
    
    (if (require 'elpy)
        (my-elpy-setup))

TODO: Find a better way to do this
create pipenv environment in the current directory from emacs

    (defun pyvenv-create ()
      "Use pipenv to create a new virtual environment at the current
      directory"
      (interactive)
      (async-shell-command "pipenv install --dev flake8" "pyvenv-create-out"))


<a id="org147a32e"></a>

## Flymake

Here is where I can configure extra addons for flymake.

This enables integration with [shellcheck](https://www.shellcheck.net/).

    (add-hook 'sh-mode-hook 'flymake-mode)
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)


<a id="orgb09d0c4"></a>

## Helm

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


<a id="org7d462d7"></a>

## Helm Tramp

Helm TRAMP is used to quickly connect to machines in `~/.ssh/config`
and Docker containers.

    (defun my-helm-tramp-setup ()
      (global-set-key (kbd "C-c h h") 'helm-tramp))

Enable the helm tramp config if it is installed

    (if (require 'helm-tramp)
        (my-helm-tramp-setup))


<a id="org69e19ae"></a>

## Magit

Magit is something that, in my opinion, should be shipped by default
with Emacs. It's the most robust Git interface out there.

    (defun my-magit-setup ()
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))
    
    (if (require 'magit)
        (my-magit-setup))


<a id="org37a606d"></a>

## Paredit

This is for better handling of S-expressions in lisp languages.

    (defun my-paredit-setup ()
      (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
      (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
      (add-hook 'eshell-mode-hook           #'enable-paredit-mode)
      (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
      (add-hook 'cider-repl-mode            #'enable-paredit-mode))

If paredit is installed enable the config defined above

    (if (require 'paredit)
        (my-paredit-setup))


<a id="org300432e"></a>

## Projectile

Projectile makes emacs "project aware". This is good if you work on
multiple code bases and want to navigate between them and within them
efficiently.

    (defun my-projectile-keybindings ()
      (define-key projectile-mode-map (kbd "C-c a") 'helm-projectile-ag))
    
    (defun my-projectile-setup ()
      (projectile-mode)
      (projectile-discover-projects-in-directory default-directory)
      (add-hook 'projectile-mode-hook 'my-projectile-keybindings))
    
    (if (require 'projectile)
        (my-projectile-setup)
        (my-projectile-keybindings))


<a id="orgd922965"></a>

## Neotree

Adds a file tree to the left hand side, like in most IDEs. This only
works if you are in a project.

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
      (global-set-key [f8] 'neotree-project-dir)
      (setq neo-theme 'icons)
      (setq projectile-switch-project-action 'neotree-projectile-action)
      (setq neo-window-width 30))
    
    (if (require 'neotree)
        (my-neotree-setup))


<a id="orgd78a859"></a>

## Themeing


### [Nord theme](https://www.nordtheme.com/)

Here we do some themeing of emacs. None of this has any functional
impact, it just make the editor a little nicer to look at.

    (defun my-nord-theme-setup ()
      (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
      (load-theme 'nord t))
    
    (if (require 'nord-theme)
        (my-nord-theme-setup))
    
    (if (require 'org-bullets)
          (add-hook 'org-mode-hook
    		(lambda ()
    		  (org-bullets-mode 1))))


<a id="org0b16a52"></a>

# Systemd unit file

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

To launch a client map a keyboard shortcut to:

    /usr/bin/emacsclient -c -e "(progn (raise-frame) (x-focus-frame (selected-frame)))"


<a id="org9b9863e"></a>

# Nautilus Scripts

Nautilus allows users to create scripts that are included in the
right-click menu in the file browser. Place these in individual files
located in `$HOME/.local/share/nautilus/scripts/` and mark the as
executable.

    #!/bin/bash
    
    emacsclient -c "$@"


<a id="orgca81e8c"></a>

# Licensing

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

