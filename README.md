# My Emacs Configuration

This configuration aims to be a little different from what seems to be
a typical Emacs configuration. To me, a typical config is aimed at
turning Emacs into a specialized development environment targeted at
specific programming languages and development workflows. What I am
trying to create here is an Emacs config for Sys Admins. This should
turn Emacs into a toolbox that augments a persons ability to
interactively administer local and remote systems while remaining
unbiased in terms of programming/scripting preferences.

## Installation

To deploy this configuration issue the following command:

```
$ cd ~/

$ git clone git@bitbucket.org:sean_jones/.emacs.d.git
```

### Windows

If you are installing on windows make sure that the HOME environment
variable exists and is correct. Clone the repository to the value of
HOME.

An example of HOME would be:
```
C:\Users\MyAccount
```

## Gnus Configuration

Put the following into ~/.profile:
```
export EMAIL="<EMAIL_ADDRESS>"
export NAME="<FULL NAME>"
export SMTPSERVER="smtp.gmail.com"
```

Put the following into ~/.gnus:
```
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
```

Put the following into ~/.authinfo:
```
machine imap.gmail.com login <USER> password <PASSWORD> port imaps
machine smtp.gmail.com login <USER> password <PASSWORD> port 587
```
