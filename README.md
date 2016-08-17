# My Emacs Configuration

This configuration is for System Administrators. I am beginning to
tailor it for remote administration and popular scripting
languages. I'm currently in the process of tweaking the init.el file
for Go, Python, and Ruby code.

## Installation

To deploy this configuration issue the following command:

```
$ cd ~/

$ git clone git@bitbucket.org:sean_jones/.emacs.d.git
```

### Windows

If you are installing on windows make sure that the HOME environment
variable exists and is correct. You will need to install some
[DLLs](https://sourceforge.net/projects/ezwinports/) and external
programs to ensure a working Emacs installation. At the time of this
writting there are still some issues on Windows (ispell comes to
mind).

An example of HOME would be:
```
C:\Users\MyAccount\
```

You will also need to use [Cygwin](https://www.cygwin.com/) to install
your Linux tools. I use the default Cygwin install. Make sure that
your Cygwin install is in the Windows path. This usually looks like:

```
C:\cygwin64\bin\
```

To get TRAMP working on windows you will need to install
[Putty](http://www.putty.org/). Next you will need to, once again,
modify your windows path to allow access to "plink.exe". The init.el
file in this repo is configured to use this executable from the path
when running on windows.

Once that is all done clone the repository to the value of HOME.

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
