<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. About</a></li>
</ul>
</div>
</div>

# About<a id="sec-1" name="sec-1"></a>

This configuration is based off of the system shown [here](https://github.com/larstvei/dot-emacs). The idea is
that the configuration serves as it's own plain english
documentation.

Install with:

    git clone git@github.com:seandjones92/Emacs.git ~/.emacs.d

Once the repo is cloned execute the following commands to prevent the
dynamic configuration from being tracked in git:

    cd ~/.emacs.d
    git update-index --assume-unchanged init.el

If you want to make changes to the repo-version of init.el start tracking again with:

    git update-index --no-assume-unchanged init.el
