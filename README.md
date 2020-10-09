# dotfiles

Setting up Emacs.  Currently using version 26.1, 26.3 and 27.1:

1. remove ~/.emacs.d
2. create directory ~/.emacs.d/lisp/cwca
3. In new directory ~/.emacs.d/lisp/cwca, git clone git@github.com:cwca/dotfiles.git
4. copy ~/.emacs.d/lisp/cwca/dotfiles/dot-emacs to ~/.emacs
5. emacs ~/.emacs.d/lisp/cwca/dotfiles/package-refresh.el, then M-x eval-buffer
6. emacs ~/.emacs and modify settings to suit current machine.

Machine-local settings are kept in .emacs, since this is where emacs itself will overwrite
things like fonts, themes, and whatnot.  Common settings are kept in the git repository and
intended to be identical between all personal installs on different platforms and machines.

There are various package managers for emacs that try to automate some of this; I have not
found them to work all that well.  Updating packages is done rarely and when needed (some 
problem with a package, want some new features) and best done manually.


