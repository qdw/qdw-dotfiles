#!/bin/bash

mkdir -p ~/.emacs.d/elisp && cd $_

git clone git://github.com/emacsmirror/apache-mode.git
cvs -d:pserver:anonymous@cvs.gna.org:/cvs/color-theme update color-theme
git clone git://github.com/emacsmirror/desire.git
git clone git://github.com/emacsmirror/ecmascript-mode.git
git clone git://github.com/emacsmirror/mmm-mode.git
wget http://www.thaiopensource.com/download/nxml-mode-20041004.tar.gz
wget http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el
git clone git://github.com/thaljef/perl-critic.git
python-mode/UPDATING:bzr update
bzr branch lp:python-mode
git clone git://github.com/eschulte/rinari.git
svn co http://rel.rubyforge.org/svn/ && ln -sf svn/trunk/* .
git clone git://github.com/emacsmirror/sql-indent.git
wget http://downloads.sourceforge.net/project/emacs-template/template/3.1c/template-3.1c.tar.gz?use_mirror=iweb
git clone git://github.com/yoshiki/yaml-mode.git

####perlnow/UPDATING:http://www.obsidianrook.com/perlnow/code/
####template/UPDATING:wget http://www.emacswiki.org/emacs/download/file-template.el
