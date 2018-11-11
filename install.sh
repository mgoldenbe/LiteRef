#!/bin/bash

# install.sh --- the LiteRef installation script.

# Copyright(C) 2017-2018 Meir Goldenberg

# This program is free software you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation either version 2, or (at your
# option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see
# <http://www.gnu.org/licenses/>.

# Commentary:

# This script installs all the packages needed by the LiteRef
# server and sets up Emacs configuration for the command layer.
# The command layer automatically installs the needed Emacs
# packages.

# Code:

# Update the package database
echo "Updating package information..."
sudo apt update

# Ubuntu packages
ub="emacs python-pip python-tk pdfgrep xsel libgraph-easy-perl graphviz gdebi-core elpa-pdf-tools-server texlive-latex-base texlive-latex-extra texlive-binaries"
for i in ${ub}; do
    if [[ $(dpkg -l | grep -w $i | grep ii | wc -c) -eq 0 ]]; then
	echo "Installing package" $i "..."
	sudo apt -y install $i
    else
	echo "Package" $i "is already installed."
    fi
done

# Google Chrome
if [[ $(which google-chrome | wc -c) -eq 0 ]]; then
    echo "Installing Google Chrome ..."
    rm -f google-chrome-stable_current_amd64.deb
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
    sudo gdebi google-chrome-stable_current_amd64.deb
    rm -f google-chrome-stable_current_amd64.deb
else
   echo "Google Chrome is already installed."
fi

# Python packages
py="pyinotify flufl.enum pybtex requests selenium"
for i in ${py}; do
    if [[ $(pip list --format=columns | grep -w $i | wc -c) -eq 0 ]]; then
	echo "Installing Python package" $i "..."
	sudo pip install $i
    else
	echo "Python package" $i "is already installed."
    fi
done

# Chrome Driver
if [[ $(which chromedriver | wc -c) -eq 0 ]]; then
    read -p "Enter ChromeDriver version [2.43]: " v
    v=${v:-2.43}
    wget https://chromedriver.storage.googleapis.com/$v/chromedriver_linux64.zip
    unzip chromedriver_linux64.zip; rm -f chromedriver_linux64.zip

    read -p "Enter ChromeDriver path (should be in PATH) [/usr/bin/]: " path
    path=${path:-/usr/bin/}
    sudo mv chromedriver $path
fi

# Emacs Configuration
read -p "Should we add the needed lines to your Emacs configuration now? (y/n) [y]: " v
v=${v:-"y"}
if [ $v != "y" ]; then
    exit
fi

mkdir -p $HOME/.emacs.d/
while : ; do
    read -p 'The configuration file [~/.emacs.d/init.el]: ' conf
    conf=${conf:-"~/.emacs.d/init.el"}
    eval "conf=$conf"
    
    if [ -d `dirname $conf` ]; then
	break
    fi
    echo "The path $conf does not exist."
done

touch $conf
if [[ $(grep "'package" $conf | wc -l) -eq 0 ]]; then
   echo "(require 'package)" >> $conf
fi
if [[ $(grep "package-initialize" $conf | wc -l) -eq 0 ]]; then
   echo "(package-initialize)" >> $conf
fi
if [[ $(grep "literef-directory" $conf | wc -l) -eq 0 ]]; then
    read -p "LiteRef folder [`pwd`]: " dir
    dir=${dir:-`pwd`}

    # append $dir with trailing slash if needed
    # source: https://gist.github.com/luciomartinez/c322327605d40f86ee0c
    length=${#dir}
    last_char=${dir:length-1:1}
    [[ $last_char != "/" ]] && dir="$dir/"; :

    lines=$(cat <<-END
    
;;;; For LiteRef
(setq literef-directory "$dir")
(add-to-list 'load-path (concat literef-directory "el/"))
(load-file (concat literef-directory "el/literef.el"))
END
)
    echo "$lines" >> $conf
fi

if [[ $(grep "helm-split-window-default-side" $conf | wc -l) -eq 0 ]]; then
    optional=$(cat <<-END

;;;; Optional org-mode configuration for LiteRef.
;; Split direction
(setq helm-split-window-default-side 'right)
;; Wrap long lines
(add-hook 'org-mode-hook 'visual-line-mode)
;; Indent headlines
(add-hook 'org-mode-hook 'org-indent-mode)
END
)
    echo "$optional"
    read -p "Should we add the above optional lines? (y/n) [y]: " v
    v=${v:-"y"}
    if [ $v = "y" ]; then
	echo "$optional" >> $conf
    fi
fi
