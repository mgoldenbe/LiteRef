#!/bin/bash

# Ubuntu packages
ub="python-pip python-tk pdfgrep libgraph-easy-perl graphviz"
for i in ${ub}; do
    if [[ $(dpkg -l | grep $i | grep ii | wc -c) -eq 0 ]]; then
	sudo apt install $i
    fi
done

# Python packages
py="pyinotify flufl.enum pybtex requests selenium"
for i in ${py}; do
    if [[ $(pip list | grep $i | wc -c) -eq 0 ]]; then
	sudo pip install $i
    fi
done

if [[ $(which chromedriver | wc -c) -eq 0 ]]; then
    read -p "Enter ChromeDriver version [2.43]: " v
    v=${v:-2.43}
    wget https://chromedriver.storage.googleapis.com/$v/chromedriver_linux64.zip
    unzip chromedriver_linux64.zip; rm -f chromedriver_linux64.zip

    read -p "Enter ChromeDriver path (should be in PATH) [/usr/bin/]: " path
    path=${path:-/usr/bin/}
    sudo mv chromedriver $path
fi

