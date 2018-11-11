# utils.py --- utility functions used in the other modules of the
# server.

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

"""
This module contains utility functions used in the other modules of the
server.
"""

# Code:

import config
import Tkinter as tk
import tkFont
import tkMessageBox
import math
import os
import pdb
import subprocess

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

def dirFiles(dirName, pattern):
    """
    Return the list of files in the given directory that satisfy the
    given pattern.  

    :param dirName: Name of the directory.

    :param pattern: The pattern for the names of the files, such as \
    '\*.pdf'.

    :return: The list of files.
    """
    command = "ls -t {dirName}/{pattern}". \
              format(dirName=dirName, pattern=pattern)
    try:
        return subprocess.check_output(command, shell=True).split()
    except:
        return []
    
def readFile(fileName):
    """
    Return the contents of the file with the given name.

    :param fileName: The name of the file.

    :return: The contents of the file.
    """
    with open(fileName, "r") as myFile:
        return myFile.read()

def multisplit(s, delims):
    """ 
    Split the given string on the given delimeters. 

    The code is taken from https://stackoverflow.com/a/10655874/2725810.

    :param s: The string to be split.

    :param delims: The delimiters.

    :return: The list representing the split string.
    """ 
    pos = 0
    for i, c in enumerate(s):
        if c in delims:
            yield s[pos:i]
            pos = i + 1
    yield s[pos:]

# ## enum with automatic enumeration
# ## source: https://stackoverflow.com/a/1695250/2725810
# def enum(*sequential, **named):
#     enums = dict(zip(sequential, range(len(sequential))), **named)
#     return type('Enum', (), enums)

def center(toplevel):
    """Centers a window in ``TKinter``.

    The code is taken from https://stackoverflow.com/a/3353112/2725810.
    :param toplevel: The window to be centered.
    """
    toplevel.update_idletasks()
    w = toplevel.winfo_screenwidth()
    h = toplevel.winfo_screenheight()
    size = tuple(int(_) for _ in toplevel.geometry().split('+')[0].split('x'))
    x = w/2 - size[0]/2
    y = h/2 - size[1]/2
    toplevel.geometry("%dx%d+%d+%d" % (size + (x, y)))

class ProgressBox:
    """
    The modal window showing the status of the current operation.
    """
    
    def __init__(self, message):
        """
        Initialize the progress box with the given message.

        :param message: The message.
        """
        self.box = tk.Toplevel(padx=5, pady=5)
        self.box.title('LiteRef: Progress')
        tk.Label(self.box, text = message, font=('Times', 14)).pack()
        center(self.box)
        config.root.update()

    def __enter__(self):
        """
        For entering ``with``.
        """
        return self
    
    def __exit__(self, type, value, traceback):
        """
        For exiting ``with``. The required parameters are accepted,
        but not used.
        """
        self.box.destroy()
        config.root.update()
    
def wideYesNo(title, text):
    """
    Display a wide dialogue box for requesting a yes/no reply from the user and return the user's reply.

    :param title: The window's caption.

    :param text: The question.

    :return: The user's reply (yes or no).
    """
    config.root.option_add("*Dialog.msg.wrapLength", "10i")
    res = tkMessageBox.askyesno(title, text)
    config.root.update()
    config.root.option_add("*Dialog.msg.wrapLength", "3i")
    return res

## Functions for processing online pages

def getClassElement(driver, className):
    """
    Wait for the first element of the given HTML class to appear and return it.

    :param driver: The web driver for ``selenium``. See \
        :py:mod:`get_resource.driver`.

    :param className: The name of the HTML class.

    :return: The element of the requested HTML class.
    """
    wait = WebDriverWait(driver, 10)
    return wait.until(EC.visibility_of_element_located \
                      ((By.CLASS_NAME, className)))

def getIDElement(driver, id):
    """
    Wait for the first element with the given ID to appear and return it.

    :param driver: The web driver for ``selenium``. See \
        :py:mod:`get_resource.driver`.

    :param id: The requested element ID.

    :return: The requested element.
    """
    
    wait = WebDriverWait(driver, 10)
    return wait.until(EC.visibility_of_element_located \
                      ((By.ID, id)))
