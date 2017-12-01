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
    Return the list of files in the given directory that satisfy the given pattern, e.g. '*.pdf'.
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
    """
    with open(fileName, "r") as myFile:
        return myFile.read()

# Source: https://stackoverflow.com/a/10655874/2725810
def multisplit(s, delims):
    """ 
    Split the string s on the given delimeters.
    """ 
    pos = 0
    for i, c in enumerate(s):
        if c in delims:
            yield s[pos:i]
            pos = i + 1
    yield s[pos:]

## enum with automatic enumeration
## source: https://stackoverflow.com/a/1695250/2725810
def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)

# Source: https://stackoverflow.com/a/3353112/2725810
def center(toplevel):
    toplevel.update_idletasks()
    w = toplevel.winfo_screenwidth()
    h = toplevel.winfo_screenheight()
    size = tuple(int(_) for _ in toplevel.geometry().split('+')[0].split('x'))
    x = w/2 - size[0]/2
    y = h/2 - size[1]/2
    toplevel.geometry("%dx%d+%d+%d" % (size + (x, y)))

class ProgressBox:
    def __init__(self, message):
        self.box = tk.Toplevel(padx=5, pady=5)
        self.box.title('LiteRef: Progress')
        tk.Label(self.box, text = message, font=('Times', 14)).pack()
        center(self.box)
        config.root.update()

    def __enter__(self):
        return self
    
    def __exit__(self, type, value, traceback):
        self.box.destroy()
    
def wideYesNo(title, text):
    config.root.option_add("*Dialog.msg.wrapLength", "10i")
    res = tkMessageBox.askyesno(title, text)
    config.root.option_add("*Dialog.msg.wrapLength", "3i")
    return res

# Functions for processing online pages
def getClassElement(driver, className):
    """
    Wait for the first element of the given class to appear and return it.
    """
    wait = WebDriverWait(driver, 10)
    return wait.until(EC.visibility_of_element_located \
                      ((By.CLASS_NAME, className)))

def getIDElement(driver, id):
    """
    Wait for the first element with the given id to appear and return it.
    """
    wait = WebDriverWait(driver, 10)
    return wait.until(EC.visibility_of_element_located \
                      ((By.ID, id)))
