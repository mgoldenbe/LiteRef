import config
import urllib2
from bs4 import BeautifulSoup
import webbrowser
import os
import subprocess
import tkMessageBox
import pdb
import time
from utils import ProgressBox
import Tkinter as tk

def entry2query(entry):
    return entry.fields['title'].replace(' ', '+')

def getPdfAutomated(entry, paperDir):
    for source in config.PDF_AUTOMATED_SOURCES:
        try:
            address = source + entry2query(entry)
            with ProgressBox('Opening the url...'):
                openedUrl = urllib2.urlopen(address)
            with ProgressBox('Reading the url...'):
                page = openedUrl.read()
            with ProgressBox('Looking for link to PDF...'):    
                bs = BeautifulSoup(page, "lxml")
                link = bs.find(
                    'a', {'class':
                          'icon-button button--primary paper-link'})['href']
            with ProgressBox('Downloading the PDF...'):    
                os.system("cd {dir}; wget -O paper.pdf {link}".
                          format(dir = paperDir, link = link))
            proc = subprocess.Popen(['evince', paperDir + "/paper.pdf"])
            # Sleep needed so the message box shoud appear on top.
            # The delay needs to be long enough to see the paper.
            time.sleep(config.EVINCE_STARTUP_DELAY)
            if tkMessageBox.askyesno('LiteRef: confirm PDF',
                                     'Is this the PDF you wanted?'):
                return True
            proc.terminate()
            os.system("rm -f " + paperDir + "/paper.pdf &")
        except:
            tkMessageBox.showerror('LiteRef Error',
                                   "Something went wrong with the source\n" +
                                   source)
    return False

    ## I am choosing the simple regular expressions approach for now, as it works for both the pdf and the abstract.

def getPdfManual(entry):
    """
    Getting the pdf for the paper, whose properties are supplied
    in the `myDict` dictionary as returned by BibParser
    """
    url = config.PDF_MANUAL_SOURCE + entry2query(entry)
    webbrowser.get(config.BROWSER).open_new_tab(url)
