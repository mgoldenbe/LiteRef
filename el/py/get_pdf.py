import config
import webbrowser
import os
import subprocess
import tkMessageBox
import pdb
import time
from utils import ProgressBox
import Tkinter as tk

from selenium import webdriver
driver = webdriver.PhantomJS()
import re
import requests

def entry2query(entry):
    """
    Compute a query based based on a bibtex entry.
    """ 
    return entry.fields['title'].replace(' ', '+')

def sourceQueryAddress(source, entry):
    """
    Get the address corresponding to the query from the give source.
    """
    return \
        {"Google Scholar": \
         'https://scholar.google.co.il/scholar?q='}[source] + \
         entry2query(entry)

def sourceSearchResultsHTML(source, entry):
    """
    Return the HTML source of the page of search results.
    """
    driver.get(sourceQueryAddress(source, entry))
    return driver.page_source

def firstLink(source, page):
    """
    Get the first PDF link from the HTML source page.
    """
    if source == "Google Scholar":
        bracketed_pdf = page.find("[PDF]")
        if bracketed_pdf == -1:
            return None
        href = page[:bracketed_pdf].rfind("href")
        p = re.compile("href=\"([^\"]*)\"")
        res = p.search(page[href:])
        return res.group(1)

def followRedirections(link):
    """
    Follow redirections beginning from the given link until a direct link to the PDF file is obtained.
    """
    while True:
        with ProgressBox('Following re-directions:\n' + link):
            ## The option verify=False is generally not safe,
            ## but it should be fine for the current sources.
            ## It avoids the error with SSL sertificates,
            ## which sometimes shows up for sites of very fine venues. 
            res = requests.get(link, allow_redirects=False, verify=False)
            if res.headers.get('Content-Type') == 'application/pdf':
                return link
            if res.headers.get('Refresh') != None:
                # https://stackoverflow.com/a/46487516/2725810
                link = res.headers['Refresh'].split("url=")[-1]
                continue
            if res.headers.get('Location') != None:
                # stackoverflow.com/a/32528675/2725810
                link = res.headers['Location']
                continue
        with ProgressBox('The file is not found.'):
            time.sleep(2)
            return None
            
def getPdfAutomated(entry, paperDir):
    """
    Get the PDF for a bibtex entry in an automated manner and put it into the given paperDir directory.
    """ 
    for source in config.PDF_AUTOMATED_SOURCES:
        try:
            # pdb.set_trace()
            with ProgressBox('Fetching search results from ' + source):
                page = sourceSearchResultsHTML(source, entry)
            with ProgressBox('Looking for link to PDF...'):
                link = firstLink(source, page)
            if link == None:
                with ProgressBox('No PDF link is found.'):
                    time.sleep(2)
                    continue
            link = followRedirections(link)
            with ProgressBox('Downloading the PDF...'):
                os.system("cd {dir}; " \
                          "wget --no-check-certificate " \
                          "-O paper.temp.pdf {link}".
                          format(dir = paperDir, link = link))
            proc = subprocess.Popen(['evince', \
                                     paperDir + "/paper.temp.pdf"])
            # Sleep needed so the message box shoud appear on top.
            # The delay needs to be long enough to see the paper.
            time.sleep(config.EVINCE_STARTUP_DELAY)
            if tkMessageBox.askyesno('LiteRef: confirm PDF',
                                     'Is this the PDF you wanted?'):
                os.system("mv " + \
                          paperDir + "/paper.temp.pdf " + \
                          paperDir + "/paper.pdf ")
                proc.terminate()
                return True
            proc.terminate()
            os.system("rm -f " + paperDir + "/paper.temp.pdf")
        except:
            tkMessageBox.showerror('LiteRef Error',
                                   "Something went wrong with the source\n" +
                                   source)
    return False

    ## I am choosing the simple regular expressions approach for now, as it works for both the pdf and the abstract.

def getPdfManual(entry):
    """
    Get the PDF for the paper, whose properties are supplied
    in the `myDict` dictionary as returned by BibParser
    """
    url = sourceQueryAddress(config.PDF_MANUAL_SOURCE, entry)
    webbrowser.get(config.BROWSER).open_new_tab(url)

