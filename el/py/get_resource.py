# get_resource.py --- search for paper PDF or BibTeX.

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
This module handles searching for PDF of a paper in the online sources given by the value of the variables :py:mod:`config.PDF_AUTOMATED_SOURCES` and :py:mod:`config.BIB_AUTOMATED_SOURCES`. 
"""

# Code:

import config
import webbrowser
import os
import subprocess
import tkMessageBox
import pdb
import time
from utils import ProgressBox, dirFiles, wideYesNo
import Tkinter as tk

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import re
import requests

from online_sources import *

# driver = webdriver.Firefox()

chrome_options = Options()  
chrome_options.add_argument("--headless")

#: The web driver for ``selenium``.
driver = webdriver.Chrome(chrome_options=chrome_options)

# driver = webdriver.PhantomJS(service_args=['--ignore-ssl-errors=true',
#                                             '--ssl-protocol=any'])
driver.set_window_size(1124, 850) # Avoid the error of the element not being displayed

def confirmPDF(fileName):
    """
    Open the given PDF and ask the user whether is the one he wanted.

    :param fileName: The name of the PDF file.

    :return: The yes/no answer of the user.
    """
    proc = subprocess.Popen(['evince', fileName])
    # Sleep needed so the message box shoud appear on top.
    # The delay needs to be long enough to see the paper.
    time.sleep(config.EVINCE_STARTUP_DELAY)
    answer = tkMessageBox.askyesno('LiteRef: confirm PDF',
                                   'Is this the required PDF?')
    config.root.update()
    proc.terminate()
    return answer

def pdfExists(entry, paperDir):
    """
    Check whether the needed PDF file is among the PDF files in the
    drop folder.

    :param entry: The paper's BibTeX entry. This argument is provided \
    for consistency of interface, but is not actually used.

    :param paperDir: The folder containing the files associated with \
    the paper.

    :return: ``True`` if the paper PDF exists and ``False`` otherwise.
    """
    
    for fileName in dirFiles(config.DROP_DIR, '*.pdf'):
        if confirmPDF(fileName):
            os.rename(fileName, paperDir + "/paper.pdf")
            return True
    return False

def entry2query(entry):
    """
    Compute a search query based on the given BibTeX entry or search
    string. If a BibTeX entry is supplied, then extract only the
    title. In both cases, replace spaces by pluses.

    :param entry: BibTeX entry or search string.

    :return: The computed search query.
    """
    
    if type(entry) == type(""):
        entryStr = entry
    else:
        entryStr = entry.fields['title']
    return entryStr.replace(' ', '+')

def sourceQueryAddress(source, entry):
    """
    Compute the web address containing the query for the given search entry and source.

    :param source: The online source.

    :param entry: BibTeX entry or search string.

    :return: The computed web address.
    """
    
    return source.queryAddress + entry2query(entry)

def sourceSearchResultsHTML(source, entry):
    """
    Compute the string HTML source of the page of search results.

    :param source: The online source.

    :param entry: BibTeX entry or search string.

    :return: The contents of the HTML page containing the search \
    results.
    """
    driver.get(sourceQueryAddress(source, entry))
    try:
        getClassElement(driver, source.searchPageElementName)
    except: pass
    return driver.page_source

def firstLink(source, searchType, page):
    """
    Get the first link to the resource corresponding to the given
    search type (PDF of BibTeX) from the given HTML source page.

    :param source: The online source.

    :param searchType: The search type ("pdf" or "bib").

    :return: The first link to the resource being searched \
    for. Returns an empty string if the ``afterLinkForPDF`` (or \
    ``afterLinkForBib``) method of the source (see the documentation \
    of the :py:mod:`online_sources` module) raises an \
    exception. Returns ``None`` If the link is not found on the page.
    """
    # pdb.set_trace()
    try:
        if searchType == "pdf":
            page, position = source.afterLinkForPDF(driver, page)
        if searchType == "bib":
            page, position = source.afterLinkForBib(driver, page)
    except:
        return "" # same page
    if position == -1: return None
    href = page[:position].rfind("href")
    p = re.compile("href=\"([^\"]*)\"")
    res = p.search(page[href:])
    return res.group(1).replace('&amp;', '&')

def followRedirections(source, searchType, link):
    """
    Follow ``http`` re-directions beginning from the given *link*
    until a direct link to the searched for resource is
    obtained. Different sources and search types are handled
    appropriately.

    :param source: The online source.

    :param searchType: The search type ("pdf" or "bib").

    :return: The direct link to the searched for resource. 
    """
    if searchType == "bib": return link
    try:
        if source.redirections != True: return link
    except: return link

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

def candidatePDFFeedback(link, paperDir):
    """
    Download the found candidate PDF and ask the user whether it is
    the one he desired.

    :param link: The direct link to the PDF.

    :paperDir: The folder of the paper whose PDF is requested.

    :return: The reply of the user (yes/no).
    """
    # pdb.set_trace()
    with ProgressBox('Downloading the PDF...'):
        os.system("cd {dir}; " \
                  "wget -q --no-check-certificate " \
                  "-O paper.temp.pdf '{link}'".
                  format(dir = paperDir, link = link))
        return confirmPDF(paperDir + "/paper.temp.pdf")

def processCandidatePDF(link, paperDir):
    """
    Download the found candidate PDF and ask the user whether it is
    the one he desired. If the user confirms, then add the PDF into
    the papers database.

    :param link: The direct link to the PDF.

    :paperDir: The folder of the paper whose PDF is requested.

    :return: The reply of the user to the confirmation request \
    (yes/no).
    """
    answer = candidatePDFFeedback(link, paperDir)
    if answer:
        os.rename(paperDir + "/paper.temp.pdf", paperDir + "/paper.pdf")
    os.system("rm -f " + paperDir + "/paper.temp.pdf")
    return answer
    
def candidateBibFeedback(link, source):
    """
    Download the found candidate BibTeX entry and ask the user whether
    it is the one he desired.

    :param link: The direct link to the BibTeX entry.

    :source: The online source.

    :return: The reply of the user (yes/no).
    """
    if link != "": driver.get(link)
    entry = source.bibEntry(driver)

    return (wideYesNo(
        'LiteRef: confirm BibTex entry',
        "Is the follwing the BibTex entry you wanted?\n" + \
        entry), entry)

def processCandidateBib(link, source):
    """
    Download the found candidate PDF and ask the user whether it is
    the one he desired. If the user confirms, then add the PDF into
    the papers database.

    :param link: The direct link to the BibTeX entry.

    :source: The online source.

    :return: The reply of the user to the confirmation request \
    (yes/no).
    """
    answer, entry = candidateBibFeedback(link, source)
    if answer:
        fileName = config.DROP_DIR + "/temp.bib"
        os.system("rm -f " + fileName)
        with open(fileName, "w") as myFile:
            myFile.write(entry)
    return answer

def getResourceAutomated(entry, searchType, paperDir):
    """
    Search for the requested resource in the online sources given by
    the value of :py:mod:`config.PDF_AUTOMATED_SOURCES` or
    :py:mod:`config.BIB_AUTOMATED_SOURCES` depending on the search
    type.

    :param entry: The paper's BibTeX entry. This argument is provided \
    for consistency of interface, but is not actually used.
    
    :param searchType: The search type ("pdf" or "bib").

    :param paperDir: The folder containing the files associated with \
    the paper.

    :return: ``True`` if the user confirmed the found resource and \
    ``False`` otherwise.
    """
    sources = config.PDF_AUTOMATED_SOURCES
    if searchType == "bib":
        sources = config.BIB_AUTOMATED_SOURCES
    for source in [globals()[s] for s in sources]:
        try:
            # pdb.set_trace()
            resourceString = "PDF"
            if searchType == "bib": resourceString = "BibTex entry"
            with ProgressBox('Fetching search results from ' + source.name):
                page = sourceSearchResultsHTML(source, entry)
                # driver.save_screenshot("myshot.png")
            with ProgressBox("Looking for link to {resource}..." \
                             .format(resource = resourceString)):
                link = firstLink(source, searchType, page)
            if link == None:
                with ProgressBox("No {resource} link is found." \
                                 .format(resource = resourceString)):
                    time.sleep(2)
                    continue
            link = followRedirections(source, searchType, link)

            if searchType == "pdf":
                answer = processCandidatePDF(link, paperDir)
            if searchType == "bib":
                answer = processCandidateBib(link, source)
            
            if answer: return True
        except:
            tkMessageBox.showerror(
                'LiteRef Error',
                "Something went wrong with the source:\n" + \
                source.name + "\n" + \
                "It is possible that there are no resources "
                "matching the query.")
            config.root.update()
    return False

def getResourceManual(entry, searchType):
    """
    Obtain the resource for the entry in a manual manner from the
    online source given by the value of
    :py:mod:`config.PDF_MANUAL_SOURCE` or
    :py:mod:`config.BIB_MANUAL_SOURCE` depending on the search type.

    :param entry: The paper's BibTeX entry. This argument is provided \
    for consistency of interface, but is not actually used.
    
    :param searchType: The search type ("pdf" or "bib").
    """
    # pdb.set_trace()
    source = config.PDF_MANUAL_SOURCE
    if searchType == "bib":
        source = config.BIB_MANUAL_SOURCE
    source = globals()[source]
    if source == None: return
    url = sourceQueryAddress(source, entry)
    webbrowser.get(config.BROWSER).open_new_tab(url)

