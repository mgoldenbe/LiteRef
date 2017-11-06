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
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import re
import requests

# driver = webdriver.Firefox()

driver = webdriver.PhantomJS(service_args=['--ignore-ssl-errors=true',
                                            '--ssl-protocol=any'])
driver.set_window_size(1124, 850) # Avoid the error of the element not being displayed

def confirmPDF(fileName):
    """
    Open the given PDF and ask the user whether is the one he wanted.
    """
    proc = subprocess.Popen(['evince', fileName])
    # Sleep needed so the message box shoud appear on top.
    # The delay needs to be long enough to see the paper.
    time.sleep(config.EVINCE_STARTUP_DELAY)
    answer = tkMessageBox.askyesno('LiteRef: confirm PDF',
                                   'Is this the required PDF?')
    proc.terminate()
    return answer

def pdfExists(entry, paperDir):
    """
    Check whether the needed pdf file is among the pdf files in the drop folder.
    """
    for fileName in dirFiles(config.DROP_DIR, '*.pdf'):
        if confirmPDF(fileName):
            os.rename(fileName, paperDir + "/paper.pdf")
            return True
    return False

def getClassElement(className):
    """
    Wait for the first element of the given class to appear and return it.
    """
    wait = WebDriverWait(driver, 10)
    return wait.until(EC.visibility_of_element_located \
                      ((By.CLASS_NAME, className)))

def entry2query(entry):
    """
    Compute a query based based on bibtex entry or search string.
    """
    if type(entry) == type(""):
        entryStr = entry
    else:
        entryStr = entry.fields['title']
    return entryStr.replace(' ', '+')

def sourceQueryAddress(source, entry):
    """
    Get the address corresponding to the query from the give source.
    """
    return \
        {"Google Scholar": \
         'https://scholar.google.co.il/scholar?q=',
         "Semantic Scholar": \
         'https://semanticscholar.org/search?q=',
         "DBLP": \
         'http://dblp.uni-trier.de/search?q='
        }[source] + \
         entry2query(entry)

def sourceSearchResultsHTML(source, entry):
    """
    Return the HTML source of the page of search results.
    """
    driver.get(sourceQueryAddress(source, entry))
    if source in ["Semantic Scholar"]:
        className = {"Semantic Scholar": 'paper-link'}[source]
        getClassElement(className)
    return driver.page_source

def firstLink(source, searchType, page):
    """
    Get the first link to the resource corresponding to searchType
    from the HTML source page. If the link is just the same page, 
    return an empty string.
    """
    position = -1
    if searchType == "pdf":
        if source == "Google Scholar":
            position = page.find("[PDF]")
        if source == "Semantic Scholar":
            position = page.find(".pdf")
    if searchType == "bib":
        if source == "Google Scholar":
            element = driver.find_element_by_class_name('gs_or_cit')
            element.click()
            wait = WebDriverWait(driver, 10)
            element = wait.until(
                EC.visibility_of_element_located((By.ID, 'gs_cit')))
            page = element.get_attribute("innerHTML")
            position = page.find("BibTeX")
        if source == "Semantic Scholar":
            return "" # same page
        if source == "DBLP":
            position = page.find("BibTeX")
            position += page[position + 1:].find("BibTeX") + 1
    if position == -1:
        return None
    href = page[:position].rfind("href")
    p = re.compile("href=\"([^\"]*)\"")
    res = p.search(page[href:])
    return res.group(1).replace('&amp;', '&')

def followRedirections(source, searchType, link):
    """
    Depending on source and searchType, follow redirections beginning 
    from the given link until a direct link to the resrouce is obtained.
    """
    if searchType == "bib": return link
    if source != "Google Scholar": return link
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
    Download the candidate PDF and ask the user whether it is the one.
    """
    with ProgressBox('Downloading the PDF...'):
        os.system("cd {dir}; " \
                  "wget --no-check-certificate " \
                  "-O paper.temp.pdf {link}".
                  format(dir = paperDir, link = link))
        return confirmPDF(paperDir + "/paper.temp.pdf")

def processCandidatePDF(link, paperDir):
    """
    Process the candidate PDF located at link.
    """
    answer = candidatePDFFeedback(link, paperDir)
    if answer:
        os.rename(paperDir + "/paper.temp.pdf", paperDir + "/paper.pdf")
    os.system("rm -f " + paperDir + "/paper.temp.pdf")
    return answer
    
def candidateBibFeedback(link, source):
    """
    Get the candidate BibTeX entry and ask the user whether it is the one.
    """
    if link != "":
        driver.get(link)
    if source == "Google Scholar":
        entry = driver.page_source
    if source == "Semantic Scholar":
        driver.find_element_by_class_name('paper-actions-toggle').click()
        getClassElement('cite-button').click()
        getClassElement('formatted-citation')
        page = driver.page_source
        end = page.find("</cite>")
        begin=page[:end].rfind("@")
        entry = page[begin:end]
    if source == "DBLP":
        entry = getClassElement('verbatim').text
    return (wideYesNo(
        'LiteRef: confirm BibTex entry',
        "Is the follwing the BibTex entry you wanted?\n" + \
        entry), entry)

def processCandidateBib(link, source):
    """
    Process the candidate PDF located at link.
    """
    answer, entry = candidateBibFeedback(link, source)
    if answer:
        with open(config.DROP_DIR + "temp.bib", "w") as myFile:
            myFile.write(entry)
    return answer

def getResourceAutomated(entry, searchType, paperDir):
    """
    Obtain the resource for the entry in an automated manner.
    """
    sources = config.PDF_AUTOMATED_SOURCES
    if searchType == "bib":
        sources = config.BIB_AUTOMATED_SOURCES
    for source in sources:
        try:
            # pdb.set_trace()
            resourceString = "PDF"
            if searchType == "bib": resourceString = "BibTex entry"
            with ProgressBox('Fetching search results from ' + source):
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
                source + "\n" + \
                "It is possible that there are no resources "
                "matching the query.")
    return False

def getResourceManual(entry, searchType):
    """
    Obtain the resource for the entry in a manual manner.
    """
    # pdb.set_trace()
    source = config.PDF_MANUAL_SOURCE
    if searchType == "bib":
        source = config.BIB_MANUAL_SOURCE
    if source == None: return
    url = sourceQueryAddress(source, entry)
    webbrowser.get(config.BROWSER).open_new_tab(url)

