from utils import getIDElement, getClassElement
import re

"""
The variables and functions (note that these functions are static) in a source are:

name -- the string representation of the source.

queryAddress -- the address of the query page.

redirections -- set to True if redirections need to be followed when looking for the PDF. Otherwise, the variable does not need to be defined. 

searchPageElementName -- the name of the class of the element on the query page that needs to appear. Sources in which no particular element needs to appear do not implement this function.

def afterLinkForPDF(driver, page) -- computes the position in PAGE following the href element pointing to the page with the sought PDF or -1 if such position is not found. Returns the possibly modified PAGE and the position. Sources that do not provide PDFs do not implement this function. 

def afterLinkForBib(driver, page) -- computes the position in PAGE following the href element pointing to the page with the sought BibTeX entry or -1 if such position is not found. Returns the possibly modified PAGE and the position. Sources that do not provide BibTeX entries or in which the BibTeX entries are located on the search page itself do not implement this function.

def bibEntry(driver) -- returns the first BibTeX entry located on the current page.

def allBibLinks(page) -- returns the list of all links to BibTeX entries for the HTML page PAGE.
"""

class GoogleScholar:
    name = 'Google Scholar'
    queryAddress = 'https://scholar.google.co.il/scholar?q='
    redirections = True
    
    @staticmethod
    def afterLinkForPDF(driver, page):
        return page, page.find("[PDF]")

    @staticmethod
    def afterLinkForBib(driver, page):
        element = driver.find_element_by_class_name('gs_or_cit')
        element.click()
        element = getIDElement(driver, 'gs_cit')
        page = element.get_attribute("innerHTML")
        return page, page.find("BibTeX")

    @staticmethod
    def bibEntry(driver):
        page = driver.page_source
        begin= page.find("@")
        end = page.find("</pre>")
        return page[begin:end]
    
class SemanticScholar:
    name = 'Semantic Scholar'
    queryAddress = 'https://semanticscholar.org/search?q='
    searchPageElementName = 'paper-link'

    @staticmethod
    def afterLinkForPDF(driver, page):
        return page, page.find(".pdf")

    @staticmethod
    def bibEntry(driver):
        driver.find_element_by_class_name('paper-actions-toggle').click()
        getClassElement(driver, 'cite-button').click()
        getClassElement(driver, 'formatted-citation')
        page = driver.page_source
        end = page.find("</cite>")
        begin=page[:end].rfind("@")
        return page[begin:end]

class DBLP:
    name = 'DBLP'
    queryAddress = 'http://dblp.uni-trier.de/search?q='

    @staticmethod
    def afterLinkForBib(driver, page):
        position = page.find("BibTeX")
        add = page[position + 1:].find("BibTeX")
        if add == -1: return -1
        return page, position + add + 1

    @staticmethod
    def bibEntry(driver):
        return getClassElement(driver, 'verbatim').text

    @staticmethod
    def allBibLinks(page):
        links = []
        p = re.compile("href=\"([^\"]*)\"")
        page = page[page.find('<h2>'):] # entries before aren't papers
        while True:
            if page.rfind('BibTeX') == -1: break
            page = page[:page.rfind('BibTeX')]
            page = page[:page.rfind('href=')]
            pos = page.rfind('href="')
            res = p.search(page[pos:])
            link = res.group(1).replace('&amp;', '&')
            links.append(link.replace("rec/bibtex", "rec/bib1") + ".bib")
            page = page[:pos]
        return links
