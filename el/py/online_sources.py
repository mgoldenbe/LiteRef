# online-sources.py --- classes representing online sources for searches.

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
This module contains classes representing online sources for
searches. These classes implement standard interfaces, so that the
user can implement additional sources. To avoid repeating the
documentation, the variables and the functions are documented once in
a dummy class :any:`DummySource`.
"""

# Code:

from utils import getIDElement, getClassElement
import re

class DummySource:
    """
    The dummy source class used to document the variables and the
    methods that appear in the resources. Note that some of the
    methods are applicable only for sources containing BibTeX/PDF
    resources. Such cases are clearly indicated.
    """
    
    name = 'Dummy'
    """The name of the source that will appear in the dialogue windows."""
    
    queryAddress = 'https://dummy.com/search?q='
    """The prefix of the address for search requests in the source."""
    
    redirections = True
    """``True`` if the source uses ``http`` redirections, which need
    to be followed when looking for the paper PDF, and ``False``
    otherwise. This variable is only applicable to sources providing
    links to PDFs stored on third-party servers."""

    searchPageElementName = 'link-element'
    """The name of the HTML class of the element on the query page
     whose appearance signifies that the results of the search can be
     read. Sources in which no particular element needs to appear do
     not define this variable."""
     
    @staticmethod
    def afterLinkForPDF(driver, page):
        """
        Compute the position in the given HTML page following the
        ``href`` element containing the direct link to the page with
        the sought PDF. Sources that do not provide PDFs do not
        implement this function.

        :param driver: The web driver for ``selenium``. See \
        :py:mod:`get_resource.driver`.

        :param page: The HTML page returned by the search.

        :return: A tuple of two elements. The first element is the \
        possibly modified HTML page. The second element is the start \
        position of the direct link to the page with the sought \
        PDF. If such position is not found, then this element is -1.  
        """
        pass

    @staticmethod
    def afterLinkForBib(driver, page):
        """
        Compute the position in the given HTML page following the
        ``href`` element containing the direct link to the page with
        the sought BibTeX entry. Sources that do not provide BibTeX
        entries or in which the BibTeX entries appear on the HTML page
        containing the search results itself do not implement this
        function.

        :param driver: The web driver for ``selenium``. See \
        :py:mod:`get_resource.driver`.

        :param page: The HTML page returned by the search.

        :return: A tuple of two elements. The first element is the \
        possibly modified HTML page. The second element is the start \
        position of the direct link to the page with the sought \
        BibTeX entry. If such position is not found, then this element \
        is -1.
        """
        pass

    @staticmethod
    def bibEntry(driver):
        """
        Return the first BibTeX entry located on the current page of
        the given web driver. Sources that do not provide BibTeX
        entries do not implement this function.

        :param driver: The web driver for ``selenium``. See \
        :py:mod:`get_resource.driver`.

        :return: The first BibTeX entry located on the current page.
        """
        pass

    def allBibLinks(page):
        """
        Compute the list of all links to BibTeX entries for the given HTML page. This method is used by :py:mod:`handlers.handleNewHTML`.
        :param page: The HTML page containing the BibTeX entries.

        :return: The list of links to BibTeX entries.
        """
        pass

class GoogleScholar:
    """The Google Scholar."""
    
    name = 'Google Scholar'
    """See the documentation of :attr:`DummySource.name`."""
    
    queryAddress = 'https://scholar.google.co.il/scholar?q='
    """See the documentation of :attr:`DummySource.queryAddress`."""
    
    redirections = True
    """See the documentation of :attr:`DummySource.redirections`."""
    
    @staticmethod
    def afterLinkForPDF(driver, page):
        """See the documentation of :func:`DummySource.afterLinkForPDF`."""
        return page, page.find("[PDF]")

    @staticmethod
    def afterLinkForBib(driver, page):
        """See the documentation of :func:`DummySource.afterLinkForBib`."""
        element = driver.find_element_by_class_name('gs_or_cit')
        element.click()
        element = getIDElement(driver, 'gs_cit')
        page = element.get_attribute("innerHTML")
        return page, page.find("BibTeX")

    @staticmethod
    def bibEntry(driver):
        """See the documentation of :func:`DummySource.bibEntry`."""
        page = driver.page_source
        begin= page.find("@")
        end = page.find("</pre>")
        return page[begin:end]

class SemanticScholar:
    "The Semantic Scholar."
    
    name = 'Semantic Scholar'
    """See the documentation of :attr:`DummySource.name`."""
    queryAddress = 'https://semanticscholar.org/search?q='
    """See the documentation of :attr:`DummySource.queryAddress`."""
    
    searchPageElementName = 'paper-link'
    """See the documentation of :attr:`DummySource.searchPageElementName`."""

    @staticmethod
    def afterLinkForPDF(driver, page):
        """See the documentation of :func:`DummySource.afterLinkForPDF`."""
        position = page.find(".pdf")
        return page, position + 1 + page[position + 1:].find(".pdf")

    @staticmethod
    def bibEntry(driver):
        """See the documentation of :func:`DummySource.bibEntry`."""
        #driver.find_element_by_class_name('paper-actions').click()
        getClassElement(driver, 'cite-button').click()
        getClassElement(driver, 'formatted-citation')
        page = driver.page_source
        end = page.find("</cite>")
        begin=page[:end].rfind("@")
        return page[begin:end]

class DBLP:
    "The DBLP."
    
    name = 'DBLP'
    """See the documentation of :attr:`DummySource.name`."""
    
    queryAddress = 'http://dblp.uni-trier.de/search?q='
    """See the documentation of :attr:`DummySource.queryAddress`."""

    @staticmethod
    def afterLinkForBib(driver, page):
        """See the documentation of :func:`DummySource.afterLinkForBib`."""
        position = page.find("BibTeX")
        add = page[position + 1:].find("BibTeX")
        if add == -1: return -1
        return page, position + add + 1

    @staticmethod
    def bibEntry(driver):
        """See the documentation of :func:`DummySource.bibEntry`."""
        return getClassElement(driver, 'verbatim').text

    @staticmethod
    def allBibLinks(page):
        """See the documentation of :func:`DummySource.allBibLinks`."""
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
