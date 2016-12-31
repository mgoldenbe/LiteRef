from config import BROWSER
from config import PDF_SOURCE
import webbrowser

def getPdf(myDict):
    """
    Getting the pdf for the paper, whose properties are supplied
    in the `myDict` dictionary as returned by BibParser
    """
    url = PDF_SOURCE + myDict['title']
    webbrowser.get(BROWSER).open_new_tab(url)
