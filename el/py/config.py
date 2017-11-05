import sys

ROOT_DIR = sys.argv[1]
if (ROOT_DIR[-1] != "/"): ROOT_DIR += "/"
PAPERS_DIR = ROOT_DIR + "papers/"
NOTES_DIR = ROOT_DIR + "survey/"
DROP_DIR = ROOT_DIR + "drop/"

# Implemented PDF sources: Google Scholar, Semantic Scholar.
PDF_AUTOMATED_SOURCES = ['Semantic Scholar']
# Use None if you do not want a manual source.
PDF_MANUAL_SOURCE = 'Google Scholar'

# Implemented BibTeX sources: Google Scholar, Semantic Scholar, DBLP.
BIB_AUTOMATED_SOURCES = ['Semantic Scholar']
# Use None if you do not want a manual source.
BIB_MANUAL_SOURCE = 'Semantic Scholar'

BROWSER = 'chrome'
EVINCE_STARTUP_DELAY = 3

MESSAGE_WIDTH = 40 # in characters
MIN_MESSAGE_WINDOW_WIDTH = 200 # in pixels
MIN_MESSAGE_WINDOW_HEIGHT = 150 # in pixels

root = None

def isDir(s, d):
    return s[:len(d)] == d

    
