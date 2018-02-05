import sys

ROOT_DIR = sys.argv[1]
if (ROOT_DIR[-1] != "/"): ROOT_DIR += "/"
PAPERS_DIR = ROOT_DIR + "papers/"
NOTES_DIR = ROOT_DIR + "survey/"
DROP_DIR = ROOT_DIR + "drop/"

# Implemented PDF sources: GoogleScholar, SemanticScholar.
PDF_AUTOMATED_SOURCES = ['GoogleScholar']
# Use None if you do not want a manual source.
PDF_MANUAL_SOURCE = 'GoogleScholar'

# Implemented BibTeX sources: GoogleScholar, SemanticScholar, DBLP.
BIB_AUTOMATED_SOURCES = ['DBLP']
# Use None if you do not want a manual source.
BIB_MANUAL_SOURCE = 'SemanticScholar'

BROWSER = 'chrome'
EVINCE_STARTUP_DELAY = 3

    
