ROOT_DIR = '/home/meir/Dropbox/Parnasa/Research/LiteRef/'
PAPERS_DIR = ROOT_DIR + "papers/"
NOTES_DIR = ROOT_DIR + "survey/"
DROP_DIR = ROOT_DIR + "drop/"

PDF_AUTOMATED_SOURCES = ['https://semanticscholar.org/search?q=']
PDF_MANUAL_SOURCE = 'google.com/search?q='
BROWSER = 'chrome'
EVINCE_STARTUP_DELAY = 3

MESSAGE_WIDTH = 40 # in characters
MIN_MESSAGE_WINDOW_WIDTH = 200 # in pixels
MIN_MESSAGE_WINDOW_HEIGHT = 150 # in pixels

root = None

def isDir(s, d):
    return s[:len(d)] == d

    
