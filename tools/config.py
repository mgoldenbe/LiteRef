ROOT_DIR = '/home/meir/Dropbox/Parnasa/Research/LiteRef/'
BIB_DIR = ROOT_DIR + "bib/"
PDF_DIR = ROOT_DIR + "pdf/"
BROWSER = 'chrome'
PDF_SOURCE = 'scholar.google.com/scholar?q='

def isDir(s, d):
    return s[:len(d)] == d

def isBibDir(s):
    return isDir(s, BIB_DIR)

def isPdfDir(s):
    return isDir(s, PDF_DIR)
    
