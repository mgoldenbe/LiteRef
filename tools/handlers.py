import os
import bibtexparser
from get_pdf import getPdf

def handleBibCreated(fileName):
    name, ext = os.path.splitext(fileName)
    if ext != '.bib':
        if ext == '.crdownload': return
        print "LiteRef Error: the new file " + fileName + " is not a .bib file"
        return
    print "Analyzing " + fileName
    with open(fileName) as bibtex_file:
        bib_database = bibtexparser.load(bibtex_file)
    getPdf(bib_database.entries[0])

