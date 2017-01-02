import config
import os
import bibtexparser
from bibtexparser.bparser import BibTexParser
from bibtexparser.customization import *
import subprocess
from get_pdf import getPdf
from string import ascii_lowercase

# https://bibtexparser.readthedocs.io/en/v0.6.2/tutorial.html#parsing-the-file-into-a-bibliographic-database-object
def _customizations(record):
    record = type(record)
    record = author(record)
    return record

def readBib(fileName):
    with open(fileName) as bibtex_file:
        parser = BibTexParser()
        parser.customization = _customizations
        bib_database = bibtexparser.load(bibtex_file, parser=parser)
    return bib_database.entries[0]

def authors(entry):
    res = []
    for el in entry['author']:
        res += [el.split(',')[0]]
    return res

def keyExists(key):
    command = "find {dir} -name {key}.bib | wc -l".format(dir=config.BIB_DIR, key=key)
    output = subprocess.check_output(command, shell=True)
    return int(output) > 0

def key(entry):
    """
    Builds LiteRef key.
    """
    myAuthors = authors(entry)
    res = myAuthors[0] 
    for el in myAuthors[1:]:
        res += el[0]
    res += str(entry['year'])
    for c in [''] + list(ascii_lowercase):
        if not keyExists(res + c): return res + c

def replaceKey(fileName, entry):
    """
    Replace the key and move the bib file
    """
    oldKey = entry['ID'].replace('/', '\\/')
    myKey = key(entry)
    command = "sed -i 's/{old}/{new}/g' {fileName};". \
              format(old = oldKey, new = myKey, fileName = fileName)
    command += "mv {old} {new}". \
               format(old = fileName, \
                      new = os.path.dirname(fileName) + "/" + myKey + ".bib") 
    os.system(command)
        
def handleBibCreated(fileName):
    name, ext = os.path.splitext(fileName)
    if ext != '.bib':
        if ext == '.crdownload': return
        print "LiteRef Error: the new file " + fileName + " is not a .bib file"
        return
    entry = readBib(fileName)
    replaceKey(fileName, entry)
    getPdf(entry)

