import config
import os
from time import sleep
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
    command = "find {dir} | grep {key}/paper.bib | wc -l".format(dir=config.PAPERS_DIR, key=key)
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
    Compute and replace the key, return the new key.
    """
    oldKey = entry['ID'].replace('/', '\\/')
    newKey = key(entry)
    command = "sed -i 's/{old}/{new}/g' {fileName}". \
              format(old = oldKey, new = newKey, fileName = fileName)
    os.system(command)
    return newKey
        
def handleNewBib(fileName):
    entry = readBib(fileName)
    newKey = replaceKey(fileName, entry)
    paperDir = config.PAPERS_DIR + newKey
    os.system("mkdir -p " + paperDir)
    command = "mv {old} {newBib}; touch {newOrg}". \
               format(old = fileName, \
                      newBib = paperDir + "/paper.bib", \
                      newOrg = paperDir + "/paper.org") 
    os.system(command)
    getPdf(entry)

def handleNewPdf(fileName):
    command = "ls -t {dir}/*/*.bib | head -n 1".format(dir=config.PAPERS_DIR, key=key)
    bibFile = subprocess.check_output(command, shell=True)
    paperDir = os.path.dirname(bibFile)
    command = "mv {old} {newPdf}". \
               format(old = fileName, \
                      newPdf = paperDir + "/paper.pdf") 
    os.system(command)
    return

def handleNewFile(fileName):
    # Make sure it's not a short-lived temporary file, e.g. of sed
    sleep(0.1)
    if not os.path.isfile(fileName):
        return      
    name, ext = os.path.splitext(fileName)
    if ext == '.crdownload': return
    if ext != '.bib' and ext != '.pdf':
        print "LiteRef Error: the new file " + fileName + \
            " is neither a .bib nor a .pdf file."
        return
    sleep(0.1) # Make sure that the file is fully written
    if ext == '.bib':
        handleNewBib(fileName);
    if ext == '.pdf':
        handleNewPdf(fileName);
