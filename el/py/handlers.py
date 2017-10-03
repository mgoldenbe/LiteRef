import config
from utils import *
import os
import re
from time import sleep
import subprocess
from string import ascii_lowercase
import pdb

from pybtex.database import parse_file, BibliographyData, Entry, OrderedCaseInsensitiveDict
from pybtex import errors as pybtexErrors
import tkMessageBox
import pyperclip
from get_pdf import getPdfAutomated, getPdfManual


Modes = enum('REGULAR', 'PERSIST_SKIP', 'PERSIST_CREATE') 

def saveEntry(key, entry, fileName, appendFlag = False):
    try:
        new_data = BibliographyData(entries=OrderedCaseInsensitiveDict(),
                                    preamble=[])
        new_data.entries[key] = entry
        tempFileName = config.ROOT_DIR + '.temp.bib'
        new_data.to_file(tempFileName)
        command = 'cat {temp} {arrows} {real}; rm -f {temp}'. \
                  format(temp = tempFileName, arrows = ('>>' if appendFlag else '>'), real = fileName)
        os.system(command) 
    except:
        tkMessageBox.showerror(
            'LiteRef Error',
            'Could not create the file: ' + fileName +
            '\nAbandoning the key.')

def readBib(fileName):
    return parse_file(fileName) # This is a BibliographyData object
    
def keyExists(key):
    command = "find {dir} | grep {key}/paper.bib | wc -l".format(dir=config.PAPERS_DIR, key=key)
    output = subprocess.check_output(command, shell=True)
    return int(output) > 0

## Check the word against the patters translated from
## bibtex-autokey-titleword-ignore of Emacs's bibtex library
def ignoreTitleWord(word):
    patterns = [r'^A$', r'^An$', r'^The$', r'^Eine[e]?$', r'^Der$', r'^Die$', r'^Das$', r'^[^A-Z].*$', r'^.*[^A-Za-z0-9].*$']
    for p in patterns:
      if re.search(re.compile(p), word) != None:
          return True
    return False

def abbreviateWord(word):
    minLength = 5
    res = word[0:minLength]
    vowels = "aeiou"
    for c in word[minLength:]:
        if res[-1] not in vowels: break
        res += c
    return res

def keygen(entry):
    """
    Builds LiteRef key.
    """
    try:
        myAuthors = entry.persons['author']
    except:
        return ""
    
    # Add the first author's last name
    # Remove all non-alphanumeric characters.
    # See https://stackoverflow.com/a/25183802/2725810
    res = re.sub('\W+','', myAuthors[0].last_names[0])

    # Add the first letter of the last name
    # of each of the remaining authors
    try:
        for el in myAuthors[1:]:
            res += el.last_names[0][0]
    except:
        tkMessageBox.showwarning(
            'LiteRef: Bad BibTex Entry',
            'Something is wrong with the last names in the entry.\n' +
            'The entry will be saved in drop/bad_entries.bib')
        saveEntry(entry.key, entry, config.DROP_DIR + 'bad_entries.bib', True)
        return 'BadKey'
        
    # Add year
    try:
        res += str(int(entry.fields['year']))
    except:
        tkMessageBox.showwarning(
            'LiteRef: Bad BibTex Entry',
            'Invalid year field in the entry.\n' +
            'The entry will be saved in drop/bad_entries.bib')
        saveEntry(entry.key, entry, config.DROP_DIR + 'bad_entries.bib', True)
        return 'BadKey'

    # Add a letter denoting the publication type
    # See http://www.openoffice.org/bibliographic/bibtex-defs.html
    try:
        res += {'article': 'j', 'book': 'b', 'booklet': 'bl',
                'conference': 'c', 'inbook': 'bc', 'incollection': 'bc',
                'inproceedings': 'c', 'manual': 'mn',
                'mastersthesis':'tm',
                'misc': 'm', 'phdthesis': 'tp', 'proceedings': 'p',
                'techreport': 'r', 'unpublished':'u'}[entry.type]
    except:
        tkMessageBox.showwarning(
            'LiteRef: Bad BibTex Entry',
            'Invalid publication type in the entry.\n' +
            'The entry will be saved in drop/bad_entries.bib')
        saveEntry(entry.key, entry, config.DROP_DIR + 'bad_entries.bib', True)
        return 'BadKey'
    
    # Add some title words
    nWords = 2
    nAdded = 0
    for word in re.sub(r'[.!,;?-]', ' ', entry.fields['title']).split():
        word = word.capitalize()
        if ignoreTitleWord(word): continue
        res += '-' + abbreviateWord(word)
        nAdded += 1
        if nAdded == nWords:
            break

    # Deal with the special characters
    res = re.sub(r'\{[\\\'\\"]+(?P<nn>[A-Za-z])\}', '\g<nn>', res)

    return res

def unduplicateKey(key):
    # Add a letter in the case that an entry with the key already exists
    res = key
    for c in [''] + list(ascii_lowercase):
        if not (keyExists(res + ('' if c == '' else '-') + c)):
            if c == '': return res
            return res + '-' + c
        
def duplicateCheck(key, mode, askPersist):
    if not keyExists(key): return (key, mode)
    if mode == Modes.PERSIST_SKIP: return ('BadKey', mode)
    if mode == Modes.REGULAR:
        skipFlag = not tkMessageBox.askyesno('LiteRef: Duplicate Key',
                                             'Duplicate key is detected: ' + key + '.\n' +
                                             'Would you like to create an entry in any case?')
        persistFlag = False
        if askPersist:
            persistFlag = tkMessageBox.askyesno('LiteRef: Repeat Choice',
                                                'Would you like to apply this choice for the remaining entries?')
        if persistFlag: mode = (Modes.PERSIST_SKIP if skipFlag else Modes.PERSIST_CREATE)
        if skipFlag: return ('BadKey', mode)
    return (unduplicateKey(key), mode)
    
def handleNewBib(fileName):
    clipboard = ""
    pybtexErrors.set_strict_mode(False) # do not exit on an exception
    bib_data = readBib(fileName)
    mode = Modes.REGULAR
    index = -1; count = len(bib_data.entries)
    for key in bib_data.entries:
        index += 1
        entry = bib_data.entries[key]
        newKey = keygen(entry)
        
        # '' -- no author, i.e. the venue itself
        if newKey == '' or newKey == 'BadKey':
            continue

        (newKey, mode) = duplicateCheck(newKey, mode, index < count - 1)
        if newKey == 'BadKey': continue # the user decided to not create duplicate entry

        # delete the crossref field; using a hack (access to _dict and order)
        try:
            entry.fields._dict.pop('crossref')
            entry.fields.order.remove('crossref')
        except:
            pass
            
        paperDir = config.PAPERS_DIR + newKey
        bibFileName = paperDir + "/paper.bib"
        orgFileName = paperDir + "/paper.org"
        try:
            #pdb.set_trace()
            os.system("mkdir -p " + paperDir) # create the paper directory
            saveEntry(newKey, entry, bibFileName)
            os.system("touch " + orgFileName) # create the org file
            os.system("rm -f " + fileName)     # remove the bib file in drop/
            clipboard += newKey + ','
            pyperclip.copy(clipboard[:-1])
        except:
            tkMessageBox.showerror(
            'LiteRef Error',
            'Could not create the files for the key: ' + newKey +
            '\nAbandoning the key.')
            
    #getPdf(entry)

def handleNewCsv(fileName):
    command = "mkdir {tempDir}; origDir=`pwd`;cd {tempDir}; " \
              "wget `grep \"BibTeX\" {fileName} | colrm 1 1 | sed -e 's/\", BibTeX/.bib/g' | sed -e 's/rec\/bibtex/rec\/bib1/g'`; " \
              "cd $origDir; cat {tempDir}/*.bib >> {dropDir}/mybib.bib; " \
              "rm -rf {tempDir}". \
              format(fileName = fileName,
                     tempDir = config.DROP_DIR + 'temp/',
                     dropDir = config.DROP_DIR)
    os.system(command)
    os.system("rm -f " + fileName)     

## Read request created from Emacs session.
def readRequest(fileName):
    with open(fileName, "r") as myfile:
        request = myfile.readline().split()
    return request

def requestCode(request):
    return request[0]

def requestKey(request):
    return request[1]

## Handle request created from Emacs session.
def handleRequest(fileName):
    request = readRequest(fileName)
    try:
        if requestCode(request) == "getPdf":
            key = requestKey(request)
            paperDir = config.PAPERS_DIR + key
            bibFileName = paperDir + "/paper.bib"
            bibData = readBib(bibFileName)
            entry = bibData.entries[key]
            if getPdfAutomated(entry, paperDir):
                os.system("rm -f " + fileName)
            else:
                getPdfManual(entry)
    except:
        tkMessageBox.showerror('LiteRef Error', "Bad request: " + fileName)
        
def handleNewPdf(fileName):
    # Currently simply decide that it is the last request.
    #pdb.set_trace()
    command = "ls -t {dir}/*.rqt | head -n 1".format(dir=config.DROP_DIR)
    reqFile = subprocess.check_output(command, shell=True)
    if reqFile == "":
        tkMessageBox.showerror('LiteRef Error', 'No request, abandoning the new PDF.')
        return
    if reqFile[-1] == '\n': reqFile = reqFile[:-1]
    request = readRequest(reqFile)
    paperDir = config.PAPERS_DIR + requestKey(request)
    newPdfFile = paperDir + "/paper.pdf"
    command = "mv {old} {newPdfFile}". \
               format(old = fileName, \
                      newPdfFile = newPdfFile) 
    os.system(command)
    os.system("rm -f " + reqFile)

def handleNewFile(fileName):
    # Make sure it's not a short-lived temporary file, e.g. of sed
    sleep(0.1)
    if not os.path.isfile(fileName):
        return      
    name, ext = os.path.splitext(fileName)
    if ext == '.crdownload': return
    try:
        handler = {'.bib': handleNewBib,
                   '.csv':handleNewCsv,
                   '.pdf':handleNewPdf,
                   '.rqt':handleRequest}[ext]
    except:
        handler = None
        tkMessageBox.showerror('LiteRef Error', 'Unknown file extension')

    if handler != None:
        sleep(0.1) # Make sure that the file is fully written
        handler(fileName)


