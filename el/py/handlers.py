# handlers.py --- the handler functions for the events in the drop
# folder.

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
This module contains functions for handling the events in the drop
folder. These events concern appearance of new files as a result of
manual import, automated search in the online sourrces or a request
created by the command layer.
"""

# Code:

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
#import pyperclip
from get_resource import getResourceAutomated, getResourceManual, pdfExists

from online_sources import *
from flufl.enum import Enum

class Modes(Enum):
    """
    The possible modes for duplicated detection.
    """
    
    REGULAR = 0
    """
    Inform the user that the key already exists and ask him what he would like to do.
    """

    PERSIST_SKIP = 1
    """Skip all duplicate keys."""

    PERSIST_CREATE = 2
    """
    Whenever a duplicate key is detected, add a letter to the key to
    make it unique and add an entry to the papers database.
    """

neededPdfs = [] # keys for which pdfs are being awaited

def saveEntry(key, entry, fileName, appendFlag = False):
    """
    Add a found BibTeX entry into a file with the given.

    :param key: The BibTeX key for the new entry.

    :param entry: The new BibTeX entry to be added.

    :param fileName: The name of the file into which the entry is to \
    be put.

    :param appendFlag: The entry should be appended to the file if the \
    value is ``True`` and replace all the existing entries in the file \
    otherwise.
    """
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
    """
    Read the BibTeX entry contained in the given file.
    
    :param fileName: The name of the file.

    :return: The BibTeX entry.
    """
    
    return parse_file(fileName) # This is a BibliographyData object
    
def keyExists(key):
    """
    Determine whether a BibTeX entry with the given key exists in the
    papers database.

    :param key: The BibTeX key.

    :return: ``True`` if the entry exists and ``False`` otherwise.
    """
    command = "find {dir} | grep {key}/paper.bib | wc -l".format(dir=config.PAPERS_DIR, key=key)
    output = subprocess.check_output(command, shell=True)
    return int(output) > 0

def ignoreTitleWord(word):
    """
    Check whether the given word does not carry meaning, e.g. is an
    article in any language. This is done by matching against each of
    the patters given by the value of the variable
    ``bibtex-autokey-titleword-ignore`` from the ``bibtex`` package of
    ``Emacs``.

    :param word: The word to be checked.

    :return: ``False`` if the word carries meaning and ``True`` \
    otherwise.
    """
    patterns = [r'^A$', r'^An$', r'^The$', r'^Eine[e]?$', r'^Der$', r'^Die$', r'^Das$', r'^[^A-Z].*$', r'^.*[^A-Za-z0-9].*$']
    for p in patterns:
      if re.search(re.compile(p), word) != None:
          return True
    return False

def abbreviateWord(word):
    """
    Abbreviate the given word.

    :param word: The word to be abbreviated.

    :return: The abbreviated word.
    """    
    minLength = 5
    res = word[0:minLength]
    vowels = "aeiou"
    for c in word[minLength:]:
        if res[-1] not in vowels: break
        res += c
    return res

def keygen(entry):
    """
    Generates a unique BibTeX key for the given entry.

    :param entry: The BibTeX entry.

    :return: The generated BibTeX key.
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
            res += re.sub('\W+','', el.last_names[0])[0]
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
    """
    Add a letter to the given BibTeX key in the case that an entry with this key already exists.

    :param key: The BibTeX key to be checked for duplicates.

    :return: The unique key based on the given one.
    """
    res = key
    for c in [''] + list(ascii_lowercase):
        if not (keyExists(res + ('' if c == '' else '-') + c)):
            if c == '': return res
            return res + '-' + c
        
def duplicateCheck(key, mode, askPersist):
    """
    Check whether an entry with the given BibTeX key exists in the
    papers database.

    :param key: The BibTeX key to be checked for duplicates.

    :param mode: One of the modes listed by the :any:`Modes` variable.

    :param askPersist: if ``True``, ask whether the current choice of \
    the user should be assumed for all subsequent duplicates.

    :return: A tuple consisting of the added key and the mode. If the \
    key is not added, then first member of the returned tuple is \
    'BadKey'.
    """
    if not keyExists(key): return (key, mode)
    if mode == Modes.PERSIST_SKIP: return ('BadKey', mode)
    if mode == Modes.REGULAR:
        skipFlag = not tkMessageBox.askyesno('LiteRef: Duplicate Key',
                                             'Duplicate key is detected: ' + key + '.\n' +
                                             'Would you like to create an entry in any case?')
        config.root.update()
        persistFlag = False
        if askPersist:
            persistFlag = tkMessageBox.askyesno('LiteRef: Repeat Choice',
                                                'Would you like to apply this choice for the remaining entries?')
            config.root.update()
        if persistFlag: mode = (Modes.PERSIST_SKIP if skipFlag else Modes.PERSIST_CREATE)
        if skipFlag: return ('BadKey', mode)
    return (unduplicateKey(key), mode)
    
def handleNewBib(fileName):
    """
    Handle a BibTeX file that has just appeared in the drop folder.

    :param fileName: The name of the new BibTeX file.
    """
    
    clipboard = ""
    pybtexErrors.set_strict_mode(False) # do not exit on an exception
    bib_data = readBib(fileName)
    mode = Modes.REGULAR
    index = -1; count = len(bib_data.entries)
    
    # Doing it in two separate loops,
    # so all entries would get the same time stamp.
    newEntries = {}
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
        newEntries[newKey] = entry

    for newKey in newEntries:
        entry = newEntries[newKey]
        paperDir = config.PAPERS_DIR + newKey
        bibFileName = paperDir + "/paper.bib"
        orgFileName = paperDir + "/paper.org"
        try:
            #pdb.set_trace()
            os.system("mkdir -p " + paperDir) # create the paper directory
            saveEntry(newKey, entry, bibFileName)
            os.system("touch " + orgFileName) # create the org file
            clipboard += newKey + ','
            #pyperclip.copy(clipboard[:-1])
            os.system("echo -n {s} | xsel -bi".format(s=clipboard[:-1]))
        except:
            tkMessageBox.showerror(
            'LiteRef Error',
            'Could not create the files for the key: ' + newKey +
            '\nAbandoning the key.')
            config.root.update()
    os.system("mv {fileName} {archive}".format(fileName=fileName, archive=config.DROP_DIR + ".last.bib"))
            
    #getPdf(entry)

def handleNewHTML(fileName):
    """
    Handle an HTML file that has just appeared in the drop folder. The
    HTML file is treated as a request for bulk downloading of BibTeX
    entries linked from the HTML.

    :param fileName: The name of the new HTML file.
    """
    page = open(fileName, 'r').read()
    links = DBLP.allBibLinks(page)
    
    tempDir = config.DROP_DIR + 'temp/'
    os.system("mkdir -p " + tempDir)
    
    listFileName = tempDir + "links.txt"
    listFile = open(listFileName, 'w')
    listFile.write("\n".join(links))
    listFile.close()

    logFileName = tempDir + "wget.log"
    command = "origDir=`pwd`; cd {tempDir}; " \
              "wget --no-check-certificate " \
              "-i {listFileName} -o {logFileName}; " \
              "cd $origDir; " \
              "cat {tempDir}/*.bib >> {dropDir}/mybib.bib; " \
              "rm -rf \"{tempDir}\"; rm -f \"{fileName}\"". \
              format(fileName = fileName,
                     listFileName = listFileName,
                     logFileName = logFileName,
                     tempDir = tempDir,
                     dropDir = config.DROP_DIR)
    #print command
    os.system(command)
    tkMessageBox.showinfo('LiteRef Info',
                          'All entries have been fetched.')
    config.root.update()

def readRequest(fileName):
    """
    Read the request created by the command layer. The request is
    currently assumed to be contained in the first line of the file.

    :param fileName: The name of the file containing the request.

    :return: The list containing the first word of the request and the \
    rest of the request.
    """
    with open(fileName, "r") as myfile:
        request = myfile.readline().split()
    return [request[0], ' '.join(request[1:])]

def requestCode(request):
    """
    Return the request type.

    :param request: The request.

    :return: The request type.
    """
    return request[0]

def requestKey(request):
    """
    Return the contents of the request, which is either the BibTeX key
    of the paper for which a PDF is needed or the search string for
    searching for a BibTeX entry.

    :param request: The request.

    :return: The contents of the request.
    """
    return request[1]

def handleRequest(fileName):
    """
    Handle request created by the command layer.

    :param fileName: The name of the file containing the request.
    """
    request = readRequest(fileName)
    code = requestCode(request)
    try:
        if code == "getPdf":
            searchType = "pdf"
            key = requestKey(request)
            paperDir = config.PAPERS_DIR + key
            bibFileName = paperDir + "/paper.bib"
            bibData = readBib(bibFileName)
            entry = bibData.entries[key]
        if code == "getBib":
            searchType = "bib"
            paperDir = None
            entry = requestKey(request)
    except:
        tkMessageBox.showerror('LiteRef Error', "Bad request.")
        config.root.update()
        os.system("rm -f " + fileName)
        return
    os.system("rm -f " + fileName)

    if code == "getPdf" and pdfExists(entry, paperDir): return
    
    if not getResourceAutomated(entry, searchType, paperDir):
        if code == "getPdf" and key not in neededPdfs:
            neededPdfs.append(key)
        getResourceManual(entry, searchType)

def confirmKey(key):
    """
    Confirm with the user that the downloaded PDF is the desired PDF
    for the given key.

    :param key: The BibTeX key of the paper for which a PDF is needed.

    :return: The reply of the user (yes/no).
    """
    entry = readFile(config.PAPERS_DIR + key  + "/paper.bib")
    return wideYesNo(
        'LiteRef: confirm BibTex entry for the downloaded PDF',
        "Is the downloaded PDF for the follwing BibTex entry?\n" + \
        entry)
        
def handleNewPdf(fileName):
    
    # Currently simply decide that it is the last request.
    #pdb.set_trace()
    for key in neededPdfs:
        if confirmKey(key):
            neededPdfs.remove(key)
            os.rename(fileName, config.PAPERS_DIR + key  + "/paper.pdf")

def handleNewFile(fileName):
    """
    Handle a PDF file that has just appeared in the drop folder.

    :param fileName: The name of the new PDF file.
    """
    
    # Make sure it's not a short-lived temporary file, e.g. of sed
    sleep(0.1)
    if not os.path.isfile(fileName):
        return      
    name, ext = os.path.splitext(fileName)
    if ext == '.crdownload': return
    try:
        handler = {'.bib': handleNewBib,
                   '.html':handleNewHTML,
                   '.pdf':handleNewPdf,
                   '.rqt':handleRequest}[ext]
    except:
        handler = None
        tkMessageBox.showerror('LiteRef Error',
                               "Unknown file extension: " + ext)
        config.root.update()

    if handler != None:
        sleep(0.1) # Make sure that the file is fully written
        handler(fileName)


