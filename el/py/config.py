#!/usr/bin/python 

# config.py --- module that defines configuration variables.

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
This module defines the following configuration variables, which are
globally shared by all the modules:
"""

# Code:

import sys

#: The root folder of LiteRef. The value of this variable
#:  is read from the command line. Since the server is started
#:  automatically by the command layer, the user does not need to worry
#:  about supplying this value. He only needs to use the desired path in
#:  the ``Emacs`` configuration.
ROOT_DIR = sys.argv[1] if len(sys.argv[1]) > 2 else '<LiteRef root path>'
if (ROOT_DIR[-1] != "/"): ROOT_DIR += "/"

#: The location of the papers database.
PAPERS_DIR = ROOT_DIR + "papers/"

#: The location for the surveys.
NOTES_DIR = ROOT_DIR + "survey/"

#: The folder for dropping the imported files and for the
#: requests created by the command layer.
DROP_DIR = ROOT_DIR + "drop/"

#: The online sources used by the automated search for a paper
#: PDF. The implemented sources are GoogleScholar and SemanticScholar.
PDF_AUTOMATED_SOURCES = ['SemanticScholar']

#: The online sources used for manual download of a paper PDF in case
#: the automated search does no produce the desired PDF. The value
#: ``None`` means that manual downloads should not be initiated
#: automatically.
PDF_MANUAL_SOURCE = 'SemanticScholar'

#: The online sources used by the automated search for a paper's
#: BibTeX entry. The implemented sources are GoogleScholar,
#: SemanticScholar and DBLP.
BIB_AUTOMATED_SOURCES = ['SemanticScholar']

#: The online sources used for manual download of a paper's BibTeX
#: entry in case the automated search does no produce the desired
#: entry.  The value ``None`` means that manual downloads should not
#: be initiated automatically.
BIB_MANUAL_SOURCE = 'SemanticScholar'

#: The browser to be used for manual searches.
BROWSER = 'google-chrome'

#: The delay in seconds to make sure that the PDF viewer opens before
#: asking the user whether the found PDF is the desired one.
EVINCE_STARTUP_DELAY = 3

    
