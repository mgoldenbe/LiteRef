#!/usr/bin/python 

# literef_server.py --- the main module.

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
This is the main module. It simply begins the =pyinotify= loop to
listen to events from the ``drop`` folder. Namely, the server handles
any file imported into ``LiteRef`` by dropping it into the ``drop``
folder and the request files created by the ``LiteRef`` command layer.
"""

# Code:

import config
import handlers

# Source: http://github.com/seb-m/pyinotify/wiki/Tutorial
import pyinotify
import os
import Tkinter as tk

def init():
    """ 
    Creates the papers database, the survey and the drop folders
    unless they already exist. Also starts ``Tk``.  
    """
    os.system("mkdir -p " + config.DROP_DIR)
    os.system("mkdir -p " + config.PAPERS_DIR)
    os.system("mkdir -p " + config.NOTES_DIR)
    config.root = tk.Tk()
    # config.root.option_add('*Dialog.msg.font', 'Helvetica 12')
    config.root.withdraw()

class EventHandler(pyinotify.ProcessEvent):
    """
    The class for handling ``pyinotify`` events from the ``drop`` folder.
    """
    def process_IN_CREATE(self, event):
        """
        This is a dispatcher. It passes the event to the proper
        handler. Right now this is very easy as we only need to handle
        appearance of a new file in the ``drop`` folder. 

        :param event: is the event to be processed.
        """
        handlers.handleNewFile(event.pathname)

if __name__ == '__main__':
    wm = pyinotify.WatchManager()  # Watch Manager

    # Watched events, see https://github.com/seb-m/pyinotify/wiki/Events-types
    mask = pyinotify.ALL_EVENTS

    init()
    handler = EventHandler()
    notifier = pyinotify.Notifier(wm, handler)
    wdd = wm.add_watch(config.DROP_DIR, mask, rec=True)
    print "Watching " + config.DROP_DIR

    notifier.loop()
