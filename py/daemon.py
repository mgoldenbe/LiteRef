#!/usr/bin/python

import config
import handlers

# Source: http://github.com/seb-m/pyinotify/wiki/Tutorial
import pyinotify
import os
import Tkinter as tk

wm = pyinotify.WatchManager()  # Watch Manager

# Watched events, see https://github.com/seb-m/pyinotify/wiki/Events-types
mask = pyinotify.IN_DELETE | pyinotify.IN_CREATE |  pyinotify.IN_MODIFY

def init():
    os.system("mkdir -p " + config.DROP_DIR)
    os.system("mkdir -p " + config.PAPERS_DIR)
    os.system("mkdir -p " + config.NOTES_DIR)
    config.root = tk.Tk()
    # config.root.option_add('*Dialog.msg.font', 'Helvetica 12')
    config.root.withdraw()

class EventHandler(pyinotify.ProcessEvent):
    # Note that this event can be generated on modify as well, since
    # applications may choose to just overwrite the file with a new one.
    def process_IN_CREATE(self, event):  
        handlers.handleNewFile(event.pathname)

    def process_IN_DELETE(self, event):
        print "Removing:", event.pathname

    def process_IN_MODIFY(self, event):
        print "Modifying:", event.pathname

init()
handler = EventHandler()
notifier = pyinotify.Notifier(wm, handler)
wdd = wm.add_watch(config.DROP_DIR, mask, rec=True)

notifier.loop()
