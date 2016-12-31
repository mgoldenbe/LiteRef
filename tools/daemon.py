#!/usr/bin/python

from config import *
import handlers

# Source: http://github.com/seb-m/pyinotify/wiki/Tutorial
import pyinotify
from time import sleep

wm = pyinotify.WatchManager()  # Watch Manager

# Watched events, see https://github.com/seb-m/pyinotify/wiki/Events-types
mask = pyinotify.IN_DELETE | pyinotify.IN_CREATE |  pyinotify.IN_MODIFY

class EventHandler(pyinotify.ProcessEvent):
    # Note that this event can be generated on modify as well, since
    # applications may choose to just overwrite the file with a new one.
    def process_IN_CREATE(self, event):
        sleep(0.1) # Make sure that the file is fully written
        if isBibDir(event.pathname):
            handlers.handleBibCreated(event.pathname);
        else:
            print event.pathname + " " + BIB_DIR

    def process_IN_DELETE(self, event):
        print "Removing:", event.pathname

    def process_IN_MODIFY(self, event):
        print "Modifying:", event.pathname

handler = EventHandler()
notifier = pyinotify.Notifier(wm, handler)
wdd = wm.add_watch('bib/', mask, rec=True)

notifier.loop()
