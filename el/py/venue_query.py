#!/usr/bin/python

from config import BROWSER
import webbrowser
import sys

nArgs = len(sys.argv) - 1
if nArgs < 2:
    print \
        "You need to specify at least three command line arguments: " + \
        "index (e.g. dblp), venue and year."
    exit(1)

index = sys.argv[1]
venue = sys.argv[2]
year = sys.argv[3]
volume = (sys.argv[4] if nArgs > 3 else "")
print "Index: {index}    Venue: {venue}    Year: {year}    Volume: {volume}". \
    format(index = index, venue = venue, year = year, volume = volume)
query = ""
if index == 'dblp':
    query = "http://dblp.org/search?q="
if query == "":
    print "The chosen index is not supported."
    exit(1)
query += "venue:{venue} year:{year}".format(venue=venue, year=year)
if volume != "": query += " volume:{volume}".format(volume=volume)

webbrowser.get(BROWSER).open_new_tab(query)
