import config
import Tkinter as tk
import tkFont
import math
import pdb

## enum with automatic enumeration
## source: https://stackoverflow.com/a/1695250/2725810
def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)

# Source: https://stackoverflow.com/a/3353112/2725810
def center(toplevel):
    toplevel.update_idletasks()
    w = toplevel.winfo_screenwidth()
    h = toplevel.winfo_screenheight()
    size = tuple(int(_) for _ in toplevel.geometry().split('+')[0].split('x'))
    x = w/2 - size[0]/2
    y = h/2 - size[1]/2
    toplevel.geometry("%dx%d+%d+%d" % (size + (x, y)))

class ProgressBox:
    def __init__(self, message):
        self.box = tk.Toplevel(padx=5, pady=5)
        self.box.title('LiteRef: Progress')
        tk.Label(self.box, text = message, font=('Times', 14)).pack()
        center(self.box)
        config.root.update()

    def __enter__(self):
        return self
    
    def __exit__(self, type, value, traceback):
        self.box.destroy()
    
    
