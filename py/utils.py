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
def center(toplevel, message, title, font):
    toplevel.update_idletasks()
    w = toplevel.winfo_screenwidth()
    h = toplevel.winfo_screenheight()

    # calculate size based on message and font
    cWidth = min(len(message), config.MESSAGE_WIDTH)
    cHeight = int(math.ceil(float(len(message))/config.MESSAGE_WIDTH))
    lines = [message[i * cWidth : min((i + 1) * cWidth, len(message))]
             for i in range(cHeight)]
    pWidth = max(max([font.measure(line) for line in lines]),
                 font.measure(title) * 1.5,
                 config.MIN_MESSAGE_WINDOW_WIDTH)
    pHeight = max(cHeight * font.metrics("linespace"),
                  config.MIN_MESSAGE_WINDOW_HEIGHT)
    size = (pWidth * 1.05, pHeight * 1.05)
    x = w/2 - size[0]/2
    y = h/2 - size[1]/2
    toplevel.geometry("%dx%d+%d+%d" % (size + (x, y)))
    return (pWidth, pHeight)


class ProgressBox:
    def __init__(self, message):
        #pdb.set_trace()
        myFont = tkFont.Font(root=config.root,
                             family="Times New Roman", size=12,
                             weight=tkFont.BOLD)
        self.box = tk.Toplevel()
        # self.box.configure(font=myFont)
        title = 'LiteRef: Operation Progress'
        (pWidth, pHeight) = center(self.box, message, title, myFont)
        self.box.title(title)
        tk.Message(self.box, text=message, font=myFont,
                   width = pWidth).pack()
        config.root.update()

    def __enter__(self):
        return self
    
    def __exit__(self, type, value, traceback):
        self.box.destroy()
    
    
