#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      matth
#
# Created:     23.05.2020
# Copyright:   (c) matth 2020
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import os
import sys
from json import load
import subprocess

# importing tkinter module
import tkinter as tk

from tkinter import Tk
from tkinter import Scale
from tkinter.ttk import Progressbar, Style, Button
from time import sleep
from PIL import Image, ImageTk

# paths and separators
bs = "\\"
buildPath = 'build'
scriptPath = 'scripts'
ressourcesPath = 'ressources'
exePath = 'bin'
guiName = 'strakmachine_gui.jpg'

# fixed filenames
# name of python-interpreter
pythonInterpreterName = "python"

# update-rate in s
update_rate = 0.2

# colour of the backgound
bg_colour = 'gray3'

# variable to store the number of lines of the update-cycles
old_length = 0
new_length = 0

# variable that signals that strak-machine has finished work
finished = False

class slider():
    def __init__(self, root):
        s = Scale(root, from_=-100, to=100, tickinterval=1)
        s.set(0)
        #s.grid(row=0, column=0, columnspan=2, sticky=tk.N+tk.S+tk.W+tk.E)
        s.pack( side = 'right')



class show_status():
    def __init__(self):
        # get program-call from arguments
        call = sys.argv[0]

        # was it an .exe-call ?
        if call.find('.exe') >= 0:
            # yes, perform all following calls as exe-calls
            self.scriptsAsExe = True
        else:
            # yes, perform all following calls as python-calls
            self.scriptsAsExe = False

        # check working-directory, is it already the build-dir?
        if (not os.getcwd().find(buildPath)>=0):
            os.chdir("." + bs + buildPath)

        # creating tkinter window
        self.root = Tk()
        self.root.title('The Strak Machine V1.1')

        # Same size will be defined in variable for center screen in Tk_Width and Tk_height
        Tk_Width = 800
        Tk_Height = 600

        #calculate coordination of screen and window form
        x_Left = int(self.root.winfo_screenwidth()*2/3 - Tk_Width/2)
        y_Top = int(self.root.winfo_screenheight()/2 - Tk_Height/2)

        # Write following format for center screen
        self.root.geometry("+{}+{}".format(x_Left, y_Top))

        # set background-colour
        self.root.configure(bg=bg_colour)

        # display logo of the strak machine
        imagename = (".." + bs + ressourcesPath + bs + guiName)

        # Creates a Tkinter-compatible photo image, which can be used everywhere
        # Tkinter expects an image object.
        img = ImageTk.PhotoImage(Image.open(imagename).resize((600,300)))

        # The Label widget is a standard Tkinter widget used to display a text
        # or image on the screen.
        panel = tk.Label(self.root, image = img, bg=bg_colour)

        # The Pack geometry manager packs widgets in rows or columns.
        panel.pack(side = "top", fill = "both", expand = "yes")

        # create sliders
        minCLGain = slider(self.root)
        maxSpeedGain = slider(self.root)
        linearFactor_1 = slider(self.root)
        maxGlideGain = slider(self.root)
        maxGlideShift = slider(self.root)
        linearFactor_2 = slider(self.root)
        maxLiftGain = slider(self.root)

        # create dropdown-menue
        OptionList = [ "SD_root_220k", "SD_strak_150k", "SD_strak_80k"]

        variable = tk.StringVar(self.root)
        variable.set(OptionList[0])

        opt = tk.OptionMenu(self.root, variable, *OptionList)
        opt.config(width=30, font=('Helvetica', 12))
        opt.pack()

        # This button will start the visualizer
        Button(self.root, text = 'updateStrakmachine', command = self.update_strakdata).pack(pady = 10)
        # This button will start the visualizer
        Button(self.root, text = 'add strak-airfoil', command = self.addstrak_airfoil).pack(pady = 10)
        Button(self.root, text = 'remove strak-airfoil', command = self.removestrak_airfoil).pack(pady = 10)

        self.reynolds = Tk()
        self.reynolds.title('Enter ReSqrt(Cl)-value')

        # infinite loop
        self.root.mainloop()

    def quit(self):
        self.root.destroy()

    def update_strakdata(self):
        print("TODO update now")

    def addstrak_airfoil(self):
        print("TODO update now")

    def removestrak_airfoil(self):
        print("TODO update now")

def main():
    show_status()


if __name__ == '__main__':
    main()
