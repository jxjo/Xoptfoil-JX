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

# importing tkinter module
import tkinter as tk

from tkinter import Tk
from tkinter.ttk import Progressbar, Style, Button
from time import sleep
from PIL import Image, ImageTk

# paths and separators
bs = "\\"
buildPath = 'build'
scriptPath = 'scripts'
ressourcesPath = 'ressources'
exePath = 'bin'
logoName = 'strakmachine.jpg'

# fixed filenames
# name of python-interpreter
pythonInterpreterName = "python"

# filename of the visualizer
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"

# filename of progress-file
progressFileName = "progress.txt"

# update-rate in s
update_rate = 0.2

# debug
#main_progress = 0.0

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

        # set name of the progressFile
        self.progressFileName = progressFileName

        # creating tkinter window
        self.root = Tk()
        self.root.title('The Strak Machine V1.1')

        # Same size will be defined in variable for center screen in Tk_Width and Tk_height
        Tk_Width = 400
        Tk_Height = 300

        #calculate coordination of screen and window form
        x_Left = int(self.root.winfo_screenwidth()*2/3 - Tk_Width/2)
        y_Top = int(self.root.winfo_screenheight()/2 - Tk_Height/2)

        # Write following format for center screen
        self.root.geometry("+{}+{}".format(x_Left, y_Top))

        # display logo of the strak machine
        imagename = (".." + bs + ressourcesPath + bs + logoName)

        # Creates a Tkinter-compatible photo image, which can be used everywhere
        # Tkinter expects an image object.
        img = ImageTk.PhotoImage(Image.open(imagename).resize((400,100)))

        # The Label widget is a standard Tkinter widget used to display a text
        # or image on the screen.
        panel = tk.Label(self.root, image = img)

        # The Pack geometry manager packs widgets in rows or columns.
        panel.pack(side = "top", fill = "both", expand = "yes")

        # configure progress-bars
        self.s_main = Style(self.root)
        self.s_sub = Style(self.root)

        # add the label to the progressbar style
        self.s_main.layout("MainProgressbar",
             [('MainProgressbar.trough',
               {'children': [('MainProgressbar.pbar',
                              {'side': 'left', 'sticky': 'ns'}),
                             ("MainProgressbar.label",
                              {"sticky": ""})],
               'sticky': 'nswe'})])

        # add the label to the progressbar style
        self.s_sub.layout("SubProgressbar",
             [('SubProgressbar.trough',
               {'children': [('SubProgressbar.pbar',
                              {'side': 'left', 'sticky': 'ns'}),
                             ("SubProgressbar.label",
                              {"sticky": ""})],
               'sticky': 'nswe'})])

        # main-Progress bar widget
        self.main_progressBar = Progressbar(self.root, orient="horizontal", length=300,
                    mode = 'determinate',  style="MainProgressbar")

        # sub-Progress bar widget
        self.sub_progressBar = Progressbar(self.root, orient="horizontal", length=300,
                    mode = 'determinate',  style="SubProgressbar")

        self.main_progressBar.pack(pady = 10)
        self.sub_progressBar.pack(pady = 10)

        # change the text of the progressbar,
        # the trailing spaces are here to properly center the text
        self.s_main.configure("MainProgressbar", text="0 %      ")
        self.s_sub.configure("SubProgressbar", text="0 %      ")

        # textbox to display content of progress-file
        self.progressLog = tk.Text(self.root, height=10, width=100)
        self.progressLog.pack(pady = 10)

        # This button will start the visualizer
        Button(self.root, text = 'Start Visualizer', command = self.start_visualizer).pack(pady = 10)

        # This button will Quit the application
        Button(self.root, text = 'Quit', command = self.quit).pack(pady = 10)

        # update with actual values
        self.update_progressbars()

        # infinite loop
        self.root.mainloop()


    def read_progressFile(self):
        file_content = None
        airfoilname = ""
        #global main_progress Debug
        main_progress = 0.0
        sub_progress = 0.0

        try:
            file = open(self.progressFileName, 'r')
            file_content = file.readlines()
            file.close()
        except:
            print("Error, File %s could not be opened !" % self.progressFileName)
            sys.exit(-1)

        for line in file_content:
            # look for name of current airfoil
            if line.find("current airfoil") >= 0:
                splitlines = line.split(": ")
                airfoilname = splitlines[1]

            # look for main-task-progress
            if line.find("main-task progress") >= 0:
                splitlines = line.split(": ")
                #main_progress = float(splitlines[1])

            # look for sub-task-progress
            if line.find("sub-task progress") >= 0:
                splitlines = line.split(": ")
                sub_progress = float(splitlines[1])

        #main_progress = round (main_progress + 0.2,1) Debug

        return (main_progress, sub_progress, airfoilname, file_content)


    # gets the name of the airfoil that is currently processed by the strak-machine
    def get_CurrentAirfoilName(self):
        file_content = None
        airfoilname = ""

        try:
            file = open(self.progressFileName, 'r')
            file_content = file.readlines()
            file.close()
        except:
            print("Error, File %s could not be opened !" % self.progressFileName)
            sys.exit(-1)

        for line in file_content:
            # look for name of current airfoil
            if (line.find("strak-airfoil") >= 0) or line.find("preliminary-airfoil"):
                splitlines = line.split(": ")
                airfoilname = splitlines[1]

        return airfoilname


    # Function responsible for the update of the progress bar values
    def update_progressbars(self):

        # read actual values from progress-file
        (main_progress, sub_progress, current_airfoil, content) = self.read_progressFile()

        # update progress-bars
        self.main_progressBar['value'] = main_progress
        self.sub_progressBar['value'] = sub_progress
        self.s_main.configure("MainProgressbar", text="{0} %      ".format(main_progress))
        self.s_sub.configure("SubProgressbar", text="{0} %      ".format(sub_progress))

        #self.progressLog.delete()

        #for line in content:
         #   self.progressLog.insert(tk.END, line)

        self.root.update()
        self.root.after(200, self.update_progressbars)


    def start_visualizer(self):
        # get current airfoilname from progressfile for starting the visualizer
        airfoilname = self.get_CurrentAirfoilName()

        # setup tool-calls
        exeCallString =  " .." + bs + exePath + bs
        pythonCallString = pythonInterpreterName + ' ..' + bs + scriptPath + bs

        if (self.scriptsAsExe):
            xoptfoilVisualizerCall = exeCallString + xoptfoilVisualizerName + '.exe'
        else:
            xoptfoilVisualizerCall = pythonCallString + xoptfoilVisualizerName + '.py'

        # compose system-call-string
        systemString = ("%s -o 3 -c %s") % (xoptfoilVisualizerCall, airfoilname)

        # now execute system-call
        os.system(systemString)


    def quit(self):
        self.root.destroy()


def main():
    show_status()


if __name__ == '__main__':
    main()
