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
import winsound

# importing tkinter module
import tkinter as tk
import customtkinter as ctk

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
finishSound = 'fanfare.wav'

# fixed filenames
# name of python-interpreter
pythonInterpreterName = "python"

# filename of the visualizer
xoptfoilVisualizerName = "xoptfoil_visualizer-jx"

# filename of progress-file
progressFileName = "progress.txt"

# update-rate in s
update_rate = 0.2

# colour of the backgound
bg_colour = 'gray3'

# variable to store the number of lines of the update-cycles
old_length = 0
new_length = 0

# variable that signals that strak-machine has finished work
finished = False

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

        ctk.set_appearance_mode("Dark")  # Modes: "System" (standard), "Dark", "Light"
        ctk.set_default_color_theme("blue")  # Themes: "blue" (standard), "green", "dark-blue"

        # creating tkinter window
        self.root = ctk.CTk()
        self.root.title('The Strak Machine')

        # set background-colour
        self.root.configure(bg=bg_colour)

        # determine width and height
        width_value = self.root.winfo_screenwidth()
        height_value = self.root.winfo_screenheight()

        # determine scale factors
        width_scaler = width_value/1920
        heigth_scaler = height_value/1080

        # Same size will be defined in variable for center screen in Tk_Width and Tk_height
        Tk_Width = int(250 * width_scaler)
        Tk_Height = int(250 * heigth_scaler)

        # scale and place window
        self.root.geometry("%dx%d+0+0" % (Tk_Width, Tk_Height))

        # display logo of the strak machine
        imagename = (".." + bs + ressourcesPath + bs + logoName)

        # scale image
        img_width = int(400 * width_scaler)
        img_height = int(100 * heigth_scaler)

        # Creates a Tkinter-compatible photo image, which can be used everywhere
        # Tkinter expects an image object.
        img = ImageTk.PhotoImage(Image.open(imagename).resize((img_width,img_height)))

        # The Label widget is a standard Tkinter widget used to display a text
        # or image on the screen.
        panel = tk.Label(self.root, image = img, bg=bg_colour)

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

        # length of progress bars
        scaled_length = int(400 * width_scaler)

        # main-Progress bar widget
        self.main_progressBar = Progressbar(self.root, orient="horizontal",
         length=scaled_length, mode = 'determinate',  style="MainProgressbar")

        # sub-Progress bar widget
        self.sub_progressBar = Progressbar(self.root, orient="horizontal",
         length=scaled_length, mode = 'determinate',  style="SubProgressbar")

        self.main_progressBar.pack(pady = 10)
        self.sub_progressBar.pack(pady = 10)

        # change the text of the progressbar,
        # the trailing spaces are here to properly center the text
        self.s_main.configure("MainProgressbar", text="0 %      ", background = "DeepSkyBlue")
        self.s_sub.configure("SubProgressbar", text="0 %      ", background = 'DeepSkyBlue')

        # create a scrollbar
        scrollbar = tk.Scrollbar(self.root)
        scrollbar.pack( side = 'right', fill='y' )

        # create textbox to display content of progress-file
        self.progressLog = tk.Listbox(self.root, height=10, width=200, yscrollcommand = scrollbar.set)
        self.progressLog.pack(pady = 10)
        scrollbar.config( command = self.progressLog.yview )

        # This button will start the visualizer
        ctk.CTkButton(self.root, text = 'Start Visualizer', command = self.start_visualizer).pack(pady = 10)

        # This button will Quit the application
        #Button(self.root, text = 'Quit', command = self.quit).pack(pady = 10)

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
                main_progress = float(splitlines[1])

            # look for sub-task-progress
            if line.find("sub-task progress") >= 0:
                splitlines = line.split(": ")
                sub_progress = float(splitlines[1])

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
            if (line.find("finalizing airfoil") >= 0) or\
               (line.find("creating preliminary-airfoil") >=0):
                splitlines = line.split(": ")
                airfoilname = splitlines[1]
                airfoilname = airfoilname.strip("\r\n\t '")

        return airfoilname


    # function to filter out some kind of output
    def filterLines(self, line):
        filteredLine = line

        # several filters
        if (line.find("progress") >=0):
            filteredLine = None

        if (line.find("task") >=0):
            filteredLine = None

        if (line.find("timestamp") >=0):
            filteredLine = None

        return filteredLine


    # Function responsible for the update of the progress bar values
    def update_progressbars(self):
        global old_length
        global new_length
        global finished

        # read actual values from progress-file
        (main_progress, sub_progress, current_airfoil, content) = self.read_progressFile()

        # store lengths
        old_length = new_length
        new_length = len(content)

        # update progress-bars
        self.main_progressBar['value'] = main_progress
        self.sub_progressBar['value'] = sub_progress
        self.s_main.configure("MainProgressbar", text="all airfoils: {0} %      ".format(main_progress))
        self.s_sub.configure("SubProgressbar", text="current airfoil: {0} %      ".format(sub_progress))

        # update progress-log-widget (only the new lines)
        for idx in range (old_length, new_length):
            line = self.filterLines(content[idx])
            if line != None:
                self.progressLog.insert(tk.END, content[idx])
                # always show the last line, if there is a new one
                self.progressLog.see(tk.END)

        self.root.update()

        # check if strak-machine has finished
        if (finished == False):
            if (main_progress == 100.0):
                print(os.getcwd())
                soundFileName = '..' + bs + ressourcesPath + bs + finishSound
                winsound.PlaySound(soundFileName , winsound.SND_FILENAME|winsound.SND_NOWAIT)
                finished = True



        # setup next cylce
        self.root.after(200, self.update_progressbars)


    def start_visualizer(self):
        # get current airfoilname from progressfile for starting the visualizer
        airfoilname = self.get_CurrentAirfoilName()

        # setup tool-calls
        exeCallString =  " .." + bs + exePath + bs
        pythonCallString =  pythonInterpreterName + ' ..' + bs + scriptPath + bs

        if (self.scriptsAsExe):
            xoptfoilVisualizerCall =  exeCallString + xoptfoilVisualizerName + '.exe'
        else:
            xoptfoilVisualizerCall =  pythonCallString + xoptfoilVisualizerName + '.py'

        # compose subprocess-string
        cmd = (" %s -o 3 -c %s\n") % (xoptfoilVisualizerCall, airfoilname)

        # now open subprocess
        p = subprocess.Popen(cmd, shell=True)


    def quit(self):
        self.root.destroy()


def main():
    show_status()


if __name__ == '__main__':
    main()
