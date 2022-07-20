#!/usr/bin/env python

#  This file is part of "The Strak Machine".

#  "The Strak Machine" is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  "The Strak Machine" is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with "The Strak Machine".  If not, see <http://www.gnu.org/licenses/>.

#  Copyright (C) 2020-2022 Matthias Boese

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

# imports from strak machine
from strak_machine import(bs, buildPath, scriptPath, ressourcesPath, exePath,
                          xoptfoilVisualizerName, progressFileName,
                          pythonInterpreterName)

from strak_machine_gui import(logoName)

# paths and separators
finishSound = 'fanfare.wav'

# update-rate in s
update_rate = 0.2

# colour of the backgound
bg_colour = "#222222"

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

        # get screen width
        width = self.root.winfo_screenwidth()

        # determine scale factor
        scaleFactor = (self.root.winfo_screenwidth()/1920.0)
        if (width <= 1366):
            scaleFactor = scaleFactor * 1.55
        elif (width <= 1920):
            scaleFactor = scaleFactor * 1.1
        elif (width <= 3840):
            scaleFactor = scaleFactor * 1.0

        # Same size will be defined in variable for center screen in Tk_Width and Tk_height
        Tk_Width = 500
        Tk_Height = 500

        # scale and place window
        self.root.geometry("%dx%d+0+0" % (Tk_Width, Tk_Height))

        # display logo of the strak machine
        imagename = (".." + bs + ressourcesPath + bs + logoName)

        # scale image
        img_width = int(400 * scaleFactor)
        img_height = int(130 * scaleFactor)

        # Creates a Tkinter-compatible photo image, which can be used everywhere
        # Tkinter expects an image object.
        img = ImageTk.PhotoImage(Image.open(imagename).resize((img_width,img_height)))

        # The Label widget is a standard Tkinter widget used to display a text
        # or image on the screen.
        panel = tk.Label(self.root, image = img, bg=bg_colour)

        # The Pack geometry manager packs widgets in rows or columns.
        panel.pack(side = "top", fill = "both", expand = "yes")

        # length of progress bars
        scaled_length = int(220 * scaleFactor)

        # main-Progress bar widget
        self.main_progressText = ctk.CTkLabel(self.root, text="all airfoils")
        self.main_progressBar = ctk.CTkProgressBar(self.root, width=scaled_length)

        # sub-Progress bar widget
        self.sub_progressText = ctk.CTkLabel(self.root, text="current airfoil")
        self.sub_progressBar = ctk.CTkProgressBar(self.root, width=scaled_length)

        self.main_progressText.pack(side = "top", padx = 0, pady = 0, anchor = 'sw')
        self.main_progressBar.pack(side = "top", padx = 20, pady = 0, anchor = 'nw')
        self.sub_progressText.pack(side = "top", padx = 0, pady = 0, anchor = 'sw')
        self.sub_progressBar.pack(side = "top", padx = 20, pady = 0, anchor = 'nw')

        # create textbox to display content of progress-file
        self.progressLog = tk.Text(self.root, highlightthickness=0,
                          bg = bg_colour, foreground = 'lightgray',
                          font = "default_theme", height=10, width=200)

        # create a scrollbar
        scrollbar = ctk.CTkScrollbar(self.root, command = self.progressLog.yview)
        scrollbar.pack( side = 'right', fill='y', pady = 10 )

        # configure testbox
        self.progressLog.configure(yscrollcommand=scrollbar.set)
        self.progressLog.pack( side = 'top', padx = 5, pady = 10, anchor = 'nw')

        # This button will abort the optimizationQuit the application
        abort_button = ctk.CTkButton(self.root, text = 'Abort Optimization',
         fg_color='MAROON', hover_color='red', command = self.abort_optimization)
        abort_button.pack(padx = 40, pady = 10, side=tk.LEFT)

        # This button will start the visualizer
        visuStart_button = ctk.CTkButton(self.root, text = 'Start Visualizer',
                                 command = self.start_visualizer)
        visuStart_button.pack(padx = 40, pady = 10, side=tk.RIGHT)

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
        self.main_progressText.set_text("all airfoils: %.2f %%" % main_progress)
        self.main_progressBar.set(main_progress/100.0)
        self.sub_progressText.set_text("current airfoil: %.2f %%" % sub_progress)
        self.sub_progressBar.set(sub_progress/100.0)


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

    def abort_optimization(self):
        dialog = ctk.CTkInputDialog(master=None,
                       text="To abort the optimization of\n the current airfoil, type 'stop'",
                        title = 'Abort Optimization')
        inputstring = dialog.get_input()

        if inputstring == 'stop':
            runcontrol = open('run_control', 'w+')
            runcontrol.write(inputstring)
            runcontrol.close()


    def quit(self):
        self.root.destroy()


def main():
    show_status()


if __name__ == '__main__':
    main()
