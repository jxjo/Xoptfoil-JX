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

import tkinter as tk
from tkinter import ttk
import customtkinter
import os
from PIL import ImageTk, Image
from colorama import init
from copy import deepcopy

# imports to use matplotlib together with tkinter
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

# imports from strak machine
from strak_machine import (strak_machine, diagTypes,
                           ErrorMsg, WarningMsg, NoteMsg, DoneMsg,
                           bs, ressourcesPath,
                           CL_decimals, CD_decimals, CL_CD_decimals,
                           AL_decimals, camb_decimals, thick_decimals)

# some global variables
num_diagrams = 3
controlFrame = None

# name of logo-image
logoName = 'strakmachine.png'
bg_color_scrollableFrame_light = "#DDDDDD"
bg_color_scrollableFrame_dark =  "#222222"

# class control frame, change the input-variables / parameters of the
# strak machine
class control_frame():
    def __init__(self, master, side, left_Buttons, right_Buttons, strak_machine):
        global bg_color_scrollableFrame

        # store some variables in own class data structure
        self.strak_machine = strak_machine
        self.master = master

        # get some data form strak_machine
        self.airfoilNames = self.strak_machine.get_airfoilNames()

        # determine screen size
        self.width = self.master.winfo_screenwidth()
        self.heigth = self.master.winfo_screenheight()
        strak_machine.set_screenParams(self.width, self.heigth)

        # create top frame
        self.frame_top = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)

        # create scrollable Frame
        self.container = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)

        self.canvas = tk.Canvas(self.container, bg=bg_color_scrollableFrame_dark,
                                 highlightthickness=0)
        self.scrollbar_v = customtkinter.CTkScrollbar(self.container,
                                                command=self.canvas.yview)

        self.frame_bottom  = tk.Frame(self.canvas, width=180,
                                         bg = bg_color_scrollableFrame_dark)

        self.frame_bottom.bind("<Configure>", self.OnFrameConfigure)

        self.canvas.create_window((10, 10), window=self.frame_bottom , anchor="ne")
        self.canvas.configure(yscrollcommand=self.scrollbar_v.set)
        self.canvas.pack(side="left", fill="both", expand=True)
        self.scrollbar_v.pack(side="right", fill="y")


        # init nextRow (where to place next widget)
        self.nextRow = 0

        # add the strak machine logo
        self.add_logo(self.frame_top)

        # add different widgets to upper frame (not scrollable)
        self.add_label(self.frame_top)
        self.add_buttons(self.frame_top, left_Buttons, right_Buttons)
        #self.add_appearanceModeMenu(self.frame_top) #FIXME not supported at the moment
        self.add_airfoilChoiceMenu(self.frame_top)
        self.add_visiblePolarsCheckboxes(self.frame_top)
        self.add_referencePolarsCheckbox(self.frame_top)

        self.nextRow = 0
        # add geo-entries to lower frame (scrollable)
        self.add_geoEntries(self.frame_bottom)

        # add entries to lower frame (scrollable)
        self.add_entries(self.frame_bottom)

        # show upper frame
        self.frame_top.pack(side = 'top', fill=tk.BOTH)

        # show lower frame
        self.container.pack(side = 'bottom', fill=tk.BOTH, expand=1)


    def OnFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))


    def add_logo(self, frame):
        path = ".." + bs + ressourcesPath + bs + logoName
        try:
            img = Image.open(path)
        except:
            ErrorMsg("strak-machine-image was not found in path %s" % path)
            return

        scaleFactor = (self.width/1920.0)
        if (self.width <= 1366):
            scaleFactor = scaleFactor * 1.55
        elif (self.width <= 1920):
            scaleFactor = scaleFactor * 1.1
        elif (self.width <= 3840):
            scaleFactor = scaleFactor * 1.0

        img_width = int(350 * scaleFactor)
        img_height = int(113 * scaleFactor)

        # Resize the image in the given (width, height)
        sized_img = img.resize((img_width, img_height))

        # Convert the image in TkImage
        self.my_img = ImageTk.PhotoImage(sized_img)

         # Create a Label Widget to display the text or Image
        self.logo = customtkinter.CTkLabel(master=frame, image = self.my_img)

        # place the label
        self.logo.grid(row=self.nextRow, columnspan=2, pady=0, padx=0)
        self.nextRow = self.nextRow + 1


    def get_valuesFromDict(self, targetValue):
        # type of oppoint
        mode = targetValue["type"]

        if (mode == 'spec-cl'):
            # oppoint is lift (CL)
            oppoint = round(targetValue["oppoint"], CL_decimals)
            # target is drag (CD)
            target = round(targetValue["target"], CD_decimals)
        elif (mode == 'spec-al'):
            # oppoint is angle of attack (alpha)
            oppoint = round(targetValue["oppoint"], AL_decimals)
            # target is lift (CL)
            target = round(targetValue["target"], CL_decimals)
        else:
            ErrorMsg("undefined oppoint type %s" % mode)
            return (None, None, None)

        weighting = targetValue["weighting"]
        if weighting != None:
            weighting = round(weighting, 1)

        return (mode, oppoint, target, weighting)


    def write_valuesToDict(self, idx, mode, oppoint, target, weighting):
        targetValue = self.targetValues[idx]

        if (mode == 'spec-cl'):
            # oppoint is lift (CL)
            oppoint = round(oppoint, CL_decimals)

            # target is drag (CD)
            target = round(target, CD_decimals)
        elif (mode == 'spec-al'):
            # oppoint is angle of attack (alpha)
            oppoint = round(oppoint, AL_decimals)
            # target is lift (CL)
            target = round(target, CL_decimals)
        else:
            ErrorMsg("undefined oppoint type %s" % mode)

        targetValue["oppoint"] = oppoint
        targetValue["target"] = target

        if (weighting != None):
            weighting = round(weighting, 1)
        targetValue["weighting"] = weighting

        #targetValue["type"] = mode # FIXME We do not support changing mode at the moment
        self.targetValues[idx] = targetValue


    def add_geoEntries(self, frame):
        # get initial geo parameters
        self.geoParameters = self.strak_machine.get_geoParams(self.master.airfoilIdx)

        # separate tuple
        (thickness, thicknessPosition, camber, camberPosition) = self.geoParameters

        # create text-Vars to interact with entries
        self.thickness_txt = tk.StringVar(frame, value=thickness)
        self.thicknessPosition_txt = tk.StringVar(frame, value=thicknessPosition)
        self.camber_txt = tk.StringVar(frame, value=camber)
        self.camberPosition_txt = tk.StringVar(frame, value=camberPosition)

        # create entries
        self.thicknessEntry = customtkinter.CTkEntry(frame, show=None,
             textvariable = self.thickness_txt, text_font=('Roboto Medium', 11),
             width=55, height=16)
        self.thicknessPositionEntry = customtkinter.CTkEntry(frame, show=None,
             textvariable = self.thicknessPosition_txt, text_font=('Roboto Medium', 11),
             width=55, height=16)
        self.camberEntry = customtkinter.CTkEntry(frame, show=None,
             textvariable = self.camber_txt, text_font=('Roboto Medium', 11),
             width=55, height=16)
        self.camberPositionEntry = customtkinter.CTkEntry(frame, show=None,
             textvariable = self.camberPosition_txt, text_font=('Roboto Medium', 11),
             width=55, height=16)


        # bind entries to "Enter"-Message
        self.thicknessEntry.bind('<Return>', self.update_geoParams)
        self.thicknessPositionEntry.bind('<Return>', self.update_geoParams)
        self.camberEntry.bind('<Return>', self.update_geoParams)
        self.camberPositionEntry.bind('<Return>', self.update_geoParams)

        # add labels
        #self.geometryParams_label = customtkinter.CTkLabel(master=frame,
        #               text="Geometry parameters", text_font=("Roboto Medium", 13))
        self.thickness_label = customtkinter.CTkLabel(master=frame,
                text="Thickness", text_font=("Roboto Medium", 11), anchor="e")
        self.thicknessPosition_label = customtkinter.CTkLabel(master=frame,
              text="Thickness @", text_font=("Roboto Medium", 11), anchor="e")
        self.camber_label = customtkinter.CTkLabel(master=frame,
               text="Camber", text_font=("Roboto Medium", 11), anchor="e")
        self.camberPosition_label = customtkinter.CTkLabel(master=frame,
              text="Camber @", text_font=("Roboto Medium", 11), anchor="e")

        # place widgets inside frame
        #self.geometryParams_label.grid(row=self.nextRow, columnspan=2, pady=0, padx=0)
        #self.nextRow = self.nextRow + 1
        self.place_3_widgets(self.thickness_label, self.thicknessEntry, None)
        self.place_3_widgets(self.thicknessPosition_label, self.thicknessPositionEntry, None)
        self.place_3_widgets(self.camber_label, self.camberEntry, None)
        self.place_3_widgets(self.camberPosition_label, self.camberPositionEntry, None)


    def add_entries(self, frame):
        # get initial target values
        self.targetValues = self.strak_machine.get_targetValues(self.master.airfoilIdx)

        # init some structures to store data locally
        self.entries = []
        self.textVars = []

        # determine number of entries
        num_entries = len(self.targetValues)

        # local variable to place spec-al entries in the frame
        spec_al_entries = []

        # Add Label
        oppoint_label = customtkinter.CTkLabel(master=frame,
                text="CL", text_font=("Roboto Medium", 13), anchor="e")
        target_label = customtkinter.CTkLabel(master=frame,
                text="CD", text_font=("Roboto Medium", 13), anchor="e")
        weighting_label = customtkinter.CTkLabel(master=frame,
                text="weighting", text_font=("Roboto Medium", 13), anchor="e")
        self.place_3_widgets(oppoint_label, target_label, weighting_label)

        # create entries and assign values
        for i in range(num_entries):
            # get dictionary containing oppoint / type / target value
            targetValue = self.targetValues[i]

            # get values from dictinory
            (mode, oppoint, target, weighting) = self.get_valuesFromDict(targetValue)
            if (mode == None):
                # error, continue with next entry
                continue

            # create text-Vars to interact with entries
            type_txt = tk.StringVar(frame, value=mode)
            oppoint_txt = tk.StringVar(frame, value=oppoint)
            target_txt = tk.StringVar(frame, value=target)
            weighting_txt = tk.StringVar(frame, value=weighting)

            self.textVars.append((type_txt, oppoint_txt, target_txt, weighting_txt))

            # create entry for oppoint
            oppoint_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = oppoint_txt, text_font=('Roboto Medium', 11),
             width=80, height=16)

             # bind to "Enter"-Message
            oppoint_entry.bind('<Return>', self.update_TargetValues)

            # create entry for target
            target_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = target_txt, text_font=('Roboto Medium', 11),
             width=80, height=16)

            # bind to "Enter"-Message
            target_entry.bind('<Return>', self.update_TargetValues)

            # create entry for weighting
            weighting_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = weighting_txt, text_font=('Roboto Medium', 11),
             width=80, height=16)

            # bind to "Enter"-Message
            weighting_entry.bind('<Return>', self.update_TargetValues)


            # append all entries to list
            self.entries.append((oppoint_entry, target_entry, weighting_entry))

            # if oppoint is 'spec-cl' place widget now
            if (mode == 'spec-cl'):
                self.place_3_widgets(oppoint_entry, target_entry, weighting_entry)
            elif (mode == 'spec-al'):
                # append to list of spec-al entries
                spec_al_entries.append((oppoint_entry, target_entry, weighting_entry))


        # Add Label
        oppoint_label = customtkinter.CTkLabel(master=frame,
                  text="Alpha", text_font=("Roboto Medium", 13), anchor="e")
        target_label = customtkinter.CTkLabel(master=frame,
                  text="CL", text_font=("Roboto Medium", 13), anchor="e")
        weighting_label = customtkinter.CTkLabel(master=frame,
                  text="weighting", text_font=("Roboto Medium", 13), anchor="e")
        self.place_3_widgets(oppoint_label, target_label, weighting_label)

        # now place spec-al entries
        for entryTuple in spec_al_entries:
            # unpack tuple
            (oppoint_entry, target_entry, weighting_entry) = entryTuple
            self.place_3_widgets(oppoint_entry, target_entry, weighting_entry)


    def update_GeoEntries(self, airfoilIdx):
        # get actual targetValues from strak machine
        self.geoParameters = self.strak_machine.get_geoParams(airfoilIdx)

        # separate tuple
        (thickness, thicknessPosition, camber, camberPosition) = self.geoParameters

        # update textvars
        self.thickness_txt.set(thickness)
        self.thicknessPosition_txt.set(thicknessPosition)
        self.camber_txt.set(camber)
        self.camberPosition_txt.set(camberPosition)


    def update_Entries(self, airfoilIdx):
        # get actual targetValues from strak machine
        self.targetValues = self.strak_machine.get_targetValues(airfoilIdx)

        if (self.targetValues == None):
            ErrorMsg("no TargetValue for airfoilIdx %d" % airfoilIdx)
            return

        maxIdx = len(self.targetValues)-1
        idx = 0
        for element in self.textVars:
            # unpack tuple
            (type_txt, oppoint_txt, target_txt, weighting_txt) = element

            # check idx
            if (idx > maxIdx):
                ErrorMsg("idx %d > maxIdx %d" % (idx, maxIdx))
            else:
                 # get values from dictinory
                (mode, oppoint, target, weighting) = self.get_valuesFromDict(self.targetValues[idx])

                # copy values to textvars
                type_txt.set(str(mode))
                oppoint_txt.set(str(oppoint))
                target_txt.set(str(target))

                if (weighting != None):
                    weighting_txt.set(str(weighting))
                else:
                    weighting_txt.set('')

            idx = idx+1

    def change_targetValue(self, x, y, idx):
        if idx == None:
            return

        # read current value to get the mode and weighting
        (mode, oppoint, target, weighting) = self.get_valuesFromDict(self.targetValues[idx])
        # FIXME check: evaluate mode ?
        self.write_valuesToDict(idx, mode, y, x, weighting)

        # writeback dictionary to strakmachine
        self.strak_machine.set_targetValues(self.master.airfoilIdx,
                                            self.targetValues)

        # perform update of the target polars
        self.strak_machine.update_targetPolars()

        # update entries in control frame
        self.update_Entries(self.master.airfoilIdx)

        # notify the diagram frame about the change
        self.master.set_updateNeeded()


    def update_TargetValues(self, command):
        # local variable if writeback of target values to strak machine is needed
        writeback_needed = False
        idx = 0

        for entryTuple in self.entries:
            (oppoint_entry, target_entry, weighting_entry) = entryTuple

            # unpack tuple
            oppoint_entry = oppoint_entry.get()
            target_entry = target_entry.get()
            weighting_entry = weighting_entry.get()

            # get dictionary containing oppoint / type / target value
            targetValue = self.targetValues[idx]

            # get values from dictinory
            (mode, oppoint, target, weighting) = self.get_valuesFromDict(targetValue)

            # conversion of None to empty string
            try:
                str_weighting = str(weighting)
            except:
                str_weighting = ''


            # compare if something has changed
            if ((oppoint_entry   != str(oppoint)) or
                (target_entry    != str(target))  or
                (weighting_entry != str_weighting)):

                # coversion of empty string to None
                try:
                    weighting_value = float(weighting_entry)
                except:
                    weighting_value = None

                # write values to dictionary
                self.write_valuesToDict(idx, mode, float(oppoint_entry),
                                        float(target_entry), weighting_value)
                # set notification variable
                writeback_needed = True

            idx = idx + 1

        if (writeback_needed):
            # writeback dictionary to strakmachine
            self.strak_machine.set_targetValues(self.master.airfoilIdx,
                                                self.targetValues)

            # perform update of the target polars
            self.strak_machine.update_targetPolars()

            # notify the diagram frame about the change
            self.master.set_updateNeeded()


    def update_geoParams(self, command):
        # convert strings to float
        thickness =         float(self.thicknessEntry.get())
        thicknessPosition = float(self.thicknessPositionEntry.get())
        camber =            float(self.camberEntry.get())
        camberPosition =    float(self.camberPositionEntry.get())

        # remove some decimals
        thickness = round(thickness, thick_decimals)
        thicknessPosition = round(thicknessPosition, thick_decimals)
        camber = round(camber, camb_decimals)
        camberPosition = round(camberPosition, camb_decimals)

        # store in own data structure
        self.geoParams = (thickness, thicknessPosition, camber, camberPosition)

        # write targets to strakmachine
        self.strak_machine.set_geoParams(self.master.airfoilIdx,
                                                self.geoParams)


    def get_geoParams(self):
        return self.geoParameters


    def place_widgets(self, widget1, widget2):
        if widget1 != None:
            widget1.grid(row=self.nextRow, column=0, pady=5, padx=5, sticky="e")

        if widget2 != None:
            widget2.grid(row=self.nextRow, column=1, pady=5, padx=5, sticky="w")

        self.nextRow = self.nextRow + 1


    def place_3_widgets(self, widget1, widget2, widget3):
        if widget1 != None:
            widget1.grid(row=self.nextRow, column=0, pady=0, padx=1, sticky="e")

        if widget2 != None:
            widget2.grid(row=self.nextRow, column=1, pady=0, padx=1, sticky="e")

        if widget3 != None:
            widget3.grid(row=self.nextRow, column=2, pady=0, padx=1, sticky="e")
        self.nextRow = self.nextRow + 1


    def add_label(self, frame):
        # Label
        label = customtkinter.CTkLabel(master=frame,
                                              text="Select diagram",
                                              text_font=("Roboto Medium", -16))
        self.place_widgets(label, None)


    def add_buttons(self, frame, left_Buttons, right_Buttons):
        buttonsLeft = []
        buttonsRight = []

        # create all buttons and add to list
        for button in left_Buttons:
            buttonsLeft.append(self.create_button(frame, button))

        for button in right_Buttons:
            buttonsRight.append(self.create_button(frame, button))


        numButtonsLeft = len(buttonsLeft)
        numButtonsRight = len(buttonsRight)
        numTotal = max(numButtonsLeft, numButtonsRight)

        # place buttons
        for idx in range(numTotal):
            if idx < numButtonsLeft:
                left = buttonsLeft[idx]
            else:
                left = None

            if idx < numButtonsRight:
                right = buttonsRight[idx]
            else:
                right = None

            self.place_widgets(left, right)


    def create_button(self, frame, button):
        text = button["txt"]
        command = button["cmd"]

        # create new button
        button = customtkinter.CTkButton(master=frame,
                                            text=text,
                                            fg_color=("gray75", "gray30"),
                                            command=command)
        return button

    def add_appearanceModeMenu(self, frame):
        self.label_mode = customtkinter.CTkLabel(master=frame, text="Appearance Mode:")
        self.optionmenu_1 = customtkinter.CTkOptionMenu(master=frame,
                                                        values=["Dark", "Light"],
                                                        command=self.change_appearance_mode)
        self.place_widgets(self.label_mode, self.optionmenu_1)


    def add_airfoilChoiceMenu(self, frame):
        self.label_airfoilChoice = customtkinter.CTkLabel(master=frame, text="Edit polar of:")
        self.optionmenu_2 = customtkinter.CTkOptionMenu(master=frame,
                                                        values=self.airfoilNames[1:],
                                                        command=self.change_airfoil)
        self.place_widgets(self.label_airfoilChoice, self.optionmenu_2)


    def add_visiblePolarsCheckboxes(self, frame):
        self.checkBoxes = []
        self.visibleFlags = []
        self.lastVisibleFlags = []
        self.label_visiblePolars = customtkinter.CTkLabel(master=frame, text="Visible polars:")

        widget_1 = self.label_visiblePolars
        num = len(self.airfoilNames)
        idx = 0

        for airfoilName in self.airfoilNames:
            # new visibleFlag
            self.visibleFlags.append(tk.BooleanVar(value=True))
            self.lastVisibleFlags.append(True)

            # new checkbox
            checkBox = customtkinter.CTkCheckBox(master=frame, text=airfoilName,
              variable=self.visibleFlags[idx])
            self.checkBoxes.append(checkBox)

            # placing the widgets
            self.place_widgets(widget_1, checkBox)
            widget_1 = None
            idx = idx + 1


    def add_referencePolarsCheckbox(self, frame):
        self.lastReferencePolarsFlag = True
        self.referencePolarsFlag = tk.BooleanVar(value=True)

        checkBox = customtkinter.CTkCheckBox(master=frame,
        text="Reference Polars",variable=self.referencePolarsFlag)
        self.checkBoxes.append(checkBox)
        self.place_widgets(None, checkBox)


    def add_blankRow(self):
        self.nextRow = self.nextRow + 1


    def change_appearance_mode(self, new_appearance_mode):
        customtkinter.set_appearance_mode(new_appearance_mode)

         # maximize the window again using state property
        self.master.state('zoomed')


    def change_airfoil(self, airfoilName):
        # convert airfoilName to an index
        airfoilIdx = self.airfoilNames.index(airfoilName)

        # check if idx has been changed
        if (self.master.airfoilIdx == airfoilIdx):
            return

        # set new idx
        self.master.airfoilIdx = airfoilIdx
        self.strak_machine.set_activeTargetPolarIdx(airfoilIdx)

        # update entry-frame (will also update self.targetValues)
        self.update_GeoEntries(airfoilIdx)
        self.update_Entries(airfoilIdx)

        # check visible flags, is polar of selected airfoil visible?
        isVisible = self.visibleFlags[airfoilIdx].get()
        if (not isVisible):
            # set the polar visible
            self.visibleFlags[airfoilIdx].set(True)
            # this funtion call will aslo set updateNeeded flag
            self.update_visibleFlags()
        else:
            # notify the diagram frame about the change
            self.master.set_updateNeeded()


    def check_activePolarVisibility(self):
        activePolar = self.master.airfoilIdx
        isVisible = self.visibleFlags[activePolar].get()
        return isVisible


    def update_visibleFlags(self):
        newVisibleFlags = []

        # read actual values
        num = len(self.visibleFlags)
        for idx in range(num):
            newVisibleFlags.append(self.visibleFlags[idx].get())

        for idx in range (num):
            # check if something has changed
            if (self.lastVisibleFlags[idx] != newVisibleFlags[idx]):
                self.lastVisibleFlags.clear()
                self.lastVisibleFlags = newVisibleFlags

                # notify strak machine
                self.strak_machine.set_visiblePolars(newVisibleFlags)

                # notify the diagram frame about the change
                self.master.set_updateNeeded()
                break


    def update_referencePolarsFlag(self):
        # read actual value
        newReferencePolarsFlag = self.referencePolarsFlag.get()

        # check if something has changed
        if (self.lastReferencePolarsFlag != newReferencePolarsFlag):
            self.lastReferencePolarsFlag = newReferencePolarsFlag

            # notify strak machine
            self.strak_machine.set_referencePolarsVisibility(newReferencePolarsFlag)

            # notify the diagram frame about the change
            self.master.set_updateNeeded()


    def on_closing(self, event=0):
        self.destroy()

class diagram(customtkinter.CTkFrame):

    def __init__(self, parent, controller, bufferIdx, fig):
        customtkinter.CTkFrame.__init__(self, parent)

        # canvas
        canvas = FigureCanvasTkAgg(fig, self)
        canvas._tkcanvas.pack(fill=tk.BOTH, expand=1)
        canvas.draw()

        # index of targetValue that shall be graphically edited
        self._ind = None
        self.controller = controller

        canvas.mpl_connect('button_press_event', self.on_button_press)
        canvas.mpl_connect('button_release_event', self.on_button_release)
        canvas.mpl_connect('motion_notify_event', self.on_mouse_move)
        canvas.mpl_connect('scroll_event', self.on_scrollwheel_turn)

    def get_ind_under_point(self, event):
        """
        Return the index of the point closest to the event position or *None*
        if no point is within catching range to the event position.
        """
        global targetValues
        global controlFrame

        # set ranges to catch points, consider zoomfactor
        zoom_factor = self.controller.get_zoom_factor()
        catching_range_oppoint = 0.01 * zoom_factor
        catching_range_targetValue = 0.001 * zoom_factor

        mouse_target = event.xdata
        mouse_oppoint = event.ydata

        # check type of active diagram
        if (self.controller.activeDiagram == "CL_CD_diagram"):
            edit_mode = 'spec-cl'
            mouse_target = event.xdata  # target  -> CD
            mouse_oppoint = event.ydata # oppoint -> CL
        elif (self.controller.activeDiagram == "CLCD_CL_diagram"):
            edit_mode = 'spec-cl'
            mouse_oppoint = event.xdata # oppoint -> CL
            # convert y-coordinates, CL/CD -> CD
            mouse_target = event.xdata / event.ydata # target  -> CL/CD
        elif (self.controller.activeDiagram == "CL_alpha_diagram"):
            edit_mode = 'spec-al'
            mouse_target = event.ydata  # target  -> CL
            mouse_oppoint = event.xdata # oppoint -> alpha
            catching_range_oppoint = 0.1 * zoom_factor
            catching_range_targetValue = 0.01 * zoom_factor


        # check visibility of editable polar
        if (controlFrame.check_activePolarVisibility() == False):
            return None

        # search entry with closest coordinates
        idx = 0
        for targetValue in controlFrame.targetValues:
            # get values from dictinory
            (mode, oppoint, target, weighting) = controlFrame.get_valuesFromDict(targetValue)

            if (mode != edit_mode):
                # not graphically editable in this diagram
                idx = idx + 1
                continue

            if ((abs(mouse_target - target) < catching_range_targetValue) and
                (abs(mouse_oppoint - oppoint) < catching_range_oppoint)):
                return idx

            idx = idx + 1
        return None


    def on_scrollwheel_turn(self, event):
        """Callback for scrollwheel turn."""
        if event.inaxes is None:
            return
        self.controller.zoom_in_out(event)


    def on_button_press(self, event):
        """Callback for mouse button presses."""
        if event.inaxes is None:
            return
        if event.button == 1: # left mouse button
            # determine index of target point to change
            self._ind = self.get_ind_under_point(event)
        elif event.button == 2: # middle mouse button / scrollwheel
            # restore default zoom
            self.controller.default_zoom()
        elif event.button == 3: # right mouse button
            # capture actual position
            self.controller.capture_position(event)
        else:
            return



    def on_button_release(self, event):
        """Callback for mouse button releases."""
        if event.button == 1: # left mouse button
            # clear index of target point to change
            self._ind = None
        else:
            return


    def on_mouse_move(self, event):
        """Callback for mouse movements."""
        global controlFrame
        if event.inaxes is None:
            return
        if event.button == 1:
            if self._ind is None:
                return
            # check type of active diagram
            if (self.controller.activeDiagram == "CLCD_CL_diagram"):
                # convert coordinates
                x, y = event.ydata, event.xdata
                x = y/x
            elif (self.controller.activeDiagram == "CL_alpha_diagram"):
                x, y = event.ydata, event.xdata
            else:
                x, y = event.xdata, event.ydata
                    # set new target value
            controlFrame.change_targetValue(x,y,self._ind)
        elif event.button == 3: # right mouse button
            # move visible area of the window
            self.controller.move_visibleArea(event)




# class diagram frame, shows the graphical output of the strak machine
class diagram_frame():
    def __init__(self, master, side, strak_machine):
        # store strak machine instance locally
        self.strak_machine = strak_machine
        self.master = master
        self.figures = []
        self.axes = []
        self.initial_limits = {}
        self.zoomed_limits = {}
        self.offsets = {}
        self.zoom_factors = {}
        self.zoom_factors_old = {}
        self.captured_x_Position = 0.0
        self.captured_y_Position = 0.0

        # determine screen size
        self.width = self.master.winfo_screenwidth()
        self.heigth = self.master.winfo_screenheight()
        self.scaleFactor = self.width/1980
        strak_machine.set_screenParams(self.width, self.heigth)

        # create figures initially (two of each kind for buffering)
        for i in range(2):
            (figures, axes, limits) =  self.create_figures()
            self.figures.append(figures)
            self.axes.append(axes)

            # initial limits unbuffered (only once each type)
            if (i == 0):
                self.initial_limits = limits

        # set zoomed limits
        self.zoomed_limits = deepcopy(self.initial_limits)

        # set initial zoomfactors and offsets
        for diagType in diagTypes:
            self.zoom_factors[diagType] = 1.0
            self.zoom_factors_old[diagType] = 1.0
            self.offsets[diagType] = (0.0, 0.0)


        # create new frame (container)
        self.container = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)
        self.container.pack(side = side, fill=tk.BOTH, expand=1)
        self.container.grid_rowconfigure(0, weight=1)
        self.container.grid_columnconfigure(0, weight=1)

        # empty list of frame dictionaries
        self.frames = []

        # create frame for each diagram class (two of each kind for buffering)
        for i in range(2):
            self.frames.append(self.create_frames(i))

        # set initial value of active buffer idx
        self.activeBufferIdx = 0

        # set initial value of active diagram
        self.activeDiagram = "CL_CD_diagram"

        # show initial diagram
        self.master.set_updateNeeded()
        self.update_diagram(master)


    def create_figures(self):
        global num_diagrams

        # add figures for different diagram types
        figures = {}
        axes = {}
        limits = {}

        # set 'dark' style
        plt.style.use('dark_background')

        for diagType in diagTypes:
            # new figure
            x = 14* self.scaleFactor
            y = 16* self.scaleFactor
            fig = Figure(figsize=(x, y))
            ax = fig.add_subplot()

            # initial diagram (limits will be determined automatically)
            self.strak_machine.plot_diagram(diagType, ax, None, None)

            # get initial limits
            (x_limits, y_limits) = (ax.get_xlim(), ax.get_ylim())

            # append to lists
            figures[diagType] = fig
            axes[diagType] = ax
            limits[diagType] = (x_limits, y_limits)

        return (figures, axes, limits)

    def create_frames(self, bufferIdx):
        # empty dictionary of frames
        frames = {}

        for diagType in diagTypes:
            frame = diagram(self.container, self, bufferIdx,
                        self.figures[bufferIdx][diagType])
            frame.grid(row=0, column=0, sticky="nsew")

            # put into dictionary
            frames[diagType] = frame

        return frames

    def change_zoom_factor(self, step):
        zoomsteps = 30.0
        max_zoom = 1.0
        min_zoom = 0.08

        # get actual zoom factor for diagType
        zoom_factor = self.zoom_factors[self.activeDiagram]

        # store as "old" zoom factor
        self.zoom_factors_old[self.activeDiagram] = zoom_factor

        # change zoom factor, steps is either -1.0 (scroll down) or +1.0 (scroll up)
        zoom_factor = zoom_factor + (step/zoomsteps)

        # limit zoom_factor
        if (zoom_factor > max_zoom):
                zoom_factor = max_zoom
        elif (zoom_factor < min_zoom):
              zoom_factor = min_zoom

        # writeback
        self.zoom_factors[self.activeDiagram] = zoom_factor


    def get_zoom_factor(self):
        return self.zoom_factors[self.activeDiagram]


    def calculate_zoomed_limits(self, x_pos, y_pos):
        # get zoom factor for diagType
        zoom_factor = self.zoom_factors[self.activeDiagram]
        zoom_factor_old = self.zoom_factors_old[self.activeDiagram]
        zoom_change = zoom_factor / zoom_factor_old

        # get actual limits for diagType
        (x_limTuple, y_limTuple) = self.zoomed_limits[self.activeDiagram]
        (x_limits, y_limits) = (list(x_limTuple), list(y_limTuple))

        # adjust limits relative to mouse position
        x_left = x_pos - x_limits[0]
        x_right = x_limits[1] - x_pos
        x_left = x_left * zoom_change
        x_right = x_right * zoom_change
        x_left_lim = x_pos - x_left
        x_right_lim = x_pos + x_right

        y_below = y_pos - y_limits[0]
        y_beyond = y_limits[1] - y_pos
        y_below = y_below * zoom_change
        y_beyond = y_beyond * zoom_change
        y_below_lim = y_pos - y_below
        y_beyond_lim = y_pos + y_beyond

        # write zoomed limits
        self.zoomed_limits[self.activeDiagram] = ((x_left_lim, x_right_lim),
                                                  (y_below_lim, y_beyond_lim))


    def zoom_in_out(self, event):
        # check if there is an update going on
        if (self.master.get_updateNeeded()):
            return

        # change zoom_factor first
        self.change_zoom_factor(event.step)

        # calculate zoomed_limits
        self.calculate_zoomed_limits(event.xdata, event.ydata)

        # set notification flag / update diagram
        self.master.set_updateNeeded()


    def capture_position(self, event):
        # get curretn offsets
        (x_offset, y_offset) = self.offsets[self.activeDiagram]
        # capture new postion, including old offsets
        self.captured_x_Position = event.x - x_offset
        self.captured_y_Position = event.y - y_offset


    def move_visibleArea(self, event):
        # calculate offsets for x, y
        x_offset = event.x - self.captured_x_Position
        y_offset = event.y - self.captured_y_Position

        # store offsets for active diagram type
        self.offsets[self.activeDiagram] = (x_offset, y_offset)

         # set notification flag / update diagram
        self.master.set_updateNeeded()


    def default_zoom(self):
        # set zoom factor for active diagram to default
        self.zoom_factors[self.activeDiagram] = 1.0
        self.zoom_factors_old[self.activeDiagram] = 1.0

        # restore initial limits for active diagram
        self.zoomed_limits[self.activeDiagram] =\
                self.initial_limits[self.activeDiagram]

        # set offsets to zero
        self.offsets[self.activeDiagram] = (0.0, 0.0)

        # set notification flag / update diagram
        self.master.set_updateNeeded()


    def get_limits(self):
        # get zoomed limits
        (x_limTuple, y_limTuple) = self.zoomed_limits[self.activeDiagram]
        (x_limits, y_limits) = (list(x_limTuple), list(y_limTuple))

        # get offsets
        (x_offset, y_offset) = self.offsets[self.activeDiagram]

        # get diagram width and height
        x_width = x_limits[1] - x_limits[0]
        y_width = y_limits[1] - y_limits[0]

        # determine scaler, depending on the screen resolution
        scaler = 1000.0 * (self.width/1920)

        # scale offsets to diagram width / height and screen resoultion
        x_offset = x_offset * x_width / scaler
        y_offset = y_offset * y_width / scaler

        # shift the limits by offset values
        x_limits[0] = x_limits[0] - x_offset
        x_limits[1] = x_limits[1] - x_offset
        y_limits[0] = y_limits[0] - y_offset
        y_limits[1] = y_limits[1] - y_offset

        return (tuple(x_limits), tuple(y_limits))


    def update_diagram(self, master):
        # check if an update has to be carried out
        if (self.master.get_updateNeeded()):
            # get buffer idx for modifing the frames that are currently not visible
            if self.activeBufferIdx == 0:
                backgroundIdx = 1
            else:
                backgroundIdx = 0

            # get limits for active diagram
            (x_limits, y_limits) = self.get_limits()

            # update active diagram in background
            ax = self.axes[backgroundIdx][self.activeDiagram]

            # clear existing diagram
            ax.clear()

            # plot new diagram
            self.strak_machine.plot_diagram(self.activeDiagram, ax, x_limits, y_limits)

            # update figure
            figure = self.figures[backgroundIdx][self.activeDiagram]
            figure.canvas.draw()

            # show the updated frame
            frame = self.frames[backgroundIdx][self.activeDiagram]
            frame.tkraise()

            # switch buffer index
            self.activeBufferIdx = backgroundIdx

            # clear notification variable
            self.master.clear_updateNeeded()


    def change_diagram(self, diagram):
        if (self.activeDiagram != diagram):
            # set new active diagram
            self.activeDiagram = diagram
            # trigger update of diagram frame
            self.master.set_updateNeeded()


# main application
class App(customtkinter.CTk):
    def __init__(self, strak_machine):
        super().__init__()
        global controlFrame
        self.app_running = False

        # configure customtkinter
        customtkinter.set_appearance_mode("Dark")    # Modes: "System" (standard), "Dark", "Light"
        customtkinter.set_default_color_theme("blue") # Themes: "blue" (standard), "green", "dark-blue"

        # store strak_machine instance locally
        self.strak_machine = strak_machine

        # set window title
        self.title("The Strak machine")

        # maximize the window using state property
        self.state('zoomed')

        # call .on_closing() when app gets closed
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

        # set Index of airfoil, whose polar shall be editable
        self.airfoilIdx = 1

        # notification variable for updating the diagrams
        self.updateNeeded = 0

        # create diagram frame, which is on the right
        self.frame_right = diagram_frame(self, tk.RIGHT, self.strak_machine)

        # create control frame, which is on the left
        self.frame_left = control_frame(self, tk.LEFT,
         self.get_leftButtons(), self.get_rightButtons(), self.strak_machine)

        # set global variable
        controlFrame = self.frame_left



    def get_leftButtons(self):
        buttons = []
        buttons.append({"txt": "x=CD, y=CL", "cmd" : self.set_CL_CD_diagram})
        buttons.append({"txt": "x=alpha, y=CL", "cmd" : self.set_CL_alpha_diagram})
        buttons.append({"txt": "x=CL, y=CL/CD", "cmd" : self.set_CLCD_CL_diagram})
        return buttons


    def get_rightButtons(self):
        buttons = []
        buttons.append({"txt": "Load", "cmd" : self.load})
        buttons.append({"txt": "Save", "cmd" : self.save})
        buttons.append({"txt": "Reset", "cmd" : self.reset})
        return buttons

    def set_updateNeeded(self):
        self.updateNeeded = True

    def get_updateNeeded(self):
        return self.updateNeeded

    def clear_updateNeeded(self):
        self.updateNeeded = False

    def set_CL_CD_diagram(self):
        self.frame_right.change_diagram("CL_CD_diagram")

    def set_CL_alpha_diagram(self):
        self.frame_right.change_diagram("CL_alpha_diagram")

    def set_CLCD_CL_diagram(self):
        self.frame_right.change_diagram("CLCD_CL_diagram")

    def load(self):
        result = self.strak_machine.load(self.airfoilIdx)
        if (result == 0):
            self.strak_machine.update_targetPolars()
            self.frame_left.update_Entries(self.airfoilIdx)
            self.frame_left.update_GeoEntries(self.airfoilIdx)
            self.updateNeeded = True


    def save(self):
        self.strak_machine.save(self.airfoilIdx)


    def reset(self):
        result = self.strak_machine.reset(self.airfoilIdx)
        if (result == 0):
            self.strak_machine.update_targetPolars()
            self.frame_left.update_Entries(self.airfoilIdx)
            self.frame_left.update_GeoEntries(self.airfoilIdx)
            self.updateNeeded = True


    def start(self):
        self.app_running = True
        while self.app_running:
            self.update_idletasks()
            self.update()
            self.frame_left.update_visibleFlags()
            self.frame_left.update_referencePolarsFlag()
            self.frame_right.update_diagram(self)

        self.destroy()

    def on_closing(self, event=0):
        self.app_running = False

if __name__ == "__main__":
    # init colorama
    init()

     # init strakmachine
    NoteMsg("Starting Strak Machine...")
    try:
        myStrakmachine = strak_machine("ressources//strakdata.txt")
    except:
        ErrorMsg("Strak Machine could not be started")
        exit(-1)

    NoteMsg("Starting Graphical User Interface...")
    app = App(myStrakmachine)
    app.start()