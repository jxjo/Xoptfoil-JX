import tkinter as tk
from tkinter import ttk
import tkinter.messagebox
import customtkinter
import strak_machineV2
import os
from PIL import ImageTk, Image
from strak_machineV3 import diagTypes
from strak_machineV3 import NoteMsg
from strak_machineV3 import ErrorMsg
from strak_machineV3 import strak_machine
from colorama import init

# imports to use matplotlib together with tkinter
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
from matplotlib.figure import Figure
import matplotlib.animation as animation

# some global variables
num_diagrams = 3
controlFrame = None

# paths and separators
bs = "\\"
ressourcesPath = 'ressources'

# number of decimals in the generated input-files
CL_decimals = 5    # lift
CD_decimals = 6    # drag
CL_CD_decimals = 2 # lift/drag
AL_decimals = 5    # alpha

# name of logo-image
logoName = 'strakmachine.jpg'
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

        self.canvas = tk.Canvas(self.container, background=bg_color_scrollableFrame_dark)
        self.scrollbar = ttk.Scrollbar(self.container, orient="vertical", command=self.canvas.yview)
        #self.frame_bottom  = customtkinter.CTkFrame(self.canvas, width=180, corner_radius=0, background = "#000000")
        self.frame_bottom  = tk.Frame(self.canvas, width=180, background = bg_color_scrollableFrame_dark)
        self.frame_bottom.bind("<Configure>", self.OnFrameConfigure)

        self.canvas.create_window((10, 10), window=self.frame_bottom , anchor="nw")
        self.canvas.configure(yscrollcommand=self.scrollbar.set)
        self.canvas.pack(side="left", fill="both", expand=True)
        self.scrollbar.pack(side="right", fill="y")

        # init nextRow (where to place next widget)
        self.nextRow = 0

        # add the strak machine logo
        self.add_logo(self.frame_top)

        # add different widgets to upper frame (not scrollable)
        self.add_label(self.frame_top)
        self.add_buttons(self.frame_top, left_Buttons, right_Buttons)
        self.add_appearanceModeMenu(self.frame_top)
        self.add_airfoilChoiceMenu(self.frame_top)
        self.add_visiblePolarsCheckboxes(self.frame_top)

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

        img_width = int(323 * self.width/1920)
        img_height = int(87 * self.heigth/1080)

        # Resize the image in the given (width, height)
        sized_img = img.resize((img_width, img_height), Image.LANCZOS)

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

        return (mode, oppoint, target)


    def write_valuesToDict(self, idx, mode, oppoint, target):
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
        #targetValue["type"] = mode # FIXME We do not support changing mode at the moment
        self.targetValues[idx] = targetValue



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
                                              text="CL",
                                              text_font=("Roboto Medium", 13))
        target_label = customtkinter.CTkLabel(master=frame,
                                              text="CD",
                                              text_font=("Roboto Medium", 13))
        self.place_widgets(oppoint_label, target_label)

        # create entries and assign values
        for i in range(num_entries):
            # get dictionary containing oppoint / type / target value
            targetValue = self.targetValues[i]

            # get values from dictinory
            (mode, oppoint, target) = self.get_valuesFromDict(targetValue)
            if (mode == None):
                # error, continue with next entry
                continue

            # create text-Vars to interact with entries
            type_txt = tk.StringVar(frame, value=mode)
            oppoint_txt = tk.StringVar(frame, value=oppoint)
            target_txt = tk.StringVar(frame, value=target)

            self.textVars.append((type_txt, oppoint_txt, target_txt))

            # create entry for oppoint
            oppoint_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = oppoint_txt, text_font=('Roboto Medium', 11),
             width=100, height=16)

             # bind to "Enter"-Message
            oppoint_entry.bind('<Return>', self.update_TargetValues)

            # create entry for target
            target_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = target_txt, text_font=('Roboto Medium', 11),
             width=100, height=16)

            # bind to "Enter"-Message
            target_entry.bind('<Return>', self.update_TargetValues)

            # append both entries to list
            self.entries.append((oppoint_entry, target_entry))

            # if oppoint is 'spec-cl' place widget now
            if (mode == 'spec-cl'):
                self.place_widgets(oppoint_entry, target_entry)
            elif (mode == 'spec-al'):
                # append to list of spec-al entries
                spec_al_entries.append((oppoint_entry, target_entry))

        # Add Label
        oppoint_label = customtkinter.CTkLabel(master=frame,
                                              text="Alpha",
                                              text_font=("Roboto Medium", -16))
        target_label = customtkinter.CTkLabel(master=frame,
                                              text="CL",
                                              text_font=("Roboto Medium", -16))

        self.place_widgets(oppoint_label, target_label)
        # now place spec-al entries
        for entryTuple in spec_al_entries:
            # unpack tuple
            (oppoint_entry, target_entry) = entryTuple
            self.place_widgets(oppoint_entry, target_entry)


    def update_Entries(self, airfoilIdx):
        # get actual targetValues from strak machine
        self.targetValues = self.strak_machine.get_targetValues(airfoilIdx)

        idx = 0
        for element in self.textVars:
            # unpack tuple
            (type_txt, oppoint_txt, target_txt) = element

             # get values from dictinory
            (mode, oppoint, target) = self.get_valuesFromDict(self.targetValues[idx])

            # copy values to textvars
            type_txt.set(str(mode))
            oppoint_txt.set(str(oppoint))
            target_txt.set(str(target))
            idx = idx+1

    def change_targetValue(self, x, y, idx):
        if idx == None:
            return

        # read current value to get the mode
        (mode, oppoint, target) = self.get_valuesFromDict(self.targetValues[idx])
        # FIXME check: evaluate mode ?
        self.write_valuesToDict(idx, mode, y, x)

        # writeback dictionary to strakmachine
        self.strak_machine.set_targetValues(self.master.airfoilIdx, self.targetValues)

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
            (oppoint_entry, target_entry) = entryTuple

            # unpack tuple
            oppoint_entry = oppoint_entry.get()
            target_entry = target_entry.get()

            # get dictionary containing oppoint / type / target value
            targetValue = self.targetValues[idx]

            # get values from dictinory
            (mode, oppoint, target) = self.get_valuesFromDict(targetValue)

            # compare if something has changed
            if ((oppoint_entry != str(oppoint)) or (target_entry != str(target))):
                # write values to dictionary
                self.write_valuesToDict(idx, mode, float(oppoint_entry), float(target_entry))
                # set notification variable
                writeback_needed = True

            idx = idx + 1

        if (writeback_needed):
            # writeback dictionary to strakmachine
            self.strak_machine.set_targetValues(self.master.airfoilIdx, self.targetValues)

            # perform update of the target polars
            self.strak_machine.update_targetPolars()

            # notify the diagram frame about the change
            self.master.set_updateNeeded()


    def place_widgets(self, widget1, widget2):
        if widget1 != None:
            widget1.grid(row=self.nextRow, column=0, pady=5, padx=5, sticky="e")

        if widget2 != None:
            widget2.grid(row=self.nextRow, column=1, pady=5, padx=5, sticky="w")

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

    def on_closing(self, event=0):
        self.destroy()

class diagram(customtkinter.CTkFrame):

    def __init__(self, parent, controller, bufferIdx, fig):
        customtkinter.CTkFrame.__init__(self, parent)

        # canvas
        canvas = FigureCanvasTkAgg(fig, self)

        # Toolbar of Matplotlib
        toolbar = NavigationToolbar2Tk(canvas, self)
        toolbar.update()

        canvas._tkcanvas.pack(fill=tk.BOTH, expand=1)
        canvas.draw()

        # index of targetValue that shall be graphically edited
        self._ind = None
        self.controller = controller

        canvas.mpl_connect('button_press_event', self.on_button_press)
        canvas.mpl_connect('button_release_event', self.on_button_release)
        canvas.mpl_connect('motion_notify_event', self.on_mouse_move)


    def get_ind_under_point(self, event):
        """
        Return the index of the point closest to the event position or *None*
        if no point is within catching range to the event position.
        """
        global targetValues
        global controlFrame

        # set ranges to catch points
        catching_range_oppoint = 0.01
        catching_range_targetValue = 0.001

        mouse_target = event.xdata
        mouse_oppoint = event.ydata

        # check type of active diagram
        if (self.controller.activeDiagram == "CL_CD_diagram"):
            edit_mode = 'spec-cl'
            mouse_target = event.xdata
            mouse_oppoint = event.ydata
        elif (self.controller.activeDiagram == "CLCD_CL_diagram"):
            edit_mode = 'spec-cl'
            mouse_oppoint = event.xdata
            # convert y-coordinates, CL/CD -> CD
            mouse_target = event.xdata / event.ydata
        elif (self.controller.activeDiagram == "CL_alpha_diagram"):
            edit_mode == 'spec-al'
            # FIXME check if this is correct or must be swapped
            mouse_target = event.xdata
            mouse_oppoint = event.ydata


        # check visibility of editable polar
        if (controlFrame.check_activePolarVisibility() == False):
            return None

        # search entry with closest coordinates
        idx = 0
        for targetValue in controlFrame.targetValues:
            # get values from dictinory
            (mode, oppoint, target) = controlFrame.get_valuesFromDict(targetValue)

            if (mode != edit_mode):
                # not graphically editable in this diagram
                idx = idx + 1
                continue

            if ((abs(mouse_target - target) < catching_range_targetValue) and
                (abs(mouse_oppoint - oppoint) < catching_range_oppoint)):
                return idx

            idx = idx + 1
        return None


    def on_button_press(self, event):
        """Callback for mouse button presses."""
        if event.inaxes is None:
            return
        if event.button != 1:
            return
        self._ind = self.get_ind_under_point(event)


    def on_button_release(self, event):
        """Callback for mouse button releases."""
        if event.button != 1:
            return
        self._ind = None


    def on_mouse_move(self, event):
        """Callback for mouse movements."""
        global controlFrame
        if self._ind is None:
            return
        if event.inaxes is None:
            return
        if event.button != 1:
            return

        # check type of active diagram
        if (self.controller.activeDiagram == "CLCD_CL_diagram"):
            # convert coordinates
            x, y = event.ydata, event.xdata
            x = y/x
        else:
            x, y = event.xdata, event.ydata

        # set new target value
        controlFrame.change_targetValue(x,y,self._ind)


# class diagram frame, shows the graphical output of the strak machine
class diagram_frame():
    def __init__(self, master, side, strak_machine):
        # store strak machine instance locally
        self.strak_machine = strak_machine
        self.master = master
        self.figures = []
        self.axes = []

        # determine screen size
        self.width = self.master.winfo_screenwidth()
        self.heigth = self.master.winfo_screenheight()
        strak_machine.set_screenParams(self.width, self.heigth)

        # create figures initially (two of each kind for buffering)
        for i in range(2):
            (figures, axes) =  self.create_figures()
            self.figures.append(figures)
            self.axes.append(axes)

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

        # set 'dark' style
        plt.style.use('dark_background')

        for diagType in diagTypes:
            # new figure
            fig = Figure(figsize=(15, 16))
            ax = fig.add_subplot()

            # initial diagram
            self.strak_machine.plot_diagram(diagType, ax)

            # append to lists
            figures[diagType] = fig
            axes[diagType] = ax

        return (figures, axes)

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

    def update_diagram(self, master):
        # check if an update has to be carried out
        if (self.master.get_updateNeeded()):
            # get buffer idx for modifing the frames that are currently not visible
            if self.activeBufferIdx == 0:
                backgroundIdx = 1
            else:
                backgroundIdx = 0

            # get active diagram
            diagType = self.activeDiagram

            # update active diagram in background
            ax = self.axes[backgroundIdx][diagType]
            # clear existing diagram
            ax.clear()
            # plot new diagram
            self.strak_machine.plot_diagram(diagType, ax)
            # update figure
            figure = self.figures[backgroundIdx][diagType]
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
            self.activeDiagram = diagram
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
        global updateNeeded
        result = self.strak_machine.load(self.airfoilIdx)
        if (result == 0):
            self.targetValues = self.strak_machine.get_targetValues(self.airfoilIdx)
            self.strak_machine.update_targetPolars()
            self.frame_left.update_Entries(self.airfoilIdx)
            updateNeeded = True

    def save(self):
        self.strak_machine.save(self.airfoilIdx)

    def reset(self):
        global updateNeeded
        result = self.strak_machine.reset(self.airfoilIdx)
        if (result == 0):
            self.targetValues = self.strak_machine.get_targetValues(self.airfoilIdx)
            self.strak_machine.update_targetPolars()
            self.frame_left.update_Entries(self.airfoilIdx)
            updateNeeded = True

    def start(self):
        self.app_running = True
        while self.app_running:
            self.update_idletasks()
            self.update()
            self.frame_left.update_visibleFlags()
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