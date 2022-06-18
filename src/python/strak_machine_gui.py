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
polarsHaveChanged = 0

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

# class control frame, change the input-variables / parameters of the
# strak machine
class control_frame():
    def __init__(self, master, side, left_Buttons, right_Buttons, strak_machine):
        # store some variables in own class data structure
        self.strak_machine = strak_machine
        self.master = master

        self.frame_top = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)


        # Scrollable Frame
        self.container = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)

        self.canvas = tk.Canvas(self.container, background="#000000")
        self.scrollbar = ttk.Scrollbar(self.container, orient="vertical", command=self.canvas.yview)
        #self.frame_bottom  = customtkinter.CTkFrame(self.canvas, width=180, corner_radius=0, background = "#000000")
        self.frame_bottom  = tk.Frame(self.canvas, width=180, background = "#000000")
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
        self.frame_top.pack(side = 'top', fill=tk.BOTH, expand=1)

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

        # Resize the image in the given (width, height)
        sized_img = img.resize((333, 87), Image.LANCZOS)

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
        self.targetValues = self.strak_machine.get_targetValues(1)# FIXME select airfoil

        # init some strucures to store data locally
        self.entries = []
        self.textVars = []

        # determine number of entries
        num_entries = len(self.targetValues)

        # local variable to place spec-al entries in the frame
        spec_al_entries = []

        # Add Label
        oppoint_label = customtkinter.CTkLabel(master=frame,
                                              text="CL",
                                              text_font=("Roboto Medium", -16))
        target_label = customtkinter.CTkLabel(master=frame,
                                              text="CD",
                                              text_font=("Roboto Medium", -16))
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
             textvariable = oppoint_txt, text_font=('Roboto Medium', 8),
             width=80, height=16)

             # bind to "Enter"-Message
            oppoint_entry.bind('<Return>', self.update_TargetValues)

            # create entry for target
            target_entry = customtkinter.CTkEntry(frame, show=None,
             textvariable = target_txt, text_font=('Roboto Medium', 8),
             width=80, height=16)

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
            idx = idx +1


    def update_TargetValues(self, command):
        global polarsHaveChanged

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
            self.strak_machine.set_targetValues(1, self.targetValues)# FIXME only first airfoil supported at the moment

            # perform update of the target polars
            self.strak_machine.update_targetPolars()

            # notify the diagram frame about the change
            polarsHaveChanged = 2



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
        airfoilNames = self.strak_machine.get_airfoilNames()
        self.optionmenu_2 = customtkinter.CTkOptionMenu(master=frame,
                                                        values=airfoilNames[1:],
                                                        command=self.change_airfoil)
        self.place_widgets(self.label_airfoilChoice, self.optionmenu_2)


    def add_visiblePolarsCheckboxes(self, frame):
        self.checkBoxes = []
        self.visibleFlags = []
        self.lastVisibleFlags = []
        self.label_visiblePolars = customtkinter.CTkLabel(master=frame, text="Visible polars:")
        widget_1 = self.label_visiblePolars
        airfoilNames = self.strak_machine.get_airfoilNames()
        num = len(airfoilNames)
        idx = 0

        for airfoilName in airfoilNames:
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
        airfoilIdx = self.airfoilNames.index(airfoilName)
        self.master.airfoilIdx = airfoilIdx


    def update_visibleFlags(self):
        global polarsHaveChanged
        newVisibleFlags = []

        # read actual values
        num = len(self.visibleFlags)
        for idx in reversed(range(num)):
            newVisibleFlags.append(self.visibleFlags[idx].get())

        for idx in range (num):
            # check if something has changed
            if (self.lastVisibleFlags[idx] != newVisibleFlags[idx]):
                self.lastVisibleFlags.clear()
                self.lastVisibleFlags = newVisibleFlags

                # notify strak machine
                self.strak_machine.set_visiblePolars(newVisibleFlags)

                # notify the diagram frame about the change
                polarsHaveChanged = 2
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
##
##        #self.cid = self.add_callback(self.diagram_changed)
##        self._ind = None  # the active vert
##
##        canvas.mpl_connect('draw_event', self.on_draw)
##        canvas.mpl_connect('button_press_event', self.on_button_press)
##        canvas.mpl_connect('key_press_event', self.on_key_press)
##        canvas.mpl_connect('button_release_event', self.on_button_release)
##        canvas.mpl_connect('motion_notify_event', self.on_mouse_move)
##        self.canvas = canvas

    def on_draw(self, event):
        return
##        self.background = self.canvas.copy_from_bbox(self.ax.bbox)
##        self.ax.draw_artist(self.poly)
##        self.ax.draw_artist(self.line)
        # do not need to blit here, this will fire before the screen is
        # updated

    def diagram_changed(self, parent):
        """This method is called whenever the pathpatch object is called."""
        # only copy the artist props to the line (except visibility)
##        vis = self.line.get_visible()
##        Artist.update_from(self.line, poly)
##        self.line.set_visible(vis)  # don't use the poly visibility state
        return

    def get_ind_under_point(self, event):
        """
        Return the index of the point closest to the event position or *None*
        if no point is within ``self.epsilon`` to the event position.
        """
        # display coords
##        xy = np.asarray(self.poly.xy)
##        xyt = self.poly.get_transform().transform(xy)
##        xt, yt = xyt[:, 0], xyt[:, 1]
##        d = np.hypot(xt - event.x, yt - event.y)
##        indseq, = np.nonzero(d == d.min())
##        ind = indseq[0]
##
##        if d[ind] >= self.epsilon:
##            ind = None
##
##        return ind
        return

    def on_button_press(self, event):
##        """Callback for mouse button presses."""
##        if not self.showverts:
##            return
##        if event.inaxes is None:
##            return
##        if event.button != 1:
##            return
##        self._ind = self.get_ind_under_point(event)
        return

    def on_button_release(self, event):
        """Callback for mouse button releases."""
##        if not self.showverts:
##            return
##        if event.button != 1:
##            return
##        self._ind = None
        return

    def on_key_press(self, event):
        """Callback for key presses."""
##        if not event.inaxes:
##            return
##        if event.key == 't':
##            self.showverts = not self.showverts
##            self.line.set_visible(self.showverts)
##            if not self.showverts:
##                self._ind = None
##        elif event.key == 'd':
##            ind = self.get_ind_under_point(event)
##            if ind is not None:
##                self.poly.xy = np.delete(self.poly.xy,
##                                         ind, axis=0)
##                self.line.set_data(zip(*self.poly.xy))
##        elif event.key == 'i':
##            xys = self.poly.get_transform().transform(self.poly.xy)
##            p = event.x, event.y  # display coords
##            for i in range(len(xys) - 1):
##                s0 = xys[i]
##                s1 = xys[i + 1]
##                d = dist_point_to_segment(p, s0, s1)
##                if d <= self.epsilon:
##                    self.poly.xy = np.insert(
##                        self.poly.xy, i+1,
##                        [event.xdata, event.ydata],
##                        axis=0)
##                    self.line.set_data(zip(*self.poly.xy))
##                    break
##        if self.line.stale:
##            self.canvas.draw_idle()
        return

    def on_mouse_move(self, event):
        """Callback for mouse movements."""
##        if not self.showverts:
##            return
##        if self._ind is None:
##            return
##        if event.inaxes is None:
##            return
##        if event.button != 1:
##            return
##        x, y = event.xdata, event.ydata
##
##        self.poly.xy[self._ind] = x, y
##        if self._ind == 0:
##            self.poly.xy[-1] = x, y
##        elif self._ind == len(self.poly.xy) - 1:
##            self.poly.xy[0] = x, y
##        self.line.set_data(zip(*self.poly.xy))
##
##        self.canvas.restore_region(self.background)
##        self.ax.draw_artist(self.poly)
##        self.ax.draw_artist(self.line)
##        self.canvas.blit(self.ax.bbox)
        return



# class diagram frame, shows the graphical output of the strak machine
class diagram_frame():
    def __init__(self, master, side, strak_machine):
        # store strak machine instance locally
        self.strak_machine = strak_machine

        self.figures = []
        self.axes = []

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
        self.show_activeDiagram()


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

    def show_activeDiagram(self):
        frame = self.frames[self.activeBufferIdx][self.activeDiagram]
        frame.tkraise()

        # change Buffer index
        if self.activeBufferIdx == 0:
            self.activeBufferIdx = 1
        else:
            self.activeBufferIdx = 0

    def update_diagrams(self, master):
        global polarsHaveChanged

        # check if an update has to be carried out
        if polarsHaveChanged > 0:
            # get buffer idx for modifing the frames that are currently not visible
            if self.activeBufferIdx == 0:
                backgroundIdx = 1
            else:
                backgroundIdx = 0

            # update all diagrams
            for diagType in diagTypes:
                ax = self.axes[backgroundIdx][diagType]
                # clear existing diagram
                ax.clear()
                # plot new diagram
                self.strak_machine.plot_diagram(diagType, ax)
                # update figure
                figure = self.figures[backgroundIdx][diagType]
                figure.canvas.draw()

            # clear notification variable
            polarsHaveChanged = polarsHaveChanged - 1

    def change_diagram(self, diagram):
        if (self.activeDiagram != diagram):
            self.activeDiagram = diagram

            # show new diagram
            self.show_activeDiagram()



# main application
class App(customtkinter.CTk):
    def __init__(self, strak_machine):
        super().__init__()

        # configure customtkinter
        customtkinter.set_appearance_mode("Dark")    # Modes: "System" (standard), "Dark", "Light"
        customtkinter.set_default_color_theme("blue") # Themes: "blue" (standard), "green", "dark-blue"

        # store strak_machine instance locally
        self.strak_machine = strak_machine

        # set Index of airfoil, whose polar shall be editable
        self.airfoilIdx = 1

        # set window title
        self.title("The Strak machine")

        # maximize the window using state property
        self.state('zoomed')

        # call .on_closing() when app gets closed
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

        # create diagram frame, which is on the right
        self.frame_right = diagram_frame(self, tk.RIGHT, self.strak_machine)

        # create control frame, which is on the left
        self.frame_left = control_frame(self, tk.LEFT,
         self.get_leftButtons(), self.get_rightButtons(), self.strak_machine)


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


    def set_CL_CD_diagram(self):
        self.frame_right.change_diagram("CL_CD_diagram")

    def set_CL_alpha_diagram(self):
        self.frame_right.change_diagram("CL_alpha_diagram")

    def set_CLCD_CL_diagram(self):
        self.frame_right.change_diagram("CLCD_CL_diagram")

    def load(self):
        global polarsHaveChanged
        self.strak_machine.load(self.airfoilIdx)
        self.targetValues = self.strak_machine.get_targetValues(self.airfoilIdx)
        self.strak_machine.update_targetPolars()
        self.frame_left.update_Entries(self.airfoilIdx)
        polarsHaveChanged = 2

    def save(self):
        self.strak_machine.save(self.airfoilIdx)

    def reset(self):
        self.strak_machine.reset(self.airfoilIdx)

    def start(self):
        while True:
            self.update_idletasks()
            self.update()
            self.frame_left.update_visibleFlags()
            self.frame_right.update_diagrams(self)

    def on_closing(self, event=0):
        self.destroy()

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