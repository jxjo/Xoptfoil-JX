import tkinter as tk
import tkinter.messagebox
import customtkinter
import strak_machineV2
from strak_machineV2 import diagTypes
from strak_machineV2 import NoteMsg
from strak_machineV2 import ErrorMsg
from strak_machineV2 import strak_machine
from colorama import init

# imports to use matplotlib together with tkinter
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
from matplotlib.figure import Figure
import matplotlib.animation as animation

# some global variables
num_parameters = 14
num_diagrams = 3
polarsHaveChanged = 0

# Defines limits for shaping parameter raw values
limits = [
          { "min" : -0.5, "max" : 0.0}, # 0:  minCLGain
          { "min" : -0.3, "max" : 0.0}, # 1:  CL0Gain
          { "min" :  0.0, "max" : 0.5}, # 2:  maxSpeedGain
          { "min" :  0.0, "max" : 0.5}, # 3:  preMaxSpeedGain
          { "min" : -2.0, "max" : 1.0}, # 4:  maxGlideGain
          { "min" : -0.3, "max" : 1.0}, # 5:  maxLiftGain
          { "min" : -0.1, "max" : 0.1}, # 6:  maxGlideShift
          { "min" : -0.1, "max" : 0.0}, # 7:  maxSpeedShift
          { "min" :  0.0, "max" : 1.0}, # 8:  linearFactor_0
          { "min" :  0.0, "max" : 1.0}, # 9:  linearFactor_1
          { "min" :  0.0, "max" : 4.0}, # 10: linearFactor_2
          { "min" :  0.0, "max" : 0.3}, # 11: linearFactor_3
          { "min" :  0.0, "max" : 0.3}, # 12: linearFactor_4
          { "min" :  1.0, "max" : 1.02},# 13: maxGlideFactor
         ]

# Definition of slider labels
sliderLabels = [
          "minCLGain",       # 0
          "CL0Gain",         # 1
          "maxSpeedGain",    # 2
          "preMaxSpeedGain", # 3
          "maxGlideGain",    # 4
          "maxLiftGain",     # 5
          "maxGlideShift",   # 6
          "maxSpeedShift",   # 7
          "linearFactor_0",  # 8
          "linearFactor_1",  # 9
          "linearFactor_2",  # 10
          "linearFactor_3",  # 11
          "linearFactor_4",  # 12
          "maxGlideFactor"   # 13
           ]


# class shaping parameter, which is used to exchange parameters for shaping
# target polars between gui and strak machine
class shapingParameter():
    def __init__(self, idx, getter, setter, limits):
        self.idx = idx
        self.min = limits["min"]
        self.max = limits["max"]
        self.getter = getter
        self.setter = setter

        # get initial value
        self.value = self.get()

    def get(self):
        # call getter function (airfoilIdx, paramIdx)
        rawValue = self.getter(1, self.idx)
        # scale parameter
        sliderValue = self.scale_rawToSlider(rawValue)
        #print("get: raw value: %f, slider value: %f" % (rawValue, sliderValue))# FIXME Debug
        return sliderValue

    def set(self, sliderValue):
        global polarsHaveChanged
        # scale parameter
        rawValue = self.scale_sliderToRaw(sliderValue)
        # call setter function
        self.setter(1, self.idx, rawValue)
        #print("set: raw value: %f, slider value: %f" % (rawValue, sliderValue))# FIXME Debug
        # notify gui about the change
        polarsHaveChanged = 1


    def scale_rawToSlider(self, rawValue):
        sliderValue = self.interpolate_2(0.0, 1.0, self.min, self.max, rawValue)
        #print("rawToSlider: min: %f, max: %f" % (self.min, self.max))# FIXME Debug
        return sliderValue

    def scale_sliderToRaw(self, sliderValue):
        rawValue = self.interpolate_2(self.min, self.max, 0.0, 1.0, sliderValue)
        #print("sliderToRaw: min: %f, max: %f" % (self.min, self.max))# FIXME Debug
        return rawValue

    def interpolate_2(self, x1, x2, y1, y2, y):
        try:
            x = (y - y1)/((y2-y1)/(x2-x1)) + x1
        except:
            ErrorMsg("Division by zero!")
            x = 0.0
        return x

# class control frame, change the input-variables / parameters of the
# strak machine
class control_frame():
    def __init__(self, master, side, left_Buttons, right_Buttons, shapingParameters):
        self.frame = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)
        #self.frame.grid_columnconfigure(1, weight=1)

        # init nextRow (where to place next widget)
        self.nextRow = 1

        # add different widgets
        self.add_label()
        self.add_buttons(left_Buttons, right_Buttons)
        self.add_sliders(shapingParameters)

        # show frame
        self.frame.pack(side = side, fill=tk.BOTH, expand=1)

    def place_widgets(self, widget1, widget2):
        if widget1 != None:
            widget1.grid(row=self.nextRow, column=0, pady=10, padx=10)

        if widget2 != None:
            widget2.grid(row=self.nextRow, column=1, pady=0, padx=10)

        self.nextRow = self.nextRow + 1

    def add_label(self):
        # Label
        label = customtkinter.CTkLabel(master=self.frame,
                                              text="Select diagram",
                                              text_font=("Roboto Medium", -16))
        self.place_widgets(label, None)

    def add_buttons(self, left_Buttons, right_Buttons):
        buttonsLeft = []
        buttonsRight = []

        # create all buttons and add to list
        for button in left_Buttons:
            buttonsLeft.append(self.create_button(button))

        for button in right_Buttons:
            buttonsRight.append(self.create_button(button))


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

    def create_button(self, button):
        text = button["txt"]
        command = button["cmd"]

        # create new button
        button = customtkinter.CTkButton(master=self.frame,
                                            text=text,
                                            fg_color=("gray75", "gray30"),
                                            command=command)
        return button


    def add_sliders(self, shapingParameters):
        idx = 0

        # create slider for each of the shaping parameters
        for parameter in shapingParameters:
            command = parameter.set
            init_value = parameter.get()

            name = sliderLabels[idx]

            # create new slider
            slider = customtkinter.CTkSlider(master=self.frame, command=command)

            # init slider
            slider.set(init_value)

            label = customtkinter.CTkLabel(master=self.frame,
                                                   text=name ,
                                                   height=10,
                                                   fg_color=("white", "gray38"),
                                                   justify=tkinter.LEFT)

            #entry = customtkinter.CTkEntry(master=self.frame, width=20)

            # init entry
            #entry.set(init_value)

            # place slider and entry
            self.place_widgets(slider, label)
            idx = idx +1


    def add_blankRow(self):
        self.nextRow = self.nextRow + 1


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
            fig = Figure(figsize=(18, 10))
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
        if polarsHaveChanged == 1:
            #print("Update\n") #FIXME debug
            # perform update of the target polars first
            self.strak_machine.update_targetPolars()

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
            polarsHaveChanged = 0

            # switch buffers
            #self.activeBufferIdx = backgroundIdx
            #print(self.activeBufferIdx)# FIXME Debug

            # show the changes now
            #self.show_activeDiagram()

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
        customtkinter.set_appearance_mode("dark")     # Modes: "System" (standard), "Dark", "Light"
        customtkinter.set_default_color_theme("blue") # Themes: "blue" (standard), "green", "dark-blue"

        # store strak_machine instance locally
        self.strak_machine = strak_machine

        # set window title
        self.title("The Strak machine")

        # maximize the window using state property
        #self.state('zoomed')

        # call .on_closing() when app gets closed
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

        # create Variables for polar shaping
        self.create_inputVariables()

        # create diagram frame, which is on the right
        self.frame_right = diagram_frame(self.master, tk.RIGHT, self.strak_machine)

        # create control frame, which is on the left
        self.frame_left = control_frame(self.master, tk.LEFT,
         self.get_leftButtons(), self.get_rightButtons(), self.shapingParameters)


    def get_leftButtons(self):
        buttons = []
        buttons.append({"txt": "x=CD, y=CL", "cmd" : self.set_CL_CD_diagram})
        buttons.append({"txt": "x=alpha, y=CL", "cmd" : self.set_CL_alpha_diagram})
        buttons.append({"txt": "x=CL, y=CL/CD", "cmd" : self.set_CLCD_CL_diagram})
        return buttons

    def get_rightButtons(self):
        buttons = []
        buttons.append({"txt": "Load", "cmd" : self.load_strakdata})
        buttons.append({"txt": "Save", "cmd" : self.save_strakdata})
        return buttons

    def create_inputVariables(self):
        # configure getter and setter function
        getter_function = self.strak_machine.get_Param
        setter_function = self.strak_machine.set_Param

        # add input-variables
        self.shapingParameters = []
        for idx in range(num_parameters):
            self.shapingParameters.append(shapingParameter(idx, getter_function,
            setter_function, limits[idx]))


    def set_CL_CD_diagram(self):
        self.frame_right.change_diagram("CL_CD_diagram")

    def set_CL_alpha_diagram(self):
        self.frame_right.change_diagram("CL_alpha_diagram")

    def set_CLCD_CL_diagram(self):
        self.frame_right.change_diagram("CLCD_CL_diagram")

    def on_closing(self, event=0):
        self.destroy()

    def load_strakdata(self):
        print("Load strakdata")
        return

    def save_strakdata(self):
        print("Save strakdata")
        return

    def start(self):
        while True:
            self.update_idletasks()
            self.update()
            #self.frame_right.show_activeDiagram()
            self.frame_right.update_diagrams(self)

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