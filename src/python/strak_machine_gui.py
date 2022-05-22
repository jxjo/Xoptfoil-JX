import tkinter as tk
import tkinter.messagebox
import customtkinter
import strak_machineV2
from colorama import init

# imports to use matplotlib together with tkinter
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import (
    FigureCanvasTkAgg, NavigationToolbar2Tk)
from matplotlib.figure import Figure

# some global variables
numParameters = 14

# Defines limits for shaping parameter raw values
limits = [
          { "min" : -0.5, "max" : 0.0}, # 0:  minCLGain
          { "min" : -0.3, "max" : 0.0}, # 1:  CL0Gain
          { "min" :  0.0, "max" : 0.5}, # 2:  maxSpeedGain
          { "min" :  0.0, "max" : 0.5}, # 3:  preMaxSpeedGain
          { "min" : -2.0, "max" : 1.0}, # 4:  maxGlideGain
          { "min" : -0.3, "max" : 0.3}, # 5:  maxLiftGain
          { "min" : -0.1, "max" : 0.1}, # 6:  maxGlideShift
          { "min" : -0.1, "max" : 0.0}, # 7:  maxSpeedShift
          { "min" :  0.0, "max" : 1.0}, # 8:  linearFactor_0
          { "min" :  0.0, "max" : 1.0}, # 9:  linearFactor_1
          { "min" :  0.0, "max" : 4.0}, # 10: linearFactor_2
          { "min" :  1.5, "max" : 0.2}, # 11: linearFactor_3
          { "min" :  0.5, "max" : 0.1}, # 12: linearFactor_4
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
        # scale parameter
        rawValue = self.scale_rawToSlider(sliderValue)
        # call setter function
        self.setter(1, self.idx, rawValue)
        #print("set: raw value: %f, slider value: %f" % (rawValue, sliderValue))# FIXME Debug

    def scale_rawToSlider(self, rawValue):
        sliderValue = self.interpolate_2(self.min, self.max, 0.0, 1.0, rawValue)
        #print("rawToSlider: min: %f, max: %f" % (self.min, self.max))# FIXME Debug
        return sliderValue

    def scale_sliderToRaw(self, sliderValue):
        rawValue = self.interpolate(self.min, self.max, 0.0, 1.0, sliderValue)
        #print("sliderToRaw: min: %f, max: %f" % (self.min, self.max))# FIXME Debug
        return rawValue

    def interpolate(self, x1, x2, y1, y2, x):
        try:
            y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
        except:
            ErrorMsg("Division by zero, x1:%f, x2:%f", (x1, x2))
            y = 0.0
        return y

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
    def __init__(self, master, side, upperButtons, shapingParameters, lowerButtons):
        self.frame = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)
        #self.frame.grid_columnconfigure(1, weight=1)

        # init nextRow (where to place next widget)
        self.nextRow = 1

        # add different widgets
        self.add_label()
        self.add_buttons(upperButtons)
        self.add_sliders(shapingParameters)
        self.add_buttons(lowerButtons)

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

    def add_buttons(self, buttons):
        for button in buttons:
            text = button["txt"]
            command = button["cmd"]

            # create new button
            button = customtkinter.CTkButton(master=self.frame,
                                                text=text,
                                                fg_color=("gray75", "gray30"),
                                                command=command)
            # place button
            self.place_widgets(button, None)

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


# class diagram frame, shows the graphical output of the strak machine
class diagram_frame():
    def __init__(self, master, side, strak_machine):
        # store strak machine instance locally
        self.strak_machine = strak_machine

        # create new frame
        self.frame = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)
        # show up frame
        self.frame.pack(side = side, fill=tk.BOTH, expand=1)

        # set 'dark' style
        plt.style.use('dark_background')

        # new figure
        fig = Figure(figsize=(20, 4))#, dpi=100)
        self.ax = fig.add_subplot()

        # initial diagram
        self.strak_machine.plot_diagram(1, self.ax)

        # canvas
        self.canvas = FigureCanvasTkAgg(fig, self.frame)
        self.canvas._tkcanvas.pack(fill=tk.BOTH, expand=1)

        # Toolbar of Matplotlib
        self.toolbar = NavigationToolbar2Tk(self.canvas, self.frame)
        self.toolbar.update()

    def update(self, diagramType):
        self.ax.clear()
        self.strak_machine.plot_diagram(diagramType, self.ax)
        self.canvas.draw_idle()
        #self.frame.update()


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
        self.state('zoomed')

        # call .on_closing() when app gets closed
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

        # create Variables for polar shaping
        self.create_inputVariables()

        # set aktive Diagram initially (will be controlled by the buttons later)
        self.aktiveDiagram = 1

        # create control frame, which is on the left
        self.frame_left = control_frame(self.master, tk.LEFT,
         self.upperButtons(), self.shapingParameters, self.lowerButtons())

        # create diagram frame, which is on the right
        self.frame_right = diagram_frame(self.master, tk.RIGHT, self.strak_machine)

    def upperButtons(self):
        commands = []
        commands.append({"txt": "x=CD, y=CL", "cmd" : self.set_CL_CD_diagram})
        commands.append({"txt": "x=alpha, y=CL", "cmd" : self.set_CL_alpha_diagram})
        commands.append({"txt": "x=CL, y=CL/CD", "cmd" : self.set_CLCD_CL_diagram})

        return commands

    def lowerButtons(self):
        commands = []
        commands.append({"txt": "Load", "cmd" : self.load_strakdata})
        commands.append({"txt": "Save", "cmd" : self.save_strakdata})
        commands.append({"txt": "Update", "cmd" : self.update_diagram})

        return commands

    def create_inputVariables(self):
        global numTotal

        # configure getter and setter function
        getter_function = self.strak_machine.get_Param
        setter_function = self.strak_machine.set_Param

        # add input-variables
        self.shapingParameters = []
        for idx in range(numParameters):
            self.shapingParameters.append(shapingParameter(idx, getter_function,
            setter_function, limits[idx]))

    def set_CL_CD_diagram(self):
        self.aktiveDiagram = 1
        self.frame_right.update(self.aktiveDiagram)

    def set_CL_alpha_diagram(self):
        self.aktiveDiagram = 2
        self.frame_right.update(self.aktiveDiagram)

    def set_CLCD_CL_diagram(self):
        self.aktiveDiagram = 3
        self.frame_right.update(self.aktiveDiagram)

    def on_closing(self, event=0):
        self.destroy()

    def load_strakdata(self):
        print("Load strakdata")
        return

    def save_strakdata(self):
        print("Save strakdata")
        return

    def update_diagram(self):
        # perform update of the target polars first
        self.strak_machine.update_targetPolars()

        # redraw the aktive diagram
        self.frame_right.update(self.aktiveDiagram)


    def start(self):
        self.mainloop()


if __name__ == "__main__":
    # init colorama
    init()

     # init strakmachine
    strak_machineV2.NoteMsg("Starting Strak Machine...")
    try:
        myStrakmachine = strak_machineV2.strak_machine("ressources//strakdata.txt")
    except:
        strak_machineV2.ErrorMsg("Strak Machine could not be started")
        myStrakmachine = None

    strak_machineV2.NoteMsg("Starting Graphical User Interface...")
    app = App(myStrakmachine)
    app.start()