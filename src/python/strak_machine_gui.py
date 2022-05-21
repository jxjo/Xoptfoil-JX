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
num_linearFactors = 5
num_shiftValues = 2
num_gainValues = 6

class linearFactor():
    def __init__(self, idx):
        self.linearFactor = 0.0
        self.idx = idx

    def set(self, value):
        self.linearFactor = value
        print("LinearFactor %d: %f\n" %(self.idx, self.linearFactor))

    def init(self, initDict):
        self.set(0.2) #FIXME get value from dictionary

class shift():
    def __init__(self, idx):
        self.shift = 0.0
        self.idx = idx

    def set(self, value):
        self.shift = value
        print("shift %d: %f\n" %(self.idx, self.shift))

    def writeToDict(self):
        print("written")

class gain():
    def __init__(self, idx):
        self.gain = 0.0
        self.idx = idx

    def set(self, value):
        self.gain = value
        print("gain %d: %f\n" %(self.idx, self.gain))


# class control frame, change the input-variables / parameters of the
# strak machine
class control_frame():
    def __init__(self, master, side, buttonCommands, sliderCommands):
        self.frame = customtkinter.CTkFrame(master=master, width=180,
                                            corner_radius=0)
        # init nextRow (where to place next widget)
        self.nextRow = 1

        # add different widgets
        self.add_label()
        self.add_buttons(buttonCommands)
        self.add_sliders(sliderCommands)

        # show frame
        self.frame.pack(side = side, fill=tk.BOTH, expand=1)

    def place_widget(self, widget):
        widget.grid(row=self.nextRow, column=0, pady=10, padx=10)
        self.nextRow = self.nextRow + 1

    def add_label(self):
        # Label
        label = customtkinter.CTkLabel(master=self.frame,
                                              text="Select diagram",
                                              text_font=("Roboto Medium", -16))
        self.place_widget(label)

    def add_buttons(self, buttonCommands):
        for element in buttonCommands:
            text = element["txt"]
            command = element["cmd"]

            # create new button
            button = customtkinter.CTkButton(master=self.frame,
                                                text=text,
                                                fg_color=("gray75", "gray30"),
                                                command=command)
            # place button
            self.place_widget(button)

    def add_sliders(self, sliderCommands):
        for element in sliderCommands:
            command = element #FIXME

            # create new slider
            slider = customtkinter.CTkSlider(master=self.frame,
                                                command=command)

            # place slider
            self.place_widget(slider)

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
        fig = Figure(figsize=(12, 4), dpi=100)
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
        self.canvas.draw()

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

        # create Variables and commands
        self.create_inputVariables()
        self.init_inputVariables(None)#FIXME dictionary
        self.create_ButtonCommands()
        self.create_SliderCommands()

        # create two frames
        self.frame_left = control_frame(self.master, tk.LEFT,
                                   self.buttonCommands, self.sliderCommands)

        self.frame_right = diagram_frame(self.master, tk.RIGHT, self.strak_machine)


    def create_ButtonCommands(self):
        self.buttonCommands = []
        self.buttonCommands.append({"txt": "x=CD, y=CL", "cmd" : self.set_CL_CD_diagram})
        self.buttonCommands.append({"txt": "x=alpha, y=CL", "cmd" : self.set_CL_alpha_diagram})
        self.buttonCommands.append({"txt": "x=CL, y=CL/CD", "cmd" : self.set_CLCD_CL_diagram})

    def create_SliderCommands(self):
        global num_linearFactors
        global num_shiftValues
        global num_gainValues

        self.sliderCommands = []

        for idx in range(num_linearFactors):
            variable = self.linearFactors[idx]
            self.sliderCommands.append(variable.set)

        for idx in range(num_shiftValues):
            variable = self.shiftValues[idx]
            self.sliderCommands.append(variable.set)

        for idx in range(num_gainValues):
            variable = self.gainValues[idx]
            self.sliderCommands.append(variable.set)

    def create_inputVariables(self):
        global num_linearFactors
        global num_shiftValues
        global num_gainValues

        # add some input-variables
        self.linearFactors = []
        for idx in range(num_linearFactors):
            self.linearFactors.append(linearFactor(idx))

        self.shiftValues = []
        for idx in range(num_shiftValues):
            self.shiftValues.append(shift(idx))

        self.gainValues = []
        for idx in range(num_gainValues):
            self.gainValues.append(gain(idx))

    def init_inputVariables(self, init_dict):
        # init variables from dictionary
        for variable in self.linearFactors:
            variable.init(init_dict)

    def set_CL_CD_diagram(self):
        self.frame_right.update(1)

    def set_CL_alpha_diagram(self):
        self.frame_right.update(2)

    def set_CLCD_CL_diagram(self):
        self.frame_right.update(3)

    def on_closing(self, event=0):
        self.destroy()

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