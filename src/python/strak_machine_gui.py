import tkinter
import tkinter.messagebox
import customtkinter
import strak_machineV2

customtkinter.set_appearance_mode("dark")      # Modes: "System" (standard), "Dark", "Light"
customtkinter.set_default_color_theme("blue")  # Themes: "blue" (standard), "green", "dark-blue"

class linearFactor():
    def __init__(self, idx):
        self.linearFactor = 0.0
        self.idx = idx

    def set(self, value):
        self.linearFactor = value
        print("LinearFactor %d: %f\n" %(self.idx, self.linearFactor))

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

class App(customtkinter.CTk):

    WIDTH = 1366
    HEIGHT = 768

    def __init__(self):
        super().__init__()

        self.title("The Strak machine")
        self.geometry(f"{App.WIDTH}x{App.HEIGHT}")

        # Maximize the window using state property
        self.state('zoomed')

        # call .on_closing() when app gets closed
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

        customtkinter.set_appearance_mode("dark")
        # ============ create two frames ============

        # configure grid layout (2x1)
        self.grid_columnconfigure(1, weight=1)
        self.grid_rowconfigure(0, weight=1)

        self.frame_left = customtkinter.CTkFrame(master=self,
                                                 width=180,
                                                 corner_radius=0)
        self.frame_left.grid(row=0, column=0, sticky="nswe")

        # add some input-variables
        self.linearFactor_1 = linearFactor(1) # slider 1
        self.linearFactor_2 = linearFactor(2) # slider 2
        self.linearFactor_3 = linearFactor(3) # slider 3
        self.linearFactor_4 = linearFactor(4) # slider 4
        self.linearFactor_5 = linearFactor(5) # slider 5
        self.gain_1 = gain(1)                 # slider 6
        self.gain_2 = gain(2)                 # slider 7
        self.gain_3 = gain(3)                 # slider 8
        self.gain_4 = gain(4)                 # slider 9
        self.gain_5 = gain(5)                 # slider 10
        self.gain_6 = gain(6)                 # slider 11
        self.shift_1 = shift(1)               # slider 12
        self.shift_2 = shift(2)               # slider 13


        self.frame_right = customtkinter.CTkFrame(master=self)
        self.frame_right.grid(row=0, column=1, sticky="nswe", padx=20, pady=20)

        # ============ frame_left ============
        # Label
        self.label_1 = customtkinter.CTkLabel(master=self.frame_left,
                                              text="Select diagram",
                                              text_font=("Roboto Medium", -16))

##        self.label_2 = customtkinter.CTkLabel(master=self.frame_left,
##                                            text="Change polar shape",
##                                            text_font=("Roboto Medium", -16))

        # Buttons to select diagram
        self.button_1 = customtkinter.CTkButton(master=self.frame_left,
                                                text="x=CD  y=CL",
                                                fg_color=("gray75", "gray30"),
                                                command=self.set_CL_CD_diagram)
        self.button_2 = customtkinter.CTkButton(master=self.frame_left,
                                                text="x=alpha  y=CL",
                                                fg_color=("gray75", "gray30"),
                                                command=self.set_CL_alpha_diagram)
        self.button_3 = customtkinter.CTkButton(master=self.frame_left,
                                                text="x=CL  y=CL/CD",
                                                fg_color=("gray75", "gray30"),
                                                command=self.set_CLCD_CL_diagram)

       # Sliders to change target polar shape
        self.slider_1 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.linearFactor_1.set)
        self.slider_2 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.linearFactor_2.set)
        self.slider_3 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.linearFactor_3.set)
        self.slider_4 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.linearFactor_4.set)
        self.slider_5 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.linearFactor_5.set)
        self.slider_6 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_1.set)
        self.slider_7 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_2.set)
        self.slider_8 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_3.set)
        self.slider_9 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_4.set)
        self.slider_10 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_5.set)
        self.slider_11 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.gain_6.set)
        self.slider_12 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.shift_1.set)
        self.slider_13 = customtkinter.CTkSlider(master=self.frame_left,
                                                command=self.shift_2.set)



        # Place the elements in he frame
        self.label_1.grid(row=1, column=0, pady=10, padx=10)
        self.button_1.grid(row=2, column=0, pady=10, padx=20)
        self.button_2.grid(row=3, column=0, pady=10, padx=20)
        self.button_3.grid(row=4, column=0, pady=10, padx=20)
##        self.label_2.grid(row=1, column=0, pady=10, padx=10)
        self.slider_1.grid(row=5, column=0, pady=10, padx=20, sticky="we")
        self.slider_2.grid(row=6, column=0, pady=10, padx=20, sticky="we")
        self.slider_3.grid(row=7, column=0, pady=10, padx=20, sticky="we")
        self.slider_4.grid(row=8, column=0, pady=10, padx=20, sticky="we")
        self.slider_5.grid(row=9, column=0, pady=10, padx=20, sticky="we")
        self.slider_6.grid(row=10, column=0, pady=10, padx=20, sticky="we")
        self.slider_7.grid(row=11, column=0, pady=10, padx=20, sticky="we")
        self.slider_8.grid(row=12, column=0, pady=10, padx=20, sticky="we")
        self.slider_9.grid(row=13, column=0, pady=10, padx=20, sticky="we")
        self.slider_10.grid(row=14, column=0, pady=10, padx=20, sticky="we")
        self.slider_11.grid(row=15, column=0, pady=10, padx=20, sticky="we")
        self.slider_12.grid(row=16, column=0, pady=10, padx=20, sticky="we")
        self.slider_13.grid(row=17, column=0, pady=10, padx=20, sticky="we")

        # ============ frame_right ============
        self.slider_1.set(0.2)

        # init strakmachine
        self.myStrakmachine = strak_machineV2.strak_machine("ressources//strakdata.txt")


    def set_CL_CD_diagram(self):
        self.myStrakmachine.show_diagram(1)

    def set_CL_alpha_diagram(self):
        self.myStrakmachine.show_diagram(2)

    def set_CLCD_CL_diagram(self):
        self.myStrakmachine.show_diagram(3)

    def on_closing(self, event=0):
        self.destroy()

    def start(self):
        self.mainloop()


if __name__ == "__main__":
    app = App()
    app.start()