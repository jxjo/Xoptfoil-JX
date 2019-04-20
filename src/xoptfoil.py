#!/usr/bin/python3

import sys
import os
from PyQt5 import QtWidgets

# FIXME: this will have to be set at install time. Also doesn't work if not
# run in the src directory, so obviously this is only temporary.
installdir = os.path.join(os.getcwd(), '..')
sys.path.append(os.path.join(installdir, 'ui'))

import mainwindow_ui
from optimizationsettings_dialog import OptimizationSettingsDialog
from xfoilsettings_dialog import XfoilSettingsDialog
from xfoilpanelingsettings_dialog import XfoilPanelingSettingsDialog
from plotsettings_dialog import PlotSettingsDialog
from data import Data

class XoptfoilMainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super(XoptfoilMainWindow, self).__init__()
        self.data = Data()
        self._currpath = os.getcwd()

        self.ui = mainwindow_ui.Ui_MainWindow()
        self.ui.setupUi(self)
        self.setWindowTitle("Xoptfoil")

        # Connect toolbar to canvas
        self.ui.mpltoolbar.setCanvas(self.ui.mplwidget)

        # Signals and slots
        self.ui.action_Load_seed_airfoil.triggered.connect(self.loadSeed)
        self.ui.action_Quit.triggered.connect(self.close)

        self.ui.actionOptimization_settings.triggered.connect(self.showOptimizationSettings)
        self.ui.actionXfoil.triggered.connect(self.showXfoilSettings)
        self.ui.action_Xfoil_paneling.triggered.connect(self.showXfoilPanelingSettings)
        self.ui.actionPlot_settings.triggered.connect(self.showPlotSettings)

    # Loads seed airfoil from file
    def loadSeed(self):
        fname, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Load seed airfoil",
                                                         self._currpath)
        if fname == "":
            return
        else:
            self._currpath = os.path.dirname(fname)

        retval = self.data.readSeedAirfoil(fname)
        errmsg = None
        if retval == 1:
            errmsg  = "Unable to read airfoil file {:s}: I/O error.".format(fname)
        elif retval == 2:
            errmsg  = "Unable to read airfoil file {:s}:".format(fname)
            errmsg += " Not a valid airfoil file."
        if errmsg is not None:
            QtWidgets.QMessageBox.critical(self, "Error", errmsg)
        self.ui.mplwidget.plotAirfoils(self.data.seed_airfoil)

    def showOptimizationSettings(self):
        dialog = OptimizationSettingsDialog()
        if dialog.exec():
            dialog.saveSettings()

    def showXfoilSettings(self):
        dialog = XfoilSettingsDialog()
        if dialog.exec():
            dialog.saveSettings()

    def showXfoilPanelingSettings(self):
        dialog = XfoilPanelingSettingsDialog()
        if dialog.exec():
            dialog.saveSettings()

    def showPlotSettings(self):
        dialog = PlotSettingsDialog()
        if dialog.exec():
            dialog.saveSettings()


if __name__ == "__main__":
    app = QtWidgets.QApplication([])
    application = XoptfoilMainWindow()
    application.show()
    sys.exit(app.exec())
