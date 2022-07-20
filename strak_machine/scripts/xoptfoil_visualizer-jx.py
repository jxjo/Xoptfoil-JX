#!/usr/bin/env python

#  This file is part of XOPTFOIL.

#  XOPTFOIL is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  XOPTFOIL is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with XOPTFOIL.  If not, see <http://www.gnu.org/licenses/>.

#  Copyright (C) 2014 -- 2016 Daniel Prosser

import argparse
from matplotlib import pyplot as plt
from matplotlib import rcParams
import numpy as np
from math import log10, floor, sqrt
from sys import version_info
import time
import os

# Directory where all the Xoptfoil-FX data files coming from 

DESIGN_SUBDIR_POSTFIX = '_temp'


# Default plottiong options

plotoptions = dict(show_seed_airfoil = True,
                   show_seed_polar = True,
                   show_seed_airfoil_only = False,
                   show_seed_polar_only = False,
                   show_airfoil_info = True,
                   plot_airfoils = True,
                   plot_polars = True,
                   plot_optimization_history = True,
                   plot_curvature = True,
                   plot_3rd_derivative = True,
                   save_animation_frames = False,
                   color_for_seed = "dimgrey",
                   color_for_new_designs = "deeppink",
                   monitor_update_interval = 1.0)

################################################################################
#
# Airfoil class
#
################################################################################
class Airfoil:

  def __init__(self):
    self.name = ""
    self.x = np.zeros((0))
    self.y = np.zeros((0))
    self.maxt = 0.
    self.xmaxt = 0.
    self.maxc = 0.
    self.xmaxc = 0.
    self.alpha = np.zeros((0))
    self.cl = np.zeros((0))
    self.cd = np.zeros((0))
    self.cm = np.zeros((0))
    self.xtrt = np.zeros((0))
    self.xtrb = np.zeros((0))
    self.npt = 0
    self.noper = 0
    # jx-mod additional 2nd derivative
    self.deriv2 = np.zeros((0))
    self.deriv3 = np.zeros((0))
    # jx-mod additional glide and climb ratio, falpangle
    self.glide = np.zeros((0))
    self.climb = np.zeros((0))
    self.flapangle = np.zeros((0))
    # jx-mod a full Xfoil polar for reference
    self.full_polar_alpha = np.zeros((0))
    self.full_polar_cl = np.zeros((0))
    self.full_polar_cd = np.zeros((0))
    self.full_polar_cm = np.zeros((0))
    self.full_polar_xtrt = np.zeros((0))
    self.full_polar_xtrb = np.zeros((0))
    self.full_polar_glide = np.zeros((0))
    self.full_polar_climb = np.zeros((0))
    self.full_polar_re = 0.

  def setCoordinates(self, x, y):
    self.x = x
    self.y = y
    self.npt = x.shape[0]

  # jx-mod 
  def setDerivatives(self, deriv2, deriv3):
    self.deriv2 = deriv2
    self.deriv3 = deriv3
    iLE = np.argmin(self.x)
    self.deriv2[0:iLE] = np.multiply(self.deriv2[0:iLE], -1)
    self.deriv3[0:iLE] = np.multiply(self.deriv3[0:iLE], -1)

  def setGeometryInfo(self, maxt, xmaxt, maxc, xmaxc):
    self.maxt = maxt
    self.xmaxt = xmaxt
    self.maxc = maxc
    self.xmaxc = xmaxc

  def setName(self, name):
    self.name = name

  def setPolars(self, alpha, cl, cd, cm, xtrt, xtrb, flapangle):
    self.alpha = alpha
    self.cl = cl
    self.cd = cd
    self.cm = cm
    self.xtrt = xtrt
    self.xtrb = xtrb
    self.noper = alpha.shape[0]
    # jx-mod additional glide and climb ratio
    self.glide = self.cl / self.cd
    self.climb = np.zeros((len(cl)))
    for i in range(len(cl)):
      if cl[i] > 0.0:
        self.climb[i] = (cl[i]**1.5)/cd[i]
    self.flapangle = flapangle

  def setFullPolar (self, alpha, cl, cd, cm, xtrt, xtrb, re):
    self.full_polar_alpha = alpha
    self.full_polar_cl = cl
    self.full_polar_cd = cd
    self.full_polar_cm = cm
    self.full_polar_xtrt = xtrt
    self.full_polar_xtrb = xtrb
    self.full_polar_re = re
    self.full_polar_glide = cl / cd
    self.full_polar_climb = np.zeros((len(cl)))
    for i in range(len(cl)):
      if cl[i] > 0.0:
        self.full_polar_climb[i] = (cl[i]**1.5)/cd[i]

  def teGap (self):

    teGap = sqrt( (self.x[0]-self.x[-1])**2 + (self.y[0]-self.y[-1])**2)

    return teGap 

#------------------------------------------------------------------------------------
# returns max y value of y array which could be coordinates, 2nd or 3rd derivative
#    startfrom - x/c of chord to look at (backend part of airfoil)
# 
  def maxAbsyVal (self, y, startFrom, endAt):
    
    iLE = np.argmin(self.x)

    iEndTop     = iLE - np.searchsorted(np.sort(self.x[0:iLE]), startFrom)
    iStartTop   = iLE - np.searchsorted(np.sort(self.x[0:iLE]), endAt)
    iStartBot   = iLE + np.searchsorted(self.x[iLE:],  startFrom)
    iEndBot     = iLE + np.searchsorted(self.x[iLE:],  endAt)

    maxTop = np.abs(y[iStartTop:iEndTop]).max()
    maxBot = np.abs(y[iStartBot:iEndBot]).max()

    maxy = max( maxTop, maxBot) 

    return maxy

################################################################################
# Reads airfoil coordinates from file
def read_airfoil_coordinates(filename, zonetitle, designnum):

  ioerror = 0
  x = []
  y = []
  maxt = 0.
  xmaxt = 0.
  maxc = 0.
  xmaxc = 0.
  # jx-mod additionally 2nd and 3rd derivative
  deriv2 = []
  deriv3 = []
  name = ""


  # Try to open the file

  try:
    f = open(filename)
  except IOError:
    ioerror = 1
    return x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3, name

  # Read lines until we get to the correct zone

  zonefound = False
  zonelen = len(zonetitle)

  for textline in f:

    if (not zonefound):

      # Check for the zone we are looking for, and read geometry info

      if (textline[0:zonelen] == zonetitle):
        if (designnum != 0):
        # Example: zone t=" ... ", SOLUTIONTIME=1
          checkline = textline.split("SOLUTIONTIME=")
          checkdesign = int(checkline[1])
          if (checkdesign == designnum): zonefound = True
        else: zonefound = True

      if zonefound:
        # Example zone t="Airfoil, maxt=0.07599, ... xmaxc=0.35956", ...
        splitline = textline.split('"')[1].split(",")
        maxt  = float(get_argument (splitline, "maxt") or 0)
        xmaxt = float(get_argument (splitline, "xmaxt") or 0)
        maxc  = float(get_argument (splitline, "maxc") or 0)
        xmaxc = float(get_argument (splitline, "xmaxc")or 0)
        name  = get_argument (splitline, "name") 
        if (designnum != 0):
          name = name + " #" +str(designnum)

    else:

      # Check to see if we've read all the coordinates

      if (textline[0:4] == "zone"): break

      # Otherwise read coordinates

      else:

        line = textline.split()
        x.append(float(line[0]))
        y.append(float(line[1]))

        # jx-mod additionally 2nd and 3rd derivative
        if (len(line) > 2):
          deriv2.append(float(line[2]))
          deriv3.append(float(line[3]))

  # Error if zone has not been found after reading the file

  if (not zonefound):
    ioerror = 2

  # Close the file

  f.close()

  # Return coordinate data

  # jx-mod additionally 2nd and 3rd derivative
  return x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3,name

#------------------------------------------------------------------------------
# get argument value out of array of arguments
def get_argument (args,argname):

  argval = ""

  for argument in args:
    if (argument.split("=")[0].strip() == argname.strip()):
      argval = argument.split("=")[1].strip()
  return argval

################################################################################
# Reads airfoil polars from file
def read_airfoil_polars(filename, zonetitle):

  ioerror = 0
  alpha = []
  cl = []
  cd = []
  cm = []
  xtrt = []
  xtrb = []
  #jx-mod Read also flap angle from polar file
  flapangle = []

  # Try to open the file

  try:
    f = open(filename)
  except IOError:
    ioerror = 1
    return alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror

  # Read lines until we get to the correct zone

  zonefound = False
  zonelen = len(zonetitle)

  for textline in f:

    if (not zonefound):

      # Check for the zone we are looking for

      if (textline[0:zonelen] == zonetitle):

        zonefound = True

    else:

      # Check to see if we've read all the polars

      if (textline[0:4] == "zone"): break

      # Otherwise read polars

      else:

        line = textline.split()
        alpha.append(float(line[0]))
        cl.append(float(line[1]))
        cd.append(float(line[2]))
        cm.append(float(line[3]))
        xtrt.append(float(line[4]))
        xtrb.append(float(line[5]))

        #jx-mod Read flap-angle if exists
        if (len(line) > 6):
          flapangle.append(float(line[6]))

  # Error if zone has not been found after reading the file

  if (not zonefound):
    ioerror = 2

  # Close the file

  f.close()

  # Return polar data

  return alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror



################################################################################
# Reads optimization history
def read_optimization_history(prefix, step):

  ioerror = 0
  fmin = 0.
  relfmin = 0.
  rad = 0.

  # Try to open the file

  try:   
    f = open(os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Optimization_History.dat'))
  except IOError:
    ioerror = 1
    return fmin, relfmin, rad, ioerror

  # Read lines until we get to the step

  stepfound = False
  for textline in f:

    if (not stepfound):

      # Check for the step we are looking for

      splitline = textline.split()
      try:
        linestep = int(splitline[0])
      except ValueError:
        continue

      if (linestep == step):
        stepfound = True
        fmin = float(splitline[1])
        relfmin = float(splitline[2])
        rad = float(splitline[3])

  # Error if step has not been found after reading the file

  if (not stepfound):
    ioerror = 2

  # Close the file

  f.close()

  # Return optiimzation history data

  return fmin, relfmin, rad, ioerror



################################################################################
# Loads xfoil polar for a foil and certain designcounter
def load_full_polar (prefix, seedfoil, designfoils):

  filename = ''
  ioerror = 0

  # Read seed foil polar

  if (len(seedfoil.full_polar_alpha) == 0):
    filename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Seed_FullPolar.txt')
    designcounter = 0

    alpha, cl, cd, cm, xtrt, xtrb, re, ioerror = read_full_polar(filename, designcounter)

    if (ioerror == 2):
      print("Full polar for seed foil not found within file.")
    elif(ioerror == 0):
      seedfoil.setFullPolar (np.array(alpha), np.array(cl), np.array(cd), 
                              np.array(cm),np.array(xtrt), np.array(xtrb), re)
      print("Read polar Re=" + str(int(re)) +" for seed foil")

  # Read design foil polar

  if (len(designfoils) > 0 and len (designfoils[-1].full_polar_alpha) == 0 ):
    filename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Design_FullPolar.txt')
    designcounter = len(designfoils)

    alpha, cl, cd, cm, xtrt, xtrb, re, ioerror = read_full_polar(filename, designcounter)

    if (ioerror == 2):
      print("Full polar for design " + str(designcounter) + " not found within file.")
    elif(ioerror == 0):
      designfoils[designcounter-1].setFullPolar (np.array(alpha), np.array(cl), np.array(cd),
                              np.array(cm), np.array(xtrt), np.array(xtrb), re)
      print("Read polar Re=" + str(int(re)) +" for design #"+  str(designcounter))

  return seedfoil, designfoils

#-------------------------------------------------------------------------------
#
# Reads a complete airfoil polars from an Xfoil polar file. Format: 
#
#Xoptfoil-JX Design 123
#
# Calculated polar for: todo foilname
#
# 1 1 Reynolds number fixed          Mach number fixed
#
# xtrf =   1.000 (top)        1.000 (bottom)
# Mach =   0.000     Re =     0.600 e 6     Ncrit =   7.000
#
#  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr 
# ------- -------- --------- --------- -------- ------- ------- 
#  -2.000  -0.0160   0.00680   0.00000  -0.0430  0.9282  0.2707
#
def read_full_polar(filename, designcounter):

  ioerror = 0
  alpha = []
  cl = []
  cd = []
  cm = []
  xtrt =[]
  xtrb = []
  re = 0.

  # Try to open the file
  try:
    f = open(filename)
  except IOError:
    ioerror = 1
    return alpha, cl, cd, cm, xtrt, xtrb, re, ioerror

  # Read first line - check designcounter -return if its not the desired one '
  textline = f.readline()
  line = textline.split()
  if ((len (line) != 3) or (int(line[2]) != designcounter)):
    ioerror = 2
    return alpha, cl, cd, cm, xtrt, xtrb, re, ioerror
  textline = f.readline()
  textline = f.readline()
  textline = f.readline()
  textline = f.readline()
  textline = f.readline()
  textline = f.readline()
  # now we got the line with Re number 
  # Mach =   0.000     Re =     0.600 e 6     Ncrit =   7.000
  textline = f.readline()
  re =  float (textline.split('Re =')[1].split()[0]) * 1000000.0

  # Read lines until we get to the correct zone
  zonetitle = " -------"
  zonefound = False
  zonelen = len(zonetitle)
  last_alpha = -1000.0              # dummy start value

  for textline in f:
    if (not zonefound):
      # Check for the zone we are looking for
      if (textline[0:zonelen] == zonetitle): 
        zonefound = True
    else:

      # Otherwise read polars
      line = textline.split()
      current_alpha = float(line[0])

      # op points could be mixed up in order 
      if (current_alpha < last_alpha):
        alpha.insert(0,float(line[0]))
        cl.insert(0,float(line[1]))
        cd.insert(0,float(line[2]))
        cm.insert(0,float(line[4]))
        xtrt.insert(0,float(line[5]))
        xtrb.insert(0,float(line[6]))
      else:
        alpha.append(float(line[0]))
        cl.append(float(line[1]))
        cd.append(float(line[2]))
        cm.append(float(line[4]))
        xtrt.append(float(line[5]))
        xtrb.append(float(line[6]))

      last_alpha = current_alpha 

  # Error if zone has not been found after reading the file
  if (not zonefound):
    ioerror = 2

  f.close()

  return alpha, cl, cd, cm, xtrt, xtrb, re, ioerror



################################################################################
# Loads airfoil coordinates and polars from files
def load_airfoils_from_file(coordfilename, polarfilename):

  # Initialize output data

  seedfoil = Airfoil()
  designfoils = []
  ioerror = 0

  # Check for seed airfoil coordinates

  # print("Checking for airfoil coordinates file " + coordfilename + "...")

  zonetitle = 'zone t="Seed airfoil'

  # jx-mod additional 2nd and 3rd derivative
  x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3,name = read_airfoil_coordinates(
                                                    coordfilename, zonetitle, 0)
  if (ioerror == 1):
    print("Warning: file " + coordfilename + " not found.")
    return seedfoil, designfoils, ioerror
  elif (ioerror == 2):
    print("Error: zone labeled " + zonetitle + " not found in " + coordfilename
          + ".")
    return seedfoil, designfoils, ioerror

  seedfoil.setCoordinates(np.array(x), np.array(y))
  seedfoil.setGeometryInfo(maxt, xmaxt, maxc, xmaxc)
  # jx-mod additional 2nd and 3rd derivative
  seedfoil.setDerivatives(deriv2, deriv3)
  seedfoil.setName(name)

  # Read coordinate data for designs produced by optimizer

  print("Read coordinates from " + coordfilename + " ...", end=" ")

  read_finished = False
  counter = 1
  while (not read_finished):

    zonetitle = 'zone t="Airfoil'
    x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3,name = read_airfoil_coordinates(
                                              coordfilename, zonetitle, counter)
    if (ioerror == 2):
      read_finished = True
      numfoils = counter - 1
      ioerror = 0
    else:
      currfoil = Airfoil()
      currfoil.setCoordinates(np.array(x), np.array(y))
      currfoil.setGeometryInfo(maxt, xmaxt, maxc, xmaxc)

      # jx-mod additional 2nd and 3rd derivative
      currfoil.setDerivatives(deriv2, deriv3)
      currfoil.setName(name)


      designfoils.append(currfoil)
      counter += 1

  print(str(numfoils) + " designs plus seed airfoil.")

  # Read seed airfoil polars (note: negative error code means coordinates were
  # read but not polars)

  # print("Checking for airfoil polars file " + polarfilename + "...")

  zonetitle = 'zone t="Seed airfoil polar"'
  alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror = read_airfoil_polars(polarfilename,
                                                                      zonetitle)
  if (ioerror == 1):
    # print("Warning: file " + polarfilename + " not found.")
    return seedfoil, designfoils, 0 - ioerror
  elif (ioerror == 2):
    print("Error: zone labeled " + zonetitle + " not found in " + polarfilename
          + ".")
    return seedfoil, designfoils, 0 - ioerror

  seedfoil.setPolars(np.array(alpha), np.array(cl), np.array(cd), np.array(cm),
                     np.array(xtrt), np.array(xtrb), np.array(flapangle))

  # Read polar data for designs produced by optimizer

  print("Read op point data from " + polarfilename + "    ...", end=" ")

  read_finished = False
  counter = 1
  while (not read_finished):

    zonetitle = 'zone t="Polars", SOLUTIONTIME=' + str(counter)
    alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror = read_airfoil_polars(polarfilename,
                                                                      zonetitle)
    if (ioerror == 2):
      read_finished = True
      ioerror = 0
    else:
      designfoils[counter-1].setPolars(np.array(alpha), np.array(cl),
                                       np.array(cd), np.array(cm),
                                       np.array(xtrt), np.array(xtrb),
                                       np.array(flapangle))
      counter += 1

  print(str(counter-1) + " designs plus seed airfoil.")
  if (counter != numfoils+1):
    print("Error: number of airfoil coordinates and polars does not match.")
    ioerror = 3
    return seedfoil, designfoils, ioerror

  return seedfoil, designfoils, ioerror




################################################################################
# Plots airfoil coordinates
################################################################################

def plot_airfoil_coordinates(seedfoil, matchfoil, designfoils, plotnum, firsttime=True,
                             animation=False, prefix=None):
  global plotoptions

  # Set plot options ------

  plot_2nd_deriv  = plotoptions["plot_curvature"]     
  plot_3rd_deriv  = plotoptions["plot_3rd_derivative"] 
  plot_delta_y    = True                 # Plot delta of y ("z") value between current and seed
  plot_matchfoil  = True                 # Plot the matchfoil and the delta from current to match
  plot_seedfoil   = plotoptions["show_seed_airfoil_only"] or plotoptions["show_seed_airfoil"]
  plot_foil       = not plotoptions["show_seed_airfoil_only"]

  show_info       = plotoptions["show_airfoil_info"]
  show_transition = True                 # show transition points

  sc = plotoptions["color_for_seed"]
  nc = plotoptions["color_for_new_designs"]

  # --- end plot options

  # Sanity check of plot options

  if (plotnum > len(designfoils)): foil = designfoils[len(designfoils)-1]
  elif (plotnum > 0):              foil = designfoils[plotnum-1]
  else:                            
    foil  = Airfoil()

  plot_foil      = plot_foil and (plotnum > 0) and (len(foil.x) > 0)
  if not (plot_seedfoil or plot_foil): return

  plot_2nd_deriv  = plot_2nd_deriv  and (len(seedfoil.deriv2) > 0)
  plot_3rd_deriv  = plot_3rd_deriv  and (len(seedfoil.deriv3) > 0)
  plot_matchfoil  = plot_matchfoil and (matchfoil.npt > 0)
#  plot_seedfoil   = plot_seedfoil and (not plot_matchfoil)
  plot_delta_y    = plot_foil and plot_delta_y and plot_seedfoil and (not plot_matchfoil)
  plot_delta_y    = plot_delta_y and (np.array_equal(seedfoil.x, foil.x))
  show_transition = plot_foil and show_transition and (len(foil.xtrt > 0))

  # Set up coordinates plot, create figure and axes

  window_name = "Geometry  " + str(prefix)

  if firsttime:
    plt.close(window_name)
    cfig, dummy = plt.subplots(2, 1, num= window_name)
    cfig.subplots_adjust(hspace=0.1, bottom=0.07, left=0.08, right=0.92, top=0.98)

    try:                            # Qt4Agg
      plt.get_current_fig_manager().window.setGeometry(952,1,960,480) #position, size, 30px title assumed
    except:                         # TkAgg
      plt.get_current_fig_manager().window.wm_geometry("+952+1")      # position
      plt.get_current_fig_manager().resize(960, 480)                     # size
  else:
    cfig = plt.figure(num= window_name)
    if (len(cfig.get_axes()) == 0): exit()    # Window closed by user?
    cfig.clear()
    cfig.subplots (2,1)

  # Auto plotting bounds

  if ( plot_seedfoil and not plot_foil ):
    xmax = np.max(seedfoil.x)
    xmin = np.min(seedfoil.x)
  elif (plot_seedfoil and plot_foil):
    xmax = max([np.max(seedfoil.x), np.max(foil.x)])
    xmin = min([np.min(seedfoil.x), np.min(foil.x)])
  else:
    xmax = np.max(foil.x)
    xmin = np.min(foil.x)

  xrng = xmax - xmin
  xmax= xmax + 0.03*xrng
  xmin= xmin - 0.03*xrng


  # Plot 1 --------  airfoil coordinates, delta between seed and current airfoil

  ax  = cfig.get_axes()[0]
  ax.set_aspect('equal', 'datalim')
  ax.set_ylabel('y')
  ax.set_xlim([xmin,xmax])
  ax.grid (axis='x')
  ax.tick_params(labelbottom = False )
  ax.axhline(0, color='grey', linewidth=0.5)

  if plot_seedfoil: ax.plot(seedfoil.x, seedfoil.y, color=sc, linewidth=0.8)
  if plot_foil:     ax.plot(foil.x, foil.y, color=nc, linewidth=1)

  if plot_delta_y:  ax.plot(foil.x, (foil.y - seedfoil.y) * 5, color=nc, linewidth=0.8, linestyle=':')
  if plot_matchfoil:                # Plot matchfoil and delta to match foil 
    ax.plot(matchfoil.x, matchfoil.y, color='green', linewidth=0.8)
    if plot_foil:
      ax.plot(foil.x, (foil.y - matchfoil.y) * 10, color='green', linewidth=0.8, linestyle='-.')

  # Display geometry info

  if show_info:
    if plot_seedfoil:
      mytext = "Thickness: " + '{:.2%}'.format(seedfoil.maxt) + "  at: " + '{:.1%}'.format(seedfoil.xmaxt) + '\n' 
      if (seedfoil.maxc > 0):
        mytext = mytext + "Camber:    " + '{:.2%}'.format(seedfoil.maxc) + "  at: " + '{:.1%}'.format(seedfoil.xmaxc) + '\n' 
      else: 
        mytext = mytext + "symmetric" + '\n' 
      mytext = mytext + "TE gap:      " + '{:.2%}'.format(seedfoil.teGap())  

      ax.text(0.02, 0.90, mytext, color=sc, verticalalignment='center', horizontalalignment='left',
              transform=ax.transAxes, fontsize=8)
    if plot_foil:
      mytext = "Thickness: " + "{:.2%}".format(foil.maxt) + "  at: " + '{:.1%}'.format(foil.xmaxt) +  '\n'
      if (foil.maxc > 0):
        mytext = mytext + "Camber:    " + '{:.2%}'.format(foil.maxc) + "  at: " + '{:.1%}'.format(foil.xmaxc)
      else: 
        mytext = mytext + "symmetric"
      ax.text(0.98, 0.90, mytext, color=nc, verticalalignment='center', horizontalalignment='right',
              transform=ax.transAxes,  fontsize=8) 

  # show points of transition for the operating points

  if show_transition:
    iLE = np.argmin(seedfoil.x)
    plot_points_of_transition (ax, foil.x[0:iLE], foil.y[0:iLE], foil.xtrt, upperside = True)
    plot_points_of_transition (ax, foil.x[-iLE:] , foil.y[-iLE:], foil.xtrb, upperside = False)

    # legend for transition markers
    lines = []
    lines.append(plt.Line2D((0,1),(0,0), color=sc,label='Transition top',    linewidth=0.1, marker = 'v'))
    lines.append(plt.Line2D((0,1),(0,0), color=sc,label='bottom', linewidth=0.1, marker = '^'))

    labels = [l.get_label() for l in lines]
    # little trick to add a second legend to axis
    #  .. this legend will be removed from following legend and then added again ...
    legend2 = ax.legend (lines, labels, loc="upper center", numpoints=1, ncol=2)

  # Legend for coordinates plot

  lines = []
  if plot_seedfoil:  lines.append(plt.Line2D((0,1),(0,0), color=sc,label=seedfoil.name, linewidth=0.8))
  if plot_foil:      lines.append(plt.Line2D((0,1),(0,0), color=nc,label=foil.name))
  if plot_delta_y:   lines.append(plt.Line2D((0,1),(0,0), color=nc, linewidth=0.8, linestyle=':',
                                  label="Delta * 5"))
  if plot_matchfoil: 
    lines.append(plt.Line2D((0,1),(0,0), color='green', linewidth=0.8, label=matchfoil.name))
    lines.append(plt.Line2D((0,1),(0,0), color='green', linewidth=0.5, linestyle='-.',
                                  label="Delta to match * 10"))

  labels = [l.get_label() for l in lines]
  ax.legend(lines, labels, loc="lower center", numpoints=1, ncol=4)
  
  # little trick - see above
  if show_transition: ax.add_artist(legend2)


  # Plot 2 --------  Curvature and 3rd derivative 

  ax2 = cfig.get_axes()[1]
  mirrorax2 = ax2.twinx()
  mirrorax2 = ax2.get_shared_x_axes().get_siblings(ax2)[0]

  ax2.set_aspect('auto', 'datalim')
  ax2.set_xlim([xmin,xmax])
  ax2.grid (axis='x')
  ax2.axhline(0, color='grey', linewidth=0.5)

  if plot_2nd_deriv:
    ax2.set_ylabel('curvature', color='grey')

    # get max value of the backend part of airfoil and round it to pretty numbers
    if plot_foil:
      ymax =     foil.maxAbsyVal (    foil.deriv2, 0.15, 0.995)
    else:
      ymax = seedfoil.maxAbsyVal (seedfoil.deriv2, 0.15, 0.995)
    ymax = round (0.5 + ymax ,0) 
    ymax = max (min (ymax, 10), 1) 
    ax2.set_ylim(ymax, -ymax)

    if plot_seedfoil:   ax2.plot(seedfoil.x, seedfoil.deriv2, color=sc, linewidth=0.5) 
    if plot_foil:       ax2.plot(foil.x,     foil.deriv2,     color=nc, linewidth=1.0)

  if plot_3rd_deriv:
    mirrorax2.set_ylabel('3rd derivative', color='grey')

    # get max value of the backend part of airfoil and round it to pretty numbers
    if plot_foil:
      ymax =     foil.maxAbsyVal (    foil.deriv3, 0.15,0.995) 
    else:
      ymax = seedfoil.maxAbsyVal (seedfoil.deriv3, 0.15, 0.995) 
    ymax = round (0.5 + (ymax / 10),0) * 10
    ymax = max (min (ymax, 100), 10) 
    mirrorax2.set_ylim(-ymax, ymax)

    if plot_seedfoil:   mirrorax2.plot(seedfoil.x, seedfoil.deriv3, color=sc, linewidth=0.8, linestyle=':')
    if plot_foil:       mirrorax2.plot(foil.x,     foil.deriv3,     color=nc, linewidth=0.5, linestyle='--')

  # Legend for derivative plot

  lines = []
  if (plot_seedfoil and plot_2nd_deriv):
    lines.append(plt.Line2D((0,1),(0,0), color=sc, linewidth=0.8,  
                           label="Curvature " + seedfoil.name))
  if (plot_foil and plot_2nd_deriv):
    lines.append(plt.Line2D((0,1),(0,0), color=nc, linewidth=1, 
                           label="Curvature " + foil.name))
  if (plot_seedfoil and plot_3rd_deriv):
    lines.append(plt.Line2D((0,1),(0,0), color=sc, linewidth=0.8, linestyle=':', 
                           label="3rd derivative " + seedfoil.name))
  if (plot_foil and plot_3rd_deriv):
    lines.append(plt.Line2D((0,1),(0,0), color=nc, linewidth=0.8, linestyle='--', 
                           label="3rd derivative " + foil.name))

  labels = [l.get_label() for l in lines]
  ax2.legend(lines, labels, loc="lower center", numpoints=1, ncol=2)


  # Update plot for animation only (for others, plt.show() must be called
  # separately)

  if animation:
    if (firsttime): cfig.show()

    # Save animation frames if requested

    if plotoptions["save_animation_frames"]:
      if (prefix == None):
        print("Error: no file prefix specified - cannot save animation frames.")
      else:
        imagefname = prefix + '_coordinates.png'
        print("Saving image frame to file " + imagefname + ' ...')
        plt.savefig(imagefname)

  cfig.canvas.draw()

#---------------------------------------------------------------------------------------
# Plot points of transition xtrs along polyline x,y
#---------------------------------------------------------------------------------------
def plot_points_of_transition (axes, x, y, xtrs, upperside = True):

  for i in range(len(xtrs)):
    xtr = xtrs[i]
    # get best coordinate point wihich is closest to xtr point
    i_nearest = np.where(abs(x-xtr)==abs(x-xtr).min())[0][0]

    if upperside:
      my_marker = 7
      y_text = 7
    else:
      my_marker = 6
      y_text = -13

    axes.plot([x[i_nearest]], [y[i_nearest]], marker=my_marker, markersize=7, color="grey")
    axes.annotate(('{:d}'.format(i+1)), xy = (x[i_nearest], y[i_nearest]),
                  xytext = (-3,y_text), textcoords="offset points", fontsize = 8, color='dimgrey')




################################################################################
# Plots polars
################################################################################

def plot_polars(seedfoil, designfoils, plotnum, firsttime=True, animation=False,
                prefix=None):

  global plotoptions

  # Set plot options ------

  plot_polar       = True                 # Plot polar of current
  plot_seed_polar  = plotoptions["show_seed_polar_only"] or plotoptions["show_seed_polar"]

  show_flap_angle  = True                 # show flap angle if available
  plot_full_polar  = False                # will be set if a full polar exists

  sc = plotoptions["color_for_seed"]
  nc = plotoptions["color_for_new_designs"]

  # --- end plot options

  # Sanity check of plot options

  if (len(seedfoil.alpha) == 0):  plot_seed_polar   = False

  if (plotnum > 0):
    foil = designfoils[plotnum-1]
    if (len(foil.alpha) == 0):
      plot_polar   = False
    else:
      if (plotnum == 1):
        prev_foil = seedfoil
      else:
        prev_foil = designfoils[plotnum-2]
  else:
    plot_polar   = False
    foil  = Airfoil()

  if not (plot_seed_polar or plot_polar): return

  if plot_polar: plot_full_polar = len(foil.full_polar_alpha) > 0


  # Set up polars plot.

  window_name = "Polars  " + str(prefix)

  if firsttime:
    plt.close(window_name)
    pfig, dummy = plt.subplots(2, 3, num= window_name)
    pfig.subplots_adjust(hspace=0.28, wspace=0.28,  bottom=0.10, left=0.08, right=0.96, top=0.94)
    try:                            # Qt4Agg
      plt.get_current_fig_manager().window.setGeometry(952,516,960,480) #position, size 30px title assumed
    except:                         # TkAgg
      plt.get_current_fig_manager().window.wm_geometry("+952+516")    # position
      plt.get_current_fig_manager().resize(960, 480)                # size
  else:
    pfig = plt.figure(num= window_name)
    if (len(pfig.get_axes()) == 0): exit()     # User closed the window - stop
    pfig.clear()
    pfig.subplots (2,3)

  axarr = pfig.get_axes()


  # Auto plotting bounds - not optimal - the merge of the polar data could lead to wrong boundaries

  clmin = np.min(np.concatenate((seedfoil.cl, foil.cl, foil.full_polar_cl)))
  clmax = np.max(np.concatenate((seedfoil.cl, foil.cl, foil.full_polar_cl)))
  almin = np.min(np.concatenate((seedfoil.alpha, foil.alpha, foil.full_polar_alpha)))
  almax = np.max(np.concatenate((seedfoil.alpha, foil.alpha, foil.full_polar_alpha)))
  cdmin = np.min(np.concatenate((seedfoil.cd, foil.cd, foil.full_polar_cd)))
  cdmax = np.max(np.concatenate((seedfoil.cd, foil.cd, foil.full_polar_cd)))
  cmmin = np.min(np.concatenate((seedfoil.cm, foil.cm, foil.full_polar_cm)))
  cmmax = np.max(np.concatenate((seedfoil.cm, foil.cm, foil.full_polar_cm)))
  xtrmax = np.max(np.concatenate((seedfoil.xtrt, foil.xtrt, foil.full_polar_xtrt,
                                  seedfoil.xtrb, foil.xtrb, foil.full_polar_xtrb)))
  xtrmin = np.min(np.concatenate((seedfoil.xtrt, foil.xtrt, foil.full_polar_xtrt,
                                  seedfoil.xtrb, foil.xtrb, foil.full_polar_xtrb)))
  glidemin = np.min(np.concatenate((seedfoil.glide, foil.glide, foil.full_polar_glide)))
  glidemax = np.max(np.concatenate((seedfoil.glide, foil.glide, foil.full_polar_glide)))
  climbmin = np.min(np.concatenate((seedfoil.climb, foil.climb, foil.full_polar_climb)))
  climbmax = np.max(np.concatenate((seedfoil.climb, foil.climb, foil.full_polar_climb)))

  alrng = almax - almin
  almax = almax + 0.1*alrng
  almin = almin - 0.1*alrng
  cdrng = cdmax - cdmin
  # cdmax = cdmax + 0.0*cdrng          # use full range
  cdmax = min ((cdmin * 3), cdmax)     # cd tends to have a wide range - limit it ...
  # cdmin = cdmin - 0.1*cdrng
  cdmin = cdmin - 0.3 * cdmin
  clrng = clmax - clmin
  clmax = clmax + 0.0*clrng          # use full range
  clmin = clmin - 0.1*clrng
  cmrng = cmmax - cmmin
  cmmax = cmmax + 0.1*cmrng
  cmmin = cmmin - 0.1*cmrng
  xtrrng = xtrmax - xtrmin
  xtrmax = xtrmax + 0.1*xtrrng
  xtrmin = xtrmin - 0.1*xtrrng
  gliderng = glidemax - glidemin
  glidemax = glidemax + 0.1*gliderng
  glidemin = max ((glidemin - 0.1*gliderng, 0.0))
  climbrng = climbmax - climbmin
  climbmax = climbmax + 0.1*climbrng
  climbmin = max ((climbmin - 0.1*climbrng, 0.0))
  glideclmin = max ((clmin, 0.0))

  # Plot polars

  if plot_full_polar:
    plot_single_polar (axarr[0], seedfoil.full_polar_alpha, seedfoil.full_polar_cl, '--', sc, '')
    plot_single_polar (axarr[1], seedfoil.full_polar_cd,    seedfoil.full_polar_cl, '--', sc, '')
    plot_single_polar (axarr[3], seedfoil.full_polar_alpha, seedfoil.full_polar_cm, '--', sc, '')
    plot_single_polar (axarr[2], seedfoil.full_polar_cl,    seedfoil.full_polar_glide, '--', sc, '')
    plot_single_polar (axarr[4], seedfoil.full_polar_xtrt,  seedfoil.full_polar_cl , '--', sc, '')
    plot_single_polar (axarr[4], seedfoil.full_polar_xtrb,  seedfoil.full_polar_cl , ':', sc, '')
    plot_single_polar (axarr[5], seedfoil.full_polar_cl,    seedfoil.full_polar_climb, '--', sc, '')

    plot_single_polar (axarr[0], foil.full_polar_alpha, foil.full_polar_cl, '--', nc, '')
    plot_single_polar (axarr[1], foil.full_polar_cd,    foil.full_polar_cl, '--', nc, '')
    plot_single_polar (axarr[3], foil.full_polar_alpha, foil.full_polar_cm, '--', nc, '')
    plot_single_polar (axarr[2], foil.full_polar_cl,    foil.full_polar_glide, '--', nc, '')
    plot_single_polar (axarr[4], foil.full_polar_xtrt,  foil.full_polar_cl , '--', nc, '')
    plot_single_polar (axarr[4], foil.full_polar_xtrb,  foil.full_polar_cl , ':', nc, '')
    plot_single_polar (axarr[5], foil.full_polar_cl,    foil.full_polar_climb, '--', nc, '')
    linestyle = 'None'
  else:
    linestyle = 'None'        #  linestyle = '-' No more lines to connect op points

  if plot_seed_polar :
    plot_single_polar (axarr[0], seedfoil.alpha,  seedfoil.cl,    linestyle, sc, 'o')
    plot_single_polar (axarr[1], seedfoil.cd,     seedfoil.cl,    linestyle, sc, 'o')
    plot_single_polar (axarr[3], seedfoil.alpha,  seedfoil.cm,    linestyle, sc, 'o')
    plot_single_polar (axarr[4], seedfoil.xtrt,   seedfoil.cl,    linestyle, sc, 'v')
    plot_single_polar (axarr[4], seedfoil.xtrb,   seedfoil.cl,    linestyle, sc, '^')
    plot_single_polar (axarr[2], seedfoil.cl,     seedfoil.glide, linestyle, sc, 'o')
    plot_single_polar (axarr[5], seedfoil.cl,     seedfoil.climb, linestyle, sc, 'o')


  if plot_polar:
    plot_single_polar (axarr[0], foil.alpha,  foil.cl,    linestyle, nc, 's')
    plot_single_polar (axarr[1], foil.cd,     foil.cl,    linestyle, nc, 's')
    plot_single_polar (axarr[3], foil.alpha,  foil.cm,    linestyle, nc, 's')
    plot_single_polar (axarr[4], foil.xtrt,   foil.cl,    linestyle, nc, 'v')
    plot_single_polar (axarr[4], foil.xtrb,   foil.cl,    linestyle, nc, '^')
    plot_single_polar (axarr[2], seedfoil.cl, foil.glide, linestyle, nc, 's')
    plot_single_polar (axarr[5], seedfoil.cl, foil.climb, linestyle, nc, 's')

    annotate_changes (axarr[0], prev_foil.alpha,prev_foil.cl,    foil.alpha,    foil.cl,    "y")
    annotate_changes (axarr[1], prev_foil.cd,   prev_foil.cl,    foil.cd,       foil.cl,    "x")
    annotate_changes (axarr[2], prev_foil.cl,   prev_foil.glide, foil.cl,       foil.glide, "y")
    annotate_changes (axarr[3], prev_foil.alpha,prev_foil.cm,    foil.alpha,    foil.cm,    "y")
    annotate_changes (axarr[4], prev_foil.xtrt, prev_foil.cl,    foil.xtrt,     foil.cl,    "x")
    annotate_changes (axarr[4], prev_foil.xtrb, prev_foil.cl,    foil.xtrb,     foil.cl,    "x")
    annotate_changes (axarr[5], prev_foil.cl,   prev_foil.climb, foil.cl,       foil.climb, "y")

    # show  flap anglein graph

    if (show_flap_angle):
      for i in range(len(foil.cl)):
        if ((len(foil.flapangle) > 0) and (foil.flapangle[i] != 0)):
          axarr[1].annotate(('f {:5.2f}'.format(foil.flapangle[i])), (cdmax - 0.29*cdrng, foil.cl[i]),
                            fontsize = 8,color='dimgrey')

  # set axis

  axarr[0].set_xlabel('alpha')
  axarr[0].set_ylabel('cl')
  axarr[0].set_xlim([almin,almax])
  axarr[0].set_ylim([clmin,clmax])
  axarr[0].grid(True)

  axarr[1].set_xlabel('cd')
  axarr[1].set_ylabel('cl')
  axarr[1].set_xlim([cdmin,cdmax])
  axarr[1].set_ylim([clmin,clmax])
  axarr[1].set_ylim([clmin, 1])
  axarr[1].grid(True)

  axarr[3].set_xlabel('alpha')
  axarr[3].set_ylabel('cm')
  axarr[3].set_xlim([almin,almax])
  axarr[3].set_ylim([cmmin,cmmax])
  axarr[3].grid(True)

  axarr[4].set_xlabel('Transition x/c')
  axarr[4].set_ylabel('cl')
  axarr[4].set_xlim([xtrmin,xtrmax])
  axarr[4].set_ylim([clmin,clmax])
  axarr[4].grid(True)

  axarr[2].set_xlabel('cl')
  axarr[2].set_ylabel('glide ratio')
  axarr[2].set_xlim([glideclmin,clmax])
  axarr[2].set_ylim([glidemin,glidemax])
  axarr[2].grid(True)

  axarr[5].set_xlabel('cl')
  axarr[5].set_ylabel('climb ratio')
  axarr[5].set_xlim([glideclmin,clmax])
  axarr[5].set_ylim([climbmin,climbmax])
  axarr[5].grid(True)

  # Draw legend for all plots 

  lines = []

  if plot_seed_polar:
    fakeline = plt.Line2D((0,1),(0,0), linestyle='-', color=sc, marker='o', 
                          linewidth=0.1, label=seedfoil.name)
    lines.append(fakeline)

  if plot_full_polar:
    fakeline = plt.Line2D((0,1),(0,0), linestyle='--', color=sc, marker='', 
                          linewidth=1.0, 
                          label='Polar Re='+str(int(foil.full_polar_re/1000))+'k')
    lines.append(fakeline)

  if plot_polar:
    fakeline = plt.Line2D((0,1),(0,0), linestyle='-', color=nc, marker='s', 
                          linewidth=0.1, label=foil.name)
    lines.append(fakeline)

  if plot_full_polar:
    fakeline = plt.Line2D((0,1),(0,0), linestyle='--', color=nc, marker='', 
                          linewidth=1.0, 
                          label='Polar Re='+str(int(foil.full_polar_re/1000))+'k')
    lines.append(fakeline)

  bbox_loc = (0.5, 1.00)
  labels = [l.get_label() for l in lines]
  pfig.legend(lines, labels, loc="upper center", ncol=4,
              bbox_to_anchor=bbox_loc, numpoints=1)

  # Draw legend for transition plot 

  lines = []
  lines.append(plt.Line2D((0,1),(0,0), color=sc,label='top',    linewidth=0.1, marker = 'v'))
  lines.append(plt.Line2D((0,1),(0,0), color=sc,label='bottom', linewidth=0.1, marker = '^'))

  labels = [l.get_label() for l in lines]
  axarr[4].legend(lines, labels, loc="upper center", numpoints=1, ncol=2)

  # Update plot for animation only (for others, plt.show() must be called separately)

  if animation:
    if (firsttime): pfig.show()

    # Save animation frames if requested

    if plotoptions["save_animation_frames"]:
      if (prefix == None):
        print("Error: no file prefix specified - cannot save animation frames.")
      else:
        imagefname = prefix + '_polars.png'
        print("Saving image frame to file " + imagefname + ' ...')
        plt.savefig(imagefname)

  pfig.canvas.draw()

  return

#---------------------------------------------------------------------------------------
# Plots a single polar
#---------------------------------------------------------------------------------------
def plot_single_polar (axes, x, y, my_linestyle, my_color, my_marker):

  axes.plot(x, y, linestyle=my_linestyle, color=my_color, 
            marker=my_marker, linewidth=0.8)


#---------------------------------------------------------------------------------------
# Annotate marker depending value increased or decreased
#
#    change_dir == "x""  watch the x- value - else watch the y_value for changes
#
#---------------------------------------------------------------------------------------
def annotate_changes (axes, prev_x, prev_y, x, y, change_dir):

  for i in range(len(x)):

    if   (change_dir == "x") and (prev_x[i] != 0):
      rel_improv = (x[i] - prev_x[i]) / prev_x[i]
    elif (change_dir == "y") and (prev_y[i] != 0):
      rel_improv = (y[i] - prev_y[i]) / prev_y[i]
    else:
      rel_improv = 0


    if (abs(rel_improv) > 1e-4):            # show annotation only if delta > epsilon

      if (change_dir == "x"):
        if (rel_improv > 0.0):
          axes.annotate('>', xy = (x[i], y[i]),
                xytext = (5,-2),   textcoords="offset points", fontsize = 8, color='dimgrey')
        else:
          axes.annotate('<', xy = (x[i], y[i]),
                xytext = (-12,-2), textcoords="offset points", fontsize = 8, color='dimgrey')
      else:
        if (rel_improv > 0.0):
          axes.annotate('^', xy = (x[i], y[i]),
                xytext = (-3,3),   textcoords="offset points", fontsize = 8, color='dimgrey')
        else:
          axes.annotate('v', xy = (x[i], y[i]),
                xytext = (-2,-10), textcoords="offset points", fontsize = 8, color='dimgrey')



################################################################################
# Plots optimization history
################################################################################

def plot_optimization_history(steps, fmins, relfmins, rads, firsttime=True,
                              animation=False, prefix=None):
  global plotoptions

  if (len(steps) == 0): return           # nothing to show

  # Set up optimization history plot.

  window_name = "Optimization History  " + str(prefix)

  nc = plotoptions["color_for_new_designs"]

  if firsttime:
    plt.close(window_name)
    ofig, dummy  = plt.subplots(2, 1, num= window_name)
    ofig.subplots_adjust(hspace=0.1, bottom=0.12, right=0.88, left=0.2, top=0.92)

    axarr = ofig.get_axes()
    mirrorax0   = axarr[0].twinx()
    try:                            # Qt4Agg
      plt.get_current_fig_manager().window.setGeometry(520,1,440,290)
    except:                         # TkAgg
      plt.get_current_fig_manager().window.wm_geometry("+520+1")    # position
      plt.get_current_fig_manager().resize(440, 290)                  # size
  else:
    ofig  = plt.figure(num= window_name)
    axarr = ofig.get_axes()
    if (len(axarr) == 0): exit()            # User closed the window - stop
    mirrorax0 = axarr[0].get_shared_x_axes().get_siblings(axarr[0])[0]
    axarr[0].clear()
    mirrorax0.clear()
    axarr[1].clear()

  # Plot optimization history

  axarr[0].plot(steps, fmins, color='grey', linewidth=0.8)
  mirrorax0.plot(steps, relfmins, color=nc, linewidth=1)
  axarr[1].plot(steps, rads, color='grey', linewidth=0.8)

  axarr[0].tick_params(labelbottom = False )
  axarr[0].set_ylabel('Objective function')
  mirrorax0.set_ylabel('% Improvement', color=nc)
  axarr[0].grid(axis='x')
  # axarr[1].set_xlabel('Iteration')
  axarr[1].set_ylabel('Design radius')
  axarr[1].set_yscale("log")
  axarr[1].grid()

  print_improvement (mirrorax0, steps, relfmins)

  # Update plot for animation only (for others, plt.show() must be called separately)

  if animation:
    if (firsttime): ofig.show()

  ofig.canvas.draw()

  return

#---------------------------------------------------------------------------------------
# Print actual improvement in history plot
#---------------------------------------------------------------------------------------
def print_improvement (axes, steps, improvements):

  if (len(steps) < 2): return            # nothing to show

  i_best = len(steps) - 1
  best_improve   = improvements [-1]

  if (best_improve > 0):
    for i in reversed(range(len(steps)-1)):
      if (improvements[i] < best_improve):
        break
      else:
        i_best = i


    my_marker = 7
    y_text = 10
    x_text = -4

    axes.plot([steps[i_best]], [improvements[i_best]], marker=my_marker, markersize=7, color="red")
    if (i_best == (len(steps) - 1)):
      axes.annotate(('{:.5f}'.format(improvements[i_best])+'%'),
                    xy = (steps[i_best], improvements[i_best]),
                    xytext = (x_text,y_text), textcoords="offset points", ha = "center",
                    fontsize=8, color='white', bbox = dict (facecolor="green"))
    else:
      axes.annotate(('{:.5f}'.format(improvements[i_best])+'%'),
                    xy = (steps[i_best], improvements[i_best]),
                    xytext = (x_text,y_text), textcoords="offset points", ha = "center",
                    fontsize=8, color='grey')

################################################################################
# Input function that checks python version
def my_input(message):

  # Check python version

  # python_version = version_info[0]

  # Issue correct input command

  #if (python_version == 2):
  #  return raw_input(message)
  #else:
  return input(message)

################################################################################
# Plotting menu
def plotting_menu(initial_splotnum, seedfoil, designfoils, prefix):
  global plotoptions

  # Load optimization history data if it's available

  steps, fmins, relfmins, rads = read_new_optimization_history(prefix=prefix)

  numfoils = len(designfoils)
  plotting_complete = False
  validchoice = False
  splotnum = initial_splotnum

  while (not validchoice):

    if (splotnum == ""):                  # no plotnum from main menu - ask user 

      print("")
      if (numfoils == 0):
        print("There is only " + seedfoil.name + " [0]." )
      elif (numfoils == 1):
        print("There is " + seedfoil.name + " [0] and " + designfoils[0].name + " [1].")
      elif (numfoils == 2):
        print("There is " + seedfoil.name + " [0], " + designfoils[0].name + 
              " [1] and " + designfoils[1].name + " [2].")
      else:
        print("There is " + seedfoil.name + " [0] and " + str(numfoils) + " designs.")
      print ("")

      # Enter new numbber of design

      splotnum = my_input("Enter design to plot (or ENTER to return): ")

    if (splotnum == ""):
      validchoice = True          # Return to main menu
      plotting_complete = True
    else: 
      plotnum = int(splotnum)
      splotnum = ""

      # Check for bad index

      if ( (plotnum < 0) or (plotnum > numfoils) ):
        validchoice = False
        print("Error: index out of bounds.")

      # Plot design

      else:
        validchoice = True
        # plt.close()
        if plotoptions["plot_airfoils"]:
          plot_airfoil_coordinates(seedfoil, matchfoil, designfoils, plotnum, firsttime=True,
                                  prefix=prefix )
        if plotoptions["plot_polars"]:
          plot_polars(seedfoil, designfoils, plotnum, firsttime=True)
        if (plotoptions["plot_optimization_history"] and steps.shape[0] > 0):
          plot_optimization_history(steps, fmins, relfmins, rads, firsttime=True)
        plt.show(block=False)
        plotting_complete = False

  return plotting_complete

################################################################################
# Reads new airfoil coordinates and polar files for updates during optimization
def read_new_airfoil_data(seedfoil, designfoils, prefix):

  # Temporary airfoil struct

  foil = Airfoil()

  # Set up file names to monitor

  coordfilename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Design_Coordinates.dat')
  polarfilename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Design_Polars.dat')

  # Loop through files until we reach latest available design

  reading = True
  while reading:

    if (seedfoil.npt == 0):
      zonetitle = 'zone t="Seed airfoil"'
      foilstr = 'seed'
      nextdesign = 0
    else:
      nextdesign = len(designfoils) + 1
      zonetitle = 'zone t="Airfoil'
      foilstr = 'design #' + str(nextdesign)

    # Read data from coordinate file

    x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3,name = read_airfoil_coordinates(
                                           coordfilename, zonetitle, nextdesign)
    if (ioerror == 1):
      print("Airfoil coordinates file " + coordfilename + " not available yet.")
      reading = False
      break
    elif (ioerror == 2):
      reading = False
      break
    else:
      print("Read coordinates and polars for " + foilstr + ".")
      foil.setCoordinates(np.array(x), np.array(y))
      # jx-mod additional 2nd and 3rd deriv
      foil.setDerivatives (deriv2, deriv3)
      # jx-mod additional 2nd and 3rd deriv
      foil.setGeometryInfo(maxt, xmaxt, maxc, xmaxc)
      foil.setName(name)

    # Set zone title for polars

    if (foilstr == 'seed'):
      zonetitle = 'zone t="Seed airfoil polar"'
    else:
      zonetitle = 'zone t="Polars", SOLUTIONTIME=' + str(nextdesign)

    # Read data from polar file (not: negative error code means coordinates were
    # read but not polars)

    alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror = read_airfoil_polars(polarfilename,
                                                                      zonetitle)

    # retry - maybe it was a timing problem between Xoptfoil and visualizer
    if (ioerror == 2):
      time.sleep (plotoptions["monitor_update_interval"])
      print("         Retry Zone labeled " + zonetitle )
      alpha, cl, cd, cm, xtrt, xtrb, flapangle, ioerror = read_airfoil_polars(polarfilename,
                                                                      zonetitle)

    if (ioerror == 1):
      print("Warning: polars will not be available for this design.")
      ioerror = 3
      reading = False
    elif (ioerror == 2):
      print("         Zone labeled " + zonetitle + " not found in " + polarfilename + ".")
      print("Warning: polars will not be available for this design.")
      ioerror = 3
      reading = False
    else:
      foil.setPolars(np.array(alpha), np.array(cl), np.array(cd), np.array(cm),
                     np.array(xtrt), np.array(xtrb), np.array(flapangle))

    # Copy data to output objects

    if (foilstr == 'seed'): seedfoil = foil
    else: designfoils.append(foil)

  return seedfoil, designfoils, ioerror


################################################################################
# Reads match airfoil coordinates
def read_matchfoil (coordfilename):

  matchfoil = Airfoil()
  zonetitle = 'zone t="Match airfoil'
  foilstr = 'Match'

  # Read data from coordinate file
  x, y, maxt, xmaxt, maxc, xmaxc, ioerror, deriv2, deriv3,name = read_airfoil_coordinates(
                                          coordfilename, zonetitle, 0)

  if (ioerror == 1):
    print("Airfoil coordinates file " + coordfilename + " not available yet.")
  elif (ioerror == 2):
    # This is the normal "no match foil" mode
    ioerror = 2             #dummy
  else:
    print("Read coordinates for " + foilstr + ".")
    matchfoil.setCoordinates(np.array(x), np.array(y))
    matchfoil.setDerivatives (deriv2, deriv3)
    matchfoil.setGeometryInfo(maxt, xmaxt, maxc, xmaxc)
    matchfoil.setName (name)

  return matchfoil, ioerror

################################################################################
# Reads new optimization history data for updates during optimization
def read_new_optimization_history(prefix=None, steps=None, fmins=None, relfmins=None,
                                  rads=None):

  if ((steps is None) or steps.shape[0] == 0):
    steps = np.zeros((0), dtype=int)
    fmins = np.zeros((0))
    relfmins = np.zeros((0))
    rads = np.zeros((0))
    currstep = 0
  else:
    numsteps = steps.shape[0]
    currstep = steps[numsteps-1]

  # Loop through file until we reach latest available step

  reading = True
  while reading:

    if (steps.shape[0] == 0): nextstep = 1
    else:
      numsteps = steps.shape[0]
      nextstep = steps[numsteps-1] + 1

    # Read data from optimization history file
    fmin, relfmin, rad, ioerror = read_optimization_history(prefix, nextstep)
    if (ioerror == 1):
      # print("optimization_history.dat not available yet.")
      reading = False
    elif (ioerror == 2):
      reading = False
      if (nextstep - 1 > currstep):
        print("Read optimization data to step " + str(nextstep-1) + ".")
    else:
      # Copy data to output objects

      steps = np.append(steps, nextstep)
      fmins = np.append(fmins, fmin)
      relfmins = np.append(relfmins, relfmin)
      rads = np.append(rads, rad)

  return steps, fmins, relfmins, rads

################################################################################
# Gets boolean input from user
def get_boolean_input(key, keyval):

  validchoice = False
  while (not validchoice):
    print("Current value for " + key + ": " + str(keyval))
    print("Available choices: True, False\n")
    sel = my_input("Enter new value: ")
    if ( (sel == "True") or (sel == "true")):
      retval = True
      validchoice = True
    elif ( (sel == "False") or (sel == "false")):
      retval = False
      validchoice = True
    else:
      print("Please enter True or False.")
      validchoice = False

  return retval

################################################################################
# Gets color input from user
def get_color_input(key, keyval):

  colors = ["blue", "green", "red", "cyan", "magenta", "yellow", "black"]

  validchoice = False
  while (not validchoice):
    print("Current value for " + key + ": " + str(keyval))
    print("Available choices: blue, green, red, cyan, magenta, yellow, black\n")
    sel = my_input("Enter new value: ")

    # Check for valid color

    for c in colors:
      if (sel == c):
        validchoice = True
        retval = sel
        break
    if (not validchoice):
      print("Invalid color specified.  Please enter a valid color.")
      validchoice = False

  return retval

################################################################################
# Gets float input from user, subject to user-supplied min and max values
def get_float_input(key, keyval, minallow=None, maxallow=None):

  validchoice = False
  while (not validchoice):
    print("Current value for " + key + ": " + str(keyval) + '\n')
    sel = my_input("Enter new value: ")

    # Check for bad format

    try:
      val = float(sel)
    except ValueError:
      print("Error: " + key + " must be a floating point number.")
      validchoice = False
      continue

    # Check for out-of-bounds selection

    if (minallow != None):
      if (val <= minallow):
        print("Error: " + key + " must be greater than " + str(minallow) + ".")
        validchoice = False
        continue
    if (maxallow != None):
      if (val >= maxallow):
        print("Error: " + key + " must be less than " + str(maxallow) + ".")
        validchoice = False
        continue

    # If it passed all these checks, it's an acceptable input

    validchoice = True
    retval = val

  return retval


################################################################################
# Options menu: allows user to change plot options
def options_menu():
  global plotoptions

  # Status variable

  options_complete = False

  # Print list of plotting options

  print("")
  print("Available plotting options:")
  print("")
  for key in sorted(plotoptions):
    print(key + " [" + str(plotoptions[key]) + "]")
  print("")

  # Get user input

  key = my_input("Enter option to change (or 0 to return): ")

  # True/False settings

  if ( (key == "show_seed_airfoil") or (key == "show_seed_airfoil_only") or
       (key == "show_seed_polar") or (key == "show_seed_polar_only") or
       (key == "save_animation_frames") or (key == "plot_airfoils") or
       (key == "plot_3rd_derivative") or (key == "plot_curvature") or
       (key == "plot_polars") or (key == "show_airfoil_info") or
       (key == "plot_optimization_history") ):
    options_complete = False
    plotoptions[key] = get_boolean_input(key, plotoptions[key])

  # Change colors

  elif ( (key == "color_for_seed") or (key == "color_for_new_designs") ):
    options_complete = False
    plotoptions[key] = get_color_input(key, plotoptions[key])

  # Change monitor update interval

  elif (key == "monitor_update_interval"):
    options_complete = False
    plotoptions[key] = get_float_input(key, plotoptions[key], minallow=0.0)

  # Exit options menu

  elif (key == "0"):
    options_complete = True

  # Error for invalid input

  else:
    options_complete = False
    print("Unrecognized plot option.")

  return options_complete

################################################################################
# Main menu
################################################################################
#
# jx-mod initialchoice to autostart operation
#
def main_menu(initialchoice, seedfoil, designfoils, prefix):
  global plotoptions

  exitchoice = False
  rcParams['toolbar'] = 'None'    # Turn on matplotlib toolbar
  plt.style.use('seaborn-paper')
  plt.rc('lines', linewidth  = 1.0)
  plt.rc('font', size=8)
  plt.rc('axes', titlesize=9)     # fontsize of the axes title
  plt.rc('axes', labelsize=9)     # fontsize of the x and y labels
  plt.rc('xtick', labelsize=8)    # fontsize of the tick labels
  plt.rc('ytick', labelsize=8)    # fontsize of the tick labels
  plt.rc('legend', fontsize=8)    # legend fontsize
  plt.rc('figure', titlesize=8)   # fontsize of the figure title


  while (not exitchoice):

    if initialchoice:
      choice = initialchoice
    else:
      print("")
      print("Options:")
      print("[1] Plot a specific design")
      print("[2] Animate all designs")
      print("[3] Monitor an ongoing optimization")
      print("[4] Change plotting options")
      print("[ENTER] Exit")
      print("")

      choice = my_input("Enter a choice [0-4]: ")

    # Exit design_visualizer

    if (choice == "0" or choice ==""):
      exitchoice = True

    # Plot a single design

    elif (choice == "1" or choice == "1-0" or choice == "1-1" or choice == "1-2"):
      exitchoice = False

      # Go to plotting menu

      plotting_complete = False
      if (len(choice) == 3): initial_splotnum = choice [-1]
      else: initial_splotnum = ""

      while (not plotting_complete): 
        plotting_complete = plotting_menu(initial_splotnum, seedfoil, designfoils, prefix)
        initial_splotnum = ""

    # Animate all designs

    elif (choice == "2"):
      exitchoice = False

      # Close all current windows
      plt.close('all')

      # Number of digits in design counter string

      numfoils = len(designfoils)
      if (numfoils == 0):
        print("There are no designs to animate.  Run xoptfoil first.")
        continue
      width = int(floor(log10(float(numfoils)))) - 1

      # Loop through designs, updating plot

      # Show history window
      if plotoptions["plot_optimization_history"]:

        steps, fmins, relfmins, rads = read_new_optimization_history(prefix = prefix)

        plot_optimization_history(steps, fmins, relfmins, rads,
                  firsttime=True, prefix = prefix, animation=True)

      for i in range(0, numfoils):
        if (i == 0): init = True
        else: init = False

        if (plotoptions["save_animation_frames"]):

          # Determine number of zeroes to pad with and image file prefix

          currwidth = int(floor(log10(float(i+1)))) - 1
          numzeroes = width - currwidth
          imagepref = prefix + numzeroes*'0' + str(i+1)

        else: imagepref = None

        # Update plots

        if plotoptions["plot_airfoils"]:
          plot_airfoil_coordinates(seedfoil, matchfoil, designfoils, i+1, firsttime=init,
                                   animation=True, prefix=imagepref)
        if plotoptions["plot_polars"]:
          plot_polars(seedfoil, designfoils, i+1,
                             firsttime=init, animation=True, prefix=imagepref)

        plt.pause(0.1)

    # Monitor optimization progress

    elif (choice == "3"):
      exitchoice = False

      # Close all current windows
      plt.close('all')

      print ()
      print('Monitoring optimization progress. To stop, enter the command ' +
            '"stop_monitoring" in run_control.')
      print ()

      # temporarily disable saving images

      temp_save_frames = plotoptions["save_animation_frames"]
      plotoptions["save_animation_frames"] = False

      # Read airfoil coordinates, polars, and optimization history
      # (clears any data from previous run)

      if not initialchoice:                       # if choice from command line do not re-read data
        seedfoil, designfoils, ioerror = load_airfoils_from_file(coordfilename, polarfilename)

      steps, fmins, relfmins, rads = read_new_optimization_history(prefix=prefix)

      # Periodically read data and update plot

      init = True
      monitoring = True
      ioerror = 0

      while (monitoring):

        # Update plot

        if (ioerror != 1):
          if plotoptions["plot_optimization_history"]:
            plot_optimization_history(steps, fmins, relfmins, rads,
                                      firsttime=init, prefix = prefix, animation=True)
          numfoils = len(designfoils)
          if plotoptions["plot_polars"]:
            plot_polars(seedfoil, designfoils, numfoils,
                                      firsttime=init, animation=True, prefix = prefix)
          if plotoptions["plot_airfoils"]:
            plot_airfoil_coordinates(seedfoil, matchfoil, designfoils, numfoils,
                                      firsttime=init, animation=True, prefix = prefix)

          init = False

        # Show plots and pause for requested update interval
        plt.pause(plotoptions["monitor_update_interval"])
        # time.sleep(0.2)

        # Update airfoil and optimization data
        seedfoil, designfoils, ioerror = read_new_airfoil_data(seedfoil,
                                                            designfoils, prefix)
        steps, fmins, relfmins, rads  = read_new_optimization_history(prefix,
                                                   steps, fmins, relfmins, rads)

        # Is there a complete xfoil polar for reference info 
        seedfoil, designfoils = load_full_polar (prefix, seedfoil, designfoils)

        # Check for stop_monitoring in run_control file
        try:
          f = open('run_control')
        except IOError:
          continue

        commands = []
        for line in f:
          commands += [line.strip()]
          if (line.strip() == "stop_monitoring"):
            print("stop_monitoring command found. Returning to main menu.")
            monitoring = False
          if (line.strip() == "stop"):
            print("stop command found. Returning to main menu.")
            monitoring = False
        f.close()
        # do not remove stop_monitoring
        # if len(commands) > 0:
        #  f = open('run_control', 'w')
        #  for command in commands:
        #    if command != "stop_monitoring":
        #      f.write(command + '\n')
        #  f.close()

      # Change save_animation_frames back to original setting when done

      plotoptions["save_animation_frames"] = temp_save_frames

    # Change plotting options

    elif (choice == "4"):
      exitchoice = False
      options_complete = False
      while (not options_complete): options_complete = options_menu()

    # Invalid choice

    else:
      print("Error: please enter a choice 0-4.")

    initialchoice = ""


################################################################################
# Main design_visualizer program
if __name__ == "__main__":

  # Read command line arguments

# jx-mod Implementation of command line arguments - start
#
#  Following registry keys must be set to handle command line arguments to python.exe
#        HKEY_CLASSES_ROOT\Applications\python.exe\shell\open\command --> "xx:\Anaconda3\python.exe" "%1" %*
#        HKEY_CLASSES_ROOT\py_auto_file\shell\open\command            --> "xx:\Anaconda3\python.exe" "%1" %*
#
  # initiate the parser
  parser = argparse.ArgumentParser('')
  parser.add_argument("--option", "-o", help="set initial action option", 
                      choices=['1','1-0','1-1','1-2','2','3','4'])
  parser.add_argument("--case", "-c", help="the case name for the optimization (e.g., optfoil)")

  # read arguments from the command line
  args = parser.parse_args()

  if args.case:
    prefix = args.case
  else:
    print("Enter the case name for the optimization (e.g., optfoil, which ")
    prefix = my_input("is the default case name): ")
    print("")

  coordfilename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Design_Coordinates.dat')
  polarfilename = os.path.join(prefix + DESIGN_SUBDIR_POSTFIX, 'Design_Polars.dat')

  # Read airfoil coordinates and polars

  seedfoil, designfoils, ioerror = load_airfoils_from_file(
                                                   coordfilename, polarfilename)
  # Warning if file is not found

  if (ioerror == 1):
    print("You will not be able to create plots until coordinate data is read.")
  # elif (ioerror < 0):
    # print("Only airfoils are available for plotting (no polars).")
  else: 
    # Is there a complete xfoil polar for reference info 
    seedfoil, designfoils = load_full_polar (prefix, seedfoil, designfoils)

  # Is there a matchfoil? If yes switch of polars as there will be no polars..

  matchfoil, ioerror = read_matchfoil (coordfilename)
  if (ioerror == 0):
    plotoptions["plot_polars"] = False
    plotoptions["plot_optimization_history"] = False

    print ("")
    print ("Match airfoil detected in design_coordinates.")
    print ("      Polar plot will be switched off as no polars are generated in this case")
    print ("      Use option [2] to visualize optimization as match airfoil optimization is fast as lighting...")
    print ("")
  elif ((ioerror == 2)):
    ioerror = 0


  # Call main menu

  if (abs(ioerror) <= 1): main_menu(args.option, seedfoil, designfoils, prefix)
