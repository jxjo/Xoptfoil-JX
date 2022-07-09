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

from strak_machine import (ErrorMsg, WarningMsg, NoteMsg, DoneMsg, bs,
                             buildPath, ressourcesPath, airfoilPath)
from math import atan, pi
from copy import deepcopy
import numpy as np

endOfHeader_Tag = "GESAMTPOLARBERECHNUNG_SCHRITTZAHL"
startOfWing_Tag = "[FLAECHE0]"
startOfFin_Tag = "[FLAECHE1]"
endOfWingOrTail_Tag = "[FLAECHE ENDE]"
endOfRoot_Tag = "ZIRKULATIONSVORGABE"
startOfFooter_Tag = "[FLUGZEUG ENDE]"



# structure of the FLZ-file:
#
#    [FLUGZEUG]
#      data
#      [FLAECHE0]
#        data
#        [SEGMENT0]
#          data
#        [SEGMENT ENDE]
#        [SEGMENT1]
#          data
#        [SEGMENT ENDE]
#        further segments
#      [FLAECHE ENDE]
#      [FLAECHE1]
#        data, segments etc. like above
#      [FLAECHE ENDE]
#  [FLUGZEUG ENDE]
#
################################################################################
#
# segmentData class
#
################################################################################
class segmentData:
    #class init
    def __init__(self, wingData):
        # geometrical data of the wing planform
        self.widths = self.calculate_widths(wingData)
        self.chords = self.get_chords(wingData)
        self.hingeDepths = self.calculateHingeDepths(wingData)
        self.airfoilNames = self.get_airfoilNames(wingData)
        self.angles = self.calculate_angles(wingData)
        self.dihedrals = self.get_dihedrals(wingData)
        self.flapGroups = self.getFlapGroups(wingData)
        self.num = len(self.widths)


    def calculate_widths(self, wingData):
        widthsLeftHalfWing = []
        widthsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(numSections-1):
            # calculate section width
            width = wingData.sections[idx+1].y - wingData.sections[idx].y
            widthsLeftHalfWing.append(-1.0*width)
            widthsRightHalfWing.append(width)
            idx = idx + 1

        # reverse list of left half wing
        widthsLeftHalfWing.reverse()
        widthList = widthsLeftHalfWing + widthsRightHalfWing
        #print("width:")
        #print(widthList)#Debug
        return (widthList)

    def getFlapGroups(self, wingData):
        flapGroupsLeftHalfWing = []
        flapGroupsRightHalfWing = []
        numSections = len(wingData.sections)

        # determine number of different flap groups. Fuselage is not counted
        numDifferentGroups = len(np.unique(wingData.flapGroups))
        if (0 in wingData.flapGroups):
            numDifferentGroups = numDifferentGroups - 1

        for idx in range(1, numSections):
            flapGroupsRightHalfWing.append(wingData.sections[idx-1].flapGroup)
            flapGroupsLeftHalfWing.append(wingData.sections[idx-1].flapGroup)

        flapGroupsLeftHalfWing.reverse()
        for idx in range(len(flapGroupsLeftHalfWing)):
            if flapGroupsLeftHalfWing[idx] > 0:
                flapGroupsLeftHalfWing[idx] = flapGroupsLeftHalfWing[idx] + numDifferentGroups

        flapGroupList = flapGroupsLeftHalfWing + flapGroupsRightHalfWing
        return (flapGroupList)

    def calculateHingeDepths(self, wingData):
        hingeDepthsLeftHalfWing = []
        hingeDepthsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(1, numSections):
            # calculate hingeDepth
            hingeDepth = (wingData.sections[idx].hingeDepth / wingData.sections[idx].chord)*100.0
            hingeDepthsLeftHalfWing.append(hingeDepth)
            hingeDepthsRightHalfWing.append(hingeDepth)

        # reverse list of left half wing
        hingeDepthsLeftHalfWing.reverse()
        hingeDepthsLeftHalfWing.append(wingData.hingeDepthRoot)
        hingeDepthList = hingeDepthsLeftHalfWing + hingeDepthsRightHalfWing
        #print("hingeDepth:")
        #print(hingeDepthList)# Debug
        return (hingeDepthList)


    def get_chords(self, wingData):
        chordsLeftHalfWing = []
        chordsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(1, numSections):
            # calculate chords
            chordsLeftHalfWing.append(wingData.sections[idx].chord)
            chordsRightHalfWing.append(wingData.sections[idx].chord)

        chordsLeftHalfWing.reverse()
        chordList = chordsLeftHalfWing + chordsRightHalfWing
        #print("chords:")
        #print(chordList)#Debug
        return (chordList)


    def get_airfoilNames(self, wingData):
        namesLeftHalfWing = []
        namesRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(numSections-1):
            # calculate chords
            namesLeftHalfWing.append(wingData.sections[idx].airfoilName)
            namesRightHalfWing.append(wingData.sections[idx].airfoilName)

        namesLeftHalfWing.reverse()
        airFoilNames = namesLeftHalfWing + namesRightHalfWing
        #print("airfoils:")
        #print(airFoilNames)#Debug
        return (airFoilNames)

    def get_dihedrals(self, wingData):
        dihedralsLeftHalfWing = []
        dihedralsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(numSections-1):
            # calculate chords
            dihedralsLeftHalfWing.append(wingData.sections[idx].dihedral)
            dihedralsRightHalfWing.append(wingData.sections[idx].dihedral)

        dihedralsLeftHalfWing.reverse()
        dihedralList = dihedralsLeftHalfWing + dihedralsRightHalfWing
        #print("dihedral:")
        #print(dihedralList)#Debug
        return (dihedralList)


    def calculate_angles(self, wingData):
        numSections = len(wingData.sections)-1
        angles = []

        # right hand wing
        for idx in range(numSections):
            section = wingData.sections[idx]
            next_section = wingData.sections[idx+1]
            width = next_section.y - section.y

            # calculate segment angle
            AK = width
            GK = next_section.leadingEdge - section.leadingEdge
            angle_radian = atan(GK/AK)

            # convert radian measure --> degree
            angle = (angle_radian / pi) * 180.0
            angles.append(angle)

        # left hand wing
        anglesLeftHandWing = deepcopy(angles)
        anglesLeftHandWing.reverse()
        angleList = anglesLeftHandWing + angles
        #print("angles")
        #print(angleList)#Debug
        return (angleList)




# function to write the
def write_airfoilData(airfoilName, file):
    # open airfoil-file
    fileNameAndPath = buildPath + bs + airfoilPath + bs + airfoilName
    airfoilFile = open(fileNameAndPath)
    airfoilData = airfoilFile.readlines()
    airfoilFile.close()

    file.write("[PROFIL]\n")
    file.write("PROFILDATEINAME=%s\n" % airfoilName)

    idx = 0
    for line in airfoilData[1:]:
        coords = line.split()
        file.write("PK%d=%s %s\n" % (idx, coords[0], coords[1]))
        idx = idx + 1

    file.write("[PROFIL ENDE]\n")


def write_segmentData(wingData, segments, idx, file):
    klappentiefeLinks = segments.hingeDepths[idx]
    klappentiefeRechts = segments.hingeDepths[idx+1]
    Bezugspunkt = 100.0 - klappentiefeLinks

    # insert start of segment
    file.write("[SEGMENT%d]\n" % idx)
    file.write("SEGMENTBREITE=%.5f\n" % segments.widths[idx])
    file.write("PROFILTIEFE=%.5f\n" % segments.chords[idx])
    file.write("BEZUGSPUNKT_PROFILTIEFE=%.5f\n" % Bezugspunkt)
    file.write("VERWINDUNGSWINKEL=0.00000\n")
    file.write("V-FORM_WINKEL=%.5f\n" % segments.dihedrals[idx])
    file.write("PFEILWINKEL=%.5f\n" % segments.angles[idx])
    file.write("BEZUGSPUNKT_PFEILWINKEL=%.5f\n" % Bezugspunkt)
    file.write("ANZAHL PANELS Y=1\n")
    file.write("VERTEILUNG=LINEAR\n")
    file.write("KLAPPENTIEFE LINKS,RECHTS=%.5f %.5f\n" % \
                (klappentiefeLinks, klappentiefeRechts))
    file.write("KLAPPENAUSSCHLAG=0.00000\n")
    file.write("KLAPPENGRUPPE=%d\n" % segments.flapGroups[idx])
    file.write("KLAPPENINVERSE=FALSE\n")
    file.write("FLAG_MAN_BEIWERTE=FALSE\n")
    file.write("ALFA0_MAN=0.00000\n")
    file.write("CM0_MAN=0.00000\n")

    # insert data of the airfoil now
    try:
        write_airfoilData(segments.airfoilNames[idx], file)
    except:
        print("airfoil %s not found, writing airfoil data of %s (root airfoil) instead" %\
        (segments.airfoilNames[idx], wingData.airfoilNames[0]))
        write_airfoilData(wingData.airfoilNames[0], file)

    # insert end of segment
    file.write("[SEGMENT ENDE]\n")



def write_header(FLZ_fileContent, file):
    # write all lines up to end of header
    for line in FLZ_fileContent:
        file.write(line)
        if (line.find(endOfHeader_Tag) >= 0):
            # found end of header, job is finished here
            return


def write_wingHeader(wingData, file):
    file.write("[FLAECHE0]\n")
    file.write("ART=FLUEGEL\n")
    file.write("BEZEICHNUNG=%s\n" % wingData.planformName)
    file.write("POSITION X,Y,Z=0.00000 0.00000 0.00000\n") #FIXME position
    file.write("ALFA0_CM0_OPTIMIERUNG=TRUE\n")
    file.write("EINSTELLWINKEL=0.00000\n")
    file.write("PROFILTIEFE=%.5f\n" % wingData.rootchord)
    file.write("BEZUGSPUNKT_PROFILTIEFE=%.5f\n" % (100.0 - wingData.hingeDepthRoot))
    file.write("VERWINDUNGSWINKEL=0.00000\n")
    file.write("ANZAHL PANELS X=7\n")
    file.write("VERTEILUNG=SIN_L\n")
    file.write("ANZAHL PANELS VOLUMENDARSTELLUNG=30\n")
    file.write("MASSE=2.50000\n")
    file.write("FLAG_MAN_BEIWERTE=FALSE\n")
    file.write("ALFA0_MAN=0.00000\n")
    file.write("CM0_MAN=0.00000\n")
    file.write("ZIRKULATIONSVORGABE=1.00000\n")
    # insert data of the root-airfoil now
    write_airfoilData(wingData.airfoilNames[0], file)
    # job is done


def write_finHeader(wingData, file):
    file.write("[FLAECHE1]\n")
    file.write("ART=FLUEGEL\n")
    #file.write("ART=LEITWERK\n")
    file.write("BEZEICHNUNG=%s\n" % wingData.planformName)
    file.write("POSITION X,Y,Z=0.88500 0.00000 0.00000\n") #FIXME position
    file.write("ALFA0_CM0_OPTIMIERUNG=TRUE\n")
    file.write("EINSTELLWINKEL=0.00000\n")
    file.write("PROFILTIEFE=%.5f\n" % wingData.rootchord)
    file.write("BEZUGSPUNKT_PROFILTIEFE=%.5f\n" % (100.0 - wingData.hingeDepthRoot))
    file.write("VERWINDUNGSWINKEL=0.00000\n")
    file.write("ANZAHL PANELS X=7\n")
    file.write("VERTEILUNG=SIN_L\n")
    file.write("ANZAHL PANELS VOLUMENDARSTELLUNG=30\n")
    file.write("MASSE=0.0650000\n")
    file.write("FLAG_MAN_BEIWERTE=FALSE\n")
    file.write("ALFA0_MAN=0.00000\n")
    file.write("CM0_MAN=0.00000\n")
    file.write("ZIRKULATIONSVORGABE=1.00000\n")
    # insert data of the root-airfoil now
    write_airfoilData(wingData.airfoilNames[0], file)
    # job is done



def copy_wingData(FLZ_fileContent, file):
    startWriting = False
    # write all lines up to end of the first wingData
    for line in FLZ_fileContent:
        if (line.find(startOfWing_Tag) >= 0):
            startWriting = True

        if startWriting:
            file.write(line)
            if (line.find(endOfWingOrTail_Tag) >= 0):
                # found end of wing, job is finished here
                return


def copy_tailData(FLZ_fileContent, file):
    startWriting = False
    # write all lines up to end of the first wingData
    for line in FLZ_fileContent:
        if (line.find(startOfTail_Tag) >= 0):
            startWriting = True

        if startWriting:
            file.write(line)
            if (line.find(startOfFooter_Tag) >= 0):
                # found end of tail, job is finished here
                return

def write_footer(FLZ_fileContent, file):
    startWriting = False
    # write all lines from start of footer to the end of file content
    for line in FLZ_fileContent:
        if (line.find(startOfFooter_Tag) >= 0):
            startWriting = True

        if startWriting:
            file.write(line)


def export_toFLZ(wingData, inFileName, outFileName):
    # calculate segment values from wingdata
    segments = segmentData(wingData)

    # read in all the data
    NoteMsg("Reading data from FLZ file %s" % inFileName)
    try:
        # open file for reading
        FLZ_inFile = open(inFileName)
        FLZ_fileContent = FLZ_inFile.readlines()
        FLZ_inFile.close()
    except:
        ErrorMsg("failed to open file %s" % inFileName)
        return

    # open file for writing
    NoteMsg("Writing wing data to FLZ file %s" % outFileName)
    try:
        FLZ_outfile = open(outFileName, "w+")
    except:
        ErrorMsg("failed to open file %s" % outFileName)
        return

    # transfer the header from the in file to the out file
    write_header(FLZ_fileContent, FLZ_outfile)

    # if the new data to be written is the fin, the data of the wing must be
    # kept -->copy from FLZ_fileContent
    if wingData.isFin:
        copy_wingData(FLZ_fileContent, FLZ_outfile)
        write_finHeader(wingData, FLZ_outfile)
    else:
        write_wingHeader(wingData, FLZ_outfile)


    # loop over all sections of the wing
    for idx in range(segments.num):
        write_segmentData(wingData, segments, idx, FLZ_outfile)
        idx = idx + 1

    # End of Wing / Fin
    FLZ_outfile.write("[FLAECHE ENDE]\n")

    # transfer the footer from the in file to the out file
    write_footer(FLZ_fileContent, FLZ_outfile)

    # Everything is done
    FLZ_outfile.close()
    NoteMsg("wing data was successfully written.")
