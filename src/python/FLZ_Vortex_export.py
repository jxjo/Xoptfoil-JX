#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      matth
#
# Created:     18.02.2021
# Copyright:   (c) matth 2021
# Licence:     <your licence>
#-------------------------------------------------------------------------------

from strak_machineV2 import (ErrorMsg, WarningMsg, NoteMsg, DoneMsg,  bs,
                             buildPath, ressourcesPath, airfoilPath)
from math import atan, pi
from copy import deepcopy

endOfHeader_Tag = "GESAMTPOLARBERECHNUNG_SCHRITTZAHL"
startOfWing_Tag = "[FLAECHE0]"
endOfRoot_Tag = "ZIRKULATIONSVORGABE"
startOfFooter_Tag = "[FLAECHE ENDE]"

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
        self.airfoilNames = self.get_airfoilNames(wingData)
        self.angles = self.calculate_angles(wingData)
        self.dihedrals = self.get_dihedrals(wingData)
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
        return (widthsLeftHalfWing + widthsRightHalfWing)


    def get_chords(self, wingData):
        chordsLeftHalfWing = []
        chordsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(1, numSections):
            # calculate chords
            chordsLeftHalfWing.append(wingData.sections[idx].chord)
            chordsRightHalfWing.append(wingData.sections[idx].chord)

        chordsLeftHalfWing.reverse()

        return (chordsLeftHalfWing + chordsRightHalfWing)


    def get_airfoilNames(self, wingData):
        namesLeftHalfWing = []
        namesRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(numSections-1):
            # calculate chords
            namesLeftHalfWing.append(wingData.sections[idx].airfoilName)
            namesRightHalfWing.append(wingData.sections[idx].airfoilName)

        namesLeftHalfWing.reverse()

        return (namesLeftHalfWing + namesRightHalfWing)

    def get_dihedrals(self, wingData):
        dihedralsLeftHalfWing = []
        dihedralsRightHalfWing = []
        numSections = len(wingData.sections)

        for idx in range(numSections-1):
            # calculate chords
            dihedralsLeftHalfWing.append(wingData.sections[idx].dihedral)
            dihedralsRightHalfWing.append(wingData.sections[idx].dihedral)

        dihedralsLeftHalfWing.reverse()

        return (dihedralsLeftHalfWing + dihedralsRightHalfWing)


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

        return (anglesLeftHandWing + angles)




# function to write the
def write_airfoilData(airfoilName, file):
    try:
        # open airfoil-file
        fileNameAndPath = buildPath + bs + airfoilPath + bs + airfoilName
        airfoilFile = open(fileNameAndPath)
        airfoilData = airfoilFile.readlines()
        airfoilFile.close()
    except:
        ErrorMsg("failed to read arifoil data from file %s" % fileNameAndPath)

    file.write("[PROFIL]\n")
    file.write("PROFILDATEINAME=%s\n" % airfoilName)

    idx = 0
    for line in airfoilData[1:]:
        coords = line.split()
        file.write("PK%d=%s %s\n" % (idx, coords[0], coords[1]))
        idx = idx + 1

    file.write("[PROFIL ENDE]\n")


def write_segmentData(wingData, segments, idx, file):
    # insert start of segment
    file.write("[SEGMENT%d]\n" % idx)
    file.write("SEGMENTBREITE=%.5f\n" % segments.widths[idx])
    file.write("PROFILTIEFE=%.5f\n" % segments.chords[idx])
    file.write("BEZUGSPUNKT_PROFILTIEFE=%.5f\n" % (100.0 - wingData.hingeDepthPercent))
    file.write("VERWINDUNGSWINKEL=0.00000\n")
    file.write("V-FORM_WINKEL=%.5f\n" % segments.dihedrals[idx])
    file.write("PFEILWINKEL=%.5f\n" % segments.angles[idx])
    file.write("BEZUGSPUNKT_PFEILWINKEL=%.5f\n" % (100.0 - wingData.hingeDepthPercent))
    file.write("ANZAHL PANELS Y=1\n")
    file.write("VERTEILUNG=LINEAR\n")

    file.write("KLAPPENTIEFE LINKS,RECHTS=%.5f %.5f\n" % \
              (wingData.hingeDepthPercent, wingData.hingeDepthPercent))

    file.write("KLAPPENAUSSCHLAG=0.00000\n")
    file.write("KLAPPENGRUPPE=0\n")
    file.write("KLAPPENINVERSE=FALSE\n")
    file.write("FLAG_MAN_BEIWERTE=FALSE\n")
    file.write("ALFA0_MAN=0.00000\n")
    file.write("CM0_MAN=0.00000\n")

    # insert data of the airfoil now
    write_airfoilData(segments.airfoilNames[idx], file)

    # insert end of segment
    file.write("[SEGMENT ENDE]\n")


def write_rootData(wingData, FLZ_fileContent, file):
    startWriting = False

    for line in FLZ_fileContent:
        # find the line where wing data starts
        if (line.find(startOfWing_Tag) >= 0):
            startWriting = True
        # find the line where root data ends
        elif(line.find(endOfRoot_Tag) >= 0):
            file.write(line)
            # insert data of the root-airfoil now
            write_airfoilData(wingData.airfoilNames[0], file)
            # job is done
            return

        if startWriting:
            if (line.find("BEZEICHNUNG")>=0):
                file.write("BEZEICHNUNG=%s\n" % wingData.planformName)
            elif (line.find("PROFILTIEFE")==0):
                file.write("PROFILTIEFE=%.5f\n" % wingData.rootchord)
            elif (line.find("BEZUGSPUNKT_PROFILTIEFE")>=0):
                file.write("BEZUGSPUNKT_PROFILTIEFE=%.5f\n" % (100.0 - wingData.hingeDepthPercent))
            elif (line.find("VERWINDUNGSWINKEL")>=0):
                file.write("VERWINDUNGSWINKEL=0.00000\n")
#            elif (line.find("ANZAHL PANELS X")>=0):
#                file.write("ANZAHL PANELS X=20\n") #FIXME correct value?
            else:
                file.write(line)



def write_header(FLZ_fileContent, file):
    # write all lines up to end of header
    for line in FLZ_fileContent:
        file.write(line)
        if (line.find(endOfHeader_Tag) >= 0):
            # found end of header, job is finished here
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

    # write the data of the root-rib
    write_rootData(wingData, FLZ_fileContent, FLZ_outfile)

    # loop over all sections of the wing
    for idx in range(segments.num):
        write_segmentData(wingData, segments, idx, FLZ_outfile)
        idx = idx + 1

    # transfer the footer from the in file to the out file
    write_footer(FLZ_fileContent, FLZ_outfile)

    # Everything is done
    FLZ_outfile.close()
    NoteMsg("wing data was succesfully written.")
