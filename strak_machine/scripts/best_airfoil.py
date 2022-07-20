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

import argparse
import sys
from os import path, makedirs
import change_airfoilname

# paths and separators
bs = "\\"

################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("-airfoil", "-a", help="airfoil-name")
    parser.add_argument("-number", "-n", help="number of airfoils")

    # read arguments from the command line
    args = parser.parse_args()
    return (args.airfoil, int(args.number))


def readPerformanceSummary(filename):
    file_content = None
    try:
        file = open(filename, 'r')
        file_content = file.readlines()
        file.close()
    except:
        print("Error, File %s could not be opened !" % filename)
        sys.exit(-1)

    return file_content


def writeSummaryToFile(airfoilName, summary):
    # example: SD-strak-150k_1_performance_summary.dat
    summaryDir = airfoilName + "_temp"
    summaryFilename = summaryDir + bs + "Performance_Summary.dat"
    try:
        print("writing summary file..")
        if not path.exists(summaryDir):
                makedirs(summaryDir)
        file = open(summaryFilename, 'w')
        for line in summary:
            file.writelines(line)
        file.close()
        print("o.k.")
    except:
        print("Error, File %s could not be written !" % filename)
        sys.exit(-1)


def main():
    # get command-line-arguments
    (airfoilName, numCompetitors) = getArguments()

    max_improvement = 0.0
    bestCompetitor = airfoilName

    if (airfoilName == None) or (numCompetitors == None):
        print("Error, airfoilName or numCompetitors not specified!")
        sys.exit(-1)

    # open progressfile to append text-messages
    progressfile = open("progress.txt", 'a')
    progressfile.write('\nchoosing best preliminary airfoil for next stage..\n')

    for i in range(numCompetitors):
        # example: SD-strak-150k_1_1_performance_summary.dat
        summaryFileName = airfoilName + ("_%d_temp"% (i+1)) + bs + "Performance_Summary.dat"

        # example: SD-strak-150k_1_1
        competitorFileName = airfoilName + ("_%d" % (i+1))

        # read performace-summary of competitor-airfoil into ram
        summary = readPerformanceSummary(summaryFileName)

        # determine maximum improvement and thus best of the competitor-airfoils
        for line in summary:
            # find the line containg the overall improvement
            if (line.find("improvement over seed") >=0):
                splitlines = line.split(":")
                improvementString = splitlines[1].strip(" ")
                splitlines  = improvementString.split("%")
                improvement = float(splitlines[0])

                # competitor has best overall result, set as new best competitor
                if (improvement > max_improvement):
                    max_improvement = improvement
                    bestCompetitor = competitorFileName
                    bestCompetitorSummary = summary

    bestAirfoilString = "Best airfoil is: \'%s\', Improvement over seed is: %f%%\n\n" % (bestCompetitor, max_improvement)

    # print result
    print(bestAirfoilString)
    print("Renaming airfoil \'%s\' to \'%s\'\n" % (bestCompetitor, airfoilName))

    # write result to progressfile
    progressfile.write(bestAirfoilString)

    # write the summary-file of the best competitor
    writeSummaryToFile(airfoilName, bestCompetitorSummary)

    # compose complete filenames
    bestCompetitor = bestCompetitor + '.dat'
    airfoilName = airfoilName  + '.dat'

    # rename and copy the competitor-airfoil to stage-result airfoil
    change_airfoilname.change_airfoilName(bestCompetitor, airfoilName)



if __name__ == '__main__':
    main()
