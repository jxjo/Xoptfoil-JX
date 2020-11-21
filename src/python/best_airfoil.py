#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      matth
#
# Created:     23.05.2020
# Copyright:   (c) matth 2020
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import argparse
import change_airfoilname

# paths and separators
bs = "\\"
ressourcesPath = 'ressources'
buildPath = 'build'



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
        exit(-1)

    return file_content


def main():
    # get command-line-arguments
    (airfoilName, numCompetitors) = getArguments()

    max_improvement = 0.0
    bestCompetitor = airfoilName

    if (airfoilName == None) or (numCompetitors == None):
        print("Error, airfoilName or numCompetitors not specified!")
        exit(-1)

    for i in range(numCompetitors):
        # example: SD-strak-150k_1_1_performance_summary.dat
        summaryFileName = airfoilName + ("_%d_performance_summary.dat" % (i+1))

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

    # print result
    print("Best competitor is: \'%s\'" % bestCompetitor)
    print("Renaming airfoil \'%s\' to \'%s\'\n" % (bestCompetitor, airfoilName))

    # compose complete filenames
    bestCompetitor = bestCompetitor + '.dat'
    airfoilName = airfoilName  + '.dat'

    # rename and copy the competitor-airfoil to stage-result airfoil
    change_airfoilname.change_airfoilName(bestCompetitor, airfoilName)


if __name__ == '__main__':
    main()
