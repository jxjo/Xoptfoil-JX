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

################################################################################
# function that gets arguments from the commandline
def getArguments():

    # initiate the parser
    parser = argparse.ArgumentParser('')
    parser.add_argument("-polar", "-p", help="polar-filename")
    parser.add_argument("-name", "-n", help="airfoil-name")

    # read arguments from the command line
    args = parser.parse_args()
    return (args.polar, args.name)

def main():
    # get command-line-arguments
    (filename, airfoilName) = getArguments()

    # strip file-ending, if neccessary
    airfoilName = airfoilName.strip('.dat')

    file = open(filename, 'r')
    file_content = file.readlines()
    file.close()

    newfile = open(filename, 'w+')

    for line in file_content:
        if line.find('Calculated polar for:') >= 0:
            newfile.write(" Calculated polar for: %s\n" % airfoilName)
        else:
            newfile.write(line)

    newfile.close()



if __name__ == '__main__':
    main()
