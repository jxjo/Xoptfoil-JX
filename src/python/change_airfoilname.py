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
    parser.add_argument("-input", "-i", help="input-filename")
    parser.add_argument("-output", "-o", help="output-filename")

    # read arguments from the command line
    args = parser.parse_args()
    return (args.input, args.output)

def main():
    # get command-line-arguments
    (old, new) = getArguments()

   #old = 'ressources\\rg15.dat' #debug
   #new = 'ressources\\new.dat' #debug

    oldfile = open(old, 'r')
    oldfile_content = oldfile.readlines()
    oldfile.close()

    if (new.find("\\") >= 0):
        splitlines = new.split("\\")
        num = len(splitlines)
        newName = splitlines[(num-1)]
    elif (new.find("/") >= 0):
        splitlines = new.split("/")
        num = len(splitlines)
        newName = splitlines[(num-1)]
    else:
        newName = new
    newName = newName.strip('.dat')

    newfile = open(new, 'w+')
    i = 0
    for line in oldfile_content:
        if (i > 0):
            newfile.write(line)
        else:
            newfile.write("%s\n" % newName)
            i = i+1

    newfile.close()



if __name__ == '__main__':
    main()
