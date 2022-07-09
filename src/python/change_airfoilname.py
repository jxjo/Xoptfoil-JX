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
import re

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


def change_airfoilName(old, new):
    try:
        oldfile = open(old, 'r')
        oldfile_content = oldfile.readlines()
        oldfile.close()
    except:
        print("Error, failed to open file %s" % old)
        return

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

    newName = re.sub('.dat', '', newName)

    try:
        newfile = open(new, 'w+')
    except:
        print("Error, failed to open file %s" % new)
        return

    i = 0
    for line in oldfile_content:
        if (i > 0):
            newfile.write(line)
        else:
            newfile.write("%s\n" % newName)
            i = i+1

    newfile.close()


def main():
    # get command-line-arguments
    (old, new) = getArguments()
    change_airfoilName(old, new)




if __name__ == '__main__':
    main()
