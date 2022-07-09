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
    parser.add_argument("-polar", "-p", help="polar-filename")
    parser.add_argument("-name", "-n", help="airfoil-name")

    # read arguments from the command line
    args = parser.parse_args()
    return (args.polar, args.name)

def main():
    # get command-line-arguments
    (filename, airfoilName) = getArguments()

    # remove file-ending, if neccessary
    airfoilName = re.sub('.dat', '', airfoilName)

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
