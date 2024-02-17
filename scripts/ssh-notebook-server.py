#!/usr/bin/python3
# vim: fileencoding=utf-8

# Version:  0.1
# Created:  17-02-2024
# Updated:  17-02-2024
# Authors:
#   Leonardo Gama <leonardo.gama@usp.br>
#   Gabriela Alcaide <gabriela.alcaide@usp.br>

# MIT License
#
# Copyright (c) 2024  Alexandre Ferreira Ramos - AMPhyBio Laboratory
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions: The above copyright notice and this
# permission notice shall be included in all copies or substantial portions of
# the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS
# OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
# IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""Launches the Jupyter Notebook as a subprocess and terminate it when the
parent process exits.
"""

import subprocess
import sys

import prctl


# Forward command-line arguments to the jupyter command.
notebook_cmd = ['jupyter', 'notebook'] + sys.argv[1:]


# Ask kernel to send INT signal (KeyboardInterrupt) when parent process exits.
prctl.set_pdeathsig(2)


process = subprocess.Popen(notebook_cmd)
try:
    process.wait()
    sys.exit(process.returncode)
except KeyboardInterrupt:
    process.terminate()
