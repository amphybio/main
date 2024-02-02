#!/usr/bin/python3
# vim: fileencoding=utf-8

# Version:  0.1.1
# Created:  01-02-2024
# Updated:  02-02-2024
# Authors:  Leonardo Gama <leonardo.gama@usp.br>

# MIT License
#
# Copyright  2024  Alexandre Ferreira Ramos
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.


"""Launch Jupyter Notebook on remote host through SSH.

This script executes four tasks by running a single 'ssh' command:
    1. Establishes a SSH connection to the remote host.
    2. Forwards a random TCP port in the dynamic range through tunnelling.
    3. Starts the Jupyter Notebook server in the remote host using that port.
    4. Opens the notebook interface on the default web browser.
"""


import argparse
import random
import re
import subprocess
import sys
import webbrowser
import __main__


DEFAULT_HOSTNAME = 'antp'

# Range of dynamic (unassigned) TCP ports.
PORT_MIN = 49152
PORT_MAX = 65535


# Accept custom hostname.

docstring = __main__.__doc__.split('\n\n', 1)
parser = argparse.ArgumentParser(
    prog = "ssh-notebook",
    description = docstring[0],
    epilog = docstring[1],
    formatter_class = argparse.RawDescriptionHelpFormatter
)
parser.add_argument(
    'host',
    help = "SSH destination (default: %(default)s)",
    metavar = "[user@]hostname[:port]",
    nargs = '?',
    default = DEFAULT_HOSTNAME
)
args = parser.parse_args()


# Choose a random port in the range.

port = random.randrange(PORT_MIN, PORT_MAX + 1)


# Launch the notebook server on remote host and forward through SSH tunnel.

ssh_cmd = f'ssh -L {port}:localhost:{port} ssh://{args.host}'
notebook_cmd = f'jupyter notebook --port={port} --log-level=WARN --no-browser'
cmd = [*ssh_cmd.split(), '--', *notebook_cmd.split()]

print(
    f"""
    Launching Jupyter Notebook at {args.host} on port {port} with command:
        $ {ssh_cmd} -- {notebook_cmd}""",
    end = '\n\n'
)

ssh_process = subprocess.Popen(cmd, stderr=subprocess.PIPE, text=True)


# Open notebook URL in the default browser.

url_regex = re.compile(r'\s+(http://localhost:\d+/\?token=\S+)')

for line in ssh_process.stderr:
    try:
        ssh_process.wait(timeout=0.1)
    except subprocess.TimeoutExpired:
        pass
    else:
        # The command exited early.
        print(line, end='')
        print(ssh_process.stderr.read(), end='')
        sys.exit(ssh_process.returncode)

    match = url_regex.match(line)
    if match is not None:
        notebook_url = match.group(1)
        break

print("    If the notebook doesn't open automatically, "
      "copy and paste one of these URLs:")
print(line, end='')  # notebook URL
print(next(ssh_process.stderr))  # alternative URL

webbrowser.open_new_tab(notebook_url)


# Wait for the SSH subprocess to finish.

print("    Terminate this program (Ctrl-C) to quit the notebook server"
      " and close the connection.\n")

try:
    for line in ssh_process.stderr:
        print(line, end='')
except KeyboardInterrupt:
    print()
    ssh_process.terminate()

sys.exit(ssh_process.wait())
