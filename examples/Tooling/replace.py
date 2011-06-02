#!/usr/bin/env python

#===- replace.py - Applying code rewrites --------------------*- python -*--===#
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
#===------------------------------------------------------------------------===#
#
# This script applies the rewrites generated by replace-cstr-calls on a source
# tree.
#
# Usage:
#   ./replace.py < /path/to/replace-cstr-calls-output
#
#===------------------------------------------------------------------------===#

import fileinput
import re
import sys

for line in sys.stdin.readlines():
  # The format is:
  # <file>:<start_line>:<start_column>:<end_line>:<end_column>:<replacement>
  # FIXME: This currently does not support files with colons, we'll need to
  # figure out a format when we implement more refactoring support.
  match = re.match(r'(.*):(\d+):(\d+):(\d+):(\d+):(.*)$', line)
  if match is not None:
    file_name = match.group(1)
    start_line, start_column = int(match.group(2)), int(match.group(3))
    end_line, end_column = int(match.group(4)), int(match.group(5))
    replacement = match.group(6)
    if start_line != end_line:
      print ('Skipping match "%s": only single line ' +
             'replacements are supported') % line.strip()
      continue
    try:
      replace_file = fileinput.input(file_name, inplace=1)
      for replace_line in replace_file:
        # FIXME: Looping over the file for each replacement is both inefficient
        # and incorrect if replacements add or remove lines.
        if replace_file.lineno() == start_line:
          sys.stdout.write(replace_line[:start_column-1] + replacement +
                           replace_line[end_column:])
        else:
          sys.stdout.write(replace_line)
    except OSError, e:
      print 'Cannot open %s for editing' % file_name