#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
(option, args) = parser.parse_args()

recout_file = open(option.filename, "r")

test_passed = True
for line in recout_file.readlines():
	split_str = line.split()
	if (len(split_str) > 1) and (split_str[0] == "Total:"):
		for i, value in enumerate(split_str[1:]):
			test_passed == test_passed and (float(value) == 1.0)

recout_file.close

if not test_passed:
	print("Signal effectiveness totals in file {0} are not all equal to 1.0.".format(option.filename))
	sys.exit(1)
