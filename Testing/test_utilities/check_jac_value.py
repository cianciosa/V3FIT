#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-i", "--ilow", dest="ilow", help="Row lower index.", metavar="ILOW", default=0)
parser.add_option("-j", "--ihigh", dest="ihigh", help="Row upper index.", metavar="IHIGH", default=0)
parser.add_option("-k", "--jlow", dest="jlow", help="Column lower index.", metavar="JLOW", default=0)
parser.add_option("-l", "--jhigh", dest="jhigh", help="Column upper index.", metavar="JHIGH", default=0)
parser.add_option("-v", "--value", dest="value", help="Value to test.", metavar="VALUE", default=0)
parser.add_option("-r", "--range", dest="range", help="Range for the value. This range is +- the value.", metavar="RANGE", default=0)
(option, args) = parser.parse_args()

runlog_file = open(option.filename, "r")

lines = runlog_file.readlines()
i = 0
test_passed = True
istart = 0
while i < len(lines):
	if lines[i].rstrip() == "  *** Jacobian":
		i = i + 1
		istart = i
		while lines[i].rstrip():
			if (i - istart >= int(option.ilow)) and (i - istart <= int(option.ihigh)):
				split_str = lines[i].split()
				for string in split_str[(int(option.jlow) + 1):(int(option.jhigh) + 1)]:
					test_passed = test_passed and ((float(string) <= (float(option.value) + float(option.range))) and (float(string) >= (float(option.value) - float(option.range))))
			i = i + 1
	i = i + 1

runlog_file.close

if not test_passed:
	print("Jacobian value is not equal to {0} +- {1} in file {2}.".format(option.value, option.range, option.filename))
	sys.exit(1)
