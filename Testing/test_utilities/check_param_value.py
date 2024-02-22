#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-p", "--p_type", dest="p_type", help="p_type to search for", metavar="P_TYPE")
parser.add_option("-v", "--value", dest="value", help="Value to test.", metavar="VALUE", default=0)
parser.add_option("-r", "--range", dest="range", help="Range for the value. This range is +- the value.", metavar="RANGE", default=0)
parser.add_option("-i", "--index1", dest="index1", help="Index 1", metavar="INDEX1", default=0)
parser.add_option("-j", "--index2", dest="index2", help="Index 2", metavar="INDEX2", default=0)
(option, args) = parser.parse_args()

recout_file = open(option.filename, "r")

test_passed = False

start = -1
end = -1
lines = recout_file.readlines()
for i, line in enumerate(lines):
	if (start == -1) and (line.rstrip() == "  *** Reconstruction parameters"):
		start = i + 2
	if (start != -1) and (end == -1) and (not line.rstrip()):
		end = i

value = 0

if start != -1:
	for line in lines[start:end]:
		split_str = line.split()
		if (split_str[1] == option.p_type) and (int(split_str[2]) == int(option.index1)) and (int(split_str[3]) == int(option.index2)):
            value = float(split_str[4])
			test_passed = (value <= (float(option.value) + float(option.range))) and (value >= (float(option.value) - float(option.range)))

recout_file.close

if not test_passed:
	print("Value {0} for p_type {1}({2},{3}) is not equal to {4} +- {5} in file {6}.".format(value, option.p_type, option.index1, option.index2, option.value, option.range, option.filename))
	sys.exit(1)
