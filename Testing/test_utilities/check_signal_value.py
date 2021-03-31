#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-s", "--s_name", dest="s_name", help="s_name to search for", metavar="S_NAME")
parser.add_option("-a", "--attribute", dest="attribute", help="Attribute to search for", metavar="ATTRIBUTE")
parser.add_option("-v", "--value", dest="value", help="Value to test.", metavar="VALUE", default=0)
parser.add_option("-r", "--range", dest="range", help="Range for the value. This range is +- the value.", metavar="RANGE", default=0)
(option, args) = parser.parse_args()

recout_file = open(option.filename, "r")

test_passed = False
s_index = -1
index = -1
signals_found = False
temp = 1
for line in recout_file.readlines():
	split_str = line.split()
	if (signals_found == False) and (len(split_str) == 2) and (split_str[1] == "Signals"):
		signals_found = True
	if signals_found == True:
		for i, string in enumerate(split_str):
			if string == "s_name":
				s_index = i
			if string == option.attribute:
				index = i
		if index != -1 and s_index != -1:
			for j, string in enumerate(split_str):
				if string == option.s_name:
					test_passed = (float(split_str[index + j - s_index]) <= (float(option.value) + float(option.range))) and (float(split_str[index + j - s_index]) >= (float(option.value) - float(option.range)))

recout_file.close

if not test_passed:
	print("{0} for signal {1} is not equal to {2} +- {3} in file {4}.".format(option.attribute, option.s_name, option.value, option.range, option.filename))
	sys.exit(1)
