#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-S", "--s_name1", dest="s_name1", help="s_name to search for", metavar="S_NAME1")
parser.add_option("-s", "--s_name2", dest="s_name2", help="s_name to search for", metavar="S_NAME2")
parser.add_option("-a", "--attribute", dest="attribute", help="Attribute to search for", metavar="ATTRIBUTE")
parser.add_option("-r", "--range", dest="range", help="Range of allowed deviation between the two signals.", metavar="RANGE", default=0.0)
(option, args) = parser.parse_args()

recout_file = open(option.filename, "r")

test_passed = False
s_index = -1
index = -1
signals_found = False
temp = 1

value1 = 0.0
value2 = 0.0

if option.s_name1 == option.s_name2:
	recout_file.close
	print("Cannot test the value of {0} aganist itself.".format(option.s_name1))
	sys.exit(1)

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
				if string == option.s_name1:
					value1 = float(split_str[index + j - s_index])
				if string == option.s_name2:
					value2 = float(split_str[index + j - s_index])
			test_passed = abs(value1 - value2) <= float(option.range)

recout_file.close

if not test_passed:
	print("{0} for signal {1} is not with in {2} of {0} for signal {3} in file {4}.".format(option.attribute, option.s_name1, option.range, option.s_name2, option.filename))
	sys.exit(1)
