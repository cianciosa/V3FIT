#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-p", "--p_type", dest="p_type", help="p_type to search for", metavar="P_TYPE")
parser.add_option("-i", "--index1", dest="index1", help="Index 1", metavar="INDEX1", default=0)
parser.add_option("-j", "--index2", dest="index2", help="Index 2", metavar="INDEX2", default=0)
parser.add_option("-x", "--range_h", dest="range_h", help="High range type.", metavar="RANGE_H", default="infinity")
parser.add_option("-y", "--range_l", dest="range_l", help="Low range type.", metavar="RANGE_L", default="infinity")
parser.add_option("-a", "--indexh1", dest="indexh1", help="High range index 1", metavar="HINDEX1", default=0)
parser.add_option("-b", "--indexh2", dest="indexh2", help="High range index 2", metavar="HINDEX2", default=0)
parser.add_option("-c", "--indexl1", dest="indexl1", help="Low range index 1", metavar="LINDEX1", default=0)
parser.add_option("-d", "--indexl2", dest="indexl2", help="Low range index 2", metavar="LINDEX2", default=0)
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

if start != -1:
	for line in lines[start:end]:
		split_str = line.split()
		if (split_str[1] == option.p_type) and (int(split_str[2]) == int(option.index1)) and (int(split_str[3]) == int(option.index2)):
			test_passed = (split_str[7] == option.range_h) and (split_str[8] == option.range_l) and (int(split_str[11]) == int(option.indexh1)) and (int(split_str[12]) == int(option.indexh2)) and (int(split_str[13]) == int(option.indexl1)) and (int(split_str[14]) == int(option.indexl2))

recout_file.close

if not test_passed:
	print("Range for p_type {0}({1},{2}) not correct for file {3}".format(option.p_type, option.index1, option.index2, option.filename))
	sys.exit(1)
