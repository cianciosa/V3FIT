#!/usr/bin/python

from optparse import OptionParser
import sys

def get_num_iter(lines, start, offset):
	if (start + offset >= len(lines)):
		return -1
	if (len(lines[start+offset].split()) != 6):
		return -1
	return int(lines[start+offset].split()[4])

def check_num_iter(lines, start, offset):
	test_value = get_num_iter(lines, start, 0)
	if (test_value == -1):
		return False
	return get_num_iter(lines, start, offset) == test_value

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
(option, args) = parser.parse_args()

runlog_file = open(option.filename, "r")

test_passed = False

start = -1
lines = runlog_file.readlines()
for i, line in enumerate(lines):
	if (start == -1) and (line.rstrip() == "  *** Reconstruction step    1"):
		start = i + 1
		break

if start != -1:
	test_passed = check_num_iter(lines, start, 1) and check_num_iter(lines, start, 2) and check_num_iter(lines, start, 3)
	test_passed = test_passed and check_num_iter(lines, start + 4, 1) and check_num_iter(lines, start + 4, 2) and check_num_iter(lines, start + 4, 3)
	test_passed = test_passed and check_num_iter(lines, start + 8, 1) and check_num_iter(lines, start + 8, 2) and check_num_iter(lines, start + 8, 3)
	test_passed = test_passed and check_num_iter(lines, start + 12, 1) and check_num_iter(lines, start + 12, 2) and check_num_iter(lines, start + 12, 3)
	test_passed = test_passed and check_num_iter(lines, start + 16, 1) and check_num_iter(lines, start + 16, 2) and check_num_iter(lines, start + 16, 3)
	test_passed = test_passed and check_num_iter(lines, start + 20, 1) and check_num_iter(lines, start + 20, 2) and check_num_iter(lines, start + 20, 3)
	test_passed = test_passed and check_num_iter(lines, start + 24, 1) and check_num_iter(lines, start + 24, 2) and check_num_iter(lines, start + 24, 3)

if not test_passed:
	print("VMEC took different numbers of iterations to converge in jacobian for the same parameter.")
	sys.exit(1)
