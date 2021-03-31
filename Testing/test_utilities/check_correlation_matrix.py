#!/usr/bin/python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename", help="File name to test.", metavar="FILE")
parser.add_option("-p", "--param", dest="param", help="Parameter type", metavar="PARAM", default="Reconstruction")
(option, args) = parser.parse_args()

recout_file = open(option.filename, "r")

correlations_found = False
skip = False
matrix = []
index = 0
for line in recout_file.readlines():
	split_str = line.split()
	if (correlations_found == False) and (len(split_str) == 5) and (split_str[1] == option.param):
		correlations_found = True
		skip = True
		continue
	if skip == True:
		skip = False
		continue
	if correlations_found == True:
		if len(split_str) > 1:
			matrix.append([])
			matrix[index] = split_str[1:]
			index += 1
		else:
			break

recout_file.close

for j in range(0,index - 1):
	for i in range(0, index - 1):
		if (i == j) and (matrix[i][j] != "1.00000E+00"):
			print("Diagonal elemet {0} is not equal to 1.00000E+00. {1}".format(i, matrix[i][j]))
			sys.exit(1)
		elif (float(matrix[i][j]) < (float(matrix[j][i]) - 1.0E-15)) and (float(matrix[i][j]) > (float(matrix[j][i]) + 1.0E-15)):
			print("Offdiagonal elements {0} {1} are not equal. {2} {3}".format(i, j, matrix[i][j], matrix[j][i]))
			sys.exit(1)
