#!/usr/bin/env python
# Import modules
import time
import datetime
# Read settings file
settings_file = open('./grimm_settings.txt')
input_file = settings_file.readline().rstrip()
output_file = settings_file.readline().rstrip()
model = settings_file.readline().rstrip()
sizes_list = settings_file.readline().rstrip()
# If it is an invalid record ...put an invalid data line
if model == '107':
	pad_line = 'NaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN'
if model == '108':
	pad_line = 'Nan\tNaN\tNaN'
if model == '109':
	pad_line = 'NaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN\tNaN'
if model == '11E':
  pad_line = 'NaN\tNaN\tNaN\tNaN'
parsed_file = open(output_file,'w')
parsed_file.write('date\terror\t' + sizes_list + '\n')
with open(input_file,'r') as datafile:
	while True:
		if model == '107':
			line = datafile.readline()
			if not line: break
			if len(line)<30: continue
			while ((line) and (line[24]!='P')):
				line = datafile.readline()
				if len(line)<30:break
			if len(line)<30: continue
			if not line: break
			#The starting point is a P line that's already in c_line
			#Get date, time and error code
			#idx1 = line.find(',')
			#p_vec = line[idx1+2:].split()
			#c_timestamp = line[:idx1]
			#output_line = '"' + c_timestamp + '"' + '\t' + p_vec[7] + '\t'
			#invalid = 0
			p_vec = line[3:].split()
			c_timestamp = '\t'.join(p_vec)
			output_line = c_timestamp + '\t'
			invalid = 0
			# Get 2 C lines and populate tha data lists
			# First line
			line = datafile.readline()
			if (line[24]=='P'):continue
			if not line: break
			if (line[0]=='K'):line = datafile.readline();line = datafile.readline()
			if not line: break
			if ((line[24]=='C') and (len(line)>65)):
				idx1 = line.find('_')
				d_vec = line[idx1+1:].split()
				output_line = output_line + '\t'.join(d_vec[0:7])
			else:
				invalid = 1
			# Second line
			line = datafile.readline()
			if not line: break
			if ((line[24]=='c') and (len(line)>65)):
				idx1 = line.find('_')
				d_vec = line[idx1+1:].split()
				output_line = output_line + '\t' + '\t'.join(d_vec)
			else:
				invalid = 1
		if model == '108':
			line = datafile.readline()
			#print(line)
			if not line: break
			if len(line)<30: continue
			while ((line) and (line[0]!='P')):
				line = datafile.readline()
				if len(line)<30:break
			if len(line)<30: continue
			if not line: break
			#The starting point is a P line that's already in line
			#Get date, time and error code
			#idx1 = line.find(',')
			p_vec = line[3:].split()
			c_timestamp = '\t'.join(p_vec)
			output_line = c_timestamp + '\t'
			invalid = 0
			# Get 1 N and 1 n lines to populate tha data lists
			# First line
			line = datafile.readline()
			if not line: break
			if (line[0]=='K'):line = datafile.readline();line = datafile.readline()
			if not line: break
			if ((line[0]=='N') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t'.join(d_vec[1:9])
			else:
				invalid = 1
			# Second line
			line = datafile.readline()
			if not line: break
			if ((line[0]=='n') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t' + '\t'.join(d_vec[1:8])
			else:
				invalid = 1
		if model == '109':
			line = datafile.readline()
			#print(line)
			if not line: break
			if len(line)<30: continue
			while ((line) and (line[0]!='P')):
				line = datafile.readline()
				if len(line)<30:break
			if len(line)<30: continue
			if not line: break
			#The starting point is a P line that's already in line
			#Get date, time and error code
			#idx1 = line.find(',')
			p_vec = line[3:].split()
			c_timestamp = '\t'.join(p_vec)
			output_line = c_timestamp + '\t'
			invalid = 0
			# Get 2 C and 2 c lines to populate tha data lists
			# First line
			line = datafile.readline()
			if not line: break
			if (line[0]=='K'):line = datafile.readline();line = datafile.readline()
			if not line: break
			if ((line[0]=='C') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t'.join(d_vec[1:9])
			else:
				invalid = 1
			# Second line
			line = datafile.readline()
			if not line: break
			if ((line[0]=='C') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t' + '\t'.join(d_vec[1:8])
			else:
				invalid = 1
			line = datafile.readline()
			if not line: break
			if ((line[0]=='c') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t' + '\t'.join(d_vec[1:9])
			else:
				invalid = 1
			# Second line
			line = datafile.readline()
			if not line: break
			if ((line[0]=='c') and (len(line)>65)):
				d_vec = line[2:].split()
				output_line = output_line + '\t' + '\t'.join(d_vec[1:9])
			else:
				invalid = 1
		if model == '11E':
			line = datafile.readline()
			#print(line)
			if not line: break
			if len(line)<20: continue
			while ((line) and (line[0]!='P')):
				line = datafile.readline()
				if len(line)<20:break
			if len(line)<20: continue
			if not line: break
			#The starting point is a P line that's already in line
			#Get date, time and error code
			#idx1 = line.find(',')
			p_vec = line[3:33].split()
			c_timestamp = '\t'.join(p_vec)
			output_line = c_timestamp + '\t'
			invalid = 0
			# Get 1 N_ line to populate the data lists
			# First line
			line = datafile.readline()
			if not line: break
			if (line[0]=='K'):line = datafile.readline();line = datafile.readline()
			if not line: break
			if ((line[0]=='N') and (len(line)>35)):
				d_vec = line[2:].split()
				output_line = output_line + '\t'.join(d_vec)
			else:
				invalid = 1
		if invalid:
			#There was not a full sample
			#So, print an invalid record
			parsed_file.write(c_timestamp + '\t' + pad_line + '\n')
		else:
			#We have a full period so print the record to file
			parsed_file.write(output_line + '\n')
parsed_file.close()
datafile.close()
