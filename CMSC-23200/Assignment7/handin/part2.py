import numpy as np
import struct

def float_to_binary(num):
	return ''.join(bin(c).replace('0b', '').rjust(8, '0') for c in struct.pack('!d', num))

def binary_to_float(b):
	bf = int(b, 2).to_bytes(8,'big')  
	return struct.unpack('>d', bf)[0]

def my_laplace(scale=1):
	u = np.random.uniform()
	b = np.random.randint(2)
	l = (2*b-1)*np.log(u)
	return l*scale

'''
def find_bits():
	ret_count = 0
	for x in np.arange(1000):
		x = np.random.uniform()
		x_p = np.exp(np.log(x))
		if (float_to_binary(x) != float_to_binary(x_p)):
			print("******************************")
			print("{:>20} : {:>12}".format(str(x),float_to_binary(x)))
			print("{:>20} : {:>12}".format(str(x_p),float_to_binary(x_p)))
			count = 1
			x_int = int(float_to_binary(x), 2)
			x_p_int = int(float_to_binary(x_p), 2)
			while( x_int != x_p_int ):
				x_int = x_int >> 1 
				x_p_int = x_p_int >> 1
				count = count + 1

			count = count - 1
			print("Differ by: " + str(count))

			if (count > ret_count):
				ret_count = count

	return ret_count

def diff_1():

	print(float_to_binary(1.0))
	y = my_laplace()
	y_p = 1.0 + y
	print("{:>20} : {:>12}".format(str(y),float_to_binary(y)))
	print("{:>20} : {:>12}".format(str(y_p),float_to_binary(y_p))) 

'''



# return true if its the pure scaled laplace ( not adding by 1.0 )

#if the (number represented by the exp bit - 1023) 

#check of (exp - 1023) is bigger than -53
# if so, float must be in U


# check if you can get an x to match x_p
def is_scaled_laplace_img(y, scale):
	#Send y to negative to make easier to operate on data
   	#Generated Bit Difference Between x and x_p
	ret = False
	diff = 6
	#print("Using Bit Diff of: " + str(diff))

	y = -abs(y)

	#print("Y:")
	#print("{:>20} : {:>12}".format(str(y),float_to_binary(y)))
	#if the log of cnadidates generated from x_p = y -> we've found a float that works
	x_p = np.exp(y/scale)

	#print("x_p:")
	#print("{:>20} : {:>12}".format(str(x_p),float_to_binary(x_p)))

	#x_int = base candidate
	x_int = ((int(float_to_binary(x_p), 2) >> diff) << diff)
	#print("x_int: " + str(x_int))
	#print(format(x_int, 'b'))


	#y_int = (int(float_to_binary(y), 2))
	#print("y_int: " + str(y_int))
	#print(format(y_int, 'b'))

	diff = 2**diff
	#print("Testing # of Xs: " + str(diff))

	x_float = 0.0

	for off in range(diff):
		#print("Testing x_int: ")
		#print(format(x_int + off, 'b'))
		x_float = binary_to_float( format(x_int + off, 'b') )
		#print("Testing x_float: ")
		#print(float_to_binary(x_float))

		y_test = scale*(1)*np.log(x_float)
		#print("Testing y_test: ")
		#print("{:>20} : {:>12}".format(str(y_test),float_to_binary(y_test)))

		if( float_to_binary(y_test) == float_to_binary(y) ):
			ret = True
			break


	#print("{:>20} : {:>12}".format(str(x_float),float_to_binary(x_float)))
	bias = ((int(float_to_binary(x_float),2) >> 52) & 2047)
	trail = len(float_to_binary(x_float))-len(float_to_binary(x_float).rstrip('0'))

	#print("{:>20} : {:>12}".format(str(bias),format(bias, 'b')))

	if( (bias - 1023) > trail ):
		ret = False

	if ( (x_float % (1 / (2 ** 53) ) ) != 0 ):
		ret = False
	#if( (bias == 1017) or (bias == 1021) or (bias == 1019) ):
	#	ret = False


	#if(ret): 
		#print("Found one!")
	#else:
		#print("None Found")
	return ret

# driver/test code below here
#print("Largest Deviation: " + str(find_U()))


t = 0
f = 0
for x in range(1000):

	y =  my_laplace()
	ret = is_scaled_laplace_img(y, 1.0)
	if ( ret ):
		t = t + 1
	else:
		f = f + 1
	
print("Trues: " + str(t))
print("Falses: " + str(f))

'''
y = my_laplace()
is_scaled_laplace_img(y, 1.0)
'''
