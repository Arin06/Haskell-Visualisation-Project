from PIL import Image
import numpy as np

def avg(lst):
	return int(sum(lst)/len(lst))

file_name = "Don.jpg"

im = Image.open(file_name)
array = np.asarray(im)
# array.setflags(write=True)
r = {}; g = {}; b = {}

def add_tuple(xs,ys):
	return tuple([x+y for x,y in zip(xs,ys)])

def d2avg(x0,x1,y0,y1,array):
	# x: x0 .. x1 y: y0 .. y1
	temp = array[(x1,y1)]
	if x0 != 0:
		temp -= array[(x0-1,y1)]
	if y0 != 0:
		temp -= array[(x1,y0-1)]
	if x0 != 0 and y0 != 0:
		temp += array[(x0-1, y0-1)]
	temp /= (x1-x0+1) * (y1-y0+1)
	return temp

for row in range(len(array)):
	for col in range(len(array[row])):
		position = (row, col)
		r[position], g[position], b[position] = map(int, array[row][col])
		# print(r,row,col,array[row][col])
		# print(type(temp))

		if row == 0 and col == 0:
			continue
		if col != 0:
			# print(r[add_tuple(position,(0,-1))],r[position])
			r[position] += r[add_tuple(position,(0,-1))]
			g[position] += g[add_tuple(position,(0,-1))]
			b[position] += b[add_tuple(position,(0,-1))]
		if row != 0:
			r[position] += r[add_tuple(position,(-1,0))]
			g[position] += g[add_tuple(position,(-1,0))]
			b[position] += b[add_tuple(position,(-1,0))]
		if row != 0 and col != 0:
			r[position] -= r[add_tuple(position,(-1,-1))]
			g[position] -= g[add_tuple(position,(-1,-1))]
			b[position] -= b[add_tuple(position,(-1,-1))]
		# break
	# break

# test_case for avg
# 
# print(r)
# for i in range(5):
# 	for j in range(5):
# 		print(array[i][j],end = "\t")
# 	print()
# print(d2avg(1,1,5,5,r))
# # print(add_tuple((1,1),(0,-1)))

res = []
maxVal = 0

radii = 3

for row in range(len(array)-radii):
	rowTemp = []
	for col in range(len(array[row])-radii):
		temp = 0
		avg = d2avg(row,row+radii-1,col,col+radii-1,r)
		for i in range(radii):
			for j in range(radii):
				temp += (array[row+i,col+j][0] - avg) ** 2
		avg = d2avg(row,row+radii-1,col,col+radii-1,r)
		for i in range(radii):
			for j in range(radii):
				temp += (array[row+i,col+j][1] - avg) ** 2
		avg = d2avg(row,row+radii-1,col,col+radii-1,r)
		for i in range(radii):
			for j in range(radii):
				temp += (array[row+i,col+j][2] - avg) ** 2
		temp = temp ** 0.5
		if temp > maxVal:
			maxVal = temp
		rowTemp.append([temp] * 3)
	res.append(rowTemp)

res = np.array(res)
res /= (maxVal/255)
# res = res ** 2 / 255 
res = res * -1 + 255
res = res.astype(np.uint8)
im = Image.fromarray(res)

im.save(file_name.split(".")[0] + "_spread.png","PNG")