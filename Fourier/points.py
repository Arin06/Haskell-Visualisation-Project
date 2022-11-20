from PIL import Image, ImageDraw
import numpy as np

file_name = "Don_spread.png"

im = Image.open(file_name)
array = np.asarray(im)
# array.setflags(write=True)
# r = {}; g = {}; b = {}

odd_points = []

def distinct(lst):
	if lst == []:
		return True
	return not (lst[0] in lst[1:]) and distinct(lst[1:])

for row in range(len(array)):
	for col in range(len(array[row])):
		# print(array[row][col])
		r, g, b = map(int, array[row][col][0:3])
		if r != g or g != b or r != b:
			odd_points.append((col,row))

def getDistance(pa, pb):
	dx = pa[0] - pb[0]
	dy = pa[1] - pb[0]
	return (dx**2 + dy ** 2) ** 0.5

# print(odd_points)
print(distinct(odd_points))

distances = {}
n         = len(odd_points)

for i in range(n):
	for j in range(i):
		distances[(j,i)] = getDistance(odd_points[i],odd_points[j])

# print(distances)

loop = [(n-1,0)]

for i in range(n-1):
	loop.append((i,i+1))

# totalDistance = 0
# for i in loop:
# 	totalDistance += distances[i]

# print(loop,totalDistance)

# for i in range(n):
# 	print(loop[i])

def tord(a, b):
	if a < b:
		return (a, b)
	return (b, a)

while True:
	looper = False
	for i in range(n):
		for j in range(i):
			vi1, vi2 = loop[i]
			vj1, vj2 = loop[j]
			# testing vi1 < vj2 and vj1 < vi2
			# if vi1 > vj2 or vj1 > vi2:
			# 	continue
			if not distinct([vi1, vi2, vj1, vj2]):
				continue
			if distances[tord(vi1, vi2)] + distances[tord(vj1, vj2)] > \
				distances[tord(vi1, vj1)] + distances[tord(vi2, vj2)]:
				loop.remove((vi1, vi2))
				loop.remove((vj1, vj2))
				loop.append((vi1, vj1))
				loop.append((vi2, vj2))
				looper = True
				break
	if looper == False:
		break

im = Image.open(file_name)
for edge in loop:
	i, j = edge
	shape = [odd_points[j],odd_points[i]]
	# create line image
	img = ImageDraw.Draw(im)  
	img.line(shape, fill="blue", width = 0)
im.show()

# print(sortedList,len(sortedList))



# temp_v = 0
# # print(temp_v, loop)
# sortedList = []

# while loop != []:
# 	m = loop.pop(0)
# 	if m[0] == temp_v:
# 		sortedList.append(m[1])
# 		temp_v = m[1]
# 	elif m[1] == temp_v:
# 		sortedList.append(m[0])
# 		temp_v = m[0]
# 	else:
# 		loop.append(m)



# temp = []
# for i in loop:
# 	temp += [i[0], i[1]]
# temp.sort()
# print(temp)