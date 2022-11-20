import cmath
from PIL import Image, ImageDraw
import numpy as np
import math
# import matplotlib.pyplot as plt



s = open("points.txt", "r").read().split("\n")

temp = []

for i in s:
	# temp.append()
	x,y = map(eval, [i.split(" ")[3],i.split(" ")[6]])
	temp.append((int(x), int(y)))

points = []
edges = []

for i in range(len(temp)-1):
	edges.append([temp[i],temp[i+1]])

edges.append([temp[0],temp[-1]])

for (x,y) in temp:
    points.append(complex(x,y))

numpoint = len(points)

def Distance(point0, point1):
    return abs(point1-point0)

total_Distance = Distance(points[0],points[-1])

for i in range(len(points)-1):
    total_Distance += Distance(points[i],points[i+1])

def midpoint(edge):
    p0,p1 = edge[0],edge[1]
    return complex((p1[0]+p0[0])/2,(p1[1]+p0[1])/2)


def fourierN(n): 
    sum = 0
    AccumDist = 0 #im not sure.. choose between line 46 and line 48
    for edge in edges:
        Dis = Distance(complex(edge[0][0], edge[0][1]),complex(edge[1][0],edge[1][1]))
        AccumDist += 1
        sum += (midpoint(edge) * cmath.exp(-2*cmath.pi*n*complex(0,1)*(AccumDist/numpoint)) * (1/numpoint))
    return sum


vectors = []

for n in range(-100,100):
    num = fourierN(n)
    # print(num)
    vectors.append((n,num))

##################################################################

print(vectors)

power = 5

final_points = []

for t in range(0,10**power):
    t /= 10**(power)
    p = 0
    for n,c in vectors:
        p += c * cmath.exp(-2*cmath.pi*n*complex(0,1)*t)
    final_points.append(p)

print(final_points)

file_name = "Don.png"

im = Image.open(file_name)

for n in range(len(final_points)-1):
    p1 = (final_points[n].real, final_points[n].imag)
    p2 = (final_points[n+1].real, final_points[n+1].imag)
    shape = [p1,p2]
    # create line image
    img = ImageDraw.Draw(im)  
    img.line(shape, fill="blue", width = 0)
im.show()