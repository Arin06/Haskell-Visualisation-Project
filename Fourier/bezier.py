
points = [(0,0),(1,1),(2,1),(3,0),(4,0)]

def average(x,y):
	return ((x[0]+y[0])/2, (x[1]+y[1])/2)

def vectorize(x, y):
	return (x[0] - y[0], x[1] - y[1])

def vecadd(x,y):
	return (x[0] + y[0], x[1] + y[1])

def cross(x,y):
	return x[0]*y[1] - x[1]*y[0]

def scale(x,c):
	return (x[0]*c, x[1]*c)

guesses = []
for (i,j) in zip(points,points[1:]):
	guesses.append(average(i,j))

for _ in range(100):
	edges = []
	for e,(i,j) in enumerate(zip(guesses, guesses[1:])):
		edges.append((vectorize(j,i),vectorize(points[e+1],i)))

	for e,(i,j) in enumerate(edges):
		guesses[e+0] = vecadd(guesses[e+0],scale((-i[1],i[0]),cross(i,j)*0.1))
		guesses[e+1] = vecadd(guesses[e+1],scale((-i[1],i[0]),cross(i,j)*0.1))

print(guesses)