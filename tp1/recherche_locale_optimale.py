import math
import itertools as it
import copy
import Queue
import time
import sys
from random import randint
from random import seed

def distSqrd(p1, p2):
	p = (p1[0]-p2[0], p1[1]-p2[1])
	return p[0]*p[0] + p[1]*p[1]

class Antenna:
	K = 200
	C = 1
	
	def __init__(self, positions):
		self.positions = []
		self.r = None
		self.pos = None
		self.cost = 0
		self.setPoints(positions)
		self.cost = Antenna.K + Antenna.C * self.r * self.r
		
	def cost(self):
		return self.cost
		
	def info(self):
		return (self.pos[0], self.pos[1], self.r)
		
	def merge(self, a):
		return Antenna(self.positions + a.positions)
		
		
	def setPoints(self, points):
		self.positions = points
		minX = sys.maxint
		maxX = -sys.maxint-1
		minY = sys.maxint
		maxY = -sys.maxint-1
		
		for p in self.positions:
			minX = min(minX,p[0]);
			maxX = max(maxX,p[0]);
			minY = min(minY,p[1]);
			maxY = max(maxY,p[1]);
		
		self.pos = (minX + (maxX-minX)//2, minY + (maxY-minY)//2)
		
		distMax = -sys.maxint - 1
		for point in self.positions:
			distMax = max(distMax, distSqrd(self.pos,point))
		self.r = int(math.ceil(math.sqrt(distMax)))
		
	def __str__(self):
		out = 'Antenna : p = ' + str(self.pos) + '  r = ' + str(self.r) + '\n'
		for point in self.positions:
			 out += '\t Point :' + str(point) + '\n'
		return out
		

def search(positions, K, C):
	Antenna.C = C
	Antenna.K = K
	state = [Antenna([point]) for point in positions]
	
	while True:
		maxDiff = 0
		minMerge = (-1,-1)
		
		for i, a1 in enumerate(state):
			for j in range(i+1,len(state)):
				a2 = state[j]
				
				costBefore = a1.cost + a2.cost
				diff = costBefore - a1.merge(a2).cost
				
				if diff > maxDiff:
					maxDiff = diff
					minMerge = (i,j)
		
		if maxDiff <= 0:
			break;
		
		mergedAntenna = state[minMerge[0]].merge(state[minMerge[1]])
		
		minIndex = min(minMerge)
		maxIndex = max(minMerge)
		del state[maxIndex]
		del state[minIndex]
		
		state.append(mergedAntenna)
			
	return [a.info() for a in state]		

def main():
	K = 200
	C = 1
	
	seed(100)
	
	points = []
	for i in range(200):
		points.append((randint(0,200),randint(0,200)))
	
	start = time.clock()
	result = search(points,K,C)
	end = time.clock()
	
	max = sum([K + C * a[2]*a[2] for a in result]);
	print 'Cost :', max
	print 'Time elapsed :', end-start 
	print result

if __name__ == '__main__':
	main()
	