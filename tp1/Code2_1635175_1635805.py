import math
import itertools as it
import copy
import Queue
import time
import sys
from random import randint
from random import seed

class Antenna:
	K = 200
	C = 1

	def __init__(self, position):
		self.r = 0
		self.pos = position
		self.minX = position[0]
		self.maxX = position[0]
		self.minY = position[1]
		self.maxY = position[1]
		self.cost = Antenna.K + Antenna.C * self.r * self.r

	def cost(self):
		return self.cost

	def info(self):
		return (self.pos[0], self.pos[1], self.r)

	def merge(self, a):
		newAntenna = Antenna((0,0))
		newAntenna.setRange(self,a)
		return newAntenna

	def setRange(self, a1, a2):
		self.minX = min(a1.minX,a2.minX)
		self.maxX = max(a1.maxX,a2.maxX)
		self.minY = min(a1.minY,a2.minY)
		self.maxY = max(a1.maxY,a2.maxY)
		self.pos = (self.minX + (self.maxX-self.minX)//2, self.minY + (self.maxY-self.minY)//2)

		self.r = int(math.ceil(max((self.maxX - self.minX), (self.maxY - self.minY)) / 1.4142))
		self.cost = Antenna.K + Antenna.C * self.r * self.r

	def __str__(self):
		out = 'Antenna : p = ' + str(self.pos) + '  r = ' + str(self.r) + '\n'
		for point in self.positions:
			 out += '\t Point :' + str(point) + '\n'
		return out


def search(positions, K, C):
	Antenna.C = C
	Antenna.K = K
	state = [Antenna(point) for point in positions]

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
	K = 20000
	C = 100

	seed(110)

	points = []
	for i in range(500):
		points.append((randint(0,1000),randint(0,1000)))

	start = time.clock()
	result = search(points,K,C)
	end = time.clock()
	max = sum([K + C * a[2]*a[2] for a in result]);
	print 'Cost :', max
	print 'Time elapsed :', end-start
	print result

if __name__ == '__main__':
	main()
