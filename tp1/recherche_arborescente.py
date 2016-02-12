import math
import itertools as it
import copy
import Queue
import time
import sys

def distSqrd(p1, p2):
	p = (p1[0]-p2[0], p1[1]-p2[1])
	return p[0]*p[0] + p[1]*p[1]

class Antenna:
	def __init__(self, positions):
		self.minX = 2**31
		self.minY = 2**31
		self.maxX = -2**31 - 1
		self.maxY = -2**31 - 1
		self.positions = []
		self.r = None
		self.pos = None
		for pos in positions:
			self.addPoint(pos)
		
	def cost(self, K, C):
		return K + C * self.r * self.r
		
	def info(self):
		return (self.pos[0], self.pos[1], self.r)
		
	def addPoint(self, point):
		self.positions.append(point)
		self.minX = min(self.minX, point[0])
		self.maxX = max(self.maxX, point[0])
		self.minY = min(self.minY, point[1])
		self.maxY = max(self.maxY, point[1])
		
		self.pos = (self.minX + (self.maxX-self.minX)//2, self.minY + (self.maxY-self.minY)//2)
		
		distMax = -2**31 - 1
		for point in self.positions:
			distMax = max(distMax, distSqrd(self.pos,point))
		self.r = int(math.ceil(math.sqrt(distMax)))
		
	def __str__(self):
		out = 'Antenna : p = ' + str(self.pos) + '  r = ' + str(self.r) + '\n'
		for point in self.positions:
			 out += '\t Point :' + str(point) + '\n'
		return out
		
class State:
	def __init__(self, antennas, unassigned):
		self.antennas = antennas
		self.unassigned = unassigned
		self.children = []
		
	def __lt__(self, other):
		return True
		
	def generateChildren(self):
		self.children = []
		
		#Create new antenna
		for point in self.unassigned:
			antenna = Antenna([point])
			unassigned = copy.deepcopy(self.unassigned)
			unassigned.remove(point)
			antennas = copy.deepcopy(self.antennas)
			antennas.append(antenna)
			self.children.append(State(antennas, unassigned))
			
		#Assign point to an already existing antenna
		for index, antenna in enumerate(self.antennas):
			for point in self.unassigned:
				antennas = copy.deepcopy(self.antennas)
				antennas[index].addPoint(point)
				unassigned = copy.deepcopy(self.unassigned)
				unassigned.remove(point)
				self.children.append(State(antennas, unassigned))
				
	def children(self):
		return self.children
		
	def unassigned(self):
		return self.unassigned
		
	def antennas(self):
		return self.antennas
		
	def printState(self):
		print '### State ###'
		for a in self.antennas:
			print(a)
		print 'unassigned points : ', self.unassigned
		
	def stateCost(self, K, C):
		return sum([antenna.cost(K,C) for antenna in self.antennas])
		
	def heuristic(self, K, C):
		return 0
		
	def cost(self, K, C):
		return self.stateCost(K,C) + self.heuristic(K, C)
		

def search(positions, K, C):
	initialState = State([], positions)
	
	sortedQueue = Queue.PriorityQueue()
	sortedQueue.put_nowait((initialState.cost(K, C), initialState))
	
	while sortedQueue:
		state = sortedQueue.get_nowait()[1]
		
		if not state.unassigned:
			return [a.info() for a in state.antennas]
		else:
			state.generateChildren()
			for c in state.children:
				sortedQueue.put_nowait((c.cost(K,C), c))
			

def main():
	K = 200
	C = 1
	start = time.clock()
	result = search([(30,0),(10,10),(20,20),(30,40),(50,40), (45,40)],K,C)
	end = time.clock()
	
	print 'Time elapsed :', end-start 
	print result

if __name__ == '__main__':
	main()
	