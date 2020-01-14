import os
import sys
from time import sleep
from random import randint
from copy import deepcopy

width = 100
height = 25
jungle = [25,10,10,10]
plant_energy = 20
reproduction_energy = 10
plants = set()
animals = []
day = 0
sym_animal = "M"
sym_plant = "*"

def random_plant(left,top,width,height):
	x = (left+randint(1,width))
	y = (top+randint(0,height))
	new_plant_pos = (x,y)
	plants.add(new_plant_pos)

def add_plant():
	global width
	global height

	# ジャングルとマップ全体の両方に草を生やす
	l,t,w,h = jungle
	random_plant(l,t,w,h)
	random_plant(0,0,width-1,height-1)

class Animal:
	def __init__(self,x,y,energy,direction,genes):
		self.x = x
		self.y = y
		self.energy = energy
		self.dir = direction
		self.genes = genes
	
	def move(self):
		if 2<= self.dir < 5 :
			self.x += 1
		elif self.dir == 1 or self.dir == 5:
			self.x += 0
		else :
			self.x -= 1

		if 0 <= self.dir < 3 :
			self.y -= 1
		elif 4 <= self.dir < 7:
			self.y += 1
		
		self.x = self.x % (width-1)
		self.y = self.y % (height)
		self.energy -= 1
	
	def turn(self):
		s = sum(self.genes)
		r = randint(0,s)

		for i,gene in enumerate(self.genes) :
			r -= gene
			if r <= 0 :
				self.dir = i
				break
	
	def eat(self):
		global plants
		global plant_energy
		remove_plants = []
		for plant in plants:
			if (self.x,self.y) == plant:
				self.energy += plant_energy
				remove_plants.append(plant)
				break

		if remove_plants :
			for plant in remove_plants :
				plants.remove(plant)

	def reproduce(self):
		global reproduction_energy
		if self.energy < reproduction_energy :
			return
		
		energy = int(self.energy/2)
		genes = deepcopy(self.genes)

		# 突然変異 -1,0,1をランダムのスロットに加算
		slot = randint(0,7)
		genes[slot] = max(0,genes[slot]+randint(0,2)-1)

		child = Animal(self.x,self.y,energy,self.dir,genes)
		animals.append(child)

def update_world():
	global animals
	global day
	animals = [animal for animal in animals if animal.energy > 0]

	for i in range(len(animals)):
		animals[i].turn()
		animals[i].move()
		animals[i].eat()
		if day%1 == 0:
			animals[i].reproduce()
	
	add_plant()
	day += 1

def draw_world():
	write,flush = sys.stdout.write,sys.stdout.flush
	write("\x08" * width)

	print("-"*width)
	row = "|"+" "*(width-1)+"|\n"
	view = [row for _ in range(height)]

	for animal in animals :
		view[animal.y] = view[animal.y][:animal.x+1] + sym_animal + view[animal.y][animal.x+2:]

	for plant in plants :
		x,y = plant
		#print(plant)
		view[y-1] = view[y-1][:x] + sym_plant + view[y-1][x+1:]
	
	for v in view :
		write(v)
		flush()
	print("-"*width,end="  ")
	print("DAY : ",day)



def main():
	os.system("clear")
	adam_genes = [randint(0, 10) for i in range(8)]
	adam = Animal(int(width/2),int(height/2),80,0,adam_genes)
	animals.append(adam)

	while True :
		draw_world()
		if len(animals) <= 0:
			print("絶滅してしまいました")
			break
		update_world()
		sleep(.2)


if __name__ == "__main__":
	main()