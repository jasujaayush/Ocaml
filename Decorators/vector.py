from misc import Failure

"""
Vector class with implemented methods like addition, 
dot product, comparision and repr.
"""
class Vector(object):
    def __init__(self, param):
    	if isinstance(param, (int, long)):
    		if param < 0:
    			raise ValueError("Vector length cannot be negative")
    		self.l = param*[0.0]
    	else:
    		try:
	    		l = []
	    		for x in param:
	    			l.append(x)
	    		self.l = l
    		except Exception:
    			raise ValueError("Object not iterable or integer or long")

    def __repr__(self):
    	return (self.__class__.__name__+"(" + repr(self.l)+")")

    def __len__(self):
    	return len(self.l)

    def __iter__(self):
    	for x in self.l:
    		yield x

    def dot(self, other):
    	if len(other) != len(self.l):
    		raise ValueError("Vector length mismatch")

    	i = 0
    	prod = 0
    	for x in other:
    		prod += self.l[i]*x
    		i += 1
    	return prod	

    def __add__(self, other):
    	if len(other) != len(self.l):
    		raise ValueError("Vector length mismatch")

    	newList = []
    	i = 0
    	for x in other:
    		newList.append(self.l[i] + x)
    		i += 1
    	return Vector(newList)	

    def __radd__(self, other):
    	if len(other) != len(self.l):
    		raise ValueError("Vector length mismatch")

    	newList = []
    	i = 0
    	for x in other:
    		newList.append(self.l[i] + x)
    		i += 1
    	return Vector(newList)	

    def __iadd__(self, other):
    	if len(other) != len(self.l):
    		raise ValueError("Vector length mismatch")

    	i = 0
    	for x in other:
    		self.l[i] += x
    		i += 1
    	return self

    def __gt__(self, other):
    	a = self.l[:]
    	b = other.l[:]
    	a.sort(reverse=True)
    	b.sort(reverse=True)
    	for i in range(len(b)):
    		if a[i]>b[i]:
    			return True
    	return False
    	
    def __lt__(self, other):
    	a = self.l[:]
    	b = other.l[:]
    	a.sort(reverse=True)
    	b.sort(reverse=True)
    	for i in range(len(b)):
    		if a[i]<b[i]:
    			return True
    	return False

    def __eq__(self, other):
    	if type(other) != Vector:
    		return False
    	a = self.l[:]
    	b = other.l[:]
    	for i in range(len(b)):
    		if a[i] != b[i]:
    			return False
    	return True

    def __ne__(self, other):
    	if type(other) != Vector:
    		return True
    	return not (self==other)

    def __ge__(self, other):
    	a = self.l[:]
    	b = other.l[:]
    	a.sort(reverse=True)
    	b.sort(reverse=True)
    	for i in range(len(b)):
    		if a[i]<b[i]:
    			return False
    	return True
    			
    def __le__(self, other):
    	a = self.l[:]
    	b = other.l[:]
    	a.sort(reverse=True)
    	b.sort(reverse=True)
    	for i in range(len(b)):
    		if a[i]>b[i]:
    			return False
    	return True

    def __getitem__(self, index):
    	return self.l[index]

    def __setitem__(self, index, value):
    	orig = self.l[:]
    	orig[index] = value
    	if len(orig) != len(self.l):
    		raise ValueError("Changing the length of Vector")
    	self.l = orig	


'''
imp.reload(vector)
from vector import *
v = Vector([1,2,3])
x = len(v)

a=Vector([1,3,5])
b=Vector([5,1,3])
c=Vector([4,5,4])
'''    	