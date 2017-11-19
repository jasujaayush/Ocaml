def logger(f):
	def g(*args):
		print "calling " + f.__name__ + " args:" + str(args)
		rv = f(*args)
		print "finished calling " + f.__name__
		return rv
	return g	

@logger
def sum(x,y):
	return x+y


def paramLogger(param):
	def logger(f):
		def g(*args):
			print "calling " + f.__name__ + " args:" + str(args)
			print "param logged : "+ str(param)
			rv = f(*args)
			print "finished calling " + f.__name__
			return rv
		return g
	return logger	

@paramLogger('param test')
def sum(x,y):
	return x+y	