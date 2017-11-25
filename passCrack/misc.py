#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1
# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned.""" 
    if len(l) == 0: #Base case, length 0, return None
      return None

    #maintain variable c as closest, update it new element comes up  
    c = l[0]
    for e in l:
      if abs(e - v) < abs(c - v):
        c = e
    return c
        
def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    #iterate over both lists simultaneously and make a dict
    d = {}
    for i in range(len(keys)):
      d[keys[i]] = values[i]
    return d  
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    #using str.alnum to check if character in string is alpha numeric
    #iterate over the whole test one character at a time
    #Form a word when something no alpha-numeric comes up and word so far has length > 0
    count = {}
    f = open(fn, 'r')
    s = f.read().lower()
    word = ""
    for c in s:
      if str.isalnum(c) or c == "_":
        word += c
      elif len(word) > 0:
        if not count.has_key(word): 
          count[word] = 0
        count[word] += 1
        word = ""
    return count  
        


    








