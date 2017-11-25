
from misc import *
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""   
    l = []   
    f = open(filename)   
    for word in f: #iterate one line at a time
      if re.match(regexp, word[:-1]): #removing the endline character at the end of word
        l.append(word[:-1])
    return l

def transform_reverse(string):
    return [string, string[::-1]]

def transform_capitalize(string):
    # use recursion, modify first element to capital if it is an alphabet
    # make a call for remaining string, append the results with both lower case
    # and upper case version of the character
    if string == "": 
        results = [""]
    else:
        results = []
        c = string[0]    
        l = transform_capitalize(string[1:])
        for sub in l:
          results.append(c + sub)
          if c.upper() != c:
            results.append(c.upper() + sub)
    return results

#dict to handle similar looking character transformations       
smap = {}
smap['o'] = 0          
smap['i'] = 1
smap['l'] = 1          
smap['z'] = 2
smap['e'] = 3          
smap['a'] = 4          
smap['s'] = 5          
smap['b'] = [6,8]          
smap['t'] = 7          
smap['g'] = 9                    
smap['q'] = 9

# Simmilar logic as handling upper case but we use smap here for transformations
def transform_digits(string):
    if string == "":
        results = [""]
    else:
        results = []
        c = string[0]   
        l = transform_digits(string[1:])
        for sub in l:
          results.append(c + sub)
          if smap.has_key(c.lower()) and c.lower() != "b":
            results.append(str(smap[c.lower()]) + sub)
          elif c.lower() == "b":
            results.append(str(smap['b'][0]) + sub)  
            results.append(str(smap['b'][1]) + sub)  
    return results                                                                                                                

def check_pass(plain,enc):
    return (crypt.crypt(plain, enc[:2]) == enc)

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    results = []   
    fields = ["account", "password", "UID", "GID", "GECOS", "directory", "shell"] #fields in one line   
    f = open(filename)
    for line in f: #one line at a a time
      words = line.strip('\n').split(':')  #get words split by character ':'
      d= {}
      for i in range(len(words)):
        d[fields[i]] = words[i]
      results.append(d)  
    return results

import time   
def crack_pass_file(pass_filename,words_filename,out_filename):
      """Crack as many passwords in file fn_pass as possible using words
         in the file words"""
      start_time = time.time()   
      outfile = open(out_filename,'w')
      passwds = load_passwd("passwd")
      cracked = [] #list to store users with cracked passwords
      words = load_words(words_filename,r"^\w{6,8}$") #regex to match word length 6 to 8
      rev_words = [t for w in words for t in transform_reverse(w)]
      for p in passwds:  #processing to see if words or their reversed version match a password
        for w in rev_words:
          if check_pass(w,p["password"]):
            outfile.write( p["account"] + "=" + w + "\n")
            cracked.append(p["account"])
            break
      outfile.flush()      
      print "Done with reverse word checks"

      dig_words = [w for t in rev_words for w in transform_digits(t)]
      for p in passwds: #processing to see if words or their "reversed and digit" version match a password
        if not p["account"] in cracked:
          for w in dig_words:
            if check_pass(w,p["password"]):
              outfile.write(p["account"] + "=" + w + "\n")
              outfile.flush()      
              cracked.append(p["account"])
              break 
      print "Done with digitalized word checks"

      cap_words = [w for t in rev_words for w in transform_capitalize(t)]
      for p in passwds:#processing to see if words or their "reversed and caps" version match a password
        if not p["account"] in cracked:
          for w in cap_words:
            if check_pass(w,p["password"]):
              outfile.write(p["account"] + "=" + w + "\n")
              outfile.flush()      
              cracked.append(p["account"])
              break 
      print "Done with capitalized word checks"

      if len(cracked) < len(passwds): #only is there is something remaining to be cracked
        for word in rev_words:  #All the possible transformations
          dig_cptlz_words = [t for w in transform_digits(word) for t in transform_capitalize(w)]
          for w in dig_cptlz_words:
            for p in passwds:
              if p["account"] in cracked:
                continue
              elif check_pass(w,p["password"]):
                outfile.write(p["account"] + "=" + w + "\n")
                cracked.append(p["account"])
                continue           
      outfile.close()   
      print("--- %s seconds ---" % (time.time() - start_time))

crack_pass_file("passwd","words","out")