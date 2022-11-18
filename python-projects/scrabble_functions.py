def length_n(file_name,n):
	f = open(file_name, "r")
	len_n = []
	for k in f.readlines():
		k = k.strip().lower() #remove trailing or leading white spaces and lower case the word
		if (len(k) == n):
			len_n.append(k)
	f.close()		
	return len_n


def starts_with(file_name, n, first_letter):
	f = open(file_name, "r")
	len_n = []
	for k in f.readlines():
		k = k.strip().lower() #remove trailing or leading white spaces and lower case the word
		if ( (len(k) == n) and (k[0] == first_letter.strip().lower()) ):
			len_n.append(k)
	f.close()		
	return len_n


def contains_letter(file_name, n, included):
	f = open(file_name, "r")
	len_n = []
	for k in f.readlines():
		k = k.strip().lower() #remove trailing or leading white spaces and lower case the word
		if ( (len(k) == n) and (included.lower() in k[1:]) ):
			len_n.append(k)
	f.close()		
	return len_n


def vowel_heavy(file_name, n, m):
	f = open(file_name, "r")
	len_n = []; vow = ['a','e','i','o','u'];
	for k in f.readlines():
		k = k.strip().lower() #remove trailing or leading white spaces and lower case the word
		cnt_vow = len( set(k) & set(vow) )
		if ( (len(k) == n) and (cnt_vow == m) ) :
			len_n.append(k)
	f.close()		
	return len_n



#e.g:
## file_name = '/Users/granth/Desktop/dictionary.txt' or 'dictionary.txt'
## vowel_heavy(file_name, 2, 2) > returns: ['ae', 'ai', 'oe']
## starts_with(file_name, 2, 'a') > returns: ['aa', 'ad', 'ae', 'ag', 'ah', 'ai', 'al', 'am', 'an', 'ar', 'as', 'at', 'aw', 'ax', 'ay']


