import socket
from random import shuffle

if __name__ == '__main__':
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	sock.connect(('localhost', 789))

	corpus = open('korpus.txt').read().split('\n')
	print(str(len(corpus)) + " lines in corpus.")

	corpus_bad = open('korpus_bad.txt', 'w+')
	corpus_good = open('korpus_good.txt', 'w+')

	found_ct = 0
	not_found_ct = 0

	total_ct = 0
	corpus = corpus
	for line in corpus:
		total_ct += 1
		print(total_ct, found_ct, not_found_ct)
		sock.send(line + '\n\n')
		resp = sock.recv(512)
		if resp == 'DONE token found':
			found_ct += 1
			corpus_good.write(line + '\n')
		elif resp == 'No done token found':
			not_found_ct += 1
			corpus_bad.write(line + '\n')
			print(line)
		else:
			print("Unexpected response " + resp)
	print("found_ct: ", str(found_ct), " not_found_ct: ", str(not_found_ct))

	corpus_bad.close()
	corpus_good.close()
