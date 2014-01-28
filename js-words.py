word_list = []
with open("scrabble-words.txt", "r") as words:
    for line in words:
    	word_list.append(line.rstrip())

with open("scrabble-words.js", "w") as output:
	output.write("window.WORDS = ")
	output.write(str(word_list))
	output.write(";")


