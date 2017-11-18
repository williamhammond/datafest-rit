with open('data.txt', 'r') as f:
    for line in f:
        if len(line.split('\t')) != 27:
            print line
