all: dancing-circle-optimizer

dancing-circle-optimizer: dancing-circle-optimizer.pl
	swipl -O -q -g main --stand_alone=true -o dancing-circle-optimizer -c dancing-circle-optimizer.pl

clean:
	rm -rf dancing-circle-optimizer