all: dancing-circle-optimizer

rotllanes: rotllanes.pl
	swipl -O -q -g main --stand_alone=true -o rotllanes -c rotllanes.pl

rotllanes2: rotllanes2.pl
	swipl -O -q -g main --stand_alone=true -o rotllanes2 -c rotllanes2.pl
