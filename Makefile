.PHONY: all compile tidy clean

all: compile tidy view

compile: README.html

clean:
	rm -f *.html

README.html:
	markdown README.md > README.html

tidy: README.html
	tidy --input-encoding utf8 --output-encoding latin1 --tidy-mark no -m README.html

view: README.html
	xdg-open README.html &
