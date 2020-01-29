SRC = $(wildcard src/*.elm)

# Toggle debug mode with "make debug=1"
ifdef debug
FLAGS = --debug
else
FLAGS = --optimize
endif

all: elm.js

elm.js: $(SRC)
	elm make $(SRC) $(FLAGS) --output=elm.js

clean:
	rm -f elm.js

.PHONY: clean
