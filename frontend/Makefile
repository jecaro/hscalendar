# The source elm file
SRC = src/Main.elm

# The fronted files to install
FRONTEND= elm.js index.html bulma.min.css

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

# Install target only when given destdir argument
# ex: "make destdir=/opt/bin/frontend"
ifdef destdir
install: all
	mkdir -p $(destdir)
	cp $(FRONTEND) $(destdir)
endif

.PHONY: clean
