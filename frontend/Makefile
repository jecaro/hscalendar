# The source elm file
SRC = src/Main.elm

# The fronted files to install
FRONTEND= elm.js index.html bulma.min.css fontawesome-free-5.12.1-web

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
	cp -r $(FRONTEND) $(destdir)
endif

.PHONY: clean
