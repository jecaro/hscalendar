# Variables to create version name
DATE := $(shell date +%Y%m%d)
COMMIT := $(shell git rev-parse --short HEAD)
# Travis checkout the repo in detached HEAD so it's quite tricky to get the
# branch name:
# https://stackoverflow.com/questions/6059336/how-to-find-the-current-git-branch-in-detached-head-state
BRANCH := $(shell git for-each-ref --format='%(objectname) %(refname:short)' refs/heads | awk "/^$$(git rev-parse HEAD)/ {print \$$2}")
VERSION := $(BRANCH).$(DATE).$(COMMIT)

# Docker images
DOCKER_IMAGE_BASE := jecaro/hscalendar-server
DOCKER_IMAGE_CURRENT := $(DOCKER_IMAGE_BASE):$(VERSION)
DOCKER_IMAGE_LATEST := $(DOCKER_IMAGE_BASE):latest

# Get the cache dir from the command line
ifdef cache_dir
DOCKER_SAVE_FILE := $(cache_dir)/images.tar
else
DOCKER_SAVE_FILE := docker_image/images.tar
endif

.PHONY: build test deploy load save

# Create the docker image
build:
	docker build -t $(DOCKER_IMAGE_CURRENT) .

# Test the docker image
test:
	docker run -t $(DOCKER_IMAGE_CURRENT) ./hscalendar-test

# Tag and push only master branch as latest
deploy:
	echo "$(DOCKER_PASSWORD)" | docker login -u "$(DOCKER_USERNAME)" --password-stdin
	docker push $(DOCKER_IMAGE_CURRENT)
ifeq ($(BRANCH), master)
	docker tag $(DOCKER_IMAGE_CURRENT) $(DOCKER_IMAGE_LATEST)
	docker push $(DOCKER_IMAGE_LATEST)
endif

# Save image docker in cache
save:
	docker save -o $(DOCKER_SAVE_FILE) $(DOCKER_IMAGE_CURRENT)

# Restore image docker in cache
load:
	docker load -i $(DOCKER_SAVE_FILE)
