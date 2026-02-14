VERSION=$(shell git describe)

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)

# system specific variables, add more here

# On linux systems, you can access docker directly if you're in the docker group.
DOCKER_GROUP := $(shell groups | tr ' ' '\n' | grep -w docker)

ifeq ($(DOCKER_GROUP),docker)
	DOCKER.Linux.x86_64 := docker
else
	DOCKER.Linux.x86_64 := sudo docker
endif

DOCKER.Darwin.x86_64 := docker
DOCKER += $(DOCKER.$(uname_s).$(uname_m))

format:
	ormolu --mode inplace $(shell find api core executables -name '*.hs')

build-server-image:
	nix build
	$(DOCKER) load < result

# Client is now in a separate repository (aftok/aftok-client)
# Build client images from the client repository instead.

deploy-local-server-image: build-server-image
	$(DOCKER) tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)

deploy-server-image: deploy-local-server-image
	$(DOCKER) push docker.io/aftok/aftok-server:latest
	$(DOCKER) push docker.io/aftok/aftok-server:$(VERSION)
