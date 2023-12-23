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
	ormolu --mode inplace $(shell find lib "http-api" server servant daemon test -name '*.hs')

build-server-image:
	nix build
	$(DOCKER) load < result

build-client-image:
	$(DOCKER) build -t aftok/aftok-client:latest -f ./client/Dockerfile .

build-images: build-server-image build-client-image

deploy-local-server-image: build-server-image
	$(DOCKER) tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)

deploy-local-client-image: build-client-image
	$(DOCKER) tag aftok/aftok-client:latest aftok/aftok-client:$(VERSION)

deploy-server-image: deploy-local-server-image
	$(DOCKER) push docker.io/aftok/aftok-server:latest
	$(DOCKER) push docker.io/aftok/aftok-server:$(VERSION)

deploy-client-image: deploy-local-client-image
	$(DOCKER) push docker.io/aftok/aftok-client:latest
	$(DOCKER) push docker.io/aftok/aftok-client:$(VERSION)

deploy-images: deploy-server-image deploy-client-image

deploy-local-images: deploy-local-server-image deploy-local-client-image
