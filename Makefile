VERSION=$(shell git describe)

format:
	ormolu --mode inplace $(shell find lib server daemon test -name '*.hs')

build-server-image:
	nix build
	sudo docker load < result

build-client-image:
	docker build -t aftok/aftok-client:latest -f ./client/Dockerfile .

build-images: build-server-image build-client-image

deploy-server-image: build-server-image
	docker tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)
	docker push docker.io/aftok/aftok-server:latest
	docker push docker.io/aftok/aftok-server:$(VERSION)

deploy-client-image: build-client-image
	docker tag aftok/aftok-client:latest aftok/aftok-client:$(VERSION)
	docker push docker.io/aftok/aftok-client:latest
	docker push docker.io/aftok/aftok-client:$(VERSION)

deploy-images: deploy-server-image deploy-client-image
