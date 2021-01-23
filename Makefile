VERSION=$(shell git describe)

format:
	ormolu --mode inplace $(find lib server daemon test -name '*.hs')

build-server-image:
	docker build -t aftok/aftok-server:latest .

build-client-image:
	docker build -t aftok/aftok-client:latest -f ./client/Dockerfile .

build-images: build-server-image build-client-image

deploy-images: build-server-image build-client-image
	docker tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)
	docker push docker.io/aftok/aftok-server:latest
	docker push docker.io/aftok/aftok-server:$(VERSION)
	docker tag aftok/aftok-client:latest aftok/aftok-client:$(VERSION)
	docker push docker.io/aftok/aftok-client:latest
	docker push docker.io/aftok/aftok-client:$(VERSION)
