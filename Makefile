VERSION=$(shell git describe)

format:
	ormolu --mode inplace $(find lib server daemon test -name '*.hs')

build-image:
	docker build -t aftok/aftok-server:latest .

deploy-image: build-image
	docker tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)
	docker push docker.io/aftok/aftok-server:latest
	docker push docker.io/aftok/aftok-server:$(VERSION)
