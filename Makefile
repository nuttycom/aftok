VERSION=$(shell git describe)
PWD=$(shell pwd)

format:
	find lib test server daemon -name \*.hs -exec brittany --write-mode=inplace {} \;

build-image:
	docker build -t aftok/aftok-server:latest .

deploy-image: build-image
	docker tag aftok/aftok-server:latest aftok/aftok-server:$(VERSION)
	docker push docker.io/aftok/aftok-server:$(VERSION)
