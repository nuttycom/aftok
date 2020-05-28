VERSION=$(shell git describe)
PWD=$(shell pwd)

format:
	find lib test server daemon -name \*.hs -exec brittany --write-mode=inplace {} \;

build-image:
	docker build -t aftok/aftok:latest .

deploy-image: build-image
	docker tag aftok/aftok:latest aftok/aftok:$(VERSION)
	docker push docker.io/aftok/aftok:$(VERSION)

run-local-docker: build-image
	docker run --net=host -it -v $(PWD)/local/conf/:/etc/aftok aftok/aftok:latest
