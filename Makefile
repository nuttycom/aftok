build-container:
	docker build -t aftok/aftok:latest .

run-local-docker:
	docker run --net=host -it -v /home/nuttycom/projects/aftok/docker-conf/:/etc/aftok aftok/aftok:latest

