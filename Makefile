build-container:
	sudo docker build -t aftok/aftok:latest .

run-local-docker: build-container
	sudo docker run --net=host -it -v /home/nuttycom/projects/aftok/local/conf/:/etc/aftok aftok/aftok:latest

