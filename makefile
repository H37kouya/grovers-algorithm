dev:
	make build && \
	make run

build:
	cd build && \
	cmake .. && \
	make

run:
	./build/main