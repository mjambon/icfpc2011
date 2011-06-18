.PHONY: default
default:
	cd src; omake

.PHONY: clean
clean:
	rm -f run1 run2 *~
	cd src; omake clean
	rm -f src/*.omc src/.omakedb
