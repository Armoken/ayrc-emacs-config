.PHONY: all
all: clean


.PHONY: clean
clean:
	find ./rc | grep -E '.*\.el' | xargs rm --force

