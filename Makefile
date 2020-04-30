.PHONY: all
all: clean


.PHONY: clean
clean:
	@echo "Removing el org-files and temporary files:"
	@find . -path "./elpa" -prune \
				-name "*.el" \
			-or -name "*.elc" \
			-or -name ".*~" \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'


.PHONY: drop
drop: clean
	@echo "Removing installed packages:"
	@find "./elpa" -maxdepth 1 \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'
