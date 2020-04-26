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
	@echo "Removing installed packages except archives and GPG keys:"
	@find "./elpa" -maxdepth 1 \
				! -path "./elpa" \
				-and ! -path "./elpa/gnupg" \
				-and ! -path "./elpa/archives" \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'


.PHONY: full_drop
full_drop: clean
	@echo "Removing installed packages:"
	@find "./elpa" -maxdepth 1 \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'
