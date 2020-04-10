.PHONY: all
all: clean


.PHONY: clean
clean:
	find "./rc" | grep --extended-regexp '.*\.el|.*~' | xargs rm --force

.PHONY: drop
drop: clean
	@echo "Removing installed packages except archives and GPG keys:"
	@find "./elpa" -maxdepth 1 \
				! -path "./elpa" \
				-and ! -path "./elpa/gnupg" \
				-and ! -path "./elpa/archives" \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'

.PHONY: full_drop
full_drop: clean
	@echo "Removing installed packages:"
	@find "./elpa" \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'
