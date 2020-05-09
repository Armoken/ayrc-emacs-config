.PHONY: all
all: clean


.PHONY: clean
clean:
	@echo "Removing el org-files and temporary files:"
	@find . \
			-path "./elpa" -prune \
			-and -not -path "./elpa" \
			-or \
				\( \
					\( \
						-name "*.el" \
						-and -not -path "./custom.el" \
						-and -not -path "./init.el" \
						-and -not -path "./transient/history.el" \
					\) \
					-or -name "*.elc" \
					-or -name ".*~" \
					-or -name "#*" \
					-or -name "projectile.cache" \
				\) \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'


.PHONY: drop
drop: clean
	@echo "Removing installed packages:"
	@find "./elpa" -maxdepth 1 \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'
