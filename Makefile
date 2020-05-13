.PHONY: all
all: clean


.PHONY: clean
clean:
	@echo "Removing build and cache files:"
	@find   "runtime-artifacts/session-caches" "runtime-artifacts/build" \
			2>/dev/null \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'
	@rm --force --verbose "./init.elc"


.PHONY: drop
drop: clean
	@echo "Removing installed packages:"
	@rm --recursive --force --verbose   "./straight/modified" \
										"./straight/build" \
										"./straight/repos" \
										"./straight/build-cache.el"
.PHONY: full-drop
full-drop: clean
	@echo "Removing session configs:"
	@find "runtime-artifacts/session-configs" \
		| sort \
		| xargs --replace="%S" \
				sh -c '{ echo -e "\t%S"; rm --recursive --force %S; }'

	@$(MAKE) -s drop
