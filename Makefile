.PHONY: build-dev
build-dev:
	cargo build

.PHONY: publish-local

PATH_STR := '$$PATH'

publish-local: build-dev
	mkdir -p target/debug/publish
	mv target/debug/cljlint target/debug/publish/cljlint-dev
	which cljlint-dev || echo export PATH=$$PWD/target/debug/publish:$(PATH_STR) >> ~/.zshrc && exec $$SHELL -l
