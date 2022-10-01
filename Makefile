.PHONY: build-dev
build-dev:
	cargo build

.PHONY: publish-local

PATH_STR := '$$PATH'

publish-local: build-dev
	mkdir -p target/debug/publish
	mv target/debug/clj-analyzer target/debug/publish/clj-analyzer-dev
	which clj-analyzer-dev || echo export PATH=$$PWD/target/debug/publish:$(PATH_STR) >> ~/.zshrc && exec $$SHELL -l
