# Build the blog.  Results can be found in the _site directory.
all: clean
	jekyll build
.PHONY: all

# Remove build artifacts.
clean:
	jekyll clean
.PHONY: clean

# Serve the blog locally, reachable on localhost:4000 by default.
serve:
	jekyll serve --watch --incremental
.PHONY: serve
