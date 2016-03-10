INSTALL_PATH ?= /usr/local/bin/

GOX_OSARCH ?= linux/386 linux/amd64 freebsd/386 freebsd/amd64 openbsd/386 openbsd/amd64 windows/386 windows/amd64 freebsd/arm netbsd/386 netbsd/amd64

DIST_PATH ?= dist

ifeq ($(shell uname -s), Darwin)
	GOX_OSARCH += darwin/amd64
endif

GOX_FLAGS ?= -output="$(DIST_PATH)/{{.Dir}}.{{.OS}}-{{.Arch}}" -osarch="$(GOX_OSARCH)"

default: build

build:
	go build ./...

deps:
	go get ./.

updatedeps:
	go get -u -v ./...

clean:
	git clean -xdf

install:
	cp $(DIST_PATH)/gut $(INSTALL_PATH)

tools:
	go get github.com/tcnksm/ghr
	go get github.com/mitchellh/gox
	go get github.com/alecthomas/gometalinter
	gometalinter --install --update

gox: tools
	gox -build-toolchain -osarch="$(GOX_OSARCH)"

dist:
	which gox || make tools
	gox $(GOX_FLAGS) $(GOBUILD_LDFLAGS)

release: dist
	ghr $(REPO_VERSION) $(DIST_PATH)

lint:
	gometalinter ./...
