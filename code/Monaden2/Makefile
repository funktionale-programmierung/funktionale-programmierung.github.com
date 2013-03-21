.PHONY: all clean archive

Examples := $(wildcard *.hs)
Archive	 = ../../files/Monaden2/Monaden2.zip

all: $(Archive)

clean:
	rm -f *~

$(Archive) : $(Examples)
	cd .. && zip Monaden2/$(Archive) Monaden2/*.hs
