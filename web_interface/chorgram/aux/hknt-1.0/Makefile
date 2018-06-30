PROG=main.native
OUT=hkc
## uncomment the following line when compiling the libvata bridge
# OPTS=-lib unix

main:
	ocamlbuild $(OPTS) $(PROG)
	cp $(PROG) $(OUT)

clean:
	rm -rf _build $(PROG) $(OUT)
