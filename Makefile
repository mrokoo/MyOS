GPPPARAMS = -32
ASPARAMS = --32
LDPARAMS = -melf_i386
%.o: %.cpp
	g++ $(GPPPARAMS) -o $@ -c $<
%.o: %.s
	as $(ASPARAMS) -o $@ $<
mykernel.bin: linker.ld $(objects)
	ld $(LDPARAMS) -T $< -o $@ $(objects)

install: mykernel.bin
	sudo cp $< /boot/mykernel.bin