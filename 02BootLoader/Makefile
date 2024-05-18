write: boot.bin
	dd if=boot.bin of=../01环境配置/boot.img bs=512 count=1 conv=notrunc
boot.bin: boot.asm
	nasm boot.asm -o boot.bin

.PHONY: loader
loader: loader.bin
	mount ../01环境配置/boot.img /media/ -t vfat -o loop
	cp loader.bin /media/
	sync
	umount /media
loader.bin: loader.asm
	nasm loader.asm -o loader.bin