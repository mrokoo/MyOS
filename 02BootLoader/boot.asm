org 0x7c00
BaseOfStack equ 0x7c00       ; 栈基址
BaseOfLoader equ 0x1000      ; loader程序基址
OffsetOfLoader equ 0x00      ; loader程序偏移，物理地址 = 基址<<4 + 偏移
RootDirSectors equ 14         ; 根目录占用的扇区数
SectorNumOfRootDirStart equ 19 ; 根目录的起始扇区号
SectorNumOfFAT1Start equ 1   ; FAT1表的起始扇区号
SectorBalance equ 17         ; 用于平衡文件（或者目录）的起始簇号与数据区起始簇号的差值。
    jmp short Label_Start    ; 跳过FAT12文件系统的组成结构信息 2B
    nop                      ; 空指令填充字节 1B
    BS_OEMName db 'MINEboot' ; 记录制造商的名字，亦为文件系统命名
    BPB_BytesPerSec dw 512   ; 每扇区字节数
    BPB_SecPerClus db 1      ; 每簇扇区数
    BPB_RsvdSecCnt dw 1      ; 保留扇区数(FAT12必为1，用来存放引导代码)
    BPB_NumFATs db 2         ; 指定FAT12文件系统中FAT表的份数，多个表的内容一致
    BPB_RootEntCnt dw 224    ; 指定根目录可容纳的目录项数。对于FAT12文件系统而言，这个数值乘以32必须是BPB_BytesPerSec的偶数倍。
    BPB_TotSec16 dw 2880     ; 记录着总扇区数。
    BPB_Media db 0xf0        ; 描述存储介质类型。
    BPB_FATSz16 dw 9         ; 记录着FAT表占用的扇区数。
    BPB_SecPerTrk dw 18      ; 每磁道扇区数
    BPB_NumHeads dw 2        ; 磁头数
    BPB_hiddSec dd 0         ; 隐藏扇区数
    BPB_TotSec32 dd 0        ; 如果TotSec16为0，则记录扇区数量

    BS_DrvNum db 0           ; int 13h的驱动器号
    BS_Reserved1 db 0        ; 保留
    BS_BootSig db 29h        ; 扩展引导标记
    BS_VolID dd 0            ; 卷序列号
    BS_VolLab db 'boot loader'; 指定卷标。
    BS_FileSysType db 'FAT12'; 描述文件系统类型。
;==========  程序开始
Label_Start:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, BaseOfStack
;========== clear screen
    mov ax, 0600h
    mov bx, 0700h
    mov cx, 0
    mov dx, 0184fh
    int 10h
;========== set focus
    mov ax, 0200h
    mov bx, 0000h
    mov dx, 0000h
    int 10h
;========== display on screen: Start Booting...
    mov ax, 1301h
    mov bx, 000fh ; bl 00001111
    mov dx, 0000h
    mov cx, 16
    push ax
    mov ax, ds
    mov es, ax
    pop ax
    mov bp, StartBootMessage
    int 10h
;========== reset floppy
    xor ah, ah
    xor dl, dl
    int 13h
;========== search loader.bin
    mov word [SectorNo], SectorNumOfRootDirStart
Label_Search_In_Root_Dir_Begin: ;根文件目录开始
    cmp word [RootDirSizeForLoop], 0
    jz Label_No_LoaderBin
    dec word [RootDirSizeForLoop]
    mov ax, 00h
    mov es, ax
    mov bx, 8000h
    mov ax, [SectorNo]
    mov cl, 1
    call Func_ReadOneSector
    mov si, LoaderFileName
    mov di, 8000h
    cld
    mov dx, 10h
Label_Search_For_LoaderBin:
    cmp dx, 0
    jz Label_Goto_Next_Sector_In_Boot_Dir
    dec dx
    mov cx, 11
Label_Cmp_FileName:
    cmp cx, 0
    jz Label_FileName_Found
    dec cx
    lodsb
    cmp al, byte [es:di]
    jz Label_Go_On
    jmp Label_Different
Label_Go_On:
    inc di
    jmp Label_Cmp_FileName
Label_Different:
    and di, 0ffe0h
    add di, 20h
    mov si, LoaderFileName
    jmp Label_Search_For_LoaderBin
Label_Goto_Next_Sector_In_Boot_Dir:
    add word [SectorNo], 1
    jmp Label_Search_In_Root_Dir_Begin

;======= display on screen : ERROR:No LOADER Found
Label_No_LoaderBin:
    mov     ax,     1301h
    mov     bx,     008ch
    mov     dx,     0100h
    mov     cx,     21
    push    ax
    mov     ax,     ds
    mov     es,     ax
    pop     ax
    mov     bp,     NoLoaderMessage
    int     10h
    jmp     $

;======= found loader.bin name in root director struct
Label_FileName_Found:

	mov	ax,	RootDirSectors
	and	di,	0ffe0h
	add	di,	01ah
	mov	cx,	word	[es:di]
	push	cx
	add	cx,	ax
	add	cx,	SectorBalance
	mov	ax,	BaseOfLoader
	mov	es,	ax
	mov	bx,	OffsetOfLoader
	mov	ax,	cx

Label_Go_On_Loading_File:
	push	ax
	push	bx
	mov	ah,	0eh
	mov	al,	'.'
	mov	bl,	0fh
	int	10h
	pop	bx
	pop	ax

	mov	cl,	1
	call	Func_ReadOneSector
	pop	ax
	call	Func_GetFATEntry
	cmp	ax,	0fffh
	jz	Label_File_Loaded
	push	ax
	mov	dx,	RootDirSectors
	add	ax,	dx
	add	ax,	SectorBalance
	add	bx,	[BPB_BytesPerSec]
	jmp	Label_Go_On_Loading_File

Label_File_Loaded:
	
	jmp	BaseOfLoader:OffsetOfLoader

;========== read one sector from floppy
Func_ReadOneSector:
                            ;ax<=待读取的磁盘起始扇区号
                            ;cl<=读入的扇区数量
                            ;ES:BX=>目标缓冲区起始地址
    push bp
    mov bp, sp
    sub esp, 2
    mov byte [bp-2], cl
    push bx
    mov bl, [BPB_SecPerTrk]
    div bl
    inc ah
    mov cl, ah
    mov dh, al
    shr al, 1
    mov ch, al
    and dh, 1
    pop bx
    mov dl, [BS_DrvNum]
    Label_Go_On_Reading:
        mov ah, 2             ; 主功能号
        mov al, byte [bp-2]   ; 读入扇区数
        int 13h               ; 中断处理程序13h
        jc Label_Go_On_Reading
    add esp, 2
    pop bp
    ret
;======= get FAT Entry
Func_GetFATEntry:
    push es
    push bx
    push ax
    mov  ax, 00
    mov  es, ax
    pop  ax
    mov  byte [Odd], 0
    mov  bx, 3
    mul  bx
    mov  bx, 2
    div  bx
    cmp  dx, 0
    jz   Label_Even
    mov  byte [Odd], 1
    Label_Even:
        xor     dx,     dx
        mov     bx,     [BPB_BytesPerSec]
        div     bx
        push    dx
        mov     bx,     8000h
        add     ax,     SectorNumOfFAT1Start
        mov     cl,     2
        call    Func_ReadOneSector

        pop     dx
        add     bx,     dx
        mov     ax,     [es:bx]
        cmp     byte    [Odd],     1
        jnz     Label_Even_2
        shr     ax,     4

    Label_Even_2:
        and     ax,     0fffh
        pop     bx
        pop     es
        ret
;======= tmp variable
RootDirSizeForLoop dw RootDirSectors
SectorNo dw 0
Odd db 0
;========== define StartBootMessage
StartBootMessage: db "Start Booting..."
NoLoaderMessage: db "ERROR:No LOADER Found"
LoaderFileName: db "LOADER  BIN", 0
;========== fill zero until whole sector
times 510-($-$$) db 0
dw 0xaa55
