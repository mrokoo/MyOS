# Nasm伪指令
1. org伪指令  
(Origin)起始地址或源地址，默认情况下为0x0000。主要影响编译时生成的汇编地址。**作用类似于vstart=0x7c00**，即段内地址都将从0x7c00开始计算。  
例如:
    ```
    mov ax, [lable_address]
    ```
    在编译时，汇编器将为有效地址增加0x7c00偏移.
    ```
    mov ax, [lable_address + 0x7c00]
    ```
2. equ伪指令  
equ伪指令的作用是，让其左边的标识符代表右边的表达式。equ等价语句不会给标识符分配存储空间，而且标识符不能与其他符号同名，也不能被重新定义。其作用**类似于C语言中的宏定义**。
    ```
    BaseOfStack equ 0x7c00
    mov ax, BaseOfStack
    ```

# 启动bochs
1. 生成boot.bin
```
nasm boot.asm -o boot.bin
```
2. 引导程序写入虚拟软盘
```
dd if=boot.bin of=../01环境配置/boot.img bs=512 count=1 conv=notrunc
```
注：这里为了后续更改方便，创建了Makefile文件
3. 启动bochs
```
bochs -f ../01环境配置/x86
```
如果出现bx_dbg_read_linear: physical memory read error问题，可以参考[解决方案](https://zhuanlan.zhihu.com/p/130182885)，简单来说就是尝试把romimage选项改成BIOS-bochs-legacy.实测可以解决。

# 软中断实现功能
boot.asm中的许多操作都是通过使用BIOS中断服务程序INT 10h的主功能编号有06h、02h和13h，它们的功能及参数说明如下。  
1. INT 10h, AH=02h功能：设定光标位置。  
    - DH=游标的列数；  
    - DL=游标的行数；  
    - BH=页码。
2. INT 10h, AH=06h功能：按指定范围滚动窗口。
    - AL=滚动的列数，若为0则实现清空屏幕功能；
    - BH=滚动后空出位置放入的属性；
    - CH=滚动范围的左上角坐标列号；
    - CL=滚动范围的左上角坐标行号；
    - DH=滚动范围的右下角坐标列号；
    - DL=滚动范围的右下角坐标行号；
    - BH=颜色属性。
        - bit 0～2：字体颜色（0：黑，1：蓝，2：绿，3：青，4：红，5：紫，6：综，7：白）。
        - bit 3：字体亮度（0：字体正常，1：字体高亮度）。
        - bit 4～6：背景颜色（0：黑，1：蓝，2：绿，3：青，4：红，5：紫，6：综，7：白）。
        - bit 7：字体闪烁（0：不闪烁，1：字体闪烁）。
注意：这条命令主要用于按指定范围滚动窗口，**但是如果AL=0的话，则执行清屏功能。在使用清屏功能时（AL寄存器为0），其他BX、CX、DX寄存器参数将不起作用，无需纠结它们的数值。**
3. INT 10h, AH=13h功能：显示一行字符串。
    - AL=写入模式。
        - AL=00h：字符串的属性由BL寄存器提供，而CX寄存器提供字符串长度（以B为单位），显示后光标位置不变，即显示前的光标位置。
        - AL=01h：同AL=00h，但光标会移动至字符串尾端位置。
        - AL=02h：字符串属性由每个字符后面紧跟的字节提供，故CX寄存器提供的字符串长度改成以Word为单位，显示后光标位置不变。
        - AL=03h：同AL=02h，但光标会移动至字符串尾端位置。
    - CX=字符串的长度。
    - DH=游标的坐标行号。
    - DL=游标的坐标列号。
    - ES:BP=>要显示字符串的内存地址。
    - BH=页码。
    - BL=字符属性/颜色属性。
        - bit 0～2：字体颜色（0：黑，1：蓝，2：绿，3：青，4：红，5：紫，6：综，7：白）。
        - bit 3 ：字体亮度（0：字体正常，1：字体高亮度）。
        - bit 4～6：背景颜色（0：黑，1：蓝，2：绿，3：青，4：红，5：紫，6：综，7：白）。
        - bit 7：字体闪烁（0：不闪烁，1：字体闪烁）。
4. INT 13h, AH=00h功能：重置磁盘驱动器，为下一次读写软盘做准备。
    - DL=驱动器号，00H～7FH：软盘；80H～0FFH：硬盘。
        - DL=00h代表第一个软盘驱动器（“drive A:”）；
        - DL=01h代表第二个软盘驱动器（“drive B:”）；
        - DL=80h代表第一个硬盘驱动器；
        - DL=81h代表第二个硬盘驱动器。
注：这段汇编代码实现了软盘驱动器的复位功能，它相当于重新初始化了一次软盘驱动器，从而将软盘驱动器的磁头移动至默认位置。

# FAT12文件系统
FAT12文件系统相对简单、易于实现，因此使用FAT12文件系统。
## 软盘格式化
根据FAT12文件系统的要求，将软盘扇区划分为**引导扇区**、**FAT表**、**根目录区**和**数据区**4部分。
### 引导扇区
FAT12文件系统的引导扇区不仅包含有引导程序，还有FAT12文件系统的整个组成结构信息。**描述FAT12文件系统对磁盘扇区的管理情况**。  
各种字段的信息相对较多，详见FAT12文件系统要求。
### FAT表
FAT12文件系统以簇为单位来分配数据区的存储空间（扇区），每个簇的长度为BPB_ BytesPerSec ＊BPB_SecPerClus字节，**数据区的簇号与FAT表的表项是一一对应关系**。  
**FAT表中的表项位宽与FAT类型有关**，例如，FAT12文件系统的表项位宽为12 bit、FAT16文件系统的表项位宽为16 bit、FAT32文件系统的表项位宽为32 bit。
FAT项|实例值|描述
---|---|---
0  | FF0h | 磁盘标识字，低字节与BPB_Media数值保持一致
1  | FFFh | 第一个簇已经被占用
2  | 003h | 000h: 可用簇
3  | 004h | 002h~FEFh: 已用簇，标识下一个簇的簇号
...| ...  | FF0h~FF6h: 保留簇
N  | FFFh | FF7h: 坏簇
N+1| 000h | FF8~FFFh: 文件的最后一个簇

### 根目录区与数据区
根目录区和数据区都保存着与文件相关的数据，只不过根目录区只能保存目录项信息，而数据区不但可以保存目录项信息，还可以保存文件内的数据。  
文件目录项是由32B组成的结构体，既可以表示文件，也可以表示目录。详细目录项结构：

 名称 | 偏移 | 长度 | 描述
 --- | --- | --- | ---
 DIR_Name| 0x00 | 11 | 文件名8B，扩展名3B
 DIR_Attr| 0x0B | 1  | 文件属性
 保留    | 0x0C  | 10 |保留位
 DIR_WrtTime| 0x16 | 2 | 最后一次写入时间
 DIR_WrtDate| 0x18 | 2 | 最后一次写入日期
 DIR_FstClus| 0x1A | 2 | 起始簇号
 DIR_FileSize| 0x1C| 4 | 文件大小