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