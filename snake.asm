.model tiny
; .model small
.stack 100h
BLOCK_SIZE equ 5
GRID_WIDTH equ (320 / BLOCK_SIZE)
GRID_HEIGHT equ (200 / BLOCK_SIZE)
.code
main proc
    ; mov ax, @data
    ; mov ds, ax

    mov ax, 13h ; video mode
    int 10h     ; video BIOS interrupt

    mov ax, 0A000h
    mov es, ax

    mov cx, 10
    rand_test_loop:
    push cx

    call rand_position
    mov ax, cx
    mov bx, dx
    mov dl, 7   ; color
    call putsquare

    mov cx, 0007h
    mov dx, 8480h
    mov ax, 8600h
    int 15h

    pop cx
    loop rand_test_loop

    getkey:
    mov ah, 0   ; get key
    int 16h     ; interrupt
    jz getkey   ; keep waiting until key pressed

    mov ax, 3   ; normal mode
    int 10h     ; video BIOS interrupt

    mov ax, 4c00h
    int 21h
main endp

putpixel proc
    ; INPUTS:
    ;   bx - x
    ;   ax - y
    ;   dl - color
    ; UNALTERED:
    ;   bx, dx, es

    mov cx, 320
    mul cx
    add ax, bx
    mov di, ax
    mov dl, 7
    mov [es:di], dl
    ret
putpixel endp

putsquare proc
    ; INPUTS:
    ;   ax - x
    ;   bx - y
    ;   dl - color
    ; UNALTERED:
    ;   dx, es

    mov cx, BLOCK_SIZE  ; set outer loop counter

    ; multiply x and y by BLOCK_SIZE
    mul cx      ; x = x * BLOCK_SIZE
    push ax     ; save x
    mov ax, bx  ; ax = y
    mul cx      ; y = y * BLOCK_SIZE
    pop bx      ; bx = x

    putsquare_yloop:
    push cx             ; save outer loop counter
    push bx
    mov cx, BLOCK_SIZE  ; set inner loop counter

    putsquare_xloop:

    push cx
    push ax
    call putpixel
    pop ax
    pop cx

    inc bx
    loop putsquare_xloop

    pop bx
    pop cx
    inc ax
    loop putsquare_yloop

    ret
putsquare endp

rand_position proc
    ; OUTPUT:
    ;   cx - x
    ;   dx - y
    ; UNALTERED:
    ;   bx, es

    push bp
    mov bp, sp
    sub sp, 6

    ; LOCAL VARIABLES:
    ;   x = [bp - 2]
    ;   rand = [bp - 6]

    mov ax, 0
    int 1Ah     ; use time as random value

    mov [bp - 4], dx ; save rand (low word)
    mov [bp - 6], cx ; save rand (high word)

    ; TODO apply xorshift here too?

    ; get x
    mov ax, dx
    mov dx, cx
    mov cx, GRID_WIDTH
    div cx

    ; save x value
    mov [bp - 2], dx

    ; rand ^= rand << 7
    mov ax, [bp - 4]
    mov cx, ax
    mov dx, [bp - 6]
    shl ax, 7
    shr cx, 16 - 7
    shl dx, 7
    and dx, cx
    xor [bp - 4], ax
    xor [bp - 6], dx

    ; rand ^= rand >> 9
    mov ax, [bp - 4]
    mov dx, [bp - 6]
    mov cx, dx
    shr ax, 9
    shr dx, 9
    shl cx, 16 - 9
    and ax, cx
    xor [bp - 4], ax
    xor [bp - 6], dx

    ; rand ^= rand << 8
    mov ax, [bp - 4]
    mov cx, ax
    mov dx, [bp - 6]
    shl ax, 8
    shr cx, 16 - 8
    shl dx, 8
    and dx, cx
    xor [bp - 4], ax
    xor [bp - 6], dx

    ; get y
    mov ax, [bp - 4]
    mov dx, [bp - 6]
    mov cx, GRID_HEIGHT
    div cx

    mov cx, [bp - 2]

    mov sp, bp
    pop bp
    ret
rand_position endp

end main