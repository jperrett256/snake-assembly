.model tiny
; .model small
.stack 100h
BLOCK_SIZE equ 5
GRID_WIDTH equ (320 / BLOCK_SIZE)
GRID_HEIGHT equ (200 / BLOCK_SIZE)
.code

main:
    mov bp, sp
    sub sp, 8

    ; LOCAL VARIABLES:
    ;   x       = word ptr [bp - 2]
    ;   y       = word ptr [bp - 4]
    ;   x_diff  = word ptr [bp - 6]
    ;   y_diff  = word ptr [bp - 8]

    ; mov ax, @data
    ; mov ds, ax

    mov ax, 13h ; video mode
    int 10h     ; video BIOS interrupt

    mov ax, 0A000h
    mov es, ax

    mov word ptr [bp - 2], GRID_WIDTH/2
    mov word ptr [bp - 4], GRID_HEIGHT/2

    ;:::::::::::: main loop :::::::::::::
    update:

    mov word ptr [bp - 6], 0    ; clear x_diff
    mov word ptr [bp - 8], 0    ; clear y_diff

    ;:::::::: input :::::::::
    input_loop:

    mov ah, 06h     ; direct console i/o
    mov dl, 0FFh    ; read from stdin buffer
    int 21h

    jz input_loop_done  ; stop if buffer is empty

    cmp al, 71h     ; q - quit
    jz exit

    cmp al, 0       ; check if extended key code
    jnz input_loop
    int 21h         ; get extended key code

    cmp al, 48h             ; up arrow
    jnz input_down_arrow
    dec word ptr [bp - 8]
    jmp input_loop_done

    input_down_arrow:
    cmp al, 50h             ; down arrow
    jnz input_right_arrow
    inc word ptr [bp - 8]
    jmp input_loop_done

    input_right_arrow:
    cmp al, 4Dh             ; up arrow
    jnz input_left_arrow
    inc word ptr [bp - 6]
    jmp input_loop_done

    input_left_arrow:
    cmp al, 4Bh             ; up arrow
    jnz input_loop
    dec word ptr [bp - 6]

    input_loop_done:
    ;::::::::::::::::::::::::

    ; clear block that needs clearing
    push 0          ; black
    push [bp - 4]
    push [bp - 2]
    call putsquare

    ; update position that needs clearing
    mov ax, word ptr [bp - 6]
    add [bp - 2], ax
    mov ax, word ptr [bp - 8]
    add [bp - 4], ax

    ; draw new block
    push 7          ; white
    push [bp - 4]
    push [bp - 2]
    call putsquare

    ; delay
    mov cx, 0000h
    mov dx, 8480h
    mov ax, 8600h
    int 15h

    jmp update
    ;::::::::::::::::::::::::::::::::::::

    exit:

    mov ax, 3       ; normal mode
    int 10h         ; video BIOS interrupt

    mov ax, 4c00h   ; exit (return 0)
    int 21h

putpixel:
    ; INPUTS:
    ;   bx - x
    ;   ax - y
    ;   dl - color
    ; UNALTERED:
    ;   bx, dx, es

    mov cx, 320
    push dx
    mul cx
    pop dx
    add ax, bx
    mov di, ax
    mov es:[di], dl
    ret

putsquare:
    push bp
    mov bp, sp

    ; This function places a square of pixels of
    ; a specific color at the coordinates (x, y).
    ; The color is represented in the low 8 bits
    ; of the 16-bit argument.
    ;
    ; ARGUMENTS
    ;   x       = word ptr [bp + 4]
    ;   y       = word ptr [bp + 6]
    ;   color   = word ptr [bp + 8]

    mov cx, BLOCK_SIZE  ; set outer loop counter

    ; multiply x and y by BLOCK_SIZE
    mov ax, [bp + 4]
    mul cx              ; x = x * BLOCK_SIZE
    mov [bp + 4], ax

    mov ax, [bp + 6]
    mul cx              ; y = y * BLOCK_SIZE
    mov [bp + 6], ax

    putsquare_yloop:
    push cx             ; save outer loop counter
    mov cx, BLOCK_SIZE  ; set inner loop counter

    putsquare_xloop:
    push cx

    mov bx, [bp + 4]
    mov ax, [bp + 6]
    mov dx, [bp + 8]
    call putpixel

    inc word ptr [bp + 4]
    pop cx
    loop putsquare_xloop

    sub word ptr [bp + 4], BLOCK_SIZE
    inc word ptr [bp + 6]
    pop cx
    loop putsquare_yloop

    mov sp, bp
    pop bp
    ret 6

rand_position:
    push bp
    mov bp, sp
    sub sp, 6

    ; This function returns a random (x, y) coordinate
    ; in the range (0, 0) and (GRID_WIDTH - 1, GRID_HEIGHT - 1).
    ;
    ; OUTPUT:
    ;   x       = cx
    ;   y       = dx
    ;
    ; LOCAL VARIABLES:
    ;   x       = word ptr [bp - 2]
    ;   rand    = dword ptr [bp - 6]

    mov ax, 0
    int 1Ah     ; use time as random value

    mov [bp - 4], dx    ; save rand (low word)
    mov [bp - 6], cx    ; save rand (high word)

    lea ax, [bp - 6]    ; ax = &rand
    push ax             ; pass &rand to function
    call xorshift

    ; get x
    mov ax, [bp - 4]
    mov dx, [bp - 6]
    mov cx, GRID_WIDTH
    div cx

    mov [bp - 2], dx    ; save x value

    lea ax, [bp - 6]    ; ax = &rand
    push ax             ; pass &rand to function
    call xorshift

    ; get y
    mov ax, [bp - 4]
    mov dx, [bp - 6]
    mov cx, GRID_HEIGHT
    div cx

    mov cx, [bp - 2]

    mov sp, bp
    pop bp
    ret

xorshift:
    push bp
    mov bp, sp

    ; This function applies xorshift to
    ; a 32-bit input value (in place).
    ;
    ; ARGUMENTS:
    ;   &rand = word ptr [bp + 4]

    mov bx, [bp + 4]    ; bx = &rand

    ; rand ^= rand << 13
    mov dx, ss:[bx]
    mov ax, ss:[bx + 2]
    mov cx, ax
    shl ax, 13
    shr cx, 16 - 13
    shl dx, 13
    and dx, cx
    xor ss:[bx], dx
    xor ss:[bx + 2], ax

    ; rand ^= rand >> 17
    mov dx, ss:[bx]
    shr dx, 1
    xor word ptr [bx], 0
    xor ss:[bx + 2], dx

    ; rand ^= rand << 5
    mov dx, ss:[bx]
    mov ax, ss:[bx + 2]
    mov cx, ax
    shl ax, 5
    shr cx, 16 - 5
    shl dx, 5
    and dx, cx
    xor ss:[bx], dx
    xor ss:[bx + 2], ax

    mov sp, bp
    pop bp
    ret 2

end main