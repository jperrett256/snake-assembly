.model tiny
; .model small
.stack 100h
BLOCK_SIZE equ 5
GRID_WIDTH equ (320 / BLOCK_SIZE)
GRID_HEIGHT equ (200 / BLOCK_SIZE)
.code

main:
    mov bp, sp
    sub sp, 14

    ; LOCAL VARIABLES:
    ;   fruit_x = word ptr [bp - 2]
    ;   fruit_y = word ptr [bp - 4]
    ;   x_dir   = word ptr [bp - 6]
    ;   y_dir   = word ptr [bp - 8]
    ;   length  = word ptr [bp - 10]
    ;   x       = word ptr [bp - 12]
    ;   y       = word ptr [bp - 14]

    ; mov ax, @data
    ; mov ds, ax

    mov ax, 13h ; video mode
    int 10h     ; video BIOS interrupt

    mov ax, 0A000h
    mov es, ax

    call place_fruit
    mov [bp - 2], cx
    mov [bp - 4], dx

    mov word ptr [bp - 10], 1

    mov word ptr [bp - 12], GRID_WIDTH/2
    mov word ptr [bp - 14], GRID_HEIGHT/2

    mov word ptr [bp - 6], 1    ; initialise x_dir
    mov word ptr [bp - 8], 0    ; initialise y_dir

    ;:::::::::::: main loop :::::::::::::
    update:

    ;:::::::: input :::::::::
    mov ah, 06h     ; direct console i/o
    mov dl, 0FFh    ; read from stdin buffer

    input_loop:

    int 21h         ; interrupt (get character)

    jz input_loop_done  ; stop if buffer is empty

    xor bx, bx      ; new x_dir
    xor cx, cx      ; new y_dir

    cmp al, 71h     ; q - quit
    jnz no_exit
    ; unconditional jump used because it
    ; can store larger relative addresses
    jmp exit
    no_exit:

    cmp al, 72h     ; r - restart
    jnz no_restart
    jmp restart
    no_restart:

    cmp al, 0       ; check if extended key code
    jnz input_loop
    int 21h         ; get extended key code

    cmp al, 48h             ; up arrow
    jnz input_down_arrow
    dec cx
    jmp input_loop_almost_done

    input_down_arrow:
    cmp al, 50h             ; down arrow
    jnz input_right_arrow
    inc cx
    jmp input_loop_almost_done

    input_right_arrow:
    cmp al, 4Dh             ; up arrow
    jnz input_left_arrow
    inc bx
    jmp input_loop_almost_done

    input_left_arrow:
    cmp al, 4Bh             ; up arrow
    jnz input_loop
    dec bx

    input_loop_almost_done:

    ; check new direction is perpendicular
    cmp bx, [bp - 6]
    jz input_loop
    cmp cx, [bp - 8]
    jz input_loop

    mov [bp - 6], bx
    mov [bp - 8], cx

    input_loop_done:
    ;::::::::::::::::::::::::

    ; clear block that needs clearing
    push 0          ; black
    push [bp - 14]
    push [bp - 12]
    call putsquare

    ;:::::: update position :::::::
    mov ax, word ptr [bp - 6]
    add [bp - 12], ax
    jl restart
    cmp word ptr [bp - 12], GRID_WIDTH
    jge restart

    mov ax, word ptr [bp - 8]
    add [bp - 14], ax
    jl restart
    cmp word ptr [bp - 14], GRID_HEIGHT
    jge restart
    ;::::::::::::::::::::::::::::::

    ; draw new block
    push 7          ; white
    push [bp - 14]
    push [bp - 12]
    call putsquare

    ;:::::::: place fruit :::::::::
    mov cx, [bp - 2]
    cmp cx, [bp - 12]
    jnz place_fruit_done
    mov cx, [bp - 4]
    cmp cx, [bp - 14]
    jnz place_fruit_done

    inc word ptr [bp - 10]

    call place_fruit

    mov [bp - 2], cx
    mov [bp - 4], dx

    place_fruit_done:
    ;::::::::::::::::::::::::::::::

    ; delay
    mov cx, 0001h
    mov dx, 8480h
    mov ax, 8600h
    int 15h

    jmp update
    ;::::::::::::::::::::::::::::::::::::

    restart:
    mov sp, bp

    mov cx, 320 * 200
    clearscreen_loop:
    mov di, cx
    dec di
    mov byte ptr es:[di], 0
    loop clearscreen_loop

    jmp main

    exit:

    mov ax, 3       ; normal mode
    int 10h         ; video BIOS interrupt

    mov ax, 4c00h   ; exit (return 0)
    int 21h

putpixel:
    push bp
    mov bp, sp

    ; Draws a pixel of a specified color
    ; at the coordinates (x, y).
    ;
    ; ARGUMENTS:
    ;   x       = word ptr [bp + 4]
    ;   y       = word ptr [bp + 6]
    ;   color   = word ptr [bp + 8]

    mov ax, [bp + 6]    ; ax = y
    mov cx, 320
    mul cx              ; y * 320
    add ax, [bp + 4]    ; y * 320 + x
    mov di, ax
    mov dx, [bp + 8]    ; dl = color
    mov es:[di], dl     ; draw pixel

    mov sp, bp
    pop bp
    ret 6

putsquare:
    push bp
    mov bp, sp

    ; Places a square of pixels of a specific
    ; color at the coordinates (x, y).
    ;
    ; NOTES:
    ; The color is represented by the low
    ; 8 bits of the argument. The side length
    ; of the square is equal to BLOCK_SIZE.
    ;
    ; ARGUMENTS:
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

    push [bp + 8]
    push [bp + 6]
    push [bp + 4]
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

place_fruit:
    push bp
    mov bp, sp
    sub sp, 4

    ; LOCAL VARIABLES:
    ;   x   = word ptr [bp - 2]
    ;   y   = dword ptr [bp - 4]

    call rand_position
    mov [bp - 2], cx
    mov [bp - 4], dx

    push 27h
    push dx
    push cx
    call putsquare

    mov cx, [bp - 2]
    mov dx, [bp - 4]

    mov sp, bp
    pop bp
    ret

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