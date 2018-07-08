mov sp, 0
mov r13, 0 ;screen port
bump r13
send r13, 0x200F
send r13, 0x1000
;send r13, 0x0041
jmp main

bfrom:
	dw "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
	;dw "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
	dw 0x0000

;compiles into r2asm:
;
;first bytes to reset stuff:
;	mov sp, 0   ;reset stack             0x2020000E
;	mov r13, 0  ;set port                0x2020000D
;	mov r12, 0  ;set ram pointer         0x2020000C
;	mov r11, n  ;set ram index           0x202....B
;   mov r10, m  ;set the getchar address 0x202....A
;
;the instructions:
;
;	+: add [r11+r12], #+ ;0x24EB000C | #+<<4
;		mask: 0x00E00000 | r11<<16 = B0000 | r12<<0 = C | #+<<4
;		add:  0x24000000
;
;	-: sub [r11+r12], #- ;0x26EB000C | #-<<4
;		mask: 0x00E00000 | r11<<16  = B0000 | r12<<0 = C | #-<<4
;		sub:  0x26000000
;
;   >: add r12, #> ;0x2420000C | #><<4
;		mask: 0x00200000 | r12<<0 = C | #><<4
;		add:  0x24000000
;      and r12, 0xFF ;0x21200FFC
;		mask: 0x00200000 | r12 = C | 0xFF<<4 = 0xFF0
;		and:  0x21000000
;
;   <: sub r12, #< ;0x2620000C | #<<<4
;		mask: 0x00200000 | r12<<0 = C | #<<<4
;		sub:  0x26000000
;      and r12, 0xFF ;0x21200FFC
;		mask: 0x00200000 | r12 = C | 0xFF<<4 = 0xFF0
;		and:  0x21000000
;
;   .: and [r11+r12], 0x00FF ;0x21EB0FFC
;		mask: 0x00E00000 | r11<<16 = B0000 | r12<<0 = C | 0xFF<<4 = FF0
;		and:  0x21000000
;	   send r13, [r11+r12]
;		mask:
;       send:
;
;   ,: call r10
;
;totally didnt forget these when testing for the first time:
;
;	[: 
;	]: 


compile:
	mov r9, 0 ;rom counter
	mov r8, 0 ;temporary instruction storage
	mov r7, 0 ;+/-/>/< counter
	mov r6, 0 ;instruction upper (SWM)
	mov r5, 0 ;instruction lower (MOV)
	mov r4, 0 ;instruction counter
	mov r3, 0 ;old bfrom address counter for ]
	mov r2, 0 ;popped instrrom address counter for ]

	.init:
		;	mov sp, 0   ;reset stack             0x2020000E
		;	mov r13, 0  ;set port                0x2020000D
		;	mov r12, 0  ;set ram pointer         0x2020000C
		;	mov r11, n  ;set ram index           0x202....B
		mov r6, 0x2020		;pretty much the only MSB set
		swm r6              ;load em' up
		mov r5, 0x000E      ;load sp's instruction first as we'll
							;decrement for the rest (except for) r11
		mov [r4+code], r5   ;encode mov sp(r14), 0
		add r4, 1
		sub r5, 1
		mov [r4+code], r5	;encode mov r13, 0
		add r4, 1
		sub r5, 1
		mov [r4+code], r5	;encode mov r12, 0
		add r4, 1
		sub r5, 1
		;now we need to get the RAM's index address
		mov r8, ram			;using r8 as its safe
		shl r8, 4           ;shift it by 4 to fit the class 2
		or r5, r8           ;or it with LSBs of our instruction
		mov [r4+code], r5	;encode mov r11, ram
		add r4, 1

	.loop:
		mov r8, [bfrom+r9]  ;fetch the next bf instruction

		cmp r8, '+'         ;check if it is +
		je .instr_add		;^

		cmp r8, '-'			;check if it is -
		je .instr_sub       ;^

		cmp r8, '>'         ;check if it is >
		je .instr_right     ;^

		cmp r8, '<'         ;check if it is <
		je .instr_left      ;^

		cmp r8, '['         ;check if it is [
		je .instr_bopen     ;^

		cmp r8, ']'         ;check if it is ]
		je .instr_bclose    ;^

		cmp r8, '.'         ;check if it is .
		je .instr_out

		ret                 ;if none match, we hit EOF

		.instr_add:
			add r7, 1           ;add 1 to r7 since we found a +
			add r9, 1           ;set index for the next cell
			mov r8, [bfrom+r9]  ;check the next cell
			cmp r8, '+'         ;check if it is + aswell
			je .instr_add       ;if so, loop
			sub r9, 1           ;else, revert the incerased index, loop
							    ;will take care of r8
			shl r7, 4           ;shifting r7 by 4 to fit the instructi-
							    ;on, doesent matter if the MSBs get lo-
							    ;st as this is an 8 bit bf machine.
			mov r5, r7			;move the number of +s into our instru-
								;ction register         
			mov r7, 0           ;set it to 0 for future use
			or r5, 0x000C       ;load up half of our instruction
			mov r6, 0x24EB      ;load up the rest
			swm r6              ;store MSBs in the 13 bit WO register
			mov [r4+code], r5   ;write the instuction
			add r4, 1           ;move on to the next instruction
			jmp .loopiter       ;continue compilation

		.instr_sub:
			add r7, 1           ;add 1 to r7 since we found a -
			add r9, 1           ;set index for the next cell
			mov r8, [bfrom+r9]  ;check the next cell
			cmp r8, '-'         ;check if it is - aswell
			je .instr_sub       ;if so, loop
			sub r9, 1           ;else, revert the incerased index, loop
							    ;will take care of r8
			shl r7, 4           ;shifting r7 by 4 to fit the instructi-
							    ;on, doesent matter if the MSBs get lo-
							    ;st as this is an 8 bit bf machine.
			mov r5, r7			;move the number of -s into our instru-
								;ction register         
			mov r7, 0           ;set it to 0 for future use
			or r5, 0x000C       ;load up half of our instruction
			mov r6, 0x26EB      ;load up the rest
			swm r6              ;store MSBs in the 13 bit WO register
			mov [r4+code], r5   ;write the instuction
			add r4, 1           ;move on to the next instruction
			jmp .loopiter       ;continue compilation

		.instr_right:
			add r7, 1           ;add 1 to r7 since we found a >
			add r9, 1           ;set index for the next cell
			mov r8, [bfrom+r9]  ;check the next cell
			cmp r8, '+'         ;check if it is > aswell
			je .instr_right     ;if so, loop
			sub r9, 1           ;else, revert the incerased index, loop
							    ;will take care of r8
			shl r7, 4           ;shifting r7 by 4 to fit the instructi-
							    ;on, doesent matter if the MSBs get lo-
							    ;st as this is an 8 bit bf machine.
			mov r5, r7			;move the number of >s into our instru-
								;ction register         
			mov r7, 0           ;set it to 0 for future use
			or r5, 0x000C       ;load up half of our instruction
			mov r6, 0x2420      ;load up the rest
			swm r6              ;store MSBs in the 13 bit WO register
			mov [r4+code], r5   ;write the instuction
			add r4, 1           ;move on to the next instruction

			mov r5, 0x0FFC      ;we need to keep the number in 8 bit
								;bounds soo we encode `and r12, 0xFF`
			mov r6, 0x2120      ;second part
			swm r6              ;load up the MSBs
			mov [r4+code], r5   ;encode it
			add r4, 1           ;move on to the next instruction

			jmp .loopiter       ;continue compilation

		.instr_left:
			add r7, 1           ;add 1 to r7 since we found a <
			add r9, 1           ;set index for the next cell
			mov r8, [bfrom+r9]  ;check the next cell
			cmp r8, '<'         ;check if it is < aswell
			je .instr_left       ;if so, loop
			sub r9, 1           ;else, revert the incerased index, loop
							    ;will take care of r8
			shl r7, 4           ;shifting r7 by 4 to fit the instructi-
							    ;on, doesent matter if the MSBs get lo-
							    ;st as this is an 8 bit bf machine.
			mov r5, r7			;move the number of <s into our instru-
								;ction register         
			mov r7, 0           ;set it to 0 for future use
			or r5, 0x000C       ;load up half of our instruction
			mov r6, 0x2620      ;load up the rest
			swm r6              ;store MSBs in the 13 bit WO register
			mov [r4+code], r5   ;write the instuction
			add r4, 1           ;move on to the next instruction

			mov r5, 0x0FFC      ;we need to keep the number in 8 bit
								;bounds soo we encode `and r12, 0xFF`
			mov r6, 0x2120      ;second part
			swm r6              ;load up the MSBs
			mov [r4+code], r5   ;encode it
			add r4, 1           ;move on to the next instruction

			jmp .loopiter       ;continue compilation
		.instr_bropen:
			push r4				;push the current instruction as the
								;paired ] will use it
			;code is:
			;cmp [r11+r12], 0
			;jz <immediate will be inserted when ] is found>
			jmp .loopiter

		.instr_brclose:
			pop r2
			mov r3, 
			;code is:
			;cmp [r11+r12], 0
			;jnz <whatever got popped>
			jmp .loopiter

		.instr_out: ;note: fuck yes no optimisation needed
			;   .: and [r11+r12], 0x00FF ;0x21EB0FFC
			;		mask: 0x00E00000 | r11<<16 = B0000 | r12<<0 = C | 0xFF<<4 = FF0
			;		and:  0x21000000
			;	   send r13, [r11+r12] ;0x3A9B00CD
			;		mask: 0x00900000 | r13<<0 = D | r12<<4 = C0 | r11<<16 = B0000
			;       send: 0x3A000000
						  
			mov r5, 0x0FFC		;we need to keep the values in 8 bit
								;bounds and the best way to do it is
								;when it is output (for speed)
			mov r6, 0x21EB      ;
			swm r6              ;load up the MSBs
			mov [r4+code], r5   ;encode it
			add r4, 1           ;move on to the next instruction

			mov r5, 0x00CD      ;and now for sending, we encode
								;send r13, [r11+r12]
			mov r6, 0x3A9B      ;MSBs
			swm r6              ;load the MSBs
			mov [r4+code], r5   ;encode it
			add r4, 1           ;move on to the next instruction

			add r4, 1           ;move on to the next instruction
			jmp .loopiter       ;continue compilation

	.loopiter:
		add r9, 1
		;mov r6, 0 ;reset SWM data since it messes up memory writes
		cmp r9, 256 ;257th instruction, out of rom bounds
		jne .loop   ;if we are in bounds, continue
	ret

main:
	call compile
	mov r6, 0 ;reset SWM data since it messes up memory writes
	swm r6
	call code
	hlt

dw 0xDEAD ;awesome section seperation (for visual debugging)
dw 0xBEEF

ram:
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	;256byte ram

dw 0xDEAD ;awesome section seperation (for visual debugging)
dw 0xBEEF

code:
	dw 0x0000
