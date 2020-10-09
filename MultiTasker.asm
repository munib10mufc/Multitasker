;******************************* ~COMMENTS~ *****************************************************
; Muhammad Munib Tahir										*
; 12-4034											*
; Assembly(B)											*
; Sir Umar Suleman										*
; My code works with following pattern in com files to be run,example main function as follow	*
;	*****************************************************************			*
;	* parameter block layout:					*			*
; 	* cs,ip,ds,es,param						*			*
; 	* 0, 2, 4, 6, 8							*			*
;	* paramblock: times 5 dw 0 ; space for parameters		*			*
;	* start: 							*			*
;	* mov [paramblock+0], cs ; code segment parameter		*			*
;       * mov word [paramblock+2], mytask ; offset parameter		*			*
;	* mov [paramblock+4], ds ; data segment parameter		*			*
;	* mov [paramblock+6], es ; extra segment parameter		*			*
;	* mov word [paramblock+8], 0 ; parameter for thread		*			*
;	* mov si, paramblock ; address of param block in si		*			*
;	* int 0x80 ; multitasking kernel interrupt			*			*
;	* mov dx, start							*			*
;	* add dx, 15							*			*
;	* mov cl, 4							*			*
;	* shr dx, cl							*			*
;	* mov ax, 0x3100              ; terminate and stay resident	*			*
;	* int 0x21							*			*
;       *****************************************************************			*
; i have done this program till stage 1 and some part of stage2					*
;************************************************************************************************

[org 0x0100]                                 
jmp main

;///////////////////////////// Variables//////////////////////
names: db 'FL =CS =IP =BP =AX =BX =CX =DX =SI =DI =DS =ES =SP =SS =HS =FS ='                
check:db 0              
oldisr:dd 0                                      
oldkbval:dd 0                         
pcb: times 32*9 dw 0                                  
current: dw 0               ; stores index of current pcb  
nextpcb: dw 1               ; stores index of next free pcb                                                        


;//////////////////////prints the numbers on the screen //////////////////////////
printnumbers:	                                
	push bp                                 
	mov bp, sp                                 
	push es                                 
	push ax                                 
	push bx                                 
	push cx                                 
	push dx                                 
	push di                                 
	mov di, 80 	;load comulns per row in di                                 
	mov ax, [bp+8] 	;load row numbers in ax                                 
	mul di 		;multiply comumns and rows                                 
	mov di, ax 	;stores value in di                                 
	add di, [bp+6]  ;add di                                 
	shl di, 1 	;increase byte count by one                                 
	add di, 8 	;to end of number location                                 
	mov ax, 0xb800                                 
	mov es, ax 	;point es to video base                                 
	mov ax, [bp+4] 	;mov number in ax                                 
	mov bx, 16 	;divsion by base 16                                 
	mov cx, 4 	;number of digits is 4                                 

	nextchar1:                                  
		mov dx, 0                                 
		div bx 		;divide by 10                                 
		add dl, 0x30 	;digit to ascii value                                 
		cmp dl, 0x39 	;if the digit is an alphabet                                                                                                                                    
		jbe skip	;skip addition                                                                                                   
		add dl, 7 	;enter alphabet code
	                                                                  
	skip:                                                                                                    
	mov dh, 0x07 	;normal attribute white on black                                                                  
	mov [es:di], dx                                                                  
	sub di, 2                                  
	loop nextchar1	;divide again                                 
	pop di                                 
	pop dx                                 
	pop cx                                 
	pop bx                                 
	pop ax                                 
	pop es                                 
	pop bp                                 
	ret 6                                                              
           

;//////////////////Keyboard isr//////////////////////////////////
kboardisr:                                  
	push ax                                 
	push es                                 
	in al, 0x60                                 
	cmp al, 0x39    ;checks if the key "space" is pressed or not                                 
	jne skipflag	;if space is not pressed then go forward                                 
	call clrscr	;if space is pressed then clear screen and print values of every register in each task
	xor byte [cs:check],1                                 
	mov ax, 0xb800                                 
	mov es, ax 	;point es to video base                                 
	cmp byte [cs:check],1                                                                   
	jne skipflag                                 
	call debug                                 
	
	skipflag:                                 
		pop es                                 
		pop ax                                 
		jmp far [cs:oldkbval]  
                      
	debug:                                 
		push ax                                 
		push bx                                 
		push cx                                 
		push dx                                 
		push si                                                                  
		push di                                 
		push cs                                 	
		pop ds                                 
		call clrscr                                  
		mov si, pcb  	;first register position                                 
		mov ax, 4	;row 0                                 
		mov bx, 6 	;comuns 5                                 
		mov di,0                                 
	                                 
	outerloop:                                 
		mov cx, 16 	;16 registers                                 
	
	innerloop:                                 
		push ax 	;row number                                 
		push bx 	;column number                                 
		mov dx,[si]                                 
		push dx 	                                 
		call printnumbers ;print the subroutine                                 
		add si,2 	;point next register in pcb                                 
		inc ax 		                                 
		loop innerloop 	;loop for 16 regs                                 
		mov ax, 4                                 
		add bx,8                                                                  
		inc di                                 
		cmp di,9                                 
		jne outerloop                                 
                                 
	mov ax, 4 	;row 0                                 
	mov bx,0 	;column 0                                 
	mov cx, 16 	;16 register                                 
	mov si, 4 	;4 chars in eacch name                                 
	mov dx, names 	;moving offset of first name in dx                                 

	innerloop1:                                 
		push ax                                  
		push bx                                  
		push dx                                  
		push si                                  
		call printstr                                  
		add dx,4 	;point to start of next string                                 
		inc ax                                 
		loop innerloop1 ;loop for 16 register

	pop di                                 
	pop si                                 
	pop dx                                 
	pop cx                                
	pop bx                                           
	pop ax                                                            
	ret                     

;/////////////////////////////In pcb routine//////////////////////

inpcb:
	call clrscr 
	push ax
	push bx
	push cx
	push di
	mov bx, [cs:nextpcb]        ; read next pcb index
	cmp bx, 9
	je exit 
	mov cl, 5
	shl bx, cl                  ;multiply by 32
	mov ax, [si+0]              ;read code segment
	mov [cs:pcb+bx+18], ax      ;space for cs
	mov ax, [si+2]              ;offset parameter
	mov [cs:pcb+bx+16], ax      ;space for ip
	mov ax, [si+4]              ;data segment parameter
	mov [cs:pcb+bx+20], ax      ;space for ds
	mov ax, [si+6]              ;extra segment parameter
	mov [cs:pcb+bx+24], ax      ;space for es
	mov [cs:pcb+bx+22], cs      ;point stack to our segment
	mov word [cs:pcb+bx+26],0x0200
	mov word [cs:pcb+bx+28], 0  ;set as next of new thread
	mov ax, [cs:nextpcb]        ;read new thread index
	mov [cs:pcb+bx-4], ax       ;next of 0th thread
	inc word [cs:nextpcb]
	exit:
		pop di
		pop cx
		pop bx
		pop ax
		iret

;////////////////////////// subroutine to print a string////////////////////                                 

printstr:                                  
	push bp                                 
	mov bp, sp                                 
	push es                                 
	push ax                                 
	push bx                                 
	push cx                                 
	push dx                                 
	push si                                 
	push di                                 
	mov ax, 0xb800                                 
	mov es, ax                                  
	mov byte [es:0],'c'                                 
	mov byte [es:2],'u'                                 
	mov byte [es:4],'r'                                 
	mov byte [es:6],'r'                                 
	mov byte [es:8],'='                                 
	mov al,[cs:current]                                 
	add ax,0X30                                 
	mov ah,0x07                                 
	mov byte [es:10],al                                 
	mov di, 80                                  
	mov ax, [bp+10]                                  
	mul di 		;multiply with columns per row                                 
	mov di,ax 	;save result in di                                 
	add di, [bp+8]                                  
	shl di, 1 	;turn into byte count                                 
	mov si, [bp+6]                                  
	mov cx, [bp+4] 	;length of string                                 
	mov ah, 0x07 	;fixed attribute                                 
	                                 
	nextchar2:                                  
		mov al, [si] 	;load next char                                 
		mov [es:di], ax ;print next char                                 
		add di, 2 	;next screen location                                 
		add si, 1                                  
		loop nextchar2   ;loop                                 
	pop di                                 
	pop si                                 
	pop dx                        
	pop cx                                 
	pop bx                                 
	pop ax                                 
	pop es                                 
	pop bp                                 
	ret 8    

;/////////////////////timer interrupt/////////////////////////////////////                                           
timerroutine:                                  
	push ds                         
	cmp byte [cs:check],1                                    
	je endtimer                  
	push bx                         
	push cs
	pop ds
	mov bx, [current]           ;read index of current in bx
	shl bx,5                    ;multiply by 32 for pcb start
	mov [pcb+bx+0], ax          ;save ax in current pcb
	mov [pcb+bx+4], cx          
	mov [pcb+bx+6], dx          
	mov [pcb+bx+8], si          
	mov [pcb+bx+10], di         
	mov [pcb+bx+12], bp         ; save bp in current pcb
	mov [pcb+bx+24], es         
	pop ax                      ;read original bx from stack
	mov [pcb+bx+2], ax          ;save bx in current pcb
	pop ax                      ;read original ds from stack
	mov [pcb+bx+20], ax         ;save ds in current pcb
	pop ax                      ;read original ip from stack
	mov [pcb+bx+16], ax         ;save ip in current pcb
	pop ax                      ;read original cs from stack
	mov [pcb+bx+18], ax         ;save cs in current pcb
	pop ax                      ;read original flags from stack
	mov [pcb+bx+26], ax         ;save cs in current pcb
	mov [pcb+bx+22], ss         ;save ss in current pcb
	mov [pcb+bx+14], sp         ;save sp in current pcb
	mov bx, [pcb+bx+28]         ;read next pcb of this pcb
	mov [current], bx           ;update current to new pcb
	mov cl, 5
	shl bx, cl                  ;multiply by 32 for pcb start
	mov ss, [pcb+bx+22]        
	mov sp, [pcb+bx+14]         ; read sp of new process
	mov bp, [pcb+bx+12]        
	mov es, [pcb+bx+24] 
	mov di, [pcb+bx+10]         
	mov dx, [pcb+bx+8]          
	mov si, [pcb+bx+6]  
	mov cx, [pcb+bx+4]                  
	push word [pcb+bx+26]       ; push flags of new process
	push word [pcb+bx+18]       ; push cs of new process
	push word [pcb+bx+16]       ; push ip of new process
	push word [pcb+bx+20]       ; push ds of new process
	jmp endtimer                                 
	
	endtimer:
		mov al, 0x20
		out 0x20, al 
		mov ax, [pcb+bx+0]          ;read ax of new process
		mov bx, [pcb+bx+2]          ;read bx of new process
	
	pop ds
	iret                 


clrscr:
	push es
	push ax
	push cx
	push di
	mov ax,0xb800
	mov es,ax
	xor di,di
	mov ax,0x0720
	mov cx,2000
	cld
	rep stosw
	pop di
	pop cx
	pop ax
	pop es
	ret                        
       

main:    
	mov ax,0
	mov es, ax                 
	mov word [es:0x80*4], inpcb	;hooking
	mov [es:0x80*4+2], cs  
	mov ax,[es:0x9*4]
	mov [oldkbval], ax 		;storing old values
	mov ax,[es:0x9*4+2]
	mov [oldkbval+2], ax
	mov ax,[es:0x8*4]
	mov [oldisr], ax
	mov ax,[es:0x8*4+2]
	mov [oldisr+2], ax
	cli
	mov word [es:0x08*4], timerroutine
	mov [es:0x08*4+2], cs       	;hook timer interrupt
	mov word [es:0x9*4], kboardisr
	mov [es:0x9*4+2], cs      	
	sti
	mov dx,main
	add dx, 15
	mov cl, 4
	shr dx, cl
	mov ax, 0x3100              ; TSR
	int 0x21