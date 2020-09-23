; AARotate.asm  by Robert Rayment  16/4/02

; VB

; ' MCode Structure
; Public Type MCodeStruc
;    PICW As Long
;    PICH As Long
;    PtrPalBGR As Long
;    PalSize As Long
;    zang As Single
; End Type
; Public MCODE As MCodeStruc
;   
; ptMC = Ptr to mcode byte array
; ptrStruc = VarPtr(MCODE.PICW)
;
; MCODE.zang = zang
;
; ' ANTI-ALIAS ROTATE
; res = CallWindowProc(ptMC, ptrStruc, 0&, 0&, RotFlag)
;                       [ebp +  8      12  16  20 ]
; RotFlag=0 for Rotate
; RotFlag=1 for anti-alias Rotate

%macro movab 2      ; name & num of parameters
  push dword %2     ; 2nd param
  pop dword %1      ; 1st param
%endmacro           ; use  movab %1,%2
; Allows eg movab bmW,[ebx+4]

%define PICW            [ebp-4]     ; PICW Mod 4
%define PICH            [ebp-8]     ; PICH
%define PtrPalBGR       [ebp-12]    ; PTR to PalBGR(1,1,1,1)
%define PalSize         [ebp-16]    ; 4*PICW*PICH Plane size in bytes
%define zang            [ebp-20]    ; rotation, anglular step in +/- radians

%define zCos    [ebp-24]
%define zSin    [ebp-28]
%define ixc     [ebp-32] 
%define iyc     [ebp-36]

%define ix      [ebp-40] 
%define iy      [ebp-44]

%define xs      [ebp-48]
%define ys      [ebp-52]
%define ixs0    [ebp-56] 
%define iys0    [ebp-60]    

%define xsf     [ebp-64]    ; scale factors 
%define ysf     [ebp-68]    

%define culb    [ebp-72] 
%define culg    [ebp-76] 
%define culr    [ebp-80] 

%define RotFlag [ebp-84]    ; 0 Rot, 1 AARot
%define Half    [ebp-88]    ; to get int

[BITS 32]

    push ebp
    mov ebp,esp
    sub esp,88
    push edi
    push esi
    push ebx

    ; Copy structure
    mov ebx,[ebp+8]
    
    movab PICW,          [ebx]
    movab PICH,          [ebx+4]
    movab PtrPalBGR,     [ebx+8]
    movab PalSize,       [ebx+12]
    movab zang,          [ebx+16]
    

    ; Get 0.5 for int round down
    mov eax,2
    mov Half,eax
    fld1        
    fild dword Half     ; 2, 1
    fdivp st1           ; 1/2
    fstp dword Half

    ; Get sin(zang) & Cos(zang)

    fld dword zang
    fsincos
    fstp dword zCos     ; Cos(zang)
    fstp dword zSin     ; Sin(zang)

    mov eax,PICW
    shr eax,1
    mov ixc,eax         ; xcen

    mov eax,PICH
    shr eax,1
    mov iyc,eax         ; ycen


    ; Get initial source & destin pointers
    
    mov edi,PtrPalBGR   ; pts to PalBGR(1,1,1,1)
    mov esi,edi         ; esi pts to SOURCE PalBGR(1,1,1,1) Blue
    mov eax,PalSize
    add edi,eax         ; edi pts to DESTIN PalBGR(1,1,1,2) Blue

    ; Get RotFlag
    movab eax,[ebp+20]  ; RotFlag

    ;mov eax,RotFlag
    cmp eax,0
    jnz DoAAR
    
    Call Rotate32
    
    jmp GETOUT

DoAAR:

    Call AARotate32

GETOUT:
    pop ebx
    pop esi
    pop edi
    mov esp,ebp
    pop ebp
    ret 16

;############################################################
;============================================================
Rotate32: ; 0         Rotate(display) 1 into 2

    mov eax,PICH
    mov ecx,eax
IY0:
    mov iy,ecx
    push ecx
    
    mov eax,PICW
    mov ecx,eax
IX0:
    mov ix,ecx
    ;-----------------------
    ; Get ixs0
    fild dword ix
    fild dword ixc
    fsubp st1           ; (ix-ixc)
    fld dword zCos
    fmulp st1           ; (ix-Xp)*zCos
    fild dword iy
    fild dword iyc
    fsubp st1           ; (iy-iyc)
    fld dword zSin
    fmulp st1           ; (iy-iyc)*zSin, (ix-ixc)*zCos
    faddp st1           ; (ix-ixc)*zCos + (iy-iyc)*zSin
    fild dword ixc
    faddp st1           ; ixc + (ix-ixc)*zCos + (iy-iyc)*zSin
    fistp dword ixs0

    ; Get iys0
    fild dword iy
    fild dword iyc
    fsubp st1           ; (iy-iyc)
    fld dword zCos
    fmulp st1           ; (iy-iyc)*zCos
    fild dword ix
    fild dword ixc
    fsubp st1           ; (ix-ixc)
    fld dword zSin
    fmulp st1           ; (ix-ixc)*zSin, (iy-iyc)*zCos
    fsubp st1           ; (iy-Yp)*zCos - (ix-Xp)*zSin
    fild dword iyc
    faddp st1           ; Yp + (iy-iyc)*zCos + (ix-ixc)*zSin
    fistp dword iys0

    ; Check in-range
    mov eax,ixs0
    cmp eax,1
    jl nexix0
    cmp eax,PICW
    jge nexix0
    mov eax,iys0
    cmp eax,1
    jl nexix0
    cmp eax,PICH
    jge nexix0

;   InRange
    ; BGR @ ixs,iys in 3 -> ix,iy in 2
    ; esi->3  edi->2
    
    push edi
    push esi
    
    ;Call GetAddrESIixsiys       ; 1 @ ixs0,iys0
    mov eax,iys0
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ixs0
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add esi,eax
    
	;Call GetAddrEDIixiy         ; 2 @ ix,iy
    mov eax,iy
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ix
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add edi,eax
   
    mov aL,byte [esi]
    mov byte [edi],aL
    mov aL,byte [esi+1]
    mov byte [edi+1],aL
    mov aL,byte [esi+2]
    mov byte [edi+2],aL

    pop esi
    pop edi
    
    ;-----------------------
nexix0:
    dec ecx
    jnz near IX0
    
    pop ecx
    dec ecx
    jnz near IY0
RET

;============================================================
AARotate32: ; 1         Anti-alias Rotate(display) 1 into 2


    mov eax,PICH
    mov ecx,eax
IY1:
    mov iy,ecx
    push ecx
    
    mov eax,PICW
    mov ecx,eax
IX1:
    mov ix,ecx
    
    ;-----------------------
    ; Get xs
    fild dword ix
    fild dword ixc
    fsubp st1           ; (ix-ixc)
    fld dword zCos
    fmulp st1           ; (ix-ixc)*zCos
    fild dword iy
    fild dword iyc
    fsubp st1           ; (iy-iyc)
    fld dword zSin
    fmulp st1           ; (iy-iyc)*zSin, (ix-ixc)*zCos
    faddp st1           ; (ix-ixc)*zCos + (iy-iyc)*zSin
    fild dword ixc
    faddp st1           ; ixc + (ix-ixc)*zCos + (iy-iyc)*zSin
    fstp dword xs       ; REAL
 
    ; Get ys
    fild dword iy
    fild dword iyc
    fsubp st1           ; (iy-iyc)
    fld dword zCos
    fmulp st1           ; (iy-iyc)*zCos
    fild dword ix
    fild dword ixc
    fsubp st1           ; (ix-ixc)
    fld dword zSin
    fmulp st1           ; (ix-ixc)*zSin, (iy-iyc)*zCos
    fsubp st1           ; (iy-iyc)*zCos - (ix-ixc)*zSin
    fild dword iyc
    faddp st1           ; iyc + (iy-iyc)*zCos - (ix-ixc)*zSin
    fstp dword ys       ; REAL

    ;-----------------------
    ; Get ixs0,iys0  INTEGERS
    fld dword xs
    fld dword Half      ; 0.5, xs
    fsubp st1           ; xs-0.5
    fistp dword ixs0    ; truncated xs

    fld dword ys
    fld dword Half      ; 0.5, ys
    fsubp st1           ; ys-0.5
    fistp dword iys0    ; truncated ys
    ;-----------------------

    ; Check in-range
    mov eax,ixs0
    cmp eax,1
    jl near nexix1
    cmp eax,PICW
    jge near nexix1
    mov eax,iys0
    cmp eax,1
    jl near nexix1
    cmp eax,PICH
    jge near nexix1

;   InRange
    
    ; Get scale factors xsf=xs-ixs0, ysf=ys-iys0
    fld dword xs
    fild dword ixs0
    fsubp st1       ; xs-ixs0
    fstp dword xsf
    
    fld dword ys
    fild dword iys0
    fsubp st1       ; ys-iys0
    fstp dword ysf
    
    ; Pick up from 1 esi
    
    push esi        ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    ;Call GetAddrESIixsiys
    mov eax,iys0
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ixs0
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add esi,eax

    mov ebx,PICW
    shl ebx,2               ; 4*PICW
    ;===================================
    ; Get weighted Blue over 4 points
    ; y, x->x+1
    movzx eax,byte[esi]     ; B
    mov culb,eax
    fild dword culb
    movzx eax,byte[esi+4]   ; B+1
    mov culb,eax
    fild dword culb        ; P2, P1
    fsub st1                ; (P2-P1), P1
    fld dword xsf
    fmulp st1               ; xsf*(P2-P1), P1
    faddp st1               ; PA = P1 + xsf*(P2-P1)
    ; y+1, x->x+1
    movzx eax,byte[esi+ebx] ; B
    mov culb,eax
    fild dword culb
    movzx eax,byte[esi+ebx+4]   ; B+1
    mov culb,eax
    fild dword culb        ; P4, P3
    fsub st1                ; (P4-P3), P3
    fld dword xsf
    fmulp st1               ; xsf*(P4-P3), P3
    faddp st1               ; PB= P3 + xsf*(P4-P3), PA
    ; y->y+1
    fsub st1                ; (PB-PA), PA
    fld dword ysf
    fmulp st1               ; ysf*(PB-PA), PA
    faddp st1               ; PA + ysf*(PB-PA)
    
    fistp dword culb
    ;===================================
    
    ; Get weighted Green over 4 points
    
    inc esi ; GREEN
    
    movzx eax,byte[esi]     ; G
    mov culg,eax
    fild dword culg
    movzx eax,byte[esi+4]   ; G+1
    mov culg,eax
    fild dword culg        ; P2, P1
    fsub st1                ; (P2-P1), P1
    fld dword xsf
    fmulp st1               ; xsf*(P2-P1), P1
    faddp st1               ; PA = P1 + xsf*(P2-P1)
    
    movzx eax,byte[esi+ebx] ; G
    mov culg,eax
    fild dword culg
    movzx eax,byte[esi+ebx+4]   ; B+1
    mov culg,eax
    fild dword culg        ; P4, P3
    fsub st1                ; (P4-P3), P3
    fld dword xsf
    fmulp st1               ; xsf*(P4-P3), P3
    faddp st1               ; PB= P3 + xsf*(P4-P3), PA
    
    fsub st1                ; (PB-PA), PA
    fld dword ysf
    fmulp st1               ; ysf*(PB-PA), PA
    faddp st1               ; PA + ysf*(PB-PA)
    
    fistp dword culg
    ;===================================

    ; Get weighted Red over 4 points
    
    inc esi ; RED  
    
    movzx eax,byte[esi]     ; R
    mov culr,eax
    fild dword culr
    movzx eax,byte[esi+4]   ; R+1
    mov culr,eax
    fild dword culr        ; P2, P1
    fsub st1                ; (P2-P1), P1
    fld dword xsf
    fmulp st1               ; xsf*(P2-P1), P1
    faddp st1               ; PA = P1 + xsf*(P2-P1)
    
    movzx eax,byte[esi+ebx] ; R
    mov culr,eax
    fild dword culr
    movzx eax,byte[esi+ebx+4]   ; R+1
    mov culr,eax
    fild dword culr        ; P4, P3
    fsub st1                ; (P4-P3), P3
    fld dword xsf
    fmulp st1               ; xsf*(P4-P3), P3
    faddp st1               ; PB= P3 + xsf*(P4-P3), PA
    
    fsub st1                ; (PB-PA), PA
    fld dword ysf
    fmulp st1               ; ysf*(PB-PA), PA
    faddp st1               ; PA + ysf*(PB-PA)
    
    fistp dword culr
    ;===================================

    pop esi     ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    

    ; Ensure colors in range ??
    ;Call near CheckculBGRT
    
    push edi    ; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    ;Call GetAddrEDIixiy
    mov eax,iy
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ix
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add edi,eax

    mov eax,culr
    shl eax,8
    add eax,culg
    shl eax,8
    add eax,culb
    mov [edi],eax
    
	; Equiv to:-
    ;mov eax,culb
    ;mov byte[edi],aL
    ;mov eax,culg
    ;mov byte[edi+1],aL
    ;mov eax,culr
    ;mov byte[edi+2],aL
    
    pop edi     ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ;-----------------------
nexix1:
    dec ecx
    jnz near IX1
    
    pop ecx
    dec ecx
    jnz near IY1

RET
;============================================================
;============================================================

CheckculBGRT:
    
    ; Ensure colors in range
    mov eax,255
    cmp culb,eax   ; culb-255
    jle THG
    mov culb,eax
THG:
    cmp culg,eax
    jle THR
    mov culg,eax
THR:
    cmp culr,eax
    jle LoLim
    mov culr,eax
LoLim:
    mov eax,0
    cmp culb,eax   ; culb-0
    jge TLG
    mov culb,eax
TLG:
    cmp culg,eax
    jge TLR
    mov culg,eax
TLR:
    cmp culr,eax
    jge CulsDone
    mov culr,eax

CulsDone:

RET
;============================================================


GetAddrESIixsiys:   ; Src esi-> PalBGR(1,1,1,1),ixs0,iys0  Out: new esi->Blue
    ;B = esi + (4 * (iys0-1) * PICW + 4 * (ixs0-1))
    ;B = esi + 4 * [ (iys0-1) * PICW + (ixs0-1)) ]
    mov eax,iys0
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ixs0
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add esi,eax
RET

;============================================================

GetAddrEDIixiy: ; Dest edi->PalBGR(1,1,1,2),ix,iy  Out: new edi->Blue
    ;B = edi + (4 * (iy-1) * PICW + 4 * (ix-1))
    ;B = edi + 4 * [ (iy-1) * PICW + (ix-1)) ]
    mov eax,iy
    dec eax
    mov ebx,PICW
    mul ebx
    mov ebx,ix
    dec ebx
    add eax,ebx
    shl eax,2       ; x4
    add edi,eax
RET
;============================================================
