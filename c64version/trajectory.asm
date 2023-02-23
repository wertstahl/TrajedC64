
            ; hardscrolling trajectory editor for nonmathematical scrolling movements
            ; (c) feb 2012 by S.I.Hartmann aka Wertstahl aka Battlecommand

            ; this is v0.01 alpha and it can´t save! read the instruction screen carefully.


            ;------ BASIC - bootloader ------------------------------------------------------------
*=$0801 
            byte    $0c,$08,$01,$00,$9e,$34,$30,$39,$36,$00,$00,$00,$00,$00  ; 1 sys 4096

            ;--------------------------------------------------------------------------------------


*=$0e00

screenlo    byte    $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
screenhi    byte    $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07

;--------------
cramhi      byte    $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9,$d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
;------------------------------------------------------------->
                                                            ; point to this table down below to export as color ram scroller!
xval        brk
yval        brk
curr        brk
back        brk

step        brk
numlo       brk
numhi       brk

*=$1000


cls         lda #23
            sta $d018

            jsr clx
            jsr instrucx
            jsr clx

            ldx #$ff
funk        stx lda_base
            inc funk+1
            lda funk+1
            bne funk
            inc funk+2
            lda funk+2
            cmp #$80
            bne funk       



in_loop     lda step
            beq normal
            lda #$23
            bne elevated

normal      lda #$2a
elevated    sta curr
            jsr printcurr

do_loop     jsr $ffe4

            beq do_loop
            sta curr

            jsr delcurr

            lda curr
            cmp #$1d                ;
            beq cur_right           ; cursor movement
            cmp #$11                ;
            beq cur_down            ;
            cmp #$9d                ;
            beq cur_left            ;
            cmp #$91                ;
            beq cur_up              ;

            cmp #$5f
            beq redraw              ; restart
            cmp #$2a
            beq createtest          ; * = create code
            cmp #$3f
            beq help

            cmp #$20                ; space = store source position first or target position second
            beq stor

            cmp #$14
            beq delete

            jmp in_loop

cur_right   jmp cur_right_do
cur_down    jmp cur_down_do
cur_left    jmp cur_left_do
cur_up      jmp cur_up_do
redraw      jmp redraw_do
createtest  jmp create_do
help        jmp in_loop
stor        jmp stor_do
delete      jmp in_loop        

;------------------------------------------------------

cur_right_do
            inc xval
            lda xval
            cmp #40
            bne cur_ri_end
            lda #39
            sta xval
cur_ri_end  jmp in_loop

;------------------------------------------------------

cur_left_do
            dec xval
            lda xval
            cmp #$ff
            bne cur_le_end
            lda #00
            sta xval
cur_le_end  jmp in_loop



;-----------------------------------------------------

cur_down_do
            inc yval
            lda yval
            cmp #25
            bne cur_do_end
            lda #24
            sta yval
cur_do_end  jmp in_loop



;-----------------------------------------------------


cur_up_do
            dec yval
            lda yval
            cmp #$ff
            bne cur_up_end
            lda #00
            sta yval
cur_up_end  jmp in_loop



;-----------------------------------------------------


printcurr   ldx yval
            lda screenlo,x
            clc 
            adc xval
            bcc no_chup
            inx
no_chup     sta ch_out+1
            sta ch_in+1
            lda screenhi,x
            sta ch_out+2
            sta ch_in+2
ch_in       lda $1234
            sta back
            lda curr
ch_out      sta $1234
            rts

;-----------------------------------------------------


overprint   ldx yval
            lda screenlo,x
            clc 
            adc xval
            bcc no_ovp
            inx
no_ovp      sta ch_ovp+1
           
            lda screenhi,x
            sta ch_ovp+2


            lda step
            bne ovp_up
            lda #$23
            bne ch_ovp
ovp_up      lda #$56      


ch_ovp      sta $1234
            sta back
            sta curr
            rts



;-----------------------------------------------------

delcurr     ldx yval
            lda screenlo,x
            clc 
            adc xval
            bcc no_dup
            inx
no_dup      sta de_out+1
            lda screenhi,x
            sta de_out+2
            lda back
de_out      sta $1234
            rts

;-----------------------------------------------------

full        inc $d020
            jmp full

;-----------------------------------------------------


stor_do     lda step
            beq stor_lda
            
            ;----------------------------------

stor_sta    ldx yval
            lda screenlo,x
            clc 
            adc xval
            bcc pointer
            inx
pointer     sta sta_base
            ;-------------------------------------------------------------------------------------
            lda screenhi,x   ; <--- change this to "cramhi,x" to generate color-ram scrolling       
            ;------------------------------------------------------------------------------<------
pointer2    sta sta_base+1                                                                ;<      
                                                                                          ;<      
            inc pointer+1                                                                 ;<      
            inc pointer+1                                                                 ;<      
            lda pointer+1                                                                 ;<      
            bne xxnoup0                                                                   ;<      
            inc pointer+2                                                                 ;<      
                                                                                          ;<      
xxnoup0     inc pointer2+1                                                                ;<      
            inc pointer2+1                                                                ;<      
            lda pointer2+1                                                                ;<      
            cmp #$01                                                                      ;<      
            bne xxnoup3                                                                   ;<      
            inc pointer2+2                                                                ;<      
                                                                                          ;<      
xxnoup3     lda pointer+2                                                                 ;<      
            cmp #$3f                                                                      ;<      
            beq full                                                                      ;<      
                                                                                          ;<      
            dec step                                                                      ;<      
                                                                                          ;<      
            jsr overprint                                                                 ;<      
                                                                                          ;<      
            jmp in_loop                                                                   ;<      
                                                                                          ;<      
            ;-----------------------------------                                          ;<      
                                                                                          ;<      
                                                                                          ;<      
stor_lda    lda pointer3+2                                                                ;<      
            cmp #$3f                                                                      ;<      
            beq full                                                                      ;<      
                                                                                          ;<      
            ldx yval                                                                      ;<      
            lda screenlo,x                                                                ;<      
            clc                                                                           ;<      
            adc xval                                                                      ;<      
            bcc pointer3                                                                  ;<      
            inx                                                                           ;<      
pointer3    sta lda_base                                                                  ;<      
            ;------------------------------------------------------------------------------<------
            lda screenhi,x   ; <--- change this to "cramhi,x" to generate color-ram scrolling       
            ;-------------------------------------------------------------------------------------

pointer4    sta lda_base+1
                
            inc pointer3+1
            inc pointer3+1
            lda pointer3+1
            bne xxnoup
            inc pointer3+2

xxnoup      inc pointer4+1
            inc pointer4+1
            lda pointer4+1
            cmp #$01
            bne xxnoup2
            inc pointer4+2

xxnoup2     lda pointer3+2
            cmp #$3f
            beq full2

            inc step

            jsr overprint

            jmp in_loop
;-----------------------------------------------------

full2       inc $d020
            jmp full


;-----------------------------------------------------

create_do   sei
            lda #$35
            sta $01

            lda #<lda_base
            sta $02
            lda #>lda_base 
            sta $03

            lda #<sta_base
            sta $04
            lda #>sta_base
            sta $05

            lda #<tar_base
            sta $06
            lda #>tar_base
            sta $07

            ldx #$00
            ldy #$00
            
wri_loop    lda #$ad
            jsr wri_tar

            jsr ld_lda
            jsr wri_tar

            jsr ld_lda
            cmp #$ff
            beq end_wri
            jsr wri_tar

            lda #$8d
            jsr wri_tar

            jsr ld_sta
            jsr wri_tar

            jsr ld_sta
            jsr wri_tar

            jmp wri_loop

            
wri_tar     sta ($06),y
            inc $06
            lda $06
            bne notaru
            inc $07
notaru      rts


ld_lda      lda ($02),y
            inc $02
            ldx $02
            cpx #$00
            bne noldau
            inc $03
noldau      rts

ld_sta      lda ($04),y
            inc $04
            ldx $04
            cpx #$00
            bne nostau
            inc $05
nostau      rts


end_wri     dec $06
            lda $06
            cmp #$ff
            bne end_nounder
            dec $07
end_nounder dec $06
            lda $06
            cmp #$ff
            bne end2nounder
            dec $07
end2nounder lda #$60
            jsr wri_tar

deadlock    inc $d020
            jsr tar_base
            dec $d020
            jmp deadlock

;------------------------------------------------

redraw_do   lda #<lda_base
            sta pointer3+1
            sta funk+1
            tax
            inx
            txa
            sta pointer4+1            


            lda #>lda_base
            sta funk+2
            sta pointer3+2
            sta pointer4+2
      

            lda #<sta_base
            sta pointer+1
            tax
            inx
            txa
            sta pointer2+1


            lda #>sta_base
            sta pointer+2
            sta pointer2+2
            
            lda #<hjelp
            sta printhelp+1
            lda #>hjelp
            sta printhelp+2

            lda #$00
            sta fnfn+1
            lda #$04
            sta fnfn+2


            lda #$00
            sta xval
            sta yval
            sta curr
            sta back
            sta step
            sta numlo
            sta numhi

            jmp cls

;------------------------------------------------

instrucx    ldx #$00
printhelp   lda hjelp
            cmp #$ff
            beq d1o_loop
fnfn        sta $0400
            inc printhelp+1
            inc fnfn+1
            lda printhelp+1
            bne printhelp
            inc printhelp+2
            inc fnfn+2
            jmp printhelp            


d1o_loop    jsr $ffe4
            beq d1o_loop

            rts


;------------------------------------------------

clx         ldy #$00                ; clear screen & color
clr         lda #$2e
            sta $0400,y
            sta $0500,y
            sta $0600,y
            sta $06e8,y
            lda #$0e
            sta $d800,y
            sta $d900,y
            sta $da00,y
            sta $dae8,y
            iny
            cpy #$00
            bne clr
            rts


*=$1400
hjelp      
            byte $2a,$2a,$2a,$2a,$2a,$20,$14,$12,$01,$0a,$05,$03,$14,$0f,$12,$19,$20,$03,$0f,$04,$05,$20,$05,$04,$09,$14,$0f,$12,$20,$16,$30,$2e
            byte $30,$31,$20,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$20,$28,$03,$29,$06,$05,$02,$20,$32,$30,$31,$32,$20,$17,$05,$12
            byte $14,$13,$14,$01,$08,$0c,$20,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$01,$02,$13,$14,$12,$01,$03,$14,$3a,$20,$07,$05,$0e,$05,$12,$01
            byte $14,$05,$20,$01,$20,$06,$12,$05,$05,$0c,$19,$20,$04,$05,$06,$09,$0e,$01,$02,$0c,$05,$20,$20,$20,$0d,$01,$10,$20,$06,$0f,$12,$20
            byte $14,$08,$05,$20,$0f,$12,$04,$05,$12,$20,$08,$0f,$17,$20,$14,$08,$05,$20,$03,$05,$0c,$0c,$13,$20,$0f,$0e,$20,$14,$08,$05,$20,$20
            byte $13,$03,$12,$05,$05,$0e,$20,$01,$12,$05,$20,$13,$17,$01,$10,$10,$05,$04,$2e,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
            byte $20,$20,$20,$20,$20,$20,$20,$20,$06,$0f,$12,$20,$15,$0e,$15,$13,$15,$01,$0c,$20,$08,$01,$12,$04,$13,$03,$12,$0f,$0c,$0c,$09,$0e
            byte $07,$20,$10,$15,$12,$10,$0f,$13,$05,$13,$2e,$20,$20,$20,$20,$20,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
            byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$15,$13,$01,$07,$05,$3a,$20,$17
            byte $01,$12,$0e,$09,$0e,$07,$3a,$20,$0e,$0f,$20,$15,$0e,$04,$0f,$21,$20,$2d,$20,$0e,$0f,$20,$13,$01,$16,$05,$21,$20,$20,$20,$20,$20
            byte $01,$0c,$17,$01,$19,$13,$20,$15,$13,$05,$20,$01,$20,$06,$12,$05,$05,$1a,$05,$12,$20,$0f,$12,$20,$05,$0d,$15,$0c,$01,$14,$0f,$12
            byte $20,$17,$09,$14,$08,$20,$20,$20,$08,$05,$18,$20,$0d,$0f,$0e,$09,$14,$0f,$12,$20,$15,$0e,$0c,$05,$13,$13,$20,$14,$08,$05,$12,$05
            byte $20,$09,$13,$20,$01,$20,$13,$01,$16,$05,$20,$06,$15,$0e,$03,$2d,$14,$09,$0f,$0e,$20,$09,$0d,$10,$0c,$05,$0d,$05,$0e,$14,$05,$04
            byte $2e,$20,$17,$08,$09,$03,$08,$20,$09,$13,$20,$0e,$0f,$17,$20,$4e,$4f,$54,$20,$14,$08,$05,$12,$05,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
            byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
            byte $0d,$0f,$16,$05,$20,$17,$09,$14,$08,$20,$03,$15,$12,$13,$0f,$12,$20,$0b,$05,$19,$13,$2e,$20,$17,$08,$05,$0e,$20,$03,$15,$12,$13
            byte $0f,$12,$20,$13,$08,$0f,$17,$13,$22,$2a,$22,$20,$09,$14,$13,$20,$12,$05,$01,$04,$19,$20,$14,$0f,$20,$10,$09,$03,$0b,$20,$15,$10
            byte $2e,$20,$10,$12,$05,$13,$13,$20,$13,$10,$01,$03,$05,$20,$20,$20,$0f,$0e,$03,$05,$20,$14,$0f,$20,$13,$14,$0f,$12,$05,$20,$14,$08
            byte $05,$20,$13,$0f,$15,$12,$03,$05,$20,$06,$09,$05,$0c,$04,$2e,$20,$20,$20,$20,$20,$20,$20,$20,$20,$14,$08,$05,$0e,$20,$0d,$0f,$16
            byte $05,$20,$03,$15,$12,$13,$0f,$12,$20,$14,$0f,$20,$14,$08,$05,$20,$14,$01,$12,$07,$05,$14,$20,$06,$09,$05,$0c,$04,$20,$01,$0e,$04
            byte $10,$12,$05,$13,$13,$20,$13,$10,$01,$03,$05,$20,$01,$07,$01,$09,$0e,$3a,$20,$14,$01,$12,$07,$05,$14,$20,$13,$14,$0f,$12,$05,$04
            byte $2e,$20,$0e,$05,$18,$14,$2e,$20,$3d,$3d,$3d,$3d,$48,$49,$4e,$54,$3a,$20,$55,$53,$45,$20,$4f,$56,$45,$52,$4c,$41,$50,$50,$49,$4e
            byte $47,$60,$57,$49,$53,$45,$4c,$59,$21,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$10,$12,$05,$13,$13,$20,$1f,$20,$14,$0f,$20,$12,$05,$13,$14,$01
            byte $12,$14,$2e,$20,$09,$14,$20,$0b,$09,$0c,$0c,$13,$20,$05,$16,$05,$12,$19,$14,$08,$09,$0e,$07,$21,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
            byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
            byte $10,$12,$05,$13,$13,$20,$2a,$20,$14,$0f,$20,$07,$05,$0e,$05,$12,$01,$14,$05,$20,$19,$0f,$15,$12,$20,$03,$0f,$04,$05,$2e,$20,$03
            byte $0f,$04,$05,$20,$17,$09,$0c,$0c,$02,$05,$20,$13,$14,$01,$12,$14,$05,$04,$20,$01,$15,$14,$0f,$0d,$01,$14,$09,$03,$01,$0c,$0c,$19
            byte $2e,$20,$07,$12,$01,$02,$20,$09,$14,$20,$06,$12,$0f,$0d,$20,$20,$0d,$05,$0d,$20,$24,$36,$30,$30,$30,$2d,$2e,$2e,$2e,$20,$0e,$0f
            byte $20,$05,$18,$09,$14,$20,$06,$12,$0f,$0d,$20,$14,$05,$13,$14,$20,$0d,$0f,$04,$05,$21,$21,$21,$21,$4d,$41,$58,$20,$34,$30,$39,$36
            byte $20,$13,$0f,$15,$12,$03,$05,$2f,$14,$01,$12,$07,$05,$14,$20,$10,$01,$09,$12,$13,$21,$54,$08,$05,$0e,$3a,$44,$45,$41,$54,$48,$21
            byte $2d,$2d,$2d,$2d,$2d,$2d,$2d,$2d,$2d,$90,$12,$05,$13,$13,$20,$01,$0e,$19,$20,$0b,$05,$19,$20,$14,$0f,$20,$13,$14,$01,$12,$14,$2d
            byte $2d,$2d,$2d,$2d,$2d,$2d,$2d,$2d

            byte $ff
            byte $ff
            byte $ff


*=$2000
lda_base    byte 0

*=$4000
sta_base    byte 0

*=$6000     
tar_base    byte 0
