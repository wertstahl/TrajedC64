		;----------------------------------------------------------------------------------
		;
		;  important note: i had to quickly write this during my c64 demo coding efforts.
		;				   clearly: i know that my coding is suboptimal and that the 
		;				   logic and layout and overall usage of the language sXck.
		;				   i know there are some very unusual and complex ways to solve
		;				   problems, that rather cause new problems than actually help.
		;				   BUT. this works. and i had the idea first. and i made it happen.
		;				   
		;				   if you don´t like it, do not use it. simple.
		;
		;
		; trajed v1.08
		; c=64 trajectory editor (c) 2012 by wertstahl of genesis*project
		;
		; visually construct hard scrolling methods and generate assembler speedcode
		;
		; written in blitzbasic (blitz3d)
		;
		; uses Blitz3D Filerequester (2.Update) by MPZ !!! see comments in that section
		; if you want to compile this on your machine!
		;
		;----------------------------------------------------------------------------------

		; Set the valuez and the arrayz
		
			Const maximumcommands = 2250
			Const maxxcomm = maximumcommands * 3 + 60
		
			Dim commands(maxxcomm)
			Dim matrix(1000)
			Dim reloccommands(maxxcomm)
			
			Gosub formatarray
			
			Const xwidth = 960 		; x-size of window
			Const yheight = 700 	; y-size of window 4:3 aspect + 100 pix for status output area
			Const divider = 24		; this describes the raster divider 
									; resizing the app will be a bit tricky due to shortsighted programming
			
			ywidth = yheight-100 	; status area top
			statusy = ywidth+12 	; status area left margin
			
			adr = 1024 				; general exemplary base adress $0400
			
			instnr = 0				;flags
			laststate = 0
			firstinit = 0
			saved = 0
			logic = 0
			pcounter=0

			filename$="default"
			datatype$="DATA"
			suffix$=".asm"
			baseadress=1024
			status$="IDLE."
			infusion=0

		; Create a hopefully common size 16 bit color window
		
			Graphics xwidth,yheight,16,2
			SetBuffer FrontBuffer() ;Sets FrontBuffer as the current drawing buffer 
		
		; Load dem imagez 
				
			backgrnd= LoadImage("guimg/screen.png")
			emptytile=LoadImage("guimg/empty.png")
			info=     LoadImage("guimg/info.bmp")
			cursor=   LoadImage("guimg/cursor.png")
			
			tile1=LoadImage("guimg/tl-1read.png")	
			tile2=LoadImage("guimg/tl-2write.png")
			tile3=LoadImage("guimg/tl-3writeonread.png")
			tile4=LoadImage("guimg/tl-4readonwrite.png")
			tile5=LoadImage("guimg/tl-5writeonwrite.png")
			tile6=LoadImage("guimg/tl-6readonread.png")			
			tileh=LoadImage("guimg/help.png")
			
			baseadin=   LoadImage("guimg/baseadr.png")			
			exitwarn=   LoadImage("guimg/exitwarn.png")
			menuplate=  LoadImage("guimg/menu.png")
			exportplate=LoadImage("guimg/export.png")
			warnfile=   LoadImage("guimg/filewarn.png")
			flushwarn=  LoadImage("guimg/flushwarn.png")
			
			testscreen=LoadImage("testarea960x600.png") ; the only image in program root. 
														; you can edit/replace this to show
														; content of your own desire
		;	filnam=  LoadImage("guimg/filename.png")
			errror=  LoadImage("guimg/errror.png")
			er2rror= LoadImage("guimg/er2rror.png")
			adjdelay=LoadImage("guimg/delay.png")
			sorrimg= LoadImage("guimg/sorry.png")
			sorrimg2=LoadImage("guimg/sorry2.png")
			sorrimg3=LoadImage("guimg/sorry3.png")
			sorrimg4=LoadImage("guimg/sorry4.png")
			sorrimg5=LoadImage("guimg/sorry5.png")
			sorrimg6=LoadImage("guimg/sorry6.png")
			sorrimg7=LoadImage("guimg/sorry7.png")
			
			relocit=LoadImage("guimg/relocate.png")						
			


			fntArial=LoadFont("Arial",12,True,False,False)
			SetFont fntArial
			fntArial2=LoadFont("Arial",20,True,False,False)
		
			
			DrawImage backgrnd,0,0
			Delay 800

		;------------ prepare editor -------------
			
			Gosub buildmatrix
			Gosub refreshtiles
			Gosub clearinfo
			
			mx1=MouseX()
			my1=MouseY()
			mx=MouseX()
			my=MouseY()

			Color 255,255,255		
			MaskImage cursor,255,255,255
			DrawImage cursor,((mx/divider)*40),((mx/divider)*25)
			
			mx1=2
			
		;------------------------------------------------------------------------------------	
		;----------------------------------- mainloop ---------------------------------------
		;------------------------------------------------------------------------------------	

		
							.mainloop						
									Gosub refreshtiles
							.cursorloop
									mx=MouseX()
									my=MouseY()									
									If my >ywidth-1 Then my=ywidth-1
									If mx=mx1 And my=my1 Then Goto skipdraw

									Gosub drawcursor	
						
									Gosub clearinfo
									Gosub printlocs
									VWait
						
							.skipdraw	
									mx1=mx
									my1=my
									If MouseDown (1) Then Gosub actions
									If MouseDown (2) Then Gosub guides
									
									If MouseDown (1) Then Gosub waitasec
									If MouseDown (2) Then Gosub waitasec
						
									inkey=GetKey()		
									If inkey=0 Then Goto cursorloop
						
									If inkey=104 Then Goto dispmenu  	; H
									If inkey=8 	 Then Goto undofunc	 	; DEL
									If inkey=120 Then Goto flushwarn 	; X
									If inkey=103 Then Goto generatecode ; G
									If inkey=108 Then Goto togglelogic 	; L
									If inkey=32  Then Goto relocateall	; SPACE
									If inkey=115 Then Goto saveproject	; S
									If inkey=111 Then Goto openproject	; O
									If inkey<>27 Then Goto cursorloop  	; ESC
						
									;--------------- esc was pressed --------------------
						
									If saved=1 Then Goto leaveit
										.waitexit		
												menux=(xwidth/2)-(333/2)
												menuy=300
												mentexl=menux+25
												lead=30
												texline=menuy+lead
												DrawImage exitwarn,menux,menuy
																											
										.waitconfirm
												VWait 
												inkey=GetKey()
												If inkey=0 Then Goto waitconfirm

												mx1=2
												If inkey=104 Then Goto dispmenu			
												If inkey<>121 Then Goto mainloop		

		;----------------------------------------------------------------
		
							.leaveit
									FreeFont fntArial
									FreeFont fntArial2

		End
		

		;---------------------------------------------------------------------------------------------
		;---------------------------------------------------------------------------------------------
		;---------------------------------------------------------------------------------------------
		;subroutines
		;---------------------------------------------------------------------------------------------
		
		.waitasec
			Gosub refreshtiles
		.waitasecloop
			If MouseDown (1) Then Goto waitasecloop
			If MouseDown (2) Then Goto waitasecloop
			Return

		;---------------------------------------------------		

		.drawcursor			;draw mousepointer
			xreal=(mx1/divider)
			yreal=(my1/divider)
			pointr=(yreal*40)+xreal
			If matrix(pointr)=0 Then DrawImage emptytile,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=1 Then DrawImage tile1,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=2 Then DrawImage tile2,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=3 Then DrawImage tile3,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=4 Then DrawImage tile4,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=5 Then DrawImage tile5,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=6 Then DrawImage tile6,((mx1/divider)*divider),((my1/divider)*divider)
			If matrix(pointr)=7 Then DrawImage tileh,((mx1/divider)*divider),((my1/divider)*divider)
		
		
			DrawImage cursor,((mx/divider)*divider),((my/divider)*divider)
		
			Return
		
		;----------------------------------------------------------
		; display help menu
		;----------------------------------------------------------
		
		.dispmenu		
			menux=(xwidth/2)-(700/2)
			menuy=50
			mentexl=menux+25
			lead=30
			texline=menuy+lead
			
			
			DrawImage menuplate,menux,menuy
		
		
		.menukey
			inkey=GetKey()
			If inkey=0 Then Goto menukey
			
			If inkey=103 Then Goto generatecode
			If inkey=104 Then Goto closemenu  	; H
			If inkey=120 Then Goto flushwarn 	; X
			If inkey=103 Then Goto generatecode ; G
			;If inkey=32  Then Goto relocateallhelp	; SPACE
			If inkey=108 Then Goto togglelogic 	; L
		.closemenu
			mx1=2
			Goto mainloop
		
		
		;----------------------------------------------------------
		; clear status area and paint status interface
		;----------------------------------------------------------
		
		.clearinfo
			DrawImage info,0,ywidth
			Return
			

		;--------------------------------------------------------------------
		; re-interpret the current matrix and draw respective tiles to screen
		;--------------------------------------------------------------------	
			
		.refreshtiles
			For x = 0 To xwidth-divider Step divider
			For y = 0 To ywidth-divider Step divider
			xreal=(x/divider)
			yreal=(y/divider)
			pointr=(yreal*40)+xreal
			If matrix(pointr)=0 Then DrawImage emptytile,x,y
			If matrix(pointr)=1 Then DrawImage tile1,x,y
			If matrix(pointr)=2 Then DrawImage tile2,x,y
			If matrix(pointr)=3 Then DrawImage tile3,x,y
			If matrix(pointr)=4 Then DrawImage tile4,x,y
			If matrix(pointr)=5 Then DrawImage tile5,x,y
			If matrix(pointr)=6 Then DrawImage tile6,x,y
			If matrix(pointr)=7 Then DrawImage tileh,x,y
			Next 
			Next 
			Return
			
		;---------------------------------------------------------
		; switch editing logic
		;---------------------------------------------------------
		
		.togglelogic
			If logic=1 Then l = 0
			If logic=0 Then l = 1
			logic = l
			mx1=2
			Goto mainloop

		;------------------------------------------------
		; output main editor status information
		;------------------------------------------------
		
		.printlocs
		;output coords
			Locate 25,statusy+13
			Print "x "+(mx/divider)
			Locate 70,statusy+13 
			Print"y "+(my/divider)
				
		;display editing logic mode
			Locate 193,statusy+13
			If logic=0 Then Print "Placemet-Rule: STRICT"
			If logic=1 Then Print "Placemet-Rule: FLEXIBLE"
			
			Locate 398,statusy+13
			Print "PRESS H FOR HELP"
						
		;print current adress
			xreal=(mx/divider)
			yreal=(my/divider)
			hexout$=Str$(Hex$(baseadress))
			Locate 25,statusy+31
			Print "Base Address: $"+Right$(hexout$,4)
			
			curradr=baseadress+xreal+(yreal*40)
			hexout$=Str$(Hex$(curradr))
			Locate 25,statusy+48
			Print "Current: $"+Right$(hexout$,4)

			
		;output last selected adress
			Locate 398,statusy+49
			hexout$=Str$(Hex$(adr))		
			If firstinit = 1 Then Print "Last Adress: $"+Right$(hexout$,4)	
			
		;output instruction count
 		    Locate 558,statusy+42		
			Print "Instructions: "+(commandnr/3)+" (of max. "+maximumcommands+")  Code size: "+commandnr+" bytes."
			
		;output approx cycles
			Locate 558,statusy+54
			Print "approx. Cycles: "+((commandnr/3)*4+3)+" / Rasterlines: "+(1+((commandnr/3)*4)/63)+" (rough PAL estimate)"

		;output current pickup state
			Locate 193,statusy+49	
			If laststate = 0 Then Print "PICK READ SOURCE (LDA)";   0"
			If laststate = 1 Then Print "SET WRITE TARGET (STA)";   1"
			If laststate = 2 Then Print "PICK READ SOURCE (LDA)";   2"
			If laststate = 3 Then Print "PICK READ SOURCE (LDA)";   3"
			If laststate = 4 Then Print "SET WRITE TARGET (STA)";   4"
			If laststate = 5 Then Print "PICK READ SOURCE (LDA)";   5"
			If laststate = 6 Then Print "SET WRITE TARGET (STA)";   6"
			Return
			
		;------------------------------------------------
		; set or remove a guide tile
		;------------------------------------------------
		.guides
			xreal=(mx/divider)
			yreal=(my/divider)
			pointr=(yreal*40)+xreal

			If matrix(pointr)=0 Then mmm=7
			If matrix(pointr)=7 Then mmm=0
			
			If matrix(pointr)>0 And matrix(pointr)<7 Then Return
			
			matrix(pointr)=mmm
			
			mx1=2
			
			Gosub refreshtiles
			Gosub drawcursor	
			
			Return
			

		;-----------------------------------------------------------------------------------------------
		; set command or update command at current mouse pos, regarding selected mode of editing logic
		; this is the main feature routine.
		;-----------------------------------------------------------------------------------------------

		.actions
			If commandnr > (maxxcomm-6) Then Goto besorry2
		
			If logic = 0 Then Goto actionsoptimal
			If logic = 1 Then Goto actionsfree

		.actionsoptimal
			
			xreal=(mx/divider)
			yreal=(my/divider)
			
			If xbefore=xreal And ybefore=yreal Then Goto finishactions			
			
			pointr=(yreal*40)+xreal
			mm=matrix(pointr)
			
			If mm=7 Then mm=0
			
			If laststate = 0 And mm=0 Then Goto setsource ;standard read from empty cell
			If laststate = 1 And mm=0 Then Goto settarget ;standard write to empty cell
			If laststate = 2 And mm=0 Then Goto setsource ;standard read from empty cell after good read and write
			If laststate = 3 And mm=0 Then Goto setsource
			
			If laststate = 1 And mm=1 Then Goto settarget			
			Goto finishactions
			
		.actionsfree
			xreal=(mx/divider)
			yreal=(my/divider)
			pointr=(yreal*40)+xreal
			
			If laststate = 0 Then Goto setsource ;standard read from empty cell
			If laststate = 1 Then Goto settarget ;standard write to empty cell
			If laststate = 2 Then Goto setsource ;standard read from empty cell after good read and write
			If laststate = 3 Then Goto setsource
			If laststate = 4 Then Goto settarget
			If laststate = 5 Then Goto setsource
			If laststate = 6 Then Goto settarget

		.finishactions
			xreal=(mx/divider)
			yreal=(my/divider)
			pointr=(yreal*40)+xreal
			adr=baseadress+xreal+(yreal*40)
			mx1=2
			Gosub buildmatrix
			Gosub refreshtiles
			Gosub drawcursor
			Return

		;--------------------------------------------
		; matrix values
		; 0 = empty
		; 1 = read on empty field	    -> optimal
		; 2 = write on empty field		-> optimal 
		; 3 = write ontop read field 	-> optimal 
		; 4 = read ontop write field 	-> suboptimal
		; 5 = write ontop write field   -> suboptimal
	    ; 6 = read ontop read field     -> suboptimal
		;--------------------------------------------

		.setsource
	     	firstinit=1		
			commands(commandnr)=100
			commands(commandnr+1)=xreal
			commands(commandnr+2)=yreal
			commandnr=commandnr+3
			If matrix(pointr)=7 Then matrix(pointr)=0
			If matrix(pointr)=0 Then l=1
			If matrix(pointr)=1 Then l=6
			If matrix(pointr)=2 Then l=4
			If matrix(pointr)=3 Then l=4
			If matrix(pointr)=4 Then l=6
			If matrix(pointr)=5 Then l=4
			If matrix(pointr)=6 Then l=6
			laststate=l
			xbefore=xreal
			ybefore=yreal
			Goto finishactions
			
		.settarget
			firstinit=1
			commands(commandnr)=200
			commands(commandnr+1)=xreal
			commands(commandnr+2)=yreal
			commandnr=commandnr+3
			If matrix(pointr)=7 Then matrix(pointr)=0
			If matrix(pointr)=0 Then l=2
			If matrix(pointr)=1 Then l=3
			If matrix(pointr)=2 Then l=5
			If matrix(pointr)=3 Then l=5
			If matrix(pointr)=4 Then l=3
			If matrix(pointr)=5 Then l=5
			If matrix(pointr)=6 Then l=3
			laststate=l
			xbefore=xreal
			ybefore=yreal
			Goto finishactions


		;-----------------------------------------------------------------------------------------
		; undo function
		;-----------------------------------------------------------------------------------------

		.undofunc
			If commandnr=0 Then Goto endundo
			
			;step pointer backwards
			commandnr=commandnr-3
		
			;delete cellinfo
			commands(commandnr)=255		
			commands(commandnr+1)=0
			commands(commandnr+2)=0		
			
			Gosub buildmatrix
			Gosub refreshtiles
			Gosub clearinfo 
			Gosub printlocs
			
			If commandnr=0 Then Goto endundo
						
			la = commandnr-3			
			lx=commands(la+1)
			ly=commands(la+2)
			lpointr=(ly*40)+lx			
			c=0
			blink=matrix(lpointr)
			If blink = 7 Then blink = 0
						
	    .blinkabit
			VWait
			DrawImage emptytile,lx*divider,ly*divider
			VWait
			Delay 20
			If blink=1 Then DrawImage tile1,lx*divider,ly*divider
			If blink=2 Then DrawImage tile2,lx*divider,ly*divider
			If blink=3 Then DrawImage tile3,lx*divider,ly*divider
			If blink=4 Then DrawImage tile4,lx*divider,ly*divider
			If blink=5 Then DrawImage tile5,lx*divider,ly*divider
			If blink=6 Then DrawImage tile6,lx*divider,ly*divider

			VWait
			Delay 10			
			c=c+1
			If c<20 Then Goto blinkabit

		.endundo
			mx1=2
			Goto mainloop
			

		;--------------------------------------------------
		
		.buildmatrix
			Gosub flushmatrix
			
		.buildcommandnr
			pcounter=0
			a=0
		.recalccommands
			f=commands(a)
			If f=255 Then Goto setcommandnr ;if stopbyte reached, number of commands is complete
			a=a+3
			If a<maxxcomm-3 Then Goto recalccommands
						
		.setcommandnr
			commandnr=a	 ;handover number of commands
			If commandnr=0 Then Goto buildout_nocommands
			
		.buildmatrixloop
			f=commands(pcounter)
			xreal=commands(pcounter+1)
			yreal=commands(pcounter+2)
			pointr=(yreal*40)+xreal
			
			m=0
			
			If f = 255 And matrix(pointr)<7 Then m=0
			
			If f = 100 And matrix(pointr)=6 Then m=6 
			If f = 100 And matrix(pointr)=5 Then m=4 
			If f = 100 And matrix(pointr)=4 Then m=6 
			If f = 100 And matrix(pointr)=3 Then m=4
			If f = 100 And matrix(pointr)=2 Then m=4
			If f = 100 And matrix(pointr)=1 Then m=6
			If f = 100 And matrix(pointr)=0 Then m=1
			
			If f = 200 And matrix(pointr)=6 Then m=3
			If f = 200 And matrix(pointr)=5 Then m=5
			If f = 200 And matrix(pointr)=4 Then m=3
			If f = 200 And matrix(pointr)=3 Then m=5
			If f = 200 And matrix(pointr)=2 Then m=5
			If f = 200 And matrix(pointr)=1 Then m=3
			If f = 200 And matrix(pointr)=0 Then m=2

			matrix(pointr)=m
		
			pcounter=pcounter+3
			If pcounter<commandnr Then Goto buildmatrixloop

		.buildout			
			laststate=m
			Return
			
		.buildout_nocommands
			xreal=0
			yreal=0
			pointr=0
			laststate=0
			Return

		
		;--------------------------------------------------
		
		.flushwarn
			menux=(xwidth/2)-(333/2)
			menuy=300
			mentexl=menux+25
			lead=30
			texline=menuy+lead
			DrawImage flushwarn,menux,menuy

		.waitconfirm2
			VWait 
			inkey=GetKey()
			If inkey=0 Then Goto waitconfirm2
									
			mx1=2
									
			If inkey=104 Then Goto dispmenu			
			If inkey<>121 Then Goto mainloop
			
		.softreboot	
			
			DrawImage backgrnd,0,0
			Delay 500 

			Gosub formatarray
			Gosub flushmatrix
			Goto mainloop

		;---------------------------------------------------------
		; fill the command-coordinate array with stopbyte pattern	
		;---------------------------------------------------------	
			
		.formatarray
			For e = 0 To maxxcomm-3 Step 3
			commands(e)="255" ; emptyvalue
			commands(e+1)="0" ; empty
			commands(e+2)="0" ; empty
			Next
		
			laststate = 0
			commandnr = 0
			saved = 0
			firstinit = 0
			mx1=2		
			Return
			
		; flush matrix, but leave guides intact	
			
		.flushmatrix
			For e = 0 To 1000
			If matrix(e)<7 Then matrix(e)=0
			Next 
			Return
			
		;--------------------------------------------------
		; display warning screens
		;--------------------------------------------------		
		
		.besorry
			DrawImage sorrimg,xwidth/2-433/2,310
		.waitsorry
			VWait 
			inkey=GetKey()
			If inkey=0 Then Goto waitsorry
			mx1=2
			Goto mainloop
			
		;--------------------------------------------------	

		.waitsorry_g
			VWait 
			inkey=GetKey()
			If inkey=0 Then Goto waitsorry_g
			mx1=2
			Goto generatecode


		;--------------------------------------------------		

		.besorry2
			DrawImage sorrimg2,xwidth/2-433/2,310
			Goto waitsorry

		;--------------------------------------------------		

		.besorry3
			DrawImage sorrimg3,xwidth/2-433/2,310
			Goto waitsorry_g

		;--------------------------------------------------
			
		.besorry4
			DrawImage sorrimg4,xwidth/2-433/2,310
			Goto waitsorry_g

		;--------------------------------------------------
			
		.besorry5
			DrawImage sorrimg5,xwidth/2-433/2,310
			Goto waitsorry

		;--------------------------------------------------
			
		.besorry6
			DrawImage sorrimg6,xwidth/2-433/2,310
			Goto waitsorry

		;--------------------------------------------------
			
		.besorry7
			DrawImage sorrimg7,xwidth/2-433/2,310
			Goto waitsorry


		;--------------------------------------------------		
		; generate, test and export sourcecode/binary file
		;--------------------------------------------------
		
		.generatecode
		
			If laststate=1 Then Goto besorry
			If laststate=4 Then Goto besorry
			If laststate=6 Then Goto besorry		
		
			Gosub refreshtiles				
			Gosub clearinfo
			Gosub printlocs
			
			Gosub setinfusion

		.redrawgenerate		
			menux=(xwidth/2)-(500/2)
			menuy=100
			mentexl=menux+95
			lead=30
			texline=menuy+lead+288-57

			DrawImage exportplate,menux,menuy
			SetFont fntArial2

			Locate mentexl,texline+22
			Print datatype$

			Locate mentexl,texline+45
			Print status$

			hexout$=Str$(Hex$(baseadress))
			Locate mentexl+200,texline+22
			Print "$"+Right$(hexout$,4)
			
			Locate mentexl+350,texline+22
			Print in_fusion$

			SetFont fntArial
		.waitconfirm3
			VWait 
			inkey=GetKey()
			If inkey=0 Then Goto waitconfirm3
									
			mx1=2
									
			If inkey=104 Then Goto dispmenu			
			If inkey=116 Then Goto trajtest
			If inkey=100 Then Goto toggledmode
			If inkey=98 Then Goto setbaseaddr
			If inkey=97 Then Goto toggleinfusion
			If inkey=119 Then Goto exportfile
			If inkey<>121 Then Goto mainloop
		
			status$="IDLE."
		
		Goto mainloop

		;-------------------------------------------------
		; toggle infusion of AND #$0f 		
		;-------------------------------------------------
		
		.toggleinfusion
			If infusion=0 Then iif=1
			If infusion=1 Then iif=0
			infusion=iif
			
			Gosub setinfusion
			Goto redrawgenerate
			
		.setinfusion
			If infusion = 0 Then in_fusion$="OFF"
			If infusion = 1 Then in_fusion$="ON"
			Return
		

		;-------------------------------------------------
		; toggle export format 		
		;-------------------------------------------------
		
		.toggledmode
			If datatype$="DATA" Then dt=0
			If datatype$="BINARY" Then dt=1
			
			dt=dt+1
			If dt>1 Then dt =0
			
			If dt=0 Then datatype$="DATA"
			If dt=0 suffix$=".asm"
			If dt=1 Then datatype$="BINARY" 
			If dt=1 suffix$=".bin"		
			
			Goto redrawgenerate
			
		


		;------------------------------------------------------
		; visual test of what was edited in the project
		;------------------------------------------------------
		
		.trajtest
			If commandnr=0 Then Goto besorry3 ; nothing to test

			Gosub clearinfo
			Gosub printlocs
			DrawImage testscreen,0,0
			adelay=10

		.looptestreset
			e=0
			
		.looptest			
			
			f=commands(e)
			xreal=commands(e+1)
			yreal=commands(e+2)
			
			f2=commands(e+3)
			xreal2=commands(e+4)
			yreal2=commands(e+5)
			
			xreal=xreal*divider
			yreal=yreal*divider		
			xreal2=xreal2*divider
			yreal2=yreal2*divider
			
			If adelay>100 Then adelay=0
			If adelay<0 Then adelay=100
			Delay adelay
			inkey=GetKey()
			If inkey=0 Then Goto continuetest
			If inkey=43 Then adelay=adelay+1 ; + speed-
			If inkey=45 Then adelay=adelay-1 ; - speed+								
			If inkey=114 Then Gosub redrawpic ; R - redraw 			
			If inkey=27 Then Goto leavetestsite ; ESC - end test

		.continuetest
			DrawImage adjdelay,xwidth/2-433/2,statusy
			Locate xwidth/2-433/2+315,statusy+14
			SetFont fntArial2
			Print adelay
			SetFont fntArial
		
		
			If f=255 Then Goto looptestreset ; from the beginning
			If f=100 And f2=200 Then Goto testmovecell

		.errrror
			DrawImage errror,100,100
		.errrorwait
			inkey=GetKey()
			If inkey=0 Then Goto errrorwait
			Goto generatecode

		.testmovecell			
			CopyRect xreal,yreal,divider,divider,xreal2,yreal2		
			e=e+6
			Goto looptest
			
		.leavetestsite
			mx1=2
			Gosub refreshtiles	
			Gosub clearinfo
			Gosub printlocs
			VWait
			Goto generatecode


		.redrawpic
			;redraw image of the test-area
			DrawImage testscreen,0,0
			Return


		;--------------------------------------------------------
		; input base adress
		;--------------------------------------------------------

		.setbaseaddr
			hexout$=Str$(Hex$(baseadress))
			hexout$=Right$(hexout$,4)
			basestringsize=4
			Dim allowtab2(300)
			Restore allowance2		
			mx1=2
			sf=0
			
		.allowloop2
			Read a
			If a = 255 Then Goto endallowloop2
			allowtab2(sf)=a
			sf=sf+1
			Goto allowloop2
			
		.endallowloop2			
			menux=(xwidth/2)-(253/2)
			menuy=220
			mentexl=menux+20
			lead=30
			texline=menuy+lead

		.prebasinloop

			VWait		
			DrawImage baseadin,menux,menuy
			Locate mentexl+20,texline+76
			
			SetFont fntArial2
			Print Left$(hexout$,basestringsize)
			SetFont fntArial

		.basinloop
			inkey=GetKey()
			If inkey=0 Then Goto basinloop
			
			If inkey=8 Then Goto basedelchar
			If allowtab2(inkey)=1 Then Goto baseaddchar	
			If inkey=13 And basestringsize=4 Then Goto confirmbasin
			If inkey=27 Then Goto generatecode										
			Goto basinloop

		.basedelchar
			If basestringsize>0 Then basestringsize=basestringsize-1
			hexout$=Left$(hexout$,basestringsize)
			Goto prebasinloop
		
		.baseaddchar
			If basestringsize>3 Then Goto prebasinloop
			basestringsize=basestringsize+1
			hexout$=Left$(hexout$,basestringsize)
			hexout$=hexout$+Chr$(inkey)
			Goto prebasinloop
			
		.confirmbasin
			hexin$=hexout$
			baseadress=hex2dec(hexin$)
			Goto generatecode


			.allowance2 ;input char allowance table
			    ;0 1 2 3 4 5 6 7 8 9
			Data 0,0,0,0,0,0,0,0,0,0 ;0
			Data 0,0,0,0,0,0,0,0,0,0 ;10
			Data 0,0,0,0,0,0,0,0,0,0 ;20
			Data 0,0,0,0,0,0,0,0,0,0 ;30
			Data 0,0,0,0,0,0,0,0,1,1 ;40
			Data 1,1,1,1,1,1,1,1,0,0 ;50
			Data 0,0,0,0,0,0,0,0,0,0 ;60
			Data 0,0,0,0,0,0,0,0,0,0 ;70
			Data 0,0,0,0,0,0,0,0,0,0 ;80
			Data 0,0,0,0,0,0,0,1,1,1 ;90	
			Data 1,1,1,0,0,0,0,0,0,0 ;100
			Data 0,0,0,0,0,0,0,0,0,0 ;110
			Data 0,0,0,0,0,0,0,0,0,0 ;120
			Data 0,0,0,0,0,0,0,0,0,0 ;130
			Data 0,0,0,0,0,0,0,0,0,0 ;140
			Data 0,0,0,0,0,0,0,0,0,0 ;150
			Data 0,0,0,0,0,0,0,0,0,0 ;160
			Data 0,0,0,0,0,0,0,0,0,0 ;170
			Data 0,0,0,0,0,0,0,0,0,0 ;180
			Data 0,0,0,0,0,0,0,0,0,0 ;190
			Data 0,0,0,0,0,0,0,0,0,0 ;200
			Data 255

			Function hex2dec(hexin$)
			Local c, dec, hexval$ = "0123456789ABCDEF"
				For c=1 To Len(hexin$)
					dec = (dec Shl 4) Or (Instr(hexval$, Upper$(Mid$(hexin$, c, 1))) - 1)
				Next
			Return dec
			End Function
			

		;----------------------------------------------------------------
		; write source code file to disk
		;----------------------------------------------------------------


		.exportfile
				If commandnr=0 Then Goto besorry4
				
				If dt=1 Then Goto exportbin

				codeline$="byte "
				
				fullfilename$ = getsavefile$("Export Speedcode file","new_project.asm","Cross Assembler File (*.asm)" + Chr$(0) + "*.traj" + Chr$(0));; flags optional
				If fullfilename$ ="" Then Goto generatecode
		
				fileout = WriteFile (fullfilename$)
				a=0				
				linelen=0
		
		.fcreatecode		
				If commands(a)=255 Then Goto finishfile
				If commands(a)=200 Then fcommand=141 ;($8D, STA)
				If commands(a)=100 Then fcommand=173 ;($AD, LDA)

				fcom$=Str$(Hex$(fcommand))
				fcom$=Right$(fcom$,2)

				faddr=commands(a+2)*40+commands(a+1)
				faddr=faddr+baseadress

				hexout$=Str$(Hex$(faddr))
				hexout$=Right$(hexout$,4)

				fhibyte$=Left$(hexout$,2)
				flobyte$=Right$(hexout$,2)

				codeline$=codeline$+"$"+fcom$+",$"+flobyte$+",$"+fhibyte$
				fin=0
				
				If fcommand=173 Then Gosub check_infusion
								
				linelen=linelen+1
				If linelen>12 Then Gosub foutputline				
				If fin=0 Then codeline$=codeline$+","

				a = a + 3
				Goto fcreatecode

		.finishfile
				codeline$=codeline$+"$60 ;Generated with TRAJED by WERTSTAHL"
				Gosub foutputline
				status$="SUCCESS."
				CloseFile(fileout)
				Goto generatecode

		.foutputline
				codeline$=codeline$+Chr$(13)+Chr$(10)
				fa=Len(codeline$)
				For fn = 1 To fa
				fz=Asc(Mid$(codeline$,fn,1))
				WriteByte (fileout,fz)
				Next
				codeline$="byte "
				fin=1
				linelen=0
				Return
				
				
		.check_infusion
				If infusion = 0 Then Return
				codeline$=codeline$+",$29,$0f"
				Return


		;----------------------------------------------------------------
		; write binary file to disk
		;----------------------------------------------------------------


		.exportbin

				fullfilename$ = getsavefile$("Export Speedcode file","new_project.bin","Binary file (*.bin)" + Chr$(0) + "*.bin" + Chr$(0));; flags optional
		
				fileout = WriteFile (fullfilename$)

				pos=0
				
		.exportbinloop
				
				If commands(pos)=255 Then Goto appendrts
				If commands(pos)=200 Then WriteByte (fileout,141)
				If commands(pos)=100 Then WriteByte (fileout,173)
				
				faddr=commands(pos+2)*40+commands(pos+1)
				faddr=faddr+baseadress
				
				hexout$=Str$(Hex$(faddr))
				hexout$=Right$(hexout$,4)	

				fhibyte$=Left$(hexout$,2)
				flobyte$=Right$(hexout$,2)

				hibyte=hex2dec(fhibyte$)
				lobyte=hex2dec(flobyte$)
				
				WriteByte (fileout,lobyte)
				WriteByte (fileout,hibyte)
				
				If commands(pos)=100 Then Gosub appendinfusion
				
				pos=pos+3
				
				Goto exportbinloop
				
		.appendrts
				WriteByte (fileout,96) ;rts
				
				status$="SUCCESS."
				CloseFile(fileout)
				Goto generatecode

		.appendinfusion
				If infusion = 0 Then Return
				WriteByte (fileout,41) ; and
				WriteByte (fileout,15) ; #0f
				Return


		
		;----------------------------------------------------------------
		; visual manual relocation of editing area contents
		;----------------------------------------------------------------
		
		.relocateallhelp
				Gosub refreshtiles		
		
		
		.relocateall	
				xoffset=0
				yoffset=0
				ixflip=0
				iyflip=0
				
				Dim guidematrix(1000)
				For e = 0 To 1000
				If matrix(e)=7 Then guidematrix(e)=7
				If matrix(e)=7 Then matrix(e)=0			
				Next
				
				For e = 0 To maxxcomm
				reloccommands(e)=commands(e)
				Next
				
				DrawImage relocit,xwidth/2-433/2,statusy

		.waitreloc
				inkey=GetKey()
				If inkey=0 Then Goto waitreloc
				
				If inkey=28 Then Goto yminus
				If inkey=30 Then Goto xplus
				If inkey=29 Then Goto yplus
				If inkey=31 Then Goto xminus

				If inkey=120 Then Goto xflip
				If inkey=121 Then Goto yflip
				
				If inkey=13 Then Goto acceptreloc
				If inkey=27 Then Goto restorebackup
				Goto waitreloc


		.yminus
				yoffset=yoffset-1
				If yoffset<-24 Then yoffset= 0
				Goto calcreloc
				
		.xminus 
				xoffset=xoffset-1
				If xoffset<-39 Then xoffset= 0
				Goto calcreloc
				
		.xplus 
				xoffset=xoffset+1
				If xoffset>39 Then xoffset= 0
				Goto calcreloc

		.yplus
				yoffset=yoffset+1
				If yoffset>24 Then yoffset= 0
				Goto calcreloc
				
		.calcreloc
				frel=0
		.calcrelocloop
				If commands(frel)=255 Then Goto endcalcreloc
				relcomx=reloccommands(frel+1)
				relcomy=reloccommands(frel+2)
				
				If ixflip=0 Then relcomx=relcomx+xoffset
				If ixflip=1 Then relcomx=(40-relcomx)+xoffset
				If iyflip=0 Then relcomy=relcomy+yoffset
				If iyflip=1 Then relcomy=(25-relcomy)+yoffset
				
				brelcomx=relcomx
				brelcomy=relcomy

				If relcomx<0  Then brelcomx=relcomx+40
				If relcomx>39 Then brelcomx=relcomx-40
				If relcomy<0  Then brelcomy=relcomy+25
				If relcomy>24 Then brelcomy=relcomy-25

				commands(frel+1)=brelcomx
				commands(frel+2)=brelcomy
				
				frel=frel+3
				Goto calcrelocloop
				
		.endcalcreloc

				Gosub buildmatrix
				Gosub re_addguides
				Gosub buildmatrix
				Gosub refreshtiles
				Locate 0,0
				Print "xoff:"+xoffset+" yoff:"+yoffset
				
				Goto waitreloc		
				
		.restorebackup
				For e = 0 To maxxcomm
				commands(e)=reloccommands(e)
				Next
				
				For e = 0 To 1000
				If matrix(e) = 7 Then matrix(e)=0
				If guidematrix(e) = 7 Then matrix(e)=7
				Next
				
				
				Gosub buildmatrix
				Gosub refreshtiles
				mx1=2
				Goto mainloop
				
		.acceptreloc
				Gosub buildmatrix
				Gosub re_addguides
				Gosub refreshtiles
				mx1=2
				Goto mainloop

		.xflip
				ixflip=ixflip+1
				If ixflip=2 Then ixflip=0
				Goto calcreloc
		
		.yflip
				iyflip=iyflip+1
				If iyflip=2 Then iyflip=0

				Goto calcreloc
				
				
				
							;re-locate guides
				
							.re_addguides
								For e=0 To 1000
								If matrix(e)=7 Then matrix(e)=0
								Next
							
							
								For guidex = 0 To 39
								For guidey = 0 To 24
								repoint=(guidey*40)+guidex
								If guidematrix(repoint)=7 Then Gosub replaceguide
								Next
								Next
								Return

								;calculate offset+flipped location for guides
								.replaceguide								
									If ixflip=0 Then relcomx=guidex+xoffset
									If ixflip=1 Then relcomx=(40-guidex)+xoffset
									If iyflip=0 Then relcomy=guidey+yoffset
									If iyflip=1 Then relcomy=(25-guidey)+yoffset
									brelcomx=relcomx
									brelcomy=relcomy
									If relcomx<0  Then brelcomx=relcomx+40
									If relcomx>39 Then brelcomx=relcomx-40
									If relcomy<0  Then brelcomy=relcomy+25
									If relcomy>24 Then brelcomy=relcomy-25
									resultrepoint=(brelcomy*40)+brelcomx
									If matrix(resultrepoint)=0 Then matrix(resultrepoint)=7
									Return






		;----------------------------------------------------------------
		; save project
		;----------------------------------------------------------------

				.saveproject				
				projectfilename$ = getsavefile$("Save TRAJed-Project","new_project.traj","Trajed Project File (*.traj)" + Chr$(0) + "*.traj" + Chr$(0) + "Text" + Chr$(0) + "*.txt" + Chr$(0));; flags optional
				
				If projectfilename$ = "" Then Goto besorry6
				fileout = WriteFile (projectfilename$)


								fz=Asc("T")
								WriteByte (fileout,fz)
								fz=Asc("R")
								WriteByte (fileout,fz)
								fz=Asc("A")
								WriteByte (fileout,fz)
								fz=Asc("J")
								WriteByte (fileout,fz)
								fz=Asc("1")
								WriteByte (fileout,fz)	
						
								pos=0
						.swrite		
								fz=matrix(pos)
								WriteByte (fileout,fz)
								pos=pos+1
								If pos<1000 Then Goto swrite	
								
								fz=255
								WriteByte (fileout,fz)
								fz=255
								WriteByte (fileout,fz)								

								pos=0
						.savedataloop
								fz=commands(pos)
								WriteByte (fileout,fz)
								If fz=255 Then Goto closesaveproj
								pos=pos+1
								Goto savedataloop
								
						.closesaveproj		
								CloseFile(fileout)
								saved=1
								mx1=2
								Goto mainloop

		;----------------------------------------------------------------
		; open project
		;----------------------------------------------------------------

						.openproject	
				
								projectfilename$=""			
								projectfilename$ = getopenfile$("File open / Datei öffnen","%UserProfile%\Desktop\","Trajed Project Files (*.traj)" + Chr$(0) + "*.traj" + Chr$(0)); flags optional
								If projectfilename$ = "" Then Goto besorry7

								Dim matrix(1000)
								Dim guidematrix(1000)

								filein = ReadFile(projectfilename$)


								fz = ReadByte ( filein )
								If Chr$(fz) <> "T" Then Goto projectopenerror
								fz = ReadByte ( filein )
								If Chr$(fz) <> "R" Then Goto projectopenerror
								fz = ReadByte ( filein )
								If Chr$(fz) <> "A" Then Goto projectopenerror
							    fz = ReadByte ( filein )
								If Chr$(fz) <> "J" Then Goto projectopenerror
								fz = ReadByte ( filein )
								If Chr$(fz) <> "1" Then Goto projectopenerror
								
								
								pos=0 
						.oread
								fz= ReadByte ( filein )
								matrix(pos)=fz
								pos = pos + 1 
								If pos < 1000 Then Goto oread

								If ReadByte ( filein ) <> 255 Then Goto projectopenerror
								If ReadByte ( filein ) <> 255 Then Goto projectopenerror
							
								Dim commands(maxxcomm)
								Gosub formatarray
								Dim reloccommands(maxxcomm)				

								pos=0
								
						.opendataloop
								fz = ReadByte( filein )
								commands(pos)=fz
								pos=pos+1
								If fz <> 255 Then Goto opendataloop

								CloseFile( filein )
																
								saved=0
								mx1=2
								firstinit=0
								laststate=0

								Gosub buildmatrix
								Gosub refreshtiles
								Gosub clearinfo
					
								Goto mainloop
								
								
							.projectopenerror
								DrawImage er2rror,100,100
							.er2rrorwait
								inkey=GetKey()
								If inkey=0 Then Goto er2rrorwait
								Goto softreboot



	;--------------------------------------------------------------------------------------------
	;--------------------------------------------------------------------------------------------
	; end of actual trajed sourcecode.
	;
	; third-pary functions below
	;
	;--------------------------------------------------------------------------------------------
	;--------------------------------------------------------------------------------------------


			
			;----------------------------------------------------------------
			; ID: 916
			; Author: MPZ
			; Date: 2004-02-05 03:08:51
			; Title: Blitz3D Filerequester (2.Update)
			; Description: Use a Filerequester in Blitz 3D
			; This Procedure is for free MPZ (@) from Berlin
			; Version 0.2 1/2004
			; 
			; in the USERLIBS must be the file kernel32.decls
			; .lib "kernel32.dll"
			; api_RtlMoveMemory(Destination*,Source,Length) : "RtlMoveMemory"
			;			
			; in the USERLIBS must be the file comdlg32.decls
			; .lib "comdlg32.dll"
			; api_GetOpenFileName% (pOpenfilename*) : "GetOpenFileNameA"
			; api_GetSaveFileName% (pOpenfilename*) : "GetSaveFileNameA"
			
			Global hWnd=SystemProperty("AppHWND")
			
			; GetOpen/saveFileName consts Flags (useful ones only!)...
			Const OFN_CREATEPROMPT         = $2000    ; Prompts the user as to whether they want to create a file that doesnt exist.
			Const OFN_FILEMUSTEXIST        = $1000    ; File must exist for it to be returned.
			Const OFN_HIDEREADONLY         = 4        ; Hides the read only button in the dialog...
			Const OFN_NOCHANGEDIR          = 8        ; Stops the user from changing the initial directory.
			Const OFN_NONETWORKBUTTON      = $20000   ; Hides and disables the network button.
			Const OFN_NOREADONLYRETURN     = $8000    ; Stops the requester returning readonly files..
			Const OFN_NOVALIDATE           = 256      ; If selected, no check will be done for invalid characters.
			Const OFN_OVERWRITEPROMPT      = 2        ; Prompt for overwrite file...
			Const OFN_PATHMUSTEXIST        = $800     ; Specifies that the path MUST exist for it to be able to be selected.
			Const OFN_READONLY             = 1        ; Makes the read only checkbox in the dialog box to be checked immediately.
			
			; getopenfile $(Title_of_Requester$, SearchPath$,Files_with_ending$, Flags); 	
			; getsavefile $(Title_of_Requester$, Save_File_name$,Files_with_ending$, Flags); 	
			;
			; Title_of_Requester$= "Name of the Requester / Name des Dateifragefensters
			; SearchPath$ = "C:\" ; Path for File searching / Pfad wo nach der Datei gesuchet werden soll 
			; Files_with_ending$ = "All Files (*.*)" + Chr$(0) + "*.*" + Chr$(0)
			;					 = "Blitzbasic" + Chr$(0) + "*.bb" + Chr$(0) + "Text" + Chr$(0) + "*.txt" + Chr$(0)
			; Flags = See Flag lists
			; Save_File_name$ = "C:\test.bb" ; Name of the Savefile with Path / Name der Datei mit Pfad zum speichern
			
			
			;example code:
			
			;Print getopenfile$("File open / Datei öffnen","%UserProfile%\Desktop\","Trajed Project Files (*.traj)" + Chr$(0) + "*.traj" + Chr$(0)); flags optional
			
			;Print getsavefile$("File Save / Datei sichern","new_project.traj","Trajed Project File (*.traj)" + Chr$(0) + "*.traj" + Chr$(0) + "Text" + Chr$(0) + "*.txt" + Chr$(0));; flags optional
			
			;While MouseHit(1) <> 1
			;Wend
			;End
			
			;-------------------------------------------------------------------------------
			
				Function getOpenFile$(lpstrTitle$,lpstrInitialDir$,lpstrFilter$,flags=$1000)
			
				nextOffset%=0 
				theBank=CreateBank(76)
				lStructSize=76
				PokeInt theBank,nextOffset%,lStructSize
				nextOffset%=nextOffset%+4 
			
				hwndOwner=hWnd
				PokeInt theBank,nextOffset%,hwndOwner
				nextOffset%=nextOffset%+4 
			
				hInstance=0
				PokeInt theBank,nextOffset%,hInstance
				nextOffset%=nextOffset%+4 
			
				If lpstrFilter$ = "" Then
				lpstrFilter$ = "All Files (*.*)" + Chr$(0) + "*.*" + Chr$(0)+ Chr$(0)
				Else	
				lpstrFilter$ = lpstrFilter$ + Chr$(0)	
				End If
				lpstrFilter_ = CreateBank(Len(lpstrFilter$)) 
				string_in_bank(lpstrFilter$,lpstrFilter_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFilter_)
				nextOffset%=nextOffset%+4
			
				lpstrCustomFilter=0
				PokeInt theBank,nextOffset%,lpstrCustomFilter
				nextOffset%=nextOffset%+4 
			
				nMaxCustFilter=0
				PokeInt theBank,nextOffset%,nMaxCustFilter
				nextOffset%=nextOffset%+4 
			
				nFilterIndex=0
				PokeInt theBank,nextOffset%,nFilterIndex
				nextOffset%=nextOffset%+4 
			
				lpstrFile$= String$ (" ", 254)
				lpstrFile_ = CreateBank(Len(lpstrFile$)+1) 
				string_in_bank(lpstrFile$+Chr$(0),lpstrFile_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFile_)
				nextOffset%=nextOffset%+4 
			
				nMaxFile=255
				PokeInt theBank,nextOffset%,nMaxFile
				nextOffset%=nextOffset%+4 
			
				lpstrFileTitle$=String$ (" ", 254)
				lpstrFileTitle_ = CreateBank(Len(lpstrFileTitle$)) 
				string_in_bank(lpstrFileTitle$,lpstrFileTitle_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFileTitle_)
				nextOffset%=nextOffset%+4 
			
				nMaxFileTitle=255
				PokeInt theBank,nextOffset%,nMaxFileTitle
				nextOffset%=nextOffset%+4 
			
				If lpstrInitialDir$="" Then
				lpstrInitialDir$="c:\"+Chr$(0)
				Else
				lpstrInitialDir$=lpstrInitialDir$+Chr$(0)
				End If
				lpstrInitialDir_ = CreateBank(Len(lpstrInitialDir$)) 
				string_in_bank(lpstrInitialDir$,lpstrInitialDir_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrInitialDir_)
				nextOffset%=nextOffset%+4 
			
				If lpstrTitle$="" Then
				lpstrTitle$="Open"+Chr$(0) 
				Else
				lpstrTitle$ = lpstrTitle$ + Chr$(0)
				End If	
				lpstrTitle_ = CreateBank(Len(lpstrTitle$)) 
				string_in_bank(lpstrTitle$,lpstrTitle_)	
				PokeInt theBank,nextOffset%,AddressOf(lpstrTitle_)
				nextOffset%=nextOffset%+4 
			
				PokeInt theBank,nextOffset%,flags
				nextOffset%=nextOffset%+4 
			
				nFileOffset=0
				PokeShort theBank,nextOffset%,nFileOffset
				nextOffset%=nextOffset%+2
			
				nFileExtension=0
				PokeShort theBank,nextOffset%,nFileExtension
				nextOffset%=nextOffset%+2
				
				lpstrDefExt=0
				PokeInt theBank,nextOffset%,lpstrDefExt
				nextOffset%=nextOffset%+4 
			
				lCustData=0
				PokeInt theBank,nextOffset%,lCustData
				nextOffset%=nextOffset%+4 
			
				lpfnHook=0
				PokeInt theBank,nextOffset%,lpfnHook
				nextOffset%=nextOffset%+4 
			
				lpTemplateName$=""+Chr$(0)
				lpTemplateName_ = CreateBank(Len(lpTemplateName$)) 
				string_in_bank(lpTemplateName$,lpTemplateName_)
				PokeInt theBank,nextOffset%,AddressOf(lpTemplateName_)
				nextOffset%=nextOffset%+4 
				If api_GetOpenFileName (theBank) Then
				lpstrFile$ = bank_in_string$(lpstrFile_)
				Else
				lpstrFile$ =""
				End If
				FreeBank theBank
				FreeBank lpstrFilter_
				FreeBank lpstrFile_
				FreeBank lpstrFileTitle_
				FreeBank lpstrInitialDir_
				FreeBank lpstrTitle_
				FreeBank lpTemplateName_
				Return lpstrFile$
				
				End Function
			
			;--------------------------------------------------------------------------------------
			
			Function getsaveFile$(lpstrTitle$,lpstrFile$,lpstrFilter$,flags=2) ; Get a SAVEFILENAME
			
				nextOffset%=0 
				theBank=CreateBank(76)
				lStructSize=76
				PokeInt theBank,nextOffset%,lStructSize
				nextOffset%=nextOffset%+4 
					
				hwndOwner=hWnd
				PokeInt theBank,nextOffset%,hwndOwner
				nextOffset%=nextOffset%+4 
					
				hInstance=0
				PokeInt theBank,nextOffset%,hInstance
				nextOffset%=nextOffset%+4 
			
				If lpstrFilter$ = "" Then
					lpstrFilter$ = "All Files (*.*)" + Chr$(0) + "*.*" + Chr$(0)+ Chr$(0)
				Else	
					lpstrFilter$ = lpstrFilter$ + Chr$(0)		
				End If
				lpstrFilter_ = CreateBank(Len(lpstrFilter$)) 
				string_in_bank(lpstrFilter$,lpstrFilter_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFilter_)
				nextOffset%=nextOffset%+4
					
				lpstrCustomFilter=0
				PokeInt theBank,nextOffset%,lpstrCustomFilter
				nextOffset%=nextOffset%+4 
				
				nMaxCustFilter=0
				PokeInt theBank,nextOffset%,nMaxCustFilter
				nextOffset%=nextOffset%+4 
				
				nFilterIndex=0
				PokeInt theBank,nextOffset%,nFilterIndex
				nextOffset%=nextOffset%+4 
			
				lpstrFile_ = CreateBank(255) 
				string_in_bank(lpstrFile$+Chr$(0),lpstrFile_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFile_)
				nextOffset%=nextOffset%+4
				
				nMaxFile=255
				PokeInt theBank,nextOffset%,nMaxFile
				nextOffset%=nextOffset%+4 
				
				lpstrFileTitle$=String$ (" ", 254)
				lpstrFileTitle_ = CreateBank(Len(lpstrFileTitle$)) 
				string_in_bank(lpstrFileTitle$,lpstrFileTitle_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrFileTitle_)
				nextOffset%=nextOffset%+4  
				
				nMaxFileTitle=255
				PokeInt theBank,nextOffset%,nMaxFileTitle
				nextOffset%=nextOffset%+4 
				
				lpstrInitialDir$=""+Chr$(0)
				lpstrInitialDir_ = CreateBank(Len(lpstrInitialDir$)) 
				string_in_bank(lpstrInitialDir$,lpstrInitialDir_)
				PokeInt theBank,nextOffset%,AddressOf(lpstrInitialDir_)
				nextOffset%=nextOffset%+4 
				
				If lpstrTitle$="" Then
					lpstrTitle$="Save"+Chr$(0) 
				Else
					lpstrTitle$ = lpstrTitle$ + Chr$(0)
				End If	
				lpstrTitle_ = CreateBank(Len(lpstrTitle$)) 
				string_in_bank(lpstrTitle$,lpstrTitle_)	
				PokeInt theBank,nextOffset%,AddressOf(lpstrTitle_)
				nextOffset%=nextOffset%+4 
			
				PokeInt theBank,nextOffset%,flags
				nextOffset%=nextOffset%+4 
				
				nFileOffset=0
				PokeShort theBank,nextOffset%,nFileOffset
				nextOffset%=nextOffset%+2
				
				nFileExtension=0
				PokeShort theBank,nextOffset%,nFileExtension
				nextOffset%=nextOffset%+2
				
				lpstrDefExt=0
				PokeInt theBank,nextOffset%,lpstrDefExt
				nextOffset%=nextOffset%+4 
				
				lCustData=0
				PokeInt theBank,nextOffset%,lCustData
				nextOffset%=nextOffset%+4 
				
				lpfnHook=0
				PokeInt theBank,nextOffset%,lpfnHook
				nextOffset%=nextOffset%+4 
			
				lpTemplateName$=""+Chr$(0)
				lpTemplateName_ = CreateBank(Len(lpTemplateName$)) 
				string_in_bank(lpTemplateName$,lpTemplateName_)
				PokeInt theBank,nextOffset%,AddressOf(lpTemplateName_)
			
				If api_GetSaveFileName (theBank) Then
					lpstrFile$ = bank_in_string$(lpstrFile_)
				Else
					lpstrFile$ =""
				End If
				FreeBank theBank
				FreeBank lpstrFilter_
				FreeBank lpstrFile_
				FreeBank lpstrFileTitle_
				FreeBank lpstrInitialDir_
				FreeBank lpstrTitle_
				FreeBank lpTemplateName_
				Return lpstrFile$
			End Function
			
			Function AddressOf(Bank) ; Find the correct Adress of a Bank (for C *Pointer)
				Local Address = CreateBank(4) 
				api_RtlMoveMemory(Address,Bank+4,4) 
				Return PeekInt(Address,0) 
			End Function
			
			Function string_in_bank(s$,bankhandle) ; Put a String in a Bank
				Local pos=1
				Local pos2=0
				Repeat
					PokeByte(bankhandle,pos2,Asc(Mid(s$,pos,Len(s$))))
					pos=pos+1
					pos2=pos2+1
				Until pos=Len(s$)+1
			End Function
			
			Function bank_in_string$(bankhandle) ; Get a String from a Bank
				Local s$=""
				Local pos=0
				Repeat
					s$=s$+Chr(PeekByte(bankhandle,pos))
					pos=pos+1
				Until pos=BankSize(bankhandle)
				s$=Replace$(s$,Chr(0)," ")
				Return s$
			End Function
			
	;--------------------------------------------------------------------------------------------------------
	; End third-party functions
	;--------------------------------------------------------------------------------------------------------
	;--------------------------------------------------------------------------------------------------------
	; EOF

	End
;~IDEal Editor Parameters:
;~C#Blitz3D
