# TrajedC64
Trajed - C64 Scrolling Trajectory Editor

Initial Release: https://csdb.dk/release/?id=113623 <br>

# WATCH THIS TUTORIAL VIDEO:
Demo/Tutorial: https://www.youtube.com/watch?v=Gt0_kQVgApI


# System requirements:
Tested on Windows XP SP3 32, min 1024x900 pix screen resolution, runs on Win7 and 10, too.
		
This is alpha software. If you decide to use this, you do so at your own risk!
The author takes no liabilty for anything!


# important note: 
                i had to quickly write this during my c64 demo coding efforts.
	   	clearly: i know that my coding is suboptimal and
	   	that the logic and layout and overall usage of the
	   	language sXck. i know there are some very unusual 
	   	and complex ways to solve problems, that rather 
	   	cause new problems than actually help. BUT. 
	   	this works. and i had the idea first. 
	   	and i made it happen.
		if you don´t like it, do not use it. simple.
		
                the file testarea960x600.png has to stay with the 
		binaries in the root directory. or there will be crashes.
		
                the blinking and drawing upon del-undo is not really
		functional under windows 7 and 10. meh.

program name:		
		 trajed v1.08 
 		 c=64 hard scrolling trajectory editor

program type: 
		 other platform demo tool

creator:
		 (c) 2012 by wertstahl (www.wertstahl.de)

abstract:
		 visually construct hard-scrolling methods and generate 6510 assembler speedcode for 
		 use on the commodore c64 (main target)

system:
		 sourcecode: blitzbasic (blitz3d)
		 windows xp pro 32bit 
		 uses Blitz3D Filerequester (2.Update) by MPZ !!! 
		

# version history: 

v1.08	 (build 12)	minor changes prior to public release
v1.08	 (build 11)	added an option to infuse an "AND #$0F" operation in the
			sourcecode or binary export, so that after every LDA $xxxx
			an AND#$0f will remove the high nybble, which is quite
			handy for multicolor color-ram scrolling.

v1.07a (build 10) 	submenu behaviour simplified
v1.07a (build 9) 	increased number of max instructions to 2250
v1.07a -- 	bugfix: memory access violation after loading a project fixed 
			- note: v1.06a projects will not load correctly anymore.
		  	bugfix: field 0,0 would be empty after loading
		  	todo:   field 0,0 cannot be clicked on after startup or loading.
			- workaround: start somewhere else.
			dox changed.

v1.06a -- 		1st fully functional release


# introduction:
	
Trajed was done for this:
		
if you want to make some scrolling on the c64, you will typically first soft scroll
the screen by manipulating either $D011 or $D016. and then, in case you use the very
common and typical charset-scrolling, you want to hard scroll all the stuff on the 
screen one char in the direction you desire and then reset the soft scrolling position
and start over with the soft scrolling.

you can also want to only hardscroll every frame which is the same as softscrolling
8 pixels in hires or 4 in multicolor. 

for vertical, horizontal, diagonal or even mathematical sine, triangle whatever directions
you can write simple routines that write you speedcode. because doing this by using loops
is wasting raster time, which you don´t have that much of, because of all of the other
fancy effects on the screen and so on.

you can also want to scroll the color ram at $d800, for example. same as above.

but what, if the scrolling trajectory you desire is, lets say, not easy to grasp by
using math? what if you have so many different directions, you want to scroll parts 
of the screen in, that you would rather procrastinate a little more than even THINK
about coding a new program.

because i am such a lazy bastard and i totally suck at math, that is why i wrote this
editor.
		
you can now freely copy any cell on the screen anywhere. without math. without planning.
	
just click the desired source and target (one after the other) and then export a 
nice sourcecode or binary. unrolled speedcode.
		
		

# quickdocs:
		
so. how does it work?
		
if you´re on windows (tested on xp pro, 32 bit, sp3, 2gb ram, 1024x960 screen minimum): 

doubleclick the trajed.exe and after starting up, you should see the TRAJED screen,
filled with blue squares and below that, the status display.

you should have some good understanding of 6510 assembler and how scrolling works and just
need to always remember this: work *backwards*. 
the scheme should always be: 
	source1 target to source0, 
	source2 target to source1...
	source3 target to source2...
	and so on.

the editor will always auto toggle from setting sources to setting targets back and forth,
so you just can click and pick your way through the screen.

OFCOURSE: you should write down the "entry points" for new information and after each scroll
write some new information (i.e. new byte of scrolltext, new byte of color) to respective
entry points. an entry point is not something you will find in the editor. i just made this
description up here, for explanatory reasons! _you_ must take care that your new data will be
written on the screen. entry points are usually easy to spot since they are the only fields on 
the screen that you only READ of. (marked by the symbol that shows an arrow going up out of a 
circle (or two arrows if you have multiple reads from one field, in free logic mode).

specific example: if you write a horizontal (right to left) one-lined text scroller, 
if you scroll in the line $0400, your "entry point" for new data, is usually the very right
field of that line. (in that case it would be $0427). if you recreated this scroller in
TRAJED, forementioned entry point would easily be to spot, since it is the last field you
read from. (simple!) 

if you did it "wrong" for example, all the data will just pass very quickly to the very 
last position and you see no movement at all. really: write a normal horizontal scroller
first, before you attempt to use this tool, or you will think that the editor is complicated.
		
if you know all this: good. (it is actually really very very simple.)

# general keys:
		press <h> for help (this is propably all you need to know). 
		
		<LMB> 	sets a source and then a target. and then a source and a target. always in exchange.
		<RMB> 	will draw you some guides. these do nothing but are orientation-points.
		<DEL> 	will undo step by step till everything is gone. no undo for the guides. 
					i thought it would be cool if the last action you finished would blink. 
					thats why it does, so you can see where you were.
		<x> 	will flush all and you can start over. note: guides will stay. 
			i thought that would be useful. (to really flush everything, just load the empty_default project).
		
		<esc> 	will attempt to quit with a "not yet saved" (if true) warning. 
			if already saved, quits immedately. better dont rely on it.

		<o>	will open a saved project
		<s>	will save a project in a custom .traj format (guides are saved, too, base adress is not)


# editing logic:
		<l>	(lowercase L) Switch editing "logic"
			this is an important function. if you want to create stuff that is of extended
			logic, you must switch the logic restrictions by pressing <l> 
			certain order of operations does not make sense, but you could want to have it,
			so press <l> to toggle the mode.
			normally you would want to always give new data to certain fields to have 
			some good visual movement action going on.
			but you could also want to just let everything loop around. its not among the
			logical things to do, so, first just work in the restricted mode, THEN
			press <l> to switch to free mode, place your commands wisely and after that
			i recommend you switch back by pressing <l> again, so you dont accidentally
			do things you wouldnt want to happen.
			there is a very nice diagram in the <h> help that explains what the logic does
			in detail.


#visual relocation:

			<SPACE> is one of the functions i love most, you can relocate your entire stuff here.
			imagine you aimed wrong in the beginning. and now you reach the border and see
			damn, i am out of center. just press space and move everything a little in the
			direction you desire. i _love_ it.			
					
					

# export submenu:
			<g>	will give you the export screen.
			press <t> to test your trajectory. you will see an image which is broken down
			into 24x24 pix, emulating the dimensions of the c64 
			screen and all the commands you set 
			will scroll the picture around (hopefully).
			the picture you see can easily be replaced by 
			editing the picture in the base directory
			of TRAJED, called "testarea960x600.png" 
			WARNING: if that picture is not in the given name,
			size and file format, trajed won´t find
			it and will not start! 
			960x600 gives you 40x25 blocks of 24x24 pixels.
			some options will be displayed at the bottom of the screen.
						
			press <d> to toggle the data mode you desire for your exported code 
					- preformatted cross-assembler text 

					- plain 8bit binary !!C=64 loading bytes will NOT be included!!
					  if you try to load this binary on the c64 with the
					  regular kernel loader, it will most likely start
					  loading to the zeropage
					  or even worse!
							
			press <b> to set the base adress for your range of operation.
					this means: trajed code is ofcourse memory location independet.
					but you need to set the lowest adress of the screen,
					represented by the top left
					corner of the editor screen to tell trajed which part
					of the memory you want to scroll.
					examples: $0400 (c64 text base) $d800 (c64 color ram base)
					
					
			press <a> to toggle the AND #$0f infusion
					this will add two bytes after each three byte lda instruction, and 
					is necessary for cases in which you want to read from the color ram.
					because if you do so, reading from $d800 will give you bus garbage in the
					high nybble, which can corrupt your multicolor mode dispay, if written
					back without being purged. 
					so if you want to colorscroll and your screen is multicolor, set this
					function to ON.
					otherwise, for speed reasons, leave this OFF!
							
			press <w> to open the export dialog and write your file.		

		

# disclaimer/copyright/support/notes:
		
I can only give very limited support for this. 

This thing is written in Blitz Basic, Blitz3d, to be exact.

Please have fun with it, please feel free to use the results in any way you desire. 

TRAJED is freeware. Credits appreciated. Redistribution only if unchanged + completely free of charge.
Redistribution only if the access to the archive is free. Inclusion in commercial products
(software collections ect.) only by written permission. Sale strictly forbidden.

Thanks to MPZ for publishing his system requester function on blitzbasic.com

(c)2012 by Sebastian I. Hartmann aka Wertstahl
