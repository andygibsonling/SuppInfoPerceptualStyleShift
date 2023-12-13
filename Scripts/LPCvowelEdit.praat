#
# Using LPC in Praat to manipulate formants for resynthesis
# - imposes logistic functions to create smooth transitions from
# the formants of the source wave before the manipulation area
# to the new formant values, and again out to the formants of the
# source after the manipulation area.
# The opening form box allows the user to specify a directory, a file, the
# sampling frequency of that file 
# on- and off-ramp sizes (in frames of 5ms) for the tails of the logistic 
# functions, and target values for F1-F3. If F3 is set to zero then source
# values are kept. This occurs regardless for F4 and F5
#
# The user is then prompted to click on beginning and end points for
# the in- and out-transitions, and the script then does its work.
# 
# Author: Paul Warren <paul.warren@vuw.ac.nz>


#  edited by Jen Hay to allow any of the formant values to be set to zero
# and left unmodified.
# note you may have to edit the fourth-to-last line to reflect the 
# sampling frequency you're working with.

# edited by Andy Gibson to change the resampling command to 9500 as this was 
# found to create more naturalistic sounding output. 


form arguments
	word Directory /Users/Andy/
	word File bad161008_4.wav
	integer onramp1 1
	integer offramp1 1
	integer onramp2 1
	integer offramp2 1
	integer f1target 450
	integer f2target 1932
	integer f3target 0
	positive inBeg 0.099
	positive inEnd 0.124
	positive outBeg 0.471
	positive outEnd 0.495
endform

pause 'f1target'
Read from file... 'Directory$'/'File$'
object_name$ = selected$("Sound")

Scale peak... 0.9
totDur = Get total duration
sampFreq = Get sampling frequency

#Edit
#editor Sound 'object_name$'

# used to prompt user to click on in-transition beginning and end, 
# and out-transition beginning and end.
# now you can add this in the form at the beginning. (ag 161008)

#pause Click to mark the desired beginning point of the IN-transition
# inBeg = Get cursor
 
#pause Click to mark the desired end point of the IN-transition
# inEnd = Get cursor

#pause Click to mark the desired beginning point of the OUT-transition
# outBeg = Get cursor

# pause Click to mark the desired end point of the OUT-transition
# outEnd = Get cursor

# Display whole file 
#Show all

#Close
#endeditor

# resample for best LPC analysis. Note: new object will
# automatically be labelled 'object_name$'_9500

Resample... 9500 50

To Formant (burg)... 0.005 5 5500 0.025 50

# from this Formant file, obtain values for frames
# and calculate frame positions for the beginning
# and end points of the modification
# Frame numbers with 'Slope' in the name are the values
# used for the transition slope; those with 'Mod' in
# the name are the beginnings and ends of the range 
# of the logistic function

firstframe = Get time from frame number... 1
framesize = Get time step

frameno = Get frame number from time... inBeg
inSlopeStart = round(frameno)
frameno = Get frame number from time... inEnd
inSlopeEnd = round(frameno)
frameno = Get frame number from time... outBeg
outSlopeStart = round(frameno)
frameno = Get frame number from time... outEnd
outSlopeEnd = round(frameno)

inModStart = inSlopeStart - onramp1
inModEnd = inSlopeEnd + offramp1
outModStart = outSlopeStart - onramp2
outModEnd = outSlopeEnd + offramp2

# make sure the 'tails' of the logistic functions cover the whole vowel
if outModStart > inModEnd 
 	inModEnd = round((outModStart-inModEnd)/2)+inModEnd
endif
outModStart = inModEnd + 1

# convert the extreme positions back to time values
# for use in the cut-&-paste operations

editPoint1 = firstframe + ((inModStart-1)*framesize)
editPoint2 = firstframe + ((outModEnd-1)*framesize)

# select the pre-modification section from the original
# file and extract to 'firstSection'

select Sound 'object_name$'
Extract part... 0.0 editPoint1 Rectangular 1 no
Rename... firstSection

# derive the source signal using inverse filter on the 11k file
# then delete the LPC object to avoid later confusion
# and rename the source signal 'source_...'

select Sound 'object_name$'_9500
To LPC (burg)... 10 0.025 0.005 50

select Sound 'object_name$'_9500
plus LPC 'object_name$'_9500
Filter (inverse)

select LPC 'object_name$'_9500
Remove

select Sound 'object_name$'_9500
Rename... source_'object_name$'_9500

# select the Formant file & get formant values for the edges 
# of the manipulation region (so transitions will move from here.
# Note that if the edge points happen to have spurious formant values
# then the effect will be of strange transitions)

select Formant 'object_name$'_9500

startF1 = Get value at time... 1 editPoint1 Hertz Linear
startF2 = Get value at time... 2 editPoint1 Hertz Linear
startF3 = Get value at time... 3 editPoint1 Hertz Linear
finalPoint = firstframe + ((outModEnd-1)*framesize)
endF1 = Get value at time... 1 editPoint2 Hertz Linear
endF2 = Get value at time... 2 editPoint2 Hertz Linear
endF3 = Get value at time... 3 editPoint2 Hertz Linear

# workout slope midpoint, formant ranges & slopes for in-transition

midX=round((inSlopeEnd-inSlopeStart)/2)+inSlopeStart

rangeF1=f1target-startF1
slopeF1=rangeF1/(inSlopeEnd-inSlopeStart)
rangeF2=f2target-startF2
slopeF2=rangeF2/(inSlopeEnd-inSlopeStart)
rangeF3=f3target-startF3
slopeF3=rangeF3/(inSlopeEnd-inSlopeStart)

for thisPos from inModStart to inModEnd

	# need to calculate time value (which we call modPos) because 
	# "Get" functions on Formant file require a time, not a frame

	modPos = firstframe + ((thisPos-1)*framesize)

	# retrieve F3-F5 values (not changing these unless F3 is specified
	# as non-zero in the opening form) Note that all original bandwidths
	# are used - not sure that this is the best procedure

	f1b=Get bandwidth at time... 1 modPos Hertz Linear
	f2b=Get bandwidth at time... 2 modPos Hertz Linear
	f3b=Get bandwidth at time... 3 modPos Hertz Linear
	f4b=Get bandwidth at time... 4 modPos Hertz Linear
	f5b=Get bandwidth at time... 5 modPos Hertz Linear

	if f1target=0
		f1=Get value at time... 1 modPos Hertz Linear
	else 
	f1=(rangeF1/(1+exp(4*slopeF1/rangeF1*(midX-thisPos))))+startF1
	endif
	if f2target=0
		f2=Get value at time... 2 modPos Hertz Linear
	else 
	f2=(rangeF2/(1+exp(4*slopeF2/rangeF2*(midX-thisPos))))+startF2
	endif
	if f3target=0
		f3=Get value at time... 3 modPos Hertz Linear
	else 
		f3=(rangeF3/(1+exp(4*slopeF3/rangeF3*(midX-thisPos))))+startF3
	endif
	f4=Get value at time... 4 modPos Hertz Linear
	f5=Get value at time... 5 modPos Hertz Linear

	Formula (frequencies)... if ((col = thisPos) and (row = 1)) then (f1) else self fi 
	Formula (frequencies)... if ((col = thisPos) and (row = 2)) then (f2) else self fi 
	Formula (frequencies)... if ((col = thisPos) and (row = 3)) then (f3) else self fi 

endfor

# go through same process for out transitions (could probably put some
# of this repetitive stuff into procedures

midX=round((outSlopeEnd-outSlopeStart )/2)+outSlopeStart

rangeF1=f1target-endF1
slopeF1=-rangeF1/(outSlopeEnd-outSlopeStart)
rangeF2=f2target-endF2
slopeF2=-rangeF2/(outSlopeEnd-outSlopeStart)
rangeF3=f3target-endF3
slopeF3=-rangeF3/(outSlopeEnd-outSlopeStart)

for thisPos from outModStart to outModEnd
	modPos = firstframe + ((thisPos-1)*framesize)

	f1b=Get bandwidth at time... 1 modPos Hertz Linear
	f2b=Get bandwidth at time... 2 modPos Hertz Linear
	f3b=Get bandwidth at time... 3 modPos Hertz Linear
	f4b=Get bandwidth at time... 4 modPos Hertz Linear
	f5b=Get bandwidth at time... 5 modPos Hertz Linear

	f1=(rangeF1/(1+exp(4*slopeF1/rangeF1*(midX-thisPos))))+endF1 
	f2=(rangeF2/(1+exp(4*slopeF2/rangeF2*(midX-thisPos))))+endF2
	if f3target=0
		f3=Get value at time... 3 modPos Hertz Linear
	else 
		f3=(rangeF3/(1+exp(4*slopeF3/rangeF3*(midX-thisPos))))+endF3
	endif
	f4=Get value at time... 4 modPos Hertz Linear
	f5=Get value at time... 5 modPos Hertz Linear

	Formula (frequencies)... if ((col = thisPos) and (row = 1)) then (f1) else self fi 
	Formula (frequencies)... if ((col = thisPos) and (row = 2)) then (f2) else self fi 
	Formula (frequencies)... if ((col = thisPos) and (row = 3)) then (f3) else self fi 

endfor

select Formant 'object_name$'_9500
Down to FormantTier

# combine the source and the new filter to make the new sound file
select Sound source_'object_name$'_9500
plus FormantTier 'object_name$'_9500
Filter

# resample the output to sampFreq for splicing with original file
Resample... sampFreq 50



Extract part... editPoint1 editPoint2 Rectangular 1 no
Rename... secondSection

# extract the post-modification section to 'thirdSection'
select Sound 'object_name$'
Extract part... editPoint2 totDur Rectangular 1 no
Rename... thirdSection

select Sound firstSection
plus Sound secondSection
plus Sound thirdSection
Concatenate
Rename... 'f1target'_'f2target'_'object_name$'

# clean up
select Sound firstSection
plus Sound secondSection
plus Sound thirdSection
plus Sound 'object_name$'_9500
plus FormantTier 'object_name$'_9500
plus Formant 'object_name$'_9500
plus Sound source_'object_name$'_9500
plus Sound source_'object_name$'_9500_filt
# 26-3-2016, AG: NB. u have to enter your sample rate at the end of the next line
plus Sound source_'object_name$'_9500_filt_44100
plus Sound 'object_name$'
Remove

select Sound 'f1target'_'f2target'_'object_name$'
Edit
