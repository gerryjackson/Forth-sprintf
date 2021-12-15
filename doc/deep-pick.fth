\ Notes about Deep Pick
\ How to access data buried on the stack - e.g. set of arguments for SPRINTF
\ using named constants for the PICK number

0 [if]
In general at the point of wanting a value deep in the stack the stack may be:

    |  a  | args | b | c |
    <------dref------>
    <------d------------->
where
   a is data before SPRINTF was called, size #a cells
   args is the deep data, size #args, arg0 is deepest in the stack
   b is data when SPRINTF was called, size #b
   c is SPRINTF working data, size #c
   dref is a reference stack depth taken at the start of SPRINTF
   d is the stack depth at the point of access

If argn is accessed by:  pn PICK  ( pn is pick number)
then pn = #c + #b + #args -1 - argnum

#c = d - dref and is likely to be variable
#b is known and constant
#args will be calculated during processing of the format string
argnum is the required argument number
Therefore:
pn = d - dref + #b + #args - argnum - 1
   = d - (dref - #b - #args + 1) - argnum

Once #args has been found, we pre-calculate:
   argnum0 = dref - #b - #args + 1
then
   argnumn = argnum0 + n   where n = 0 to #args-1
and
   pn = d - argnumn

Note that #a does not appear in the equations therefore the PICK number is
independent of the stack depth when SPRINTF is called.

Example, the stack is ( -1 11 22 33 44 99 98 -80 -81 -82 )
where
   a is (-1) and #a = 1
   args are (11 22 33 44) and #args = 4
   b is (99 98) and #b = 2
   c is (-80 -81 -82) and #c = 3
   d is 10
   dref = 7
therefore
   argnum0 = 7 - 2 - 4 + 1
           = 2
and p0 = 10 - 2 = 8
    p1 = p0 -1  = 7
    etc

There are 2 further improvements:
1) If we define get-arg as
      : get-arg  ( argnum -- arg ) depth - negate pick  ;
   Then there is one extra argument on the stack when DEPTH is called, therefore
   we need to add 1 to ARGNUM0 i.e.
      argnum0 = dref - #b - #args + 2
2) The definition of GET-ARG in 1) can be improved if argnum0 is negated i.e.
      argnum0 = #b + #args - 2 - dref
      : get-arg  ( argnum -- arg )  depth + pick  ;
   Then argnumn = argnum0 - n

Note that for SPRINTF DREF has to be set on entry to SPRINTF and ARGNUM0
has to be calculated when the number of arguments is known

Although examples below use integers for argument numbers, they can, of course,
be given meaningful symbolic names like any other constant.
[then]

0 value dref
0 value #args

: dat -1 -2 -3 11 22 33 44 99 98 depth to dref -80 -81 -82 -83 -84  ;
\ 11 22 33 44 are arguments
\ (99 98) is (ca u) of the format string
\ -80 to -84 are working data
\ DP is deep-pick
: dp  ( argnum -- arg )
   depth + pick
;

dat .s
4 to #args
\ dref 2 - #args - 1+ 1+ negate value argnum0
2 #args + 1- 1- dref - value argnum0

argnum0 .s dup dp swap 1- .s dup dp swap 1- .s dup dp swap 1- .s dp .s
argnum0 .s dp .s

\ displays
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 -5 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 -6 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 22 -7 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 22 33 -8 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 22 33 44 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 22 33 44 -5 )
( -1 -2 -3 11 22 33 44 99 98 -80 -81 -82 -83 -84 11 22 33 44 11 )

0 [if]
A Simpler method
~~~~~~~~~~~~~~~~
In integer-sprintf a simpler method of picking arguments is used. if the data to
be PICKed has the number of arguments just above it on the data stack and the
number of arguments is on top of the stack e.g.

... x1 x2 x3 x4 x5 5

then the argument numbers for x1 to x5 can be 0 to -4 respectively and can be
read by:
: getarg   ( args* #args argnum -- #args arg )  over + pick  ;
: get2arg  ( args* #args argnum -- #args argls argms )
   tuck getarg >r swap getarg r>  
;

e.g.
-1 -2 11 22 33 44 55 5  0 getarg .s  \ displays ( -1 -2 11 22 33 44 55 5 11 )
-1 -2 11 22 33 44 55 5 -2 getarg .s  \ displays ( -1 -2 11 22 33 44 55 5 33 )
-1 -2 11 22 33 44 55 5 -3 get2arg .s \ displays ( -1 -2 11 22 33 44 55 5 44 55 )

This may be difficult in practice if there is further data on the stack then
the number of stack items has to be placed on the stack before argument access.
This may be achieved by using a reference stack depth and actual depth e.g.

0 value dref
-1 -2 -3 depth to dref 11 22 33 44 depth dref - ( -- -1 -2 -3 11 22 33 44 4 )
\ then
0 getarg .           \ displays 11
-2 get2arg swap . .  \ displays 33 44
-1 -2 -3 11 22 33 44 -100 -101 depth dref - -2 get2arg swap . . \ displays 33 44
[then]
