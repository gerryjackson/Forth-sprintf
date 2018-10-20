\ Sprintf - Formatted text output
\
\ Provides SPRINTF - saves formatted text into a buffer
\          PRINTF  - displays formatted text
\          FPRINTF - saves formatted text in a file
\
\ Copyright (C) Gerry Jackson 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

\ ------------------------------------------------------------------------------
: spf-version$  s" 1.0.0" ;
\ ------------------------------------------------------------------------------



cr cr .( PRINTF, SPRINTF, FPRINTF version ) spf-version$ type cr 

\ ---[ Sprintf wordlist for internal definitions ]------------------------------

: >order  ( wid -- )  >r get-order 1+ r> swap set-order  ;

wordlist constant sprintf-wl
: sprintf-words  ( -- )  sprintf-wl >order ;  immediate
: sprintf-definitions  ( -- )  postpone sprintf-words definitions  ; immediate
sprintf-definitions
: user-definitions     ( -- ) previous definitions  ; immediate

\ ---[ User configurable data ]-------------------------------------------------
\ VALUEs are used instead of CONSTANTs so that a user can change them in their
\ own program without modifying this file

15 value max-precision  \ Maximum usable precision in the Forth system

200 value max-conv-width   \ To limit output
200 value max-conv-prec    \ To limit output

'.' value fp-sep        \ Character to use between FP integer and fraction 

\ Maximum number of floating point arguments expected for SPRINTF
10 value max-fp-args

\ Exception codes for CATCH ... THROW returning to SPRINTF
1000 value spf-good              \ Normal completion of SPRINTF
1001 value invalid-char          \ Parser syntax error
1002 value too-few-args          \ Not enough arguments
1003 value too-few-fpargs        \ Not enough FP arguments
1004 value too-many-fps          \ FP-ARGS array too small
1005 value buf-overflow          \ Output buffer overflow
1006 value rle                   \ Run length encoding error
1007 value no-fp                 \ Floating point not enabled 

\ Internal exception
2000 value invalid-fp-number     \ Handling INF and NAN


\ ---[ Auto detection of some system characteristics ]--------------------------

\ Number of bits in one arithmetic unit (byte)
true pad aligned ! pad aligned c@ 0
2 base ! <# #s #> nip constant bits/au decimal

\ 16 bit Forth?
0 cell+ bits/au * 16 = constant 16bits?

\ Big or little endian system
$FF pad align ! pad align c@ $FF <> constant big-endian

s" [undefined]" pad c! pad char+ pad c@ move 
pad find nip 0=
[if]
   user-definitions
   : [undefined]  ( "name" -- flag )
      bl word find nip 0=
   ; immediate
   sprintf-definitions
[then]

\ Do not compile code for floating point conversions if either:
\   - the floating point word REPRESENT is undefined or
\   - the user has defined NO-FLOATING-POINT

true value fp-enabled immediate
[undefined] represent [if]       \ Is floating point present?
   false to fp-enabled           \ No, disable FP code and FP tests
   .( REPRESENT missing - )
[then]
fp-enabled [undefined] no-floating-point and [if]
   max-precision set-precision   \ Set FP precision
   .( Floating point conversions included)
[else]
   false to fp-enabled           \ Disable FP code and FP tests
   .( Floating point conversions omitted)
[then] cr cr

\ ---[ Compatibility definitions ]----------------------------------------------
\ Forth 2012 definitions and others used that systems may lack

user-definitions
wordlist constant execute-parsing-wordlist
get-current execute-parsing-wordlist set-current

\ X is prepended to the string, then the string is EVALUATEd
: X  ( xt -- )
   previous execute
   source >in ! drop       \ skip remaining input
; immediate         

set-current

: (exec-parsing)  ( ... xt u caddr -- ... )
   dup >r s" X " r> swap cmove
   tuck >r                          ( -- xt caddr u ) ( R: -- caddr )
   execute-parsing-wordlist >order
   ['] evaluate catch               ( -- )
   r> free throw throw
;

: execute-parsing ( ... caddr u xt -- ... )
   -rot dup >r 2 chars + tuck       ( -- xt u+2 caddr u+2 ) ( R: -- u )
   allocate throw                   ( -- xt u+2 caddr caddr2 )
   tuck 2 chars + r> cmove          ( -- xt u+2 caddr2 ) ( R: -- )
   (exec-parsing)                   ( -- )
;

[undefined] -rot  [if] : -rot rot rot ; [then]
[undefined] <=    [if] : <= > 0= ;      [then]
[undefined] >=    [if] : >= < 0= ;      [then]

[undefined] parse-name
[if]   \ From Forth 200X web site
   : isspace? ( c -- f ) bl 1+ u< ;

   : isnotspace? ( c -- f ) isspace? 0= ;

   : xt-skip   ( addr1 n1 xt -- addr2 n2 ) \ gforth
      \ skip all characters satisfying xt ( c -- f )
      >r
      begin
         dup
      while
         over c@ r@ execute
      while
         1 /string
      repeat  then
      r> drop
   ;

   : parse-name ( "name" -- c-addr u )
      source >in @ /string
      ['] isspace? xt-skip over >r
      ['] isnotspace? xt-skip ( end-word restlen r: start-word )
      2dup 1 min + source drop - >in !
      drop r> tuck -
   ;
[then]

[undefined] holds [if]
   : holds  ( caddr u -- )
      begin dup while 1- 2dup + c@ hold repeat 2drop
   ;
[then]

[undefined] defer [if]  \ From the Forth 200X web-site
   : defer  ( "name" -- )
      create ['] abort ,
      does> ( ... -- ... )
      @ execute
   ;
   
   : defer! ( xt2 xt1 -- )  >body ! ;
   
   : is
      state @
      if
         postpone ['] postpone defer!
      else
         ' defer!
      then
   ; immediate
[then]

sprintf-definitions

variable tokenval 1 tokenval !
: token  ( "name" -- )  tokenval @ constant 1 tokenval +!  ;


\ ---[ Stack frame for conversion specifications and arguments ]----------------

\ Given a data structure buried on the stack, stack-frame.fth enables the
\ individual fields of the data structure to be read independently of the
\ depth of the stack above or below the data structure.

variable frame  0 frame !
2 value min-frame
: set-frame  ( -- )  depth min-frame + frame !  ;

: drop-frame  ( i*x -- j*x )
   depth frame @ 2 - - dup 0>
   if
      dup 1 and if nip then 2/ 0
      ?do 2drop loop exit
   then
   drop
;

: (arg)  ( n -- x )  depth + frame @ - pick  ;

: arg  ( n -- x )  negate (arg)  ;

: (2arg)  ( n -- x1 x2 )  depth + frame @ - dup >r pick r> pick  ;

: 2arg ( n -- x1 x2 )  dup arg swap 1+ arg  ;

: field  ( n -- n-1 )
   >r : r@ postpone literal postpone (arg) postpone ;
   r> 1-
;

: 2field  ( n -- n-2 )  \ Useful for (caddr u) strings
   >r : r@ postpone literal postpone (2arg) postpone ;
   r> 1- 1-
;

0 constant begin-frame  ( -- 0 )
: end-frame  ( -n "name" -- )  negate constant  ;  \ name is frame size in cells

\ Shorthand way to declare frame field names
\ Usage:  frame{ bar1 bar2 ... barn } foo
\ Must be on one line

: frame{  ( "name0" ... "namen" -- )
   begin-frame
   begin
      >in @ >r  parse-name s" }" compare
   while
      r> >in ! field
   repeat
   end-frame r> drop
;

\ ---[ The SPRINTF output buffer ]----------------------------------------------
\ The user has to create this buffer before usng SPRINTF. This avoids passing
\ a buffer (caddr u) every time sprintf is called

\ Not used by PRINTF or FPRINTF

\ create sprbuf sprbuf-size chars allot
0 value sprbuf          \ Points to the buffer 
0 value sprbuf-size     \ Buffer size

variable sprbuf-len        \ Number of characters in the buffer

: sprbuf-clear  ( -- )  0 sprbuf-len !  ;

: init-sprbuf  ( caddr u -- )
   to sprbuf-size  to sprbuf  sprbuf-clear
;

: sprbuf$  ( -- caddr u )  sprbuf sprbuf-len @  ;

: sprbuf-here  ( -- caddr )  sprbuf sprbuf-len @ chars +  ;
: sprbuf-unused  ( -- u  )  sprbuf-size sprbuf-len @ -  ;

: buf-full?  ( u -- )
   sprbuf-unused swap < buf-overflow and throw  ;

: sprbuf+  ( caddr u )
   dup buf-full?
   sprbuf-here swap dup sprbuf-len +! cmove
;

\ ---[ SPRF-PAD ]
\ An equivalent to PAD to avoid corrupting the user PAD

32 max-precision max constant sprf-pad-size
create sprf-pad sprf-pad-size allot

\ ---[ Sprintf family interface ]-----------------------------------------------

\ Output selector values
0 constant >mem   \ for SPRINTF
1 constant >mon   \ for PRINTF
2 constant >file  \ for FPRINTF
 
>mem value output-selector \ Takes one of the three values above
0 value pf-fileid

: concat$  ( caddr u -- )
   output-selector
   case >mem of sprbuf+ endof
        >mon of type    endof
        >file of pf-fileid write-file throw endof
   endcase
;

: char>lower  ( ch -- ch|ch2 ) \ If ch is 'A' to 'Z' return it in lower case 
   dup 'A' 'Z' 1+ within if bl or then
;

: char>upper  ( ch -- ch|ch2 ) \ If ch is 'a' to 'z' return it in upper case 
   dup 'a' 'z' 1+ within if [ bl invert ] literal and then
;

\ CONVERT-CASE converts characters in place in the provided string, so do not
\ pass an S" string as the input string.
\ This has to account for run length coding of zeroes as u is artifically high

: convert-case  ( caddr u xt -- caddr u ) \ xt is >lower or >upper
   >r 2dup                 ( -- ca u ca u )  ( R: -- xt )
   begin
      dup
   while
      over dup c@ dup 0=        ( -- ca u ca u ca ch f )
      if
         drop char+ c@ - 2 +    ( -- ca u ca u' )
         1 /string
      else
         r@ execute swap c!
      then         
      1 /string
   repeat
   2drop r> drop
;

: str>lower  ( caddr u -- caddr u )  ['] char>lower convert-case  ;
: str>upper  ( caddr u -- caddr u )  ['] char>upper convert-case  ;

\ CHAR>BUF seems to be redundant but is used in the test program  *******
\ Delete when testing is complete
: char>sprbuf  ( ch -- )
   1 buf-full?
   sprbuf-here c! 1 sprbuf-len +!
;

: write-chars  ( caddr u -- )
   pf-fileid write-file throw
;

: sprf-pad-fill  ( n ch -- )
   >r sprf-pad-size min sprf-pad swap r> fill  ( -- ) 
;

: write>output  ( n xt -- )
   swap sprf-pad-size /mod
   0 ?do sprf-pad sprf-pad-size 3 pick execute loop
   sprf-pad swap rot execute
;

: concat-chars  ( n ch -- )
   over swap sprf-pad-fill
   output-selector
   case >mem  of ['] sprbuf+     endof
        >mon  of ['] type        endof
        >file of ['] write-chars endof
   endcase
   write>output
;

\ Test only
: .b cr sprbuf$ dup 0= if ." Buffer empty" then type ;
\ ---[ Tokens used for the character map and the parser]------------------------

\ Upper case character constants such as E are called "UE" as most Forths
\ are case insensitive for word names 

token "%"      \ 1   Start of a conversion specification, %% is a '%' character
token "-"      \ 2   Left justify flag
token "0"      \ 3   Pad with zeros flag
token "+"      \ 4   Positive numbers prefixed with '+'
token "bl"     \ 5   Positive numbers prefixed with a space
token "#"      \ 6   Alternate form flag
token "1-9"    \ 7   Start of a decimal number (for width and precision)
token "*"      \ 8   Width or precision is an argument (on the stack)
token "."      \ 9   Start of precision specification
token "l"      \ 10  Format a double integer (l for long)
token "c"      \ 11  Format a character
token "d"      \ 12  Format a signed decimal integer
token "e"      \ 13  Format a floating point number, exponent 'e' format
token "UE"     \ 14  Format a floating point number, exponent 'E' format
token "f"      \ 15  Format a floating point number, fractional format
token "g"      \ 16  Format a floating point number, either 'e' or 'f' format
token "UG"     \ 17  Format a floating point number, either 'E' or 'F' format
token "o"      \ 18  Format an octal integer
token "s"      \ 19  Format a string
token "u"      \ 20  Format an unsigned iteger
token "x"      \ 21  Format a hexadecimal number using 'a' to 'f'
token "UX"     \ 22  Format a hexadecimal number using 'A' to 'F'
token "b"      \ 23  Binary conversion
token "r"      \ 24  Radix conversion using BASE 2 to 36, lower case
token "UR"     \ 25  Radix conversion using BASE 2 to 36, upper case
\ Sprintf parser

\ Generated by Grace 2.0.1
\ See http://www.qlikz.org/forth/grace/grace.html

\ ---[ Map characters to token values ]-----------------------------------------
\ Map characters 32 <= char < 128 to 0 to 25, the rest to 0

create char-map   \ 0 invalid character
    "bl" c, (  )      0 c, ( !)      0 c, ( ")    "#" c, ( #)
       0 c, ( $)    "%" c, ( %)      0 c, ( &)      0 c, ( ')
       0 c, ( ()      0 c, ( )     "*" c, ( *)    "+" c, ( +)
       0 c, ( ,)    "-" c, ( -)    "." c, ( .)      0 c, ( /)
     "0" c, ( 0)  "1-9" c, ( 1)  "1-9" c, ( 2)  "1-9" c, ( 3)
   "1-9" c, ( 4)  "1-9" c, ( 5)  "1-9" c, ( 6)  "1-9" c, ( 7)
   "1-9" c, ( 8)  "1-9" c, ( 9)      0 c, ( :)      0 c, ( ;)
       0 c, ( <)      0 c, ( =)      0 c, ( >)      0 c, ( ?)
       0 c, ( @)      0 c, ( A)      0 c, ( B)      0 c, ( C)
       0 c, ( D)   "UE" c, ( E)      0 c, ( F)   "UG" c, ( G)
       0 c, ( H)      0 c, ( I)      0 c, ( J)      0 c, ( K)
       0 c, ( L)      0 c, ( M)      0 c, ( N)      0 c, ( O)
       0 c, ( P)      0 c, ( Q)   "UR" c, ( R)      0 c, ( S)
       0 c, ( T)      0 c, ( U)      0 c, ( V)      0 c, ( W)
    "UX" c, ( X)      0 c, ( Y)      0 c, ( Z)      0 c, ( [)
       0 c, ( \)      0 c, ( ])      0 c, ( ^)      0 c, ( _)
       0 c, ( `)      0 c, ( a)    "b" c, ( b)    "c" c, ( c)
     "d" c, ( d)    "e" c, ( e)    "f" c, ( f)    "g" c, ( g)
       0 c, ( h)      0 c, ( i)      0 c, ( j)      0 c, ( k)
     "l" c, ( l)      0 c, ( m)      0 c, ( n)    "o" c, ( o)
       0 c, ( p)      0 c, ( q)    "r" c, ( r)    "s" c, ( s)
       0 c, ( t)    "u" c, ( u)      0 c, ( v)      0 c, ( w)
     "x" c, ( x)      0 c, ( y)      0 c, ( z)      0 c, ( {)
       0 c, ( |)      0 c, ( })      0 c, ( ~)      0 c,
 
\ ---[ Parser interface ]-------------------------------------------------------

variable sym

1 chars constant 1char

: test-token  ( n -- f )  sym @ = ;

0 value first-set    \ Declared, actual value set by the parser

big-endian 16bits? or [if]
   : testsym?  ( set-index -- f ) \ ad is first-set
      first-set +                   ( -- ad )
      sym @ bits/au /mod rot + c@   ( -- bit byte )
      1 rot lshift and              ( -- f )
   ;
[else]
   : testsym? ( set-index -- f )
      first-set + @                 ( -- vec )
      1 sym @ lshift and
   ;
[then]

2variable $control   \ Current format control string
variable spf->in     \ Position reached in the control string before THROW

: spf-throw  ( i*x -- j*x #exc | i*x 0 )
   >in @ spf->in !
   throw
;

: format-type  ( -- c )
   output-selector
   case
      >mem of  'S' endof
      >mon of   bl endof
      >file of 'F' endof
   endcase
;

: report-error  ( caddr u -- )
   s" Format string: " dup >r cr type 
   '"' emit $control 2@ type '"' emit cr
   spf->in @ 2 - r> + spaces ." ^ "
   cr format-type emit ." PRINTF error: " type cr
;

: next-char  ( -- ch )   \ ch = -1 is end of line
   source >in @ tuck >
   if
      chars + c@ 1 >in +! exit
   then
   2drop -1
;

\ NEXTSYM sets sym to -1 for end of format string or 0 for invalid character or
\ character out of range

: nextsym  ( -- )
   next-char dup 0< if 0< sym ! exit then
   bl -                       \ Map 32..127 to 0..95
   dup 95 u> if drop 95 then  \ Out of range, set sym to 0 via char-map
   char-map + c@ sym ! 
;

: ?nextsym  ( f -- )  0= if invalid-char spf-throw then nextsym  ;

\ PREV-CHAR needed on parser exit as parser has already called NEXT-CHAR

: prev-char  ( -- )  sym @ 0< 0= >in +!  ;

\ Required for the BNF parser

: open-source ;
: close-source ;


0 constant [skip-whitespace]
32 constant bits/cell
defer parse-format
\ sprintf.fth provides sprintf functionality for ANS Forth/Forth 2012

\ ---[ The stack frame ]--------------------------------------------------------

\ The sprintf format/control string is a series of format specifiers. In BNF:
\     <format-string> ::= (<fixed-text>? ('%' <format-spec>)?)*
\ There is a pass through the format string recording the details of
\ the format specifier in a set of frames (objects) on the data stack. On
\ completion of this pass, The stack frames are processed in order, the oldest
\ (deepest) first. In this way the sprintf arguments are in the same left to
\ right order as the format specifiers in the control string.
\ The fields of the frame objects are:

begin-frame
   2field text          \ Text before this conversion
   field  width         \ Width of the conversion
   field  prec          \ Precision to be used
   field  arg-num       \ Argument number (not FP)
   field  fp-arg-num    \ FP argument number
   field  flags         \ Conversion flags, 1 bit per flag
   field  convert-xt    \ xt of an arguments convert definition
   field  prefix-xt     \ xt of a conversions prefix definition
end-frame conversion-size

variable conversions    \ Reference of first frame for conversion specifications
: set-conversions  ( -- )  set-frame frame @ conversions !  ;

variable arguments      \ Reference of the arguments frame

: set-arg-frame  ( #args -- )
   frame @ swap - dup arguments !
   min-frame < too-few-args and spf-throw
;

\ SET-ARGUMENTS is for floating point - handling infinity or not a number,
\ where a +/-INF or NAN argument + conversion frame is needed 
: set-arguments  ( -- )  frame @ >r set-frame frame @ arguments ! r> frame !  ;

: get-arg  ( n -- x )
   arguments @ frame !
   arg
   conversions @ frame !
;

: get-2arg  ( n -- x1 x2 )  dup get-arg swap 1+ get-arg  ;

\ ---[ Floating Point arguments ]-----------------------------------------------

fp-enabled [if]
\ These have to be handled differently to other arguments in a standard program
\ because there is no standard word such as FPICK and, apparently, no efficient
\ way to define one. Therefore the approach adopted is: following processing of
\ the SPRINTF control string when the number of FP arguments is known, the FP
\ arguments are moved from the FP stack into a FP array. When the SPRINTF output
\ string is generated the FP arguments are accessed directly from the array.

1 floats constant 1float

create fp-args max-fp-args floats allot

\ The FP args getter and setter are provided with an index i which is already
\ scaled by FLOATS in the parser
: get-fp-arg  ( i -- )  ( F: -- r )  fp-args + f@  ;
: set-fp-arg  ( i -- )  ( F: r -- )  fp-args + f!  ;

variable fp-depth

: move-fp-args  ( i -- )  ( F: r*x ri-1 ... r0 -- r*x )
   dup max-fp-args 1- floats > too-many-fps and spf-throw
   fdepth floats over < too-few-fpargs and spf-throw
   begin 1float - dup 0< 0= while dup set-fp-arg repeat drop
   fdepth fp-depth !    \ To ensure correct depth after a THROW
;

\ DROP-FP-ARGS is needed because a final THROW will have restored the FP
\ stack pointer to include the FP arguments.
: drop-fp-args  ( r*x rn ... r0 -- r*x )
   fdepth fp-depth @ - 0 ?do fdrop loop
;
[then]

\ ---[ Conversion flags and their operations ]----------------------------------

\ Note that each flag has the value of a power of 2 because the FLAGS word
\ is a bit vector, 1  bit per flag. 
\ Do *NOT* change:
\  - the value of 0flag as it is used in LEFT-PADDING to convert a BL
\    character to a '0' using OR

\ When development is complete see which flag+ definitions are not used and
\ remove them (comment out)

1
dup constant +flag@     \ = 1
: +flag  ( -- 2^n|0 )  +flag@ flags and  ;
: +flag+ ( n1 -- n2 )  +flag@ or  ;
2*
dup constant blflag@    \ = 2
: blflag  ( -- 2^n|0 )  blflag@ flags and  ;
: blflag+ ( n1 -- n2 )  blflag@ or ;
2*
dup constant #flag@     \ = 4
: #flag  ( -- 2^n|0 )  #flag@ flags and  ;
: #flag+ ( n1 -- n2 )  #flag@ or ;
2*
dup constant leftflag@  \ = 8
: leftflag  ( -- 2^n|0 )  leftflag@ flags and  ;
: leftflag+ ( n1 -- n2 )  leftflag@ or ;
2*
dup constant 0flag@     \ = 16
: 0flag  ( -- 2^n|0 )  0flag@ flags and  ;
: 0flag+ ( n1 -- n2 )  0flag@ or ;
2*
dup constant precflag@  \ = 32
: precflag  ( -- 2^n|0 )  precflag@ flags and  ;
: precflag+ ( n1 -- n2 )  precflag@ or ;
2*
dup constant ucflag@    \ = 64
: ucflag  ( -- 2^n|0 )  ucflag@ flags and  ;
: ucflag+ ( n1 -- n2 )  ucflag@ or ;
2*
dup constant caseflag@  \ = 128
: caseflag  ( -- 2^n|0 )  caseflag@ flags and  ;
: caseflag+ ( n1 -- n2 )  caseflag@ or ;
2*
dup constant longflag@  \ = 256
: longflag  ( -- 2^n|0 )  longflag@ flags and  ;
: longflag+ ( n1 -- n2 )  longflag@ or ;
2*
dup constant numflag@   \ = 512
: numflag  ( -- 2^n|0 )  numflag@ flags and  ;
: numflag+ ( n1 -- n2 )  numflag@ or ;
2*
dup constant %fflag@    \ = 1024
: %fflag   ( -- 2^n|0 )  %fflag@ flags and  ;
: %fflag+  ( n1 -- n2 )  %fflag@ or  ;
drop

\ ---[ Parser actions ]---------------------------------------------------------

: decimal-number  ( -- u )       \ Assume decimal base already
   0 0 source >in @ 1- /string         ( -- ud caddr1 u1 )
   >number                             ( -- ud2 caddr2 u2 )
   source rot - >in ! 2drop drop       ( -- u )
;

1 constant fp-size

: long-size?  ( -- 1 | 2)  \ 2 if long flag is set
   longflag 0= 2 + 
;

\ CLEAR-0FLAG clears the zero flag bit for %s %c %% conversions to inhibit
\ leading 0s if precision > converted string length

: clear-0flag  ( flags -- flags' )  0flag@ invert and  ;
: clear-0flag? ( flags -- flags' )  precflag if clear-0flag then ;

\ PRECISION? sets the default precision to 6 for floating point conversions

: precision?  ( prec n1 n2 flags -- prec' n1 n2 flags )
   precflag 0= if 2>r nip 6 swap 2r> then
;

\ ---[ Conversion types ]-------------------------------------------------------

\ Each conversion has two functions whose xts are in the conversion frame:
\  - to return a prefix for that conversion
\  - to convert the argument
\ Some, such as NULL$ to return no prefix, are shared

\ ---[ String %s conversion ]---------------------------------------------------

: null$  ( sign -- caddr 0 )  drop s" "  ;

: %s  ( -- caddr u sign )
   arg-num get-2arg
   precflag if prec min then 0    \ No sign
;

\ ---[ Decimal %d conversion ]--------------------------------------------------

\ Get integer argument, single or double, f true for signed, false unsigned
: int-arg  ( f -- d|ud )
   arg-num
   longflag if nip get-2arg exit then
   get-arg swap if s>d else 0 then
;

: zero-arg?  ( d -- f ) \ f=true if both argument and precision are zero 
   or if 0 exit then
   prec 0=
;

\ The prefix for the # flag is either '#' for +ve numbers or '#-' for negative
\ numbers. '#+' is not used as the Forth outer interpreter does not necessarily
\ recognise suxh a prefix.
: #prefix?  ( sign -- caddr u ) \ or ( sign -- 0 )
   #flag
   if
      if s" #-" else s" #" then exit
   then
   drop 0
;

: sign-prefix  ( sign -- caddr u )
   case   ( sign )
                       -1 of s" -" endof  ( -1 -- caddr u |  0 )
      +flag =           0 of s" +" endof  (  0 -- caddr u | -1 )
      drop blflag blflag@ of s"  " endof  ( -1 -- caddr u | 0 )
      s" " rot                      (  0 -- caddr 0 0 )
   endcase                          (  x -- caddr u )
;

: %d-prefix  ( sign -- caddr u )    \ Sign is 0 (>=0) or -1 (<0)
   true int-arg or 0= if null$ exit then  \ No prefix if arg=0
   dup #prefix? ?dup if rot drop exit then
   sign-prefix
;

\ Note that UD>STR returns null$ if both the argument and precision are zero,
\ in this case we rely on the precision to generate a zero string.
\ If the argument is zero and the precision is not zero we rely on <# #s #>
\ to generate a zero string.
\ Of course if the argument is not zero <# #s #> does the conversion.
: ud>str  ( d base -- caddr u )
   base ! 2dup zero-arg?
   if 2drop 0 null$ else <# #s #> then
   decimal         \ No sign
;

: %d  ( -- caddr u sign )  true int-arg dup 0< >r dabs #10 ud>str r>  ;

\ ---[ Unsigned %u conversion ]-------------------------------------------------

: unsigned-prefix  ( caddr u -- caddr u|0 )
   false int-arg zero-arg?
   #flag 0= or
   if drop 0 then
;

: %u-prefix  ( sign -- caddr u )  drop s" #" unsigned-prefix  ;
: %u  ( -- caddr u sign )  false int-arg #10 ud>str 0  ;    \ No sign

\ ---[ %x and %X conversions ]--------------------------------------------------

: %x-prefix  ( sign -- addr u )  drop s" $" unsigned-prefix  ;
: %x  ( -- caddr u sign )  false int-arg #16 ud>str 0  ;    \ No sign

\ ---[ %o conversion ]----------------------------------------------------------

: %o-prefix  ( #prec0s sign -- #prec0s caddr u )
   drop dup 0> false int-arg or 0= or
   if 0 null$ exit then         \ No '0' prefix if 0  or '0' to be prefixed
   s" 0" unsigned-prefix
;
: %o  ( -- caddr u sign )  false int-arg #8 ud>str 0  ;    \ No sign

\ ---[ %b conversion ]----------------------------------------------------------

: %b-prefix  ( sign -- addr u )  drop s" %" unsigned-prefix  ;
: %b  ( -- caddr u sign )  false int-arg #2 ud>str 0  ;    \ No sign

\ ---[ %r and %R conversions  ]-------------------------------------------------
\ Radix conversion is signed if the + flag or ' ' flag is set, else unsigned.
\ This is a way to output, say, signed hex integers using radix 16

: >negative?  ( caddr1 u1 sign caddr2 u2 -- caddr3 u3 )
   true int-arg or 0= if 2drop 2drop 0 exit then  ( -- caddr1 0 ) \ if arg = 0
   2>r over and
   if 2drop 2r> exit then  \ Negative argument return (caddr2 u2)
   2r> 2drop               \ Return (caddr1 u1) 
;

: %r-#prefix  ( sign -- caddr u )
   arg-num 1- get-arg          ( -- sign base )
   case
      #10 of %d-prefix endof
      #16 of dup %x-prefix rot s" $-" >negative? endof
      #8  of dup %o-prefix rot s" -0" >negative? endof
      #2  of dup %b-prefix rot s" %-" >negative? endof
      drop sign-prefix 0
   endcase
;

: %r-prefix  ( sign -- caddr u )
   #flag if %r-#prefix else sign-prefix then
;

: %r ( -- caddr u sign )
   +flag blflag+ dup int-arg rot       ( -- d flags )
   if dup 0< >r dabs else 0 >r then    ( -- d' ) ( R: -- sign )
   arg-num 1- get-arg ud>str r>
;

\ ---[ %c conversion ]----------------------------------------------------------

\ Extensions to %c are:
\ - if the # flag is present the character is output in character literal
\   format e.g. 'x'
\ - if a precision p is specified, the character is repeated p times
\   e.g. xxx for p=3. A precision of 0 outputs nothing.
\ The precision is ignored if the #flag is set

\ GET-W|P  n1 is value of width or precision field, n2 the width or precision
\ If * was used for width or precision then n1 is negative and the argument
\ number is -n1-1. If n2 is negative, which can only happen if * was used, then
\ it is set to zero.

: get-w|p  ( n1 -- n2 )  dup 0< if 1+ negate get-arg 0 max then  ;

: nchar>pad  ( ch u -- caddr u )
   <# 0 ?do dup hold loop dup #>
;

: %c  ( -- caddr u sign )
   false int-arg drop   ( -- ch )
   #flag    \ # flag outputs the character as a Forth character literal
   if
      <# ''' hold hold ''' hold 0 0 #> 0 exit       \ No sign
   then
   precflag if prec get-w|p 0 max max-conv-prec min else 1 then 
   nchar>pad 0     ( -- caddr u )    \ No sign
;

\ ---[ %% conversion ]----------------------------------------------------------

: %%  ( -- caddr 1 sign )  '%' 1 nchar>pad 0  ;    \ No sign 

\ ---[ Handling floating point 'infinity' and 'not a number']-------------------
fp-enabled [if]
\ This relies on REPRESENT returning a failure flag and a string containing
\ "INF" or "NAN". A failure flag without INF or NAN will result in a default
\ string "BAD". The string will be displayed in place of a FP number in the
\ appropriate case in the specified format (width, sign etc)

: $>lower  ( caddr u -- )
   over + swap ?do i c@ char>lower i c! loop
;

: get-invalid  ( caddr1 u1 caddr2 u2 -- [caddr1 u1 false] | true )
   search if 2drop -1 else 0 then
;

\ It is convenient to output "INF" etc using a base 24 radix conversion where
\ "INF" etc are encoded as base 24 integers. Hence the following constants.

\ Constants for outputting invalid floatint point numbers
#24 base !
inf constant fp-inf     \ #10935 for reporting +/- infinity
nan constant fp-nan     \ #13511 for reporting not a number
bad constant fp-bad     \ #6589  for reporting invalid number of unknown type
decimal

: invalid-number  ( caddr u -- n )
   s" inf" get-invalid if fp-inf exit then
   s" nan" get-invalid if fp-nan exit then
   2drop fp-bad
;

: %rfp-prefix  ( sign -- caddr u )
   if s" -" else s" " then
;

: new-radix-frame  ( -- caddr u width prec argnum fp-argnum flags xt1 xt2 )
   sprf-pad 0 width 1 1 0
   flags [ leftflag@ ucflag+ ] literal and
         [ numflag@ caseflag+ +flag+ ] literal or
   ['] %r ['] %rfp-prefix 
;

: set-radix-frame  ( n -- )  \ n is no. of arguments = 2
   arguments @ + dup conversions ! frame !
;

\ MAKE-INVALID-STRING will be set to (BUILD-STRING) eventually
defer make-invalid-string

: bad-fp-number  ( exp sign -- ?? )
   nip sprf-pad prec max-precision min 2dup $>lower
   invalid-number swap over fp-nan < and if negate then  ( -- arg )
   conversions @ arguments @ 2>r
   >r set-arguments r> 24 swap  ( -- radix arg )  \ Set frame pointer to arguments
   new-radix-frame  
   2 set-radix-frame
   1 make-invalid-string
   2r> arguments ! conversions !
   invalid-fp-number throw           \ Successful return to BUILD-STRING
;

\ ---[ %f conversion ]----------------------------------------------------------

: round?  ( n -- f )   \ True if rounding is required
   prec + max-precision <
;

: (round)  ( caddr -- )    \ caddr = sprf-pad+u (can't use a DO ... -1 +LOOP)
   begin
      dup c@ dup '9' =     ( -- caddr ch f )
   while
      drop '0' over c!     ( -- caddr )
      1- dup sprf-pad u<
   until
      drop                 ( -- )
   else
      1+ swap c!           ( -- )
   then
;

: round  ( exp caddr -- exp|exp+1 )
   dup c@                     ( -- exp caddr ch )
   '5' < if drop exit then
   dup sprf-pad =
   if
      '0' swap c!   \ To handle %f cases where (e.g.) 0.0089 rounds to 0.01
   else
      1- (round)
   then
   sprf-pad c@ '0' =
   if          \ 99...9 -> 100...0 or 0.0089 -> 0.01, exp -> exp+1
      sprf-pad max-precision '0' fill
      '1' sprf-pad c! 1+     ( -- exp+1 )
   then
;

\ The output for a %f conversion can take the following forms:
\   1230000.000000      for 1.23E6 and default precision = 6
\   123.0000            for 1.23E2 and default precision = 6
\   123                 for 1.23E2 and precision = 0
\   1.230000            for 1.23E0 and default precision = 6
\   1.23                for 1.23E0 and precision = 2
\   0.123               for 1.23E-1 and precision = 3
\   0.00000123          for 1.23E-6 and precision = 8
\ Also there may be leading zeros due to a width specification (but this is
\ ignored here as that is handled by as for other conversions in (BUILD-STRING)

\ Hence generation of output for the %f conversion is broken down into the
\ following components:
\    <integer-digits><integer-0s>. <fractional-0s><fractional-digits><end-0s>
\ The following words calculate these components which depend on the values of
\ the exponent and precision. Any of the components may be zero for particular
\ FP numbers.

: int-digits  ( exp -- n2 )  \ n is number of integer digits
   max-precision min 0 max
;

: int-0s  ( exp -- n )   \ n is number of 0's left of decimal point
   dup 0> if max-precision - 0 max exit then
   drop 1            \ Will be 0.xxxxxx where x = 0|digit1..9
;

: frac-0s  ( n -- n2 )  \ n2 is number of 0's right of decimal point
   dup 0< if negate prec min exit then
   drop 0
;

: frac-digits  ( frac0s n -- frac0s n2 ) \ n2 is number of fractional digits
   dup round? if drop prec over - 0 max exit then
   0 max max-precision swap - 0 max
;

: end-0s  ( frac0s #fdigits -- n2 )   \ n2 is number of 0's to meet precision
   + prec swap - 0 max
;

\ Save a decimal point if precision <> 0 or the # flag present
: dec-point?  ( prec -- )
   #flag or if fp-sep hold then
;

\ For floating point conversions FP-REPRESENT calls REPRESENT to get the
\ character sequence of the FP number into SPRF-PAD

: fp-represent  ( -- exp sign )
   sprf-pad max-precision 1+ 1+ '0' fill    \ To avoid rounding problems
   fp-arg-num get-fp-arg sprf-pad max-precision represent    ( -- exp sign f )
   0= if bad-fp-number then
;

\ For %f & %g conversions it is convenient to build up the output in the
\ pictured output buffer. With large exponents and/or large values of precision
\ this can lead to long runs of zeroes that can cause buffer overflow. Instead
\ of providing another large buffer, such runs of zeroes are run length encoded
\ by holding a 0 (NUL) followed by the number of zeroes (a total of 2 chars).
\ The zero count, #RLE, is limited to 64 so, for example, a run of 123 would
\ take 4 character positions. The value 64 is chosen to avoid a larger number
\ being converted by case conversion e.g. 65 is 'A'.

sprf-pad-size constant #rle  \ As SPRF-PAD is used to expand the RLE number

: hold0s  ( n -- )    \ Run-length encode n 0's as 0 n
   begin
      dup 0>
   while
      dup #rle min hold 0 hold  \ HOLD goes from right to left (reverse order)
      #rle -
   repeat drop
;

: nsum  ( ni ... n1 i -- ni ... n1 sum )  \ sum of n1 to ni
   0 swap 1+ 1 do i pick + loop
;

: %f-convert  ( #idigits #int0s #frac0s #frac-digits #end0s -- caddr u )
   3 nsum >r                     \ u2 length of fraction 
   5 nsum r@ #flag or 0> -       \ u1 total length including decimal point
   r> 2>r                        ( R: -- u1 u2 ) 
   <#
      hold0s                     \ End 0's
      3 pick sprf-pad + swap holds    \ Fraction digits
      hold0s                     \ Fraction 0's
      r> dec-point?              \ No '.' if fraction length = 0
      hold0s                     \ Integer 0's
      sprf-pad swap holds             \ Integer digits
      0 0                        \ Double integer needed by #>
   #>                            ( -- caddr u )
   drop r>                       ( -- caddr u2 )
;

: (%f1)  ( exp -- #idigits #int0s )
   dup int-digits swap int-0s
;

: %f  ( -- caddr u sign )
   fp-represent >r         ( -- exp sign )  ( R: -- sign )
   dup round?              
   if sprf-pad over + prec + round then
   dup >r (%f1)            ( -- #idigits #int0s )
   r@ frac-0s              ( -- #idigits #int0s #frac0s )
   r> frac-digits          ( -- #idigits #int0s #frac0s #fdigits )
   2dup end-0s             ( -- #idigits #int0s #frac0s #fdigits #end0s )
   %f-convert r>           ( -- caddr u sign )
;

\ ---[ %e and %E conversions ]--------------------------------------------------

\ RLE-  subtract 2 bytes for every RLE occurrence in the end zeroes string
: rle-  ( n1 -- n2 )
   dup 2 > if dup [ #rle 1- ] literal + #rle / 2* - 0 max then
; 

: (%e)  ( exp rpos #end0s -- caddr u )  \ rpos is rounding position in SPRF-PAD
   swap 1- 0 max >r >r              ( R: -- rpos' #end0s )
   <#
      1- dup abs dup 0 #s 2drop     ( -- exp-1 )
      #10 < if '0' hold then        \ Ensure at least 2 digits for exponent
      0< if '-' else '+' then hold  ( -- )
      'e' hold
      r> 0 max dup hold0s           ( -- #end0s | 0 )
      sprf-pad 1+ r@ holds
      r> dec-point?
      sprf-pad c@ hold
      0 0                           \ Needed for #> to drop
   #>                               ( #end0s caddr u )
   rot rle- +                       ( -- caddr u )
;

: %e  ( -- caddr u sign)
   fp-represent >r            ( -- exp )  ( R: -- sign )
   prec max-precision min 1+  ( -- exp rpos )
   dup >r sprf-pad + round         ( R: -- sign rpos )
   r> prec over - 1+ (%e) r>  ( -- caddr u sign )
;

\ ---[ %g and %G conversions ]--------------------------------------------------

: -trailing0's  ( caddr u1 -- caddr u2 )
   begin
      dup
   while
      1- 2dup + c@
      '0' <>
   until
      1+
   then
;

: -trailing0s?  ( caddr u -- u2 )
   #flag 0= if -trailing0's then nip
;

: %gf-end0s  ( #idig #int0s #frac0s #fdig -- #idig #int0s #frac0s #fdig #end0s)
   #flag if 4 nsum prec swap - 0 max else 0 then
;

: %ge-end0s  ( rpos -- rpos #end0s )
   prec over - #flag 0<> and
;

: %g  ( -- caddr u )
   fp-represent >r                     ( -- exp )  ( R: -- sign )
   prec 1 max max-precision min   ( -- exp rpos )
   dup >r sprf-pad + round
\   sprf-pad r@ + max-precision r@ - '0' fill   \ Shouldn't be necessary but harmless
   sprf-pad r> -trailing0s? 1 max           ( -- exp u )
   over 1- -4 prec within
   if
      >r dup >r (%f1)            ( -- #idig #i0s ) ( R: -- u exp )
      r> frac-0s r> over 0=      ( -- #idig #i0s #f0s u f ) \ f = #f0s=0 
      if
         3 pick - 0 max          ( -- #idigits #int0s #frac0s #fdigits )
      then
      %gf-end0s %f-convert        ( -- caddr u )
   else
      %ge-end0s (%e)             ( -- caddr u )
   then
   r>                         ( -- caddr u sign )
;
[then]   \ End of fp-enabled

\ ---[ SPRINTF words ]----------------------------------------------------------
\ SAVE-RLE-STRING saves a run length encoded number of zeros
: save-rle  ( caddr u -- n ) \ If u>0, caddr points to 0 byte else an error
   1- 0< rle and throw
   char+ c@ sprf-pad over concat$      ( -- n )
   2 -
;

: save-rle-string  ( caddr u -- )
   sprf-pad sprf-pad-size '0' fill
   over >r     ( R: -- caddr )
   begin
      dup 0>   \ 0> needed because length may be negative
   while
      over c@ 0=     ( -- caddr2 u2 ch )
      if
         over r> tuck - concat$   ( -- caddr2 u2 )
         2dup save-rle -          ( -- caddr2 u2 )
         1 /string
         over 1+ >r               ( R: -- caddr2+2 )
      then
      1 /string   ( -- caddr' u' )
   repeat drop
   r> tuck - 
;

\ The output for a conversion consists of a subset of these fields:
\ <left spaces> <prefix> <wzeros> <pzeros> <conversion> <right spaces>
\ where:
\    <left spaces>  - (u) for right justified, no 0 flag, to meet width
\    <prefix>       - (caddr u) for any sign, base prefix
\    <wzeros>       - (u) for right justified, 0 flag, to meet width
\    <pzeros        - (u) zeros to meet precision for integer conversions
\    <conversion>   - (caddr u) the converted value
\    <right spaces> - (u) for left justified
\
\ Only one of <left-spaces>, <wzeros> and <right spaces>can be non-zero
\ depending on leftflag 0flag.  All three may be 0 for a given conversion.
\ Using a frame for these fields saves some stack juggling as the fields are
\ calculated in turn. However not all the above fields need to be in the
\ frame as they are in the correct place to be consumed

begin-frame
   2field conversion$   \ The converted value
   field  #prec0s       \ Leading zeros to meet precision
   2field prefix$       \ The prefix
end-frame parfrsize     \ Size not used

\ #prec0s = max(precision - u, 0) where u is the converted string length

: get-#prec0s  ( u -- #prec0s )
   prec get-w|p swap - numflag 0<> and
   0 max max-conv-prec min
;

\ If non-zero the values of #rpad #left0s #lpad are set to len where
\       len = max(width - u1 - u2 - #prec0s, 0)
\ where u1 is converted string length
\       u2 is the prefix string length
\       #prec0s is defined above

: get-padding  ( prefix$ width 0flag leftflag -- prefix$ #rsp #left0s #lsp )
   2>r
   over - conversion$ nip - #prec0s - 0 max  ( -- prefix$ len )
   r> if
         r> drop 0 0    ( prefix$ #rpad 0 0 )      \ leftflag set
      else
         0 swap 0 r>
         if exit then   ( -- prefix$ 0 #left0s 0 ) \ 0flag set
         swap           ( -- prefix$ 0 0 #lpad )
      then
;

: save-conversion  ( caddr u -- )   \ Save the converted argument
   caseflag
   if 
      ucflag if str>upper else str>lower then
   then
   %fflag if save-rle-string then
   concat$
;

\ PROCESS-FORMAT analyses the sprintf control string (in an input source)
\ to create a series of conversion frames on the stack, one frame per
\ conversion specification.
\ On top of the stack 
\ (caddr u) - is the end of the control string after the last conversion
\             specification.
\ #frames   - number of conversion frames (and sprintf arguments).
\ argnum    - number of the next argument, the final value is the size of
\             all arguments
\ fpargnum  - the number of FP arguments scaled by FLOATS so that it can act
\             as an index into the fp-args array
\ Note that the whole control string has to be processed and the conversion
\ frames created because the number and type (and therefore size) of the
\ arguments is unknown until the end of the control string has been reached. 
\
\ Note the test before the WHILE that detects that a '%' has not been found.
\ An incorrect possible alternative is to do
\        '%' PARSE 2DUP CHARS + C@ '%' =
\ but as EXECUTE-PARSING copies the string to a buffer there is always the
\ (remote) possibility of a spurious '%' being found just beyond the end of
\ the buffer.
 
: process-format  ( -- frames* caddr u #frames argnum fpargnum )
   set-conversions
   0 >r 0 0 2>r            ( R: -- #args argnum fpargnum )
   begin
      set-frame
      '%' parse            ( -- ca u )
      2dup + source + <
   while
      -1 >in +!
      2r> parse-format  ( -- ca u wid prec argnum flags type argnum' fpargnum' )
      r> 1+ >r 2>r      ( R: -- #args' argnum' fpargnum' )
   repeat
   2r> r> -rot
   conversions @ frame !                   
;

\ BUILD-STRING and (BUILD-STRING) use the conversion frames from process-format
\ to create the sprintf result string. They start from the first conversion
\ frame which is deepest on the stack. See PROCESS-FORMAT 
\ In the stack effects below "..." is shorthand for "args* frames* caddr u"
\ which are used and remain on the stack

: (build-string)  ( args* frames* ca u conversion$ sign --  args* frames* ca u )
   over get-#prec0s        ( -- ... conversion$ sign #prec0s )
   swap prefix-xt execute  ( -- ... conversion$ #prec0s prefix$ )
   width get-w|p max-conv-width min 0flag leftflag
   frame @ >r set-frame -8 frame +!    \ new frame starts at conversion$
   get-padding             ( -- ... conversion$ #prec0s prefix$ #rsp #l0s #lsp )
   bl concat-chars         ( -- ... conversion$ #prec0s prefix$ #rsp #l0s )
   prefix$ concat$
   #prec0s + '0' concat-chars    ( -- ... conversion$ #prec0s prefix$ #rsp )
   conversion$ r> frame !        \ Return to conversion frame
   save-conversion
   bl concat-chars               ( -- ... conversion$ #prec0s prefix$ )
;

\ (caddr u) is the string after the last Format specification
: build-string  ( args* frames* caddr u n1 --  args* frames* caddr u )
   0 ?do
       text concat$        \ Save preceding text
       convert-xt catch ?dup    ( -- ... caddr2 u2 sign [0 | #exc #exc] )
       if
          dup invalid-fp-number <> if spf-throw then
          drop
       else
          (build-string)
       then 
       conversion-size conversions +! conversions @ frame !
     loop
;

fp-enabled [if]
   ' build-string is make-invalid-string
[else]
   : move-fp-args  ( i -- )  drop  ;
   : drop-fp-args  ( -- )  ;
[then]

: ((format$))  ( args* -- args* frames* )
   process-format move-fp-args set-arg-frame
   build-string text concat$
;

: (format$)  ( args* caddr u -- args* #exc ) \ Frames dropped due to THROW
   ['] ((format$)) execute-parsing spf-good throw
;

: check-throw-code  ( #exc -- )
   dup spf-good = if drop exit then
   case
      invalid-char   of s" Invalid character" endof
      too-few-args   of s" Too few arguments on the stack" endof
      too-few-fpargs of s" Too few arguments on the FP stack"  endof
      too-many-fps   of s" FP-ARGS array too small" endof
      buf-overflow   of s" Output buffer overflow" endof
      rle            of s" Run length encoding error" endof
      no-fp          of s" Invalid conversion - no floating point" endof
      abort
   endcase
   report-error abort
;

\ FORMAT$ interpets the format control string (caddr u). It is called by the
\ three user words FPRINTF, SPRINTF and FPRINTF to create the output string.
\ The arguments to be inserted into the control string must be on the stack
\ immediately below (caddr u)
 
: format$  ( arg* caddr u -- )
   base @ >r decimal
   2dup $control 2!
   ['] (format$) catch
   r> base ! check-throw-code
   arguments @ frame ! drop-frame drop-fp-args
;

\ ---[ PRINTF SPRINTF and FPRINTF ]---------------------------------------------
\ The User interface

user-definitions

\ Neither PRINTF or FPRINTF use an output buffer beyond those already provided
\ by the Sprintf package. Therefore the user is responsible for creating the
\ SPRINTF output buffer for SPRINTF by using ALLOT or ALLOCATE. SPRINTF-BUFFER
\ is provided to initialise the buffer. caddr is the buffer start address 

: set-sprintf-buffer  ( caddr size -- )
   sprintf-words init-sprbuf user-definitions
;

: sprintf  ( arg* caddr u -- caddr2 u2 )
   sprintf-words
   sprbuf
   if
      >mem to output-selector    \ The result is left in the buffer SPRBUF
      sprbuf-clear format$ sprbuf$
   else
      $control 2! 3 spf->in !
      s" No sprintf buffer set, use SET-SPRINTF-BUFFER" report-error
   then
   user-definitions
;

: printf  ( arg* caddr u -- )
   sprintf-words
   >mon to output-selector format$     \ Output directly to the display
   user-definitions
;

: fprintf  ( arg* caddr u fid -- )
   sprintf-words
   to pf-fileid
   >file to output-selector format$    \ Output to the file with fid
   user-definitions
;

sprintf-definitions


: spf_flags
   0
   begin
      0 testsym?
   while
      2 test-token
      if
         nextsym leftflag+
      else
         4 testsym?
         if
            3 test-token
            if
               nextsym 0flag+
            else
               4 test-token ?nextsym +flag+
            then
         else
            5 test-token
            if
               nextsym blflag+
            else
               6 test-token ?nextsym #flag+
            then
         then
      then
   repeat
;

: spf_wpnumber
   8 testsym?
   if
      drop decimal-number 3 test-token
      if
         nextsym
      else
         7 test-token ?nextsym
      then
   else
      8 test-token ?nextsym drop 1+ dup negate
   then
;

: spf_width
   2>r 0 12 testsym?
   if
      spf_wpnumber
   then
   swap 2r>
;

: spf_precision
   2>r 1 9 test-token
   if
      nextsym 2r> precflag+ 2>r 1- 12 testsym?
      if
         spf_wpnumber
      then
   then
   swap 2r>
;

: spf_long
   10 test-token ?nextsym longflag+
;

: spf_type
   16 testsym?
   if
      20 testsym?
      if
         numflag+ clear-0flag? 12 test-token
         if
            nextsym ['] %d ['] %d-prefix
         else
            24 testsym?
            if
               20 test-token
               if
                  nextsym ['] %u ['] %u-prefix
               else
                  18 test-token ?nextsym ['] %o ['] %o-prefix
               then
            else
               23 test-token
               if
                  nextsym ['] %b ['] %b-prefix
               else
                  caseflag+ numflag+ 28 testsym?
                  if
                     21 test-token
                     if
                        nextsym
                     else
                        ucflag+ 22 test-token ?nextsym
                     then
                     ['] %x ['] %x-prefix
                  else
                     2>r 1+ 2r> 24 test-token
                     if
                        nextsym
                     else
                        ucflag+ 25 test-token ?nextsym
                     then
                     ['] %r ['] %r-prefix
                  then
               then
            then
         then
         long-size? 0
      else
         32 testsym?
         if
            clear-0flag 19 test-token
            if
               nextsym ['] %s 2
            else
               11 test-token ?nextsym ['] %c 1
            then
            ['] null$ swap 0
         else
            fp-enabled [if] precision? 15 test-token
            if
               %fflag+ nextsym ['] %f
            else
               caseflag+ %fflag+ 36 testsym?
               if
                  13 test-token
                  if
                     nextsym
                  else
                     ucflag+ 14 test-token ?nextsym
                  then
                  ['] %e
               else
                  16 test-token
                  if
                     nextsym
                  else
                     ucflag+ 17 test-token ?nextsym
                  then
                  ['] %g
               then
            then
            ['] sign-prefix 0 1float [else] no-fp spf-throw [then]
         then
      then
   else
      1 test-token ?nextsym drop 2>r 2drop 0 1 2r> 0 ['] %% ['] null$ 0 0
   then
   arg-num fp-arg-num d+
;

: format_string
   1 test-token ?nextsym spf_flags spf_width spf_precision 10 test-token
   if
      spf_long
   then
   spf_type prev-char
;

: ~ 0 0 parse-name >number 2drop drop 4 0 do dup c, 8 rshift loop drop ;
here to first-set
base @ hex
~       7C
~       18
~       88
~      188
~  3FFF800
~  3F41000
~   140000
~   600000
~    80800
~     6000
base !

:noname
   open-source nextsym format_string close-source
; is parse-format


user-definitions

\ ---[ End of sprintf.fth ]-----------------------------------------------------
