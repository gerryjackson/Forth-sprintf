\ Run the sprintf test program

s" sprintf.fth" included
s" tester.fr" included
decimal

\ sprintf test program

sprintf-words

\ ---[ Test helpers ]-----------------------------------------------------------

create (buf) 128 allot
variable blen

: >buf  ( caddr u -- )  dup blen ! (buf) swap cmove ;

\ *** Do not use B" inside a colon definition - it wasn't intended to be
\ *** compiled and so doesn't work

: b"  ( -- caddr u )    \ Parse and copy a string to (buf)
   (buf) '"' parse tuck >buf
;

: buf$  ( -- caddr u )  (buf) blen @ ;
: buf$+  ( u1 --  caddr u2 )  buf$ rot /string ;
\ Note: 0 buf$+ is equivalent to buf$

: .stack  ( -- )
   depth ?dup 0>
   if
      ." Stack contents: ( "
      dup 6 > if drop 6 ." ... " then
      0 swap                  ( -- xn ... x0 0 n )
      ?do
         i ?dup 0>
         if 1- pick . then
         -1
      +loop      
      ')' emit
      depth 6 > if ."  Depth: " depth . then
   else
      ." Stack empty"
   then
;

fp-enabled
[if]
   : .fpstack  ( -- )  ." FP stack depth: " fdepth 6 .r  ;
[else]
   : .fpstack  ( -- )  ." No floating point"  ;
[then]

: >lower  ( caddr u -- caddr u ) \ Digits converted to lower case
   2dup over + swap
   ?do i c@ char>lower i c! loop  \ char>lower in sprint-buffer.fth
;

: >upper  ( caddr u -- caddr u ) \ Digits converted to upper case
   2dup over + swap
   ?do i c@ char>upper i c! loop  \ char>lower in sprint-buffer.fth
;

0 #errors !
variable #tests 0 #tests !
: t{  ( -- )  1 #tests +! t{  ;

\ ---[ Start of tests ]---------------------------------------------------------

cr .( Start of tests) cr
Testing stack frames

t{ set-frame -> }t
t{ set-frame 1 0 arg -> 1 1 }t
t{ set-frame 2 3 4 1 arg 0 arg -> 2 3 4 3 2 }t
t{ 5 6 7 set-frame 8 9 0 arg 1 arg -> 5 6 7 8 9 8 9 }t
t{ 10 set-frame 11 12 13 14 1 arg 0 arg 2 arg -> 10 11 12 13 14 12 11 13 }t
t{ 15 set-frame drop-frame -> 15 }t
t{ 16 17 set-frame 18 19 20 drop-frame -> 16 17 }t

t{ 21 field fld21 -> 20 }t
t{ begin-frame
      field fld0
      field fld1
      field fld2
      field fld3
   end-frame frm1 -> }t
t{ frm1 -> 4 }t
t{ 22 23 set-frame 24 25 26 27 28 fld0 fld3 fld1 fld2
                     -> 22 23 24 25 26 27 28 24 27 25 26 }t

t{ frame{ fld4 fld5 fld6 } frm2 -> }t
t{ frm2 -> 3 }t
t{ 29 set-frame 30 31 32 33 fld4 fld5 fld6 -> 29 30 31 32 33 30 31 32 }t

t{ 34 2field 2fld0 -> 32 }t
t{ begin-frame field fld7 2field fld8 field fld9 end-frame frm3 -> }t
t{ set-frame 35 b" abcde" 36 fld8 fld9 fld7 -> 35 buf$ 36 buf$ 36 35 }t

fp-enabled [if]
Testing the FP argument array

\ Ensure the FP stack is empty
: clear-fp-stack  ( F: rn ...r0 -- )  fdepth 0 ?do fdrop loop ;
clear-fp-stack

2e0 2e1 2e2 \ Use powers of 2 so that F- result below is exactly fp zero
t{ 3 floats move-fp-args fdepth -> 0 }t
t{ 0 floats get-fp-arg 2e0 f- fabs f0= -> true }t
t{ 1 floats get-fp-arg 2e1 f- fabs f0= -> true }t
t{ 2 floats get-fp-arg 2e2 f- fabs f0= -> true }t
t{ fdepth -> 0 }t
[then]

Testing flag definitions

\ The numbers 1 to 6 represent the stack frame fields below the flags field
: new-frame ( n -- )  >r set-frame 1 2 3 4 5 6 r>  ; \ n is flags value

t{ 0 #flag+ -> #flag@ }t
t{ 0 #flag+ ucflag+ +flag+ -> +flag@ #flag@ or ucflag@ or }t
t{ leftflag@ new-frame #flag -> 1 2 3 4 5 6 leftflag@ 0 }t
t{ blflag@ new-frame blflag -> 1 2 3 4 5 6 blflag@ dup }t
t{ numflag@ 2* 1- new-frame numflag 0flag longflag
            -> 1 2 3 4 5 6 1023 numflag@ 0flag@ longflag@ }t
t{ $157 new-frame precflag caseflag or leftflag or -> 1 2 3 4 5 6 $157 0  }t

Testing sprintf buffer

256 constant sprb-size create test-buf sprb-size allot
t{ test-buf sprb-size set-sprintf-buffer -> }t
t{ sprbuf$ -> test-buf 0 }t
t{ sprbuf-unused -> sprb-size }t
t{ sprbuf-clear -> }t
t{ sprbuf-here -> sprbuf }t
t{ sprbuf-unused -> sprbuf-size }t
t{ sprbuf$ -> sprbuf 0 }t
t{ b" abcde" sprbuf+ -> }t
t{ sprbuf$ buf$ compare -> 0 }t
t{ sprbuf-here -> sprbuf buf$ nip chars + }t
t{ sprbuf-unused -> sprbuf-size buf$ nip - }t
t{ 'f' char>sprbuf -> }t
t{ sprbuf$ b" abcdef" compare -> 0 }t
t{ sprbuf-unused -> sprbuf-size buf$ nip - }t
t{ sprbuf-clear here sprbuf-size sprbuf+ -> }t
t{ sprbuf-unused -> 0 }t
t{ :noname 'x' char>sprbuf ; catch -> buf-overflow }t
t{ :noname sprbuf-clear here sprbuf-size 1+ sprbuf+ ; catch -> buf-overflow }t
t{ sprbuf-clear b" One" sprbuf+ bl char>sprbuf b" Two" sprbuf+
        sprbuf$ b" One Two" compare -> 0 }t
: sprb-lower s" @`abcdefghijklmnopqrstuvwxyz[{+" ;
: sprb-upper s" @`ABCDEFGHIJKLMNOPQRSTUVWXYZ[{+" ;
t{ sprbuf-clear sprb-lower >buf buf$ str>upper sprb-upper compare -> 0 }t
t{ sprbuf-clear sprb-lower >buf buf$ str>lower sprb-lower compare -> 0 }t
t{ sprbuf-clear sprb-upper >buf buf$ str>lower sprb-lower compare -> 0 }t
t{ sprbuf-clear sprb-upper >buf buf$ str>upper sprb-upper compare -> 0 }t
 
Testing input scanner NEXT-CHAR PREV-CHAR NEXTSYM ?NEXTSYM TEST-TOKEN TESTSYM?

: getsym  ( -- sym )  nextsym sym @  ;
: getnsyms  ( n -- n0 .. nn-1 )  0 do getsym loop  ;
: ?getsym ( f -- sym | error ) ?nextsym sym @ ; 
: tgs  ( n -- n0 .. nn-1 )  ['] getnsyms execute-parsing ;

t{ next-char a next-char b -> 'a' 'b' }t
\ The next 3 lines need the LF between NEXT-CHAR and '->'. Do not change
t{ next-char c next-char
    -> 'c' -1 }t  \ No next character, no space at end of the previous line
t{ next-char 
    -> -1 }t  \ No next character, one space at end of the previous line
t{ next-char  
    -> bl }t  \ No next character, two spaces at end of previous line
t{ 8 getnsyms # %*+-.0 -> "#" "bl" "%" "*" "+" "-" "." "0" }t
t{ 8 getnsyms 12345678 -> "1-9" "1-9" "1-9" "1-9" "1-9" "1-9" "1-9" "1-9" }t
t{ 8 getnsyms 9EGXbcde -> "1-9" "UE" "UG" "UX" "b" "c" "d" "e" }t
t{ 8 getnsyms fglosux
     -> "f" "g" "l" "o" "s" "u" "x" -1 }t    \ Do not change these 2 lines
t{ 4 getnsyms !$S~ -> 0 0 0 0 }t
create oor-chs 0 c, #31 c, #128 c, #255 c,  \ Out of range chars
t{ oor-chs 4  >buf 4 buf$ tgs -> 0 0 0 0 }t   \ Test out of range
t{ true ?getsym s -> 19 }t
t{ :noname  0 ?getsym ; catch invalid-char = [if] 40 [else] 41 [then] -> 40 }t

t{ getsym x "x" test-token -> "x" true  }t
t{ getsym y  26 test-token ->  0  false }t

t{ getsym c 16 testsym? 0<> -> "c" true }t
t{ getsym % 20 testsym? 0<> -> "%" false }t

: pct  2 getnsyms prev-char getsym ;      \ Testing PREV-CHAR
t{ b" uds" ' pct execute-parsing -> "u" "d" "d" }t

Testing parse-format

: pf  ( caddr u -- width prec flags ad arg-size )
   ['] parse-format execute-parsing
;
: nil$ ( -- caddr u )  s" " ;
: init  ( -- caddr 1 )  set-frame nil$ 0 0 ;    \ Test string

\ Rename these words for brevity
caseflag@ constant csf
ucflag@   constant ucf
numflag@  constant nuf
longflag@ constant lof
%fflag@   constant %ff

\ Testing conversion type specifiers
t{ init b" %d" pf -> nil$ 0 1 0 0 nuf ' %d ' %d-prefix 1 0 }t
t{ init b" %u" pf -> nil$ 0 1 0 0 nuf ' %u ' %u-prefix 1 0 }t
t{ init b" %s" pf -> nil$ 0 1 0 0 0 ' %s ' null$ 2 0 }t
t{ init b" %%" pf -> nil$ 0 1 0 0 0 ' %% ' null$ 0 0 }t
t{ init b" %x" pf -> nil$ 0 1 0 0 nuf csf or ' %x ' %x-prefix 1 0 }t
t{ init b" %c" pf -> nil$ 0 1 0 0 0 ' %c ' null$ 1 0 }t
t{ init b" %o" pf -> nil$ 0 1 0 0 nuf ' %o ' %o-prefix 1 0 }t
t{ init b" %b" pf -> nil$ 0 1 0 0 nuf ' %b ' %b-prefix 1 0 }t
t{ init b" %r" pf -> nil$ 0 1 1 0 nuf csf or ' %r ' %r-prefix 2 0 }t
t{ init b" %X" pf -> nil$ 0 1 0 0 nuf csf or ucf or ' %x ' %x-prefix 1 0 }t
t{ init b" %R" pf -> nil$ 0 1 1 0 nuf csf or ucf or ' %r ' %r-prefix 2 0 }t
fp-enabled [if]
   t{ init b" %f" pf -> nil$ 0 6 0 0 %ff ' %f ' sign-prefix 0 1float }t
   t{ init b" %e" pf -> nil$ 0 6 0 0 csf %ff or ' %e ' sign-prefix 0 1float }t
   t{ init b" %g" pf -> nil$ 0 6 0 0 csf %ff or ' %g ' sign-prefix 0 1float }t
   t{ init b" %E" pf -> nil$ 0 6 0 0 csf %ff or ucf or ' %e ' sign-prefix 0 1float }t
   t{ init b" %G" pf -> nil$ 0 6 0 0 csf %ff or ucf or ' %g ' sign-prefix 0 1float }t
[then]
t{ init b" %-s" pf -> nil$ 0 1 0 0 leftflag  ' %s ' null$ 2 0 }t   \ Testing flags
t{ init b" %0s" pf -> nil$ 0 1 0 0 0  ' %s ' null$ 2 0 }t
t{ init b" %0d" pf -> nil$ 0 1 0 0 0flag nuf or ' %d ' %d-prefix 1 0 }t
t{ init b" %+s" pf -> nil$ 0 1 0 0 +flag  ' %s ' null$ 2 0 }t
t{ init b" % s" pf -> nil$ 0 1 0 0 blflag ' %s ' null$ 2 0 }t
t{ init b" %#s" pf -> nil$ 0 1 0 0 #flag  ' %s ' null$ 2 0 }t
t{ init b" %lx" pf -> nil$ 0 1 0 0 lof csf or nuf or ' %x ' %x-prefix 2 0 }t

t{ init b" %ld" pf -> nil$ 0 1 0 0 lof nuf or ' %d ' %d-prefix 2 0 }t  \ Testing long argument size
t{ init b" %lu" pf -> nil$ 0 1 0 0 lof nuf or ' %u ' %u-prefix 2 0 }t
t{ init b" %lo" pf -> nil$ 0 1 0 0 lof nuf or ' %o ' %o-prefix 2 0 }t
t{ init b" %lx" pf -> nil$ 0 1 0 0 lof csf or nuf or ' %x ' %x-prefix 2 0 }t
t{ init b" %lX" pf -> nil$ 0 1 0 0 lof nuf or csf or ucf or ' %x ' %x-prefix 2 0 }t
t{ init b" %lb" pf -> nil$ 0 1 0 0 lof nuf or ' %b ' %b-prefix 2 0 }t
t{ init b" %lr" pf -> nil$ 0 1 1 0 lof csf or nuf or ' %r ' %r-prefix 3 0 }t
t{ init b" %lR" pf -> nil$ 0 1 1 0 lof nuf or csf or ucf or ' %r ' %r-prefix 3 0 }t

+flag@  blflag+ leftflag+ 0flag+ caseflag+
ucflag+ #flag+  longflag+ numflag+ constant most-flags
t{ init b" %+ -#0lX" pf -> nil$ 0 1 0 0 most-flags ' %x ' %x-prefix 2 0 }t

t{ init b" %123s" pf -> nil$ 123 1 0 0 0 ' %s ' null$ 2 0 }t     \ Testing width
t{ init b" %*s" pf -> nil$ -1 1 1 0 0 ' %s ' null$ 3 0 }t    \ Width on the stack

t{ init b" %.98s" pf -> nil$ 0 98 0 0 precflag ' %s ' null$ 2 0 }t \ Testing precision
t{ init b" %.s"  pf -> nil$ 0 0 0 0 precflag ' %s ' null$ 2 0 }t \ No integer for precision
t{ init b" %.*s" pf -> nil$ 0 -1 1 0 precflag ' %s ' null$ 3 0 }t \ Precision on the stack
t{ init b" %*.*s" pf -> nil$ -1 -2 2 0 precflag ' %s ' null$ 4 0 }t \ Width & precision on stack

t{ init b" %-34.56ld" pf     \ Testing all specification elements
      -> nil$ 34 56 0 0 leftflag@ precflag+ longflag+ numflag+ ' %d ' %d-prefix 2 0 }t

t{ :noname s" %1" pf ; catch -> invalid-char }t     \ Syntax error throws an exception

Testing sprintf with text using %s (simple tests)

\ In SPRINTF tests most tests are consecutively number so that, in the event of
\ a failing test, the offedning test can be easily located.

t{ b" " sprintf buf$ compare -> 0 }t                  \ Empty format string
t{ b" Hello world" sprintf buf$ compare -> 0 }t       \ Text, no conversions
t{ 50 51 b" abcd" sprintf buf$ compare -> 50 51 0 }t  \ Is stack depth correct
t{ b" bottles" s" Green %s" sprintf s" Green bottles" compare -> 0 }t
t{ b" green" s" Ten %s bottles" sprintf s" Ten green bottles" compare -> 0 }t
t{ b" Five" s" %s blue bananas" sprintf s" Five blue bananas" compare -> 0 }t
t{ b" Twobottles" drop 3 dup buf$+ s" %s red %s" sprintf
      s" Two red bottles" compare -> 0 }t
t{ 52 53 54 b" ne,othe" drop 3 3 buf$+ drop 1 4 buf$+ drop 2 6 buf$+ drop 1
      s" O%s tw%s, %sr%se!" sprintf s" One, two, three!" compare -> 52 53 54 0 }t

Testing %d and sprintf flags, width and precision

t{ 60 b" %d" sprintf s" 60" compare -> 0 }t        \ Bare %d
t{ 61 62 b" %d%d" sprintf s" 6162" compare -> 0 }t \ 2 bare %d's
t{ 63 64 b" Total: %d+%d items" sprintf
         s" Total: 63+64 items" compare -> 0 }t    \ With text
t{ 65 b" [%5d]" sprintf s" [   65]" compare -> 0 }t      \ Width only
t{ 66 b" [%.6d]" sprintf s" [000066]" compare -> 0 }t    \ Precision only
t{ 67 b" [%8.5d]" sprintf s" [   00067]" compare -> 0 }t \ Width & Precision
t{ 68 b" [%1d]" sprintf s" [68]" compare -> 0 }t         \ String length > width
t{ 69 b" [%0d]" sprintf s" [69]" compare -> 0 }t         \ No 0s
t{ 70 b" [%05d]" sprintf s" [00070]" compare -> 0 }t     \ Padded with 0's
t{ 71 b" [%07.4d]" sprintf s" [   0071]" compare -> 0 }t \ 0 flag ignored
t{ -72 b" [%d]" sprintf s" [-72]" compare -> 0 }t        \ Negative argument
t{ 73 b" [%+d]" sprintf s" [+73]" compare -> 0 }t        \ '+' needed
t{ 74 b" [% d]" sprintf s" [ 74]" compare -> 0 }t        \ Space needed
t{ 75 b" [%+ d]" sprintf s" [+75]" compare -> 0 }t       \ + overrides space
t{ -76 b" [%+ d]" sprintf s" [-76]" compare -> 0 }t      \ -ve beats + and space
t{ 77 b" [%+5d]" sprintf s" [  +77]" compare -> 0 }t     \ Width spaces and sign
t{ 78 b" [% 5d]" sprintf s" [   78]" compare -> 0 }t     \ Ditto
t{ 79 b" [%0+5d]" sprintf s" [+0079]" compare -> 0 }t    \ Width 0s and sign
t{ 80 b" [%0 6d]" sprintf s" [ 00080]" compare -> 0 }t   \ Width 0s and sign
t{ 81 b" [%+.5d]" sprintf s" [+00081]" compare -> 0 }t   \ Precision 0s
t{ 82 b" [% .4d]" sprintf s" [ 0082]" compare -> 0 }t    \ Space + precision 0s
t{ 83 b" [%+7.4d]" sprintf s" [  +0083]" compare -> 0 }t \ Width, sign, prec 0s
t{ 84 b" [% 7.4d]" sprintf s" [   0084]" compare -> 0 }t \ Ditto
t{ 85 b" [%0+6.3d]" sprintf s" [  +085]" compare -> 0 }t \ Prec beats 0 flag
t{ 86 b" [%0 6.3d]" sprintf s" [   086]" compare -> 0 }t \ Prec beats 0 flag
t{ -87 b" [%0 5.3d]" sprintf s" [ -087]" compare -> 0 }t \ Prec beats 0 flag

\ Tests 60, 65 to 87 with - flag (left justified
t{ 88 b" [%-d]" sprintf s" [88]" compare -> 0 }t          \ - flag irrelevant
t{ 89 b" [%-5d]" sprintf s" [89   ]" compare -> 0 }t      \ Width only
t{ 90 b" [%-.6d]" sprintf s" [000090]" compare -> 0 }t    \ Precision only
t{ 91 b" [%-8.5d]" sprintf s" [00091   ]" compare -> 0 }t \ Width & Precision
t{ 92 b" [%-1d]" sprintf s" [92]" compare -> 0 }t         \ String len > width
t{ 93 b" [%-0d]" sprintf s" [93]" compare -> 0 }t         \ No 0s
t{ 94 b" [%-05d]" sprintf s" [94   ]" compare -> 0 }t     \ 0 flag ignored
t{ 95 b" [%0-7.4d]" sprintf s" [0095   ]" compare -> 0 }t \ 0 flag ignored
t{ -96 b" [%-d]" sprintf s" [-96]" compare -> 0 }t        \ Negative argument
t{ 97 b" [%-+d]" sprintf s" [+97]" compare -> 0 }t        \ '+' needed
t{ 98 b" [% -d]" sprintf s" [ 98]" compare -> 0 }t        \ Space needed
t{ 99 b" [%-+ d]" sprintf s" [+99]" compare -> 0 }t       \ + overrides space
t{ -100 b" [%-+ d]" sprintf s" [-100]" compare -> 0 }t      \ -ve beats sign
t{ 101 b" [%-+5d]" sprintf s" [+101 ]" compare -> 0 }t    \ Width, spaces, sign
t{ -102 b" [%- 5d]" sprintf s" [-102 ]" compare -> 0 }t   \ Ditto
t{ 103 b" [%-0+6d]" sprintf s" [+103  ]" compare -> 0 }t   \ 0 flag ignored
t{ 104 b" [%-0 7d]" sprintf s" [ 104   ]" compare -> 0 }t  \ 0 flag ignored
t{ 105 b" [%-+.5d]" sprintf s" [+00105]" compare -> 0 }t   \ Precision 0s
t{ 106 b" [%- .4d]" sprintf s" [ 0106]" compare -> 0 }t    \ Space + prec 0s
t{ 107 b" [%-+7.4d]" sprintf s" [+0107  ]" compare -> 0 }t \ Width,sign, prec 0s
t{ 108 b" [%- 7.4d]" sprintf s" [ 0108  ]" compare -> 0 }t \ Ditto
t{ 109 b" [%-0+6.3d]" sprintf s" [+109  ]" compare -> 0 }t   \ Prec beats 0 flag
t{ 110 b" [%-0 6.3d]" sprintf s" [ 110  ]" compare -> 0 }t \ Prec beats 0 flag
t{ -111 b" [%0- 5.3d]" sprintf s" [-111 ]" compare -> 0 }t \ Prec beats 0 flag

\ Special treatment of 0 argument

t{ 112 0 b" [%d]" sprintf s" [0]" compare -> 112 0 }t    \ Default precision = 1
t{ 113 0 b" [%+d]" sprintf s" [0]" compare -> 113 0 }t   \ Signed
t{ 114 0 b" [% d]" sprintf s" [0]" compare -> 114 0 }t   \ Space for sign
t{ 115 0 b" [%+.3d]" sprintf s" [000]" compare -> 115 0 }t \ '0' padded
t{ 116 0 b" [%+.0d]" sprintf s" []" compare -> 116 0 }t \ precision = 0
t{ 117 0 b" [%+.d]" sprintf s" []" compare -> 117 0 }t  \ precision = 0
t{ 118 0 b" [% d]" sprintf s" [0]" compare -> 118 0 }t    \ no space for +ve
t{ 119 0 b" [% d]" sprintf s" [0]" compare -> 119 0 }t    \ no space for +ve
t{ 120 0 b" [% .2d]" sprintf s" [00]" compare -> 120 0 }t \ '0' padded, no space
t{ 121 0 b" [% .0d]" sprintf s" []" compare -> 121 0 }t \ '0' padded, no space
t{ 122 0 b" [% .d]" sprintf s" []" compare -> 122 0 }t \ precision = 0
t{ 123 0 b" [%05.0d]" sprintf s" [     ]" compare -> 123 0 }t
t{ 124 0 b" [%05d]" sprintf s" [00000]" compare -> 124 0 }t
t{ 125 0 b" [%#d]" sprintf s" [0]" compare -> 125 0 }t   \ Default precision=1
t{ 126 0 b" [%#+d]" sprintf s" [0]" compare -> 126 0 }t  \ Signed, no +/-sign
t{ 127 0 b" [%#+d]" sprintf s" [0]" compare -> 127 0 }t  \ Signed, no space
t{ 128 0 b" [%#+.3d]" sprintf s" [000]" compare -> 128 0 }t \ '0' padded
t{ 129 0 b" [%#+.0d]" sprintf s" []" compare -> 129 0 }t \ '0' padded, no space
t{ 130 0 b" [%#+.d]" sprintf s" []" compare -> 130 0 }t \ precision = 0
t{ 131 0 b" [%# d]" sprintf s" [0]" compare -> 131 0 }t   \ no space for +ve
t{ 132 0 b" [%# d]" sprintf s" [0]" compare -> 132 0 }t   \ no space for +ve
t{ 133 0 b" [%# .2d]" sprintf s" [00]" compare -> 133 0 }t \ 0 padded no space
t{ 134 0 b" [%# .0d]" sprintf s" []" compare -> 134 0 }t \ '0' padded, no space
t{ 135 0 b" [%# .d]" sprintf s" []" compare -> 135 0 }t \ precision = 0

\ %d with hash flag
t{ 136 b" [%#d]" sprintf s" [#136]" compare -> 0 }t   \ # no sign, width etc
t{ -137 b" [%#d]" sprintf s" [#-137]" compare -> 0 }t \ # -ve number
t{ 138 b" [%#+d]" sprintf s" [#138]" compare -> 0 }t  \ No + sign with #
t{ 139 b" [%# d]" sprintf s" [#139]" compare -> 0 }t  \ No space with #
t{ 140 b" [%#6d]" sprintf s" [  #140]" compare -> 0 }t \ width >length
t{ 141 b" [%0#7d]" sprintf s" [#000141]" compare -> 0 }t \ padded with 0
t{ -142 b" [%0#7d]" sprintf s" [#-00142]" compare -> 0 }t \ -ve, padded with 0
t{ 143 b" [%#7.4d]" sprintf s" [  #0143]" compare -> 0 }t \ precision > length
t{ 144 b" [%#07.4d]" sprintf s" [  #0144]" compare -> 0 }t \ 0 flag ignored
t{ -145 b" [%#07.4d]" sprintf s" [ #-0145]" compare -> 0 }t \ -ve
t{ 146 b" [%-#d]" sprintf s" [#146]" compare -> 0 }t   \ # no sign, width etc
t{ -147 b" [%-#d]" sprintf s" [#-147]" compare -> 0 }t \ # -ve number
t{ 148 b" [%#+-d]" sprintf s" [#148]" compare -> 0 }t  \ No + sign with #
t{ 149 b" [%-# d]" sprintf s" [#149]" compare -> 0 }t  \ No space with #
t{ 150 b" [%-#6d]" sprintf s" [#150  ]" compare -> 0 }t \ width >length
t{ 151 b" [%0-#7d]" sprintf s" [#151   ]" compare -> 0 }t \ padded with 0
t{ -152 b" [%-0#7d]" sprintf s" [#-152  ]" compare -> 0 }t \ -ve, padded with 0
t{ 153 b" [%#-7.4d]" sprintf s" [#0153  ]" compare -> 0 }t \ precision > length
t{ 154 b" [%#-07.4d]" sprintf s" [#0154  ]" compare -> 0 }t \ 0 flag ignored
t{ -155 b" [%-#07.4d]" sprintf s" [#-0155 ]" compare -> 0 }t \ -ve

\ %d with the long modifier
\ Only need a few tests as the software converts a single integer to a double
\ before any conversion.

\ 32 or 64 bit Forths? For double word tests
0 cell+ bits/au * dup 32 = constant 32bits? 64 = constant 64bits?

t{ 156 157  b" %ld" sprintf 156 157 <# #s #> compare -> 0 }t
t{ 158 -159 b" %ld" sprintf 158 -159 dabs <# #s '-' hold #> compare -> 0 }t

16bits? [if]
t{ 160 161  b" [%14ld]" sprintf s" [      10551456]" compare -> 0 }t
t{ 162 163  b" [%0+14ld]" sprintf s" [+0000010682530]" compare -> 0 }t
t{ 164 -165 b" [%#-+14ld]" sprintf s" [#-10813276    ]" compare -> 0 }t
[then]

32bits? [if]
t{ 160 161   b" [%20ld]"    sprintf s" [        691489734816]" compare -> 0 }t
t{ 162 163   b" [%0+20ld]"  sprintf s" [+0000000700079669410]" compare -> 0 }t
t{ 164 -165  b" [%#-+20ld]" sprintf s" [#-708669603676      ]" compare -> 0 }t
[then]

64bits? [if]
t{ 160 161  b" [%26ld]" sprintf s" [    2969925795867237810336]" compare -> 0 }t
t{ 162 163  b" [%0+26ld]" sprintf
            s" [+0003006819284014656913570]" compare -> 0 }t
t{ 164 -165 b" [%#-+26ld]" sprintf
            s" [#-3043712772162076016476  ]" compare -> 0 }t
[then]

\ With arguments, leading 0's for Width and precision, and argument = 0
t{ 6 166 b" [%0*.4d]" sprintf s" [  0166]" compare -> 0 }t \ Width on stack
t{ 5 167 b" [%07.*d]" sprintf s" [  00167]" compare -> 0 }t \ Precision on stack
t{ 8 6 168 b" [%0*.*d]" sprintf s" [  000168]" compare -> 0 }t \ Both on stack
t{ 169 b" [%006.04d]" sprintf s" [  0169]" compare -> 0 }t \ 0's at start
t{ 170 5 3 0 b" [%*.*d]" sprintf s" [  000]" compare -> 170 0 }t
t{ 171 4 0 0 b" [%*.*d]" sprintf s" [    ]" compare -> 171 0 }t

Testing %u
t{ 180 b" [%u]" sprintf s" [180]" compare -> 0 }t
t{ 181 b" [%8u]" sprintf s" [     181]" compare -> 0 }t
t{ 182 b" [%08u]" sprintf s" [00000182]" compare -> 0 }t
t{ 183 b" [%08.5u]" sprintf s" [   00183]" compare -> 0 }t
t{ -184 b" %u" sprintf -184 0 <# #s #> compare -> 0 }t   \ Unsigned
t{ 185 b" [%+u]" sprintf s" [185]" compare -> 0 }t \ + flag ignored
t{ 186 b" [% u]" sprintf s" [186]" compare -> 0 }t \ Space flag ignored
t{ 187 b" [%#u]" sprintf s" [#187]" compare -> 0 }t   \ # flag
t{ 188 b" [%#7u]" sprintf s" [   #188]" compare -> 0 }t
t{ 189 b" [%#07u]" sprintf s" [#000189]" compare -> 0 }t
t{ 190 b" [%#07.5u]" sprintf s" [ #00190]" compare -> 0 }t
t{ -191 b" %#u" sprintf -191 0 <# #s s" #" holds #> compare -> 0 }t
t{ 192  b" [%#+ u]" sprintf s" [#192]" compare -> 0 }t

t{ 193 0 b" [%u]" sprintf s" [0]" compare -> 193 0 }t
t{ 194 0 b" [%05.3u]" sprintf s" [  000]" compare -> 194 0 }t
t{ 195 0 b" [%0.0u]" sprintf s" []" compare -> 195 0 }t
t{ 196 0 b" [%05.0u]" sprintf s" [     ]" compare -> 196 0 }t
t{ 197 0 b" [%#u]" sprintf s" [#0]" compare -> 197 0 }t
t{ 198 0 b" [%#05.3u]" sprintf s" [ #000]" compare -> 198 0 }t
t{ 199 0 b" [%#0.0u]" sprintf s" []" compare -> 199 0 }t
t{ 200 0 b" [%#05.0u]" sprintf s" [     ]" compare -> 200 0 }t

: ud>$  ( caddr u ud caddr2 u2 -- caddr3 u3 )
   2>r 2swap <# ']' hold holds #s 2r> holds '[' hold #>
;
t{ 201  202 b" %lu" sprintf 201  202 <# #s #> compare -> 0 }t
t{ 203 -204 b" %lu" sprintf 203 -204 <# #s #> compare -> 0 }t

t{ 205 b" [%-u]" sprintf s" [205]" compare -> 0 }t
t{ 206 b" [%-8u]" sprintf s" [206     ]" compare -> 0 }t
t{ 207 b" [%-08u]" sprintf s" [207     ]" compare -> 0 }t
t{ 208 b" [%-08.5u]" sprintf s" [00208   ]" compare -> 0 }t
t{ -209 b" [%-u]" sprintf nil$ -209 0 nil$ ud>$ compare -> 0 }t   \ Unsigned
t{ 210 b" [%-+u]" sprintf s" [210]" compare -> 0 }t \ + flag ignored
t{ 211 b" [%- u]" sprintf s" [211]" compare -> 0 }t \ Space flag ignored
t{ 212 b" [%#-u]" sprintf s" [#212]" compare -> 0 }t   \ # flag
t{ 213 b" [%#-7u]" sprintf s" [#213   ]" compare -> 0 }t
t{ 214 b" [%#-07u]" sprintf s" [#214   ]" compare -> 0 }t
t{ 215 b" [%-#07.5u]" sprintf s" [#00215 ]" compare -> 0 }t
t{ -216 b" [%-#u]" sprintf nil$ -216 0 s" #" ud>$ compare -> 0 }t
t{ 217  b" [%-#+ u]" sprintf s" [#217]" compare -> 0 }t

t{ 218 0 b" [%-u]" sprintf s" [0]" compare -> 218 0 }t
t{ 219 0 b" [%-05.3u]" sprintf s" [000  ]" compare -> 219 0 }t
t{ 220 0 b" [%-0.0u]" sprintf s" []" compare -> 220 0 }t
t{ 221 0 b" [%-05.0u]" sprintf s" [     ]" compare -> 221 0 }t
t{ 222 0 b" [%#-u]" sprintf s" [#0]" compare -> 222 0 }t
t{ 223 0 b" [%#-05.3u]" sprintf s" [#000 ]" compare -> 223 0 }t
t{ 224 0 b" [%#-0.0u]" sprintf s" []" compare -> 224 0 }t
t{ 225 0 b" [%#-05.0u]" sprintf s" [     ]" compare -> 225 0 }t

Testing %x and %X
\ As %x uses the same code as %u we only need simple tests

t{ 240 $abcd b" [%x]" sprintf s" [abcd]" compare -> 240 0 }t
t{ 241 $ef01 b" [%x]" sprintf s" [ef01]" compare -> 241 0 }t
t{ 242 $ABCD b" [%x]" sprintf s" [abcd]" compare -> 242 0 }t
t{ 243 $EF34 b" [%x]" sprintf s" [ef34]" compare -> 243 0 }t
t{ 244 $-abcd b" [%x]" sprintf
       nil$ $-abcd 0 nil$ hex ud>$ decimal >lower compare -> 244 0 }t
t{ 245 $aBCd b" [%0+ 8x]" sprintf s" [0000abcd]" compare -> 245 0 }t
t{ 246 $eF56 b" [%+0 8.5x]" sprintf s" [   0ef56]" compare -> 246 0 }t
t{ 247 $bcd  b" [%#8.5x]" sprintf s" [  $00bcd]" compare -> 247 0 }t
t{ $248 b" [%-0+ 8.3x]" sprintf s" [248     ]" compare -> 0 }t
t{ $249 b" [%-#+0 8.5x]" sprintf s" [$00249  ]" compare -> 0 }t

t{ 250 $abcd b" [%X]" sprintf s" [ABCD]" compare -> 250 0 }t
t{ 251 $ef78 b" [%X]" sprintf s" [EF78]" compare -> 251 0 }t
t{ 252 $-abcd b" [%X]" sprintf
       nil$ $-abcd 0 nil$ hex ud>$ decimal >upper compare -> 252 0 }t
t{ 253 $fedc b" [%#-+ 09.6X]" sprintf s" [$00FEDC  ]" compare -> 253 0 }t
t{ 254 $FAB $-DCE b" [%lx]" sprintf
       nil$ $fab $-dce nil$ hex ud>$ decimal >lower compare -> 254 0 }t

Testing %o  \ Only simple tests needed
t{ 260 1023 b" [%o]" sprintf s" [1777]" compare -> 260 0 }t
t{ 261 1024 b" [%#o]" sprintf s" [02000]" compare -> 261 0 }t  \ Prefix of '0'
t{ 262 127  b" [%#+ 7.5o]" sprintf s" [  00177]" compare -> 262 0 }t \ No prefix
t{ 263 b" [%#-7.5o]" sprintf s" [00407  ]" compare -> 0 }t \ No prefix
t{ 264 -265 b" [%lo]" sprintf
       nil$ 264 -265 nil$ #8 base ! ud>$ decimal compare -> 0 }t
t{ 266 0 b" [%#o]" sprintf s" [0]" compare -> 266 0 }t   \ 0 argument, no prefix
t{ 267 0 0 b" [%#lo]" sprintf s" [0]" compare -> 267 0 }t


Testing %b  \ Only simple tests needed
t{ 270 b" [%b]" sprintf s" [100001110]" compare -> 0 }t
t{ 271 b" [%#b]" sprintf s" [%100001111]" compare -> 0 }t
t{ 272 b" [%#15.12b]" sprintf s" [  %000100010000]" compare -> 0 }t
t{ 273 b" [%-#15.11b]" sprintf s" [%00100010001   ]" compare -> 0 }t
t{ 13 11 274 b" [%#*.*b]" sprintf s" [ %00100010010]" compare -> 0 }t

Testing %r and %R

t{ 36 281  b" [%r]" sprintf s" [7t]" compare -> 0 }t
t{ 282 23 $FFFF b" [%R]" sprintf s" [58K8]" compare -> 282 0 }t
t{ 7 17 283 b" [%#*.3r]" sprintf s" [    0gb]" compare -> 0 }t
t{ 5 17 284 b" [%-7.*R]" sprintf s" [000GC  ]" compare -> 0 }t
t{ 9 4 17 285 b" [%*.*r]" sprintf s" [     00gd]" compare -> 0 }t
t{ 29 286 287 b" [%lr]" sprintf
       nil$ 286 287 nil$ #29 base ! ud>$ >lower decimal compare -> 0 }t
t{ 17 -288 b" [%+.5R]" sprintf s" [-000GG]" compare -> 0 }t
t{ 17  289 b" [% r]" sprintf s" [ 100]" compare -> 0 }t

\ %r with bases having a #flag prefix
t{ 10 290 b" [%#r]" sprintf s" [#290]" compare -> 0 }t
t{ 10 -291 b" [%#r]" sprintf s" [#-291]" compare -> 0 }t
t{ 292 10 0 b" [%#r]" sprintf s" [0]" compare -> 292 0 }t

t{ 16 293 b" [%#r]" sprintf s" [$125]" compare -> 0 }t
t{ 16 -294 b" [%#r]" sprintf s" [$-126]" compare -> 0 }t
t{ 295 16 0 b" [%#r]" sprintf s" [0]" compare -> 295 0 }t

t{ 8 296 b" [%#r]" sprintf s" [0450]" compare -> 0 }t
t{ 8 -297 b" [%#r]" sprintf s" [-0451]" compare -> 0 }t
t{ 298 8 0 b" [%#r]" sprintf s" [0]" compare -> 298 0 }t

t{ 2 299 b" [%#r]" sprintf s" [%100101011]" compare -> 0 }t
t{ 2 -300 b" [%#r]" sprintf s" [%-100101100]" compare -> 0 }t
t{ 301 2 0 b" [%#r]" sprintf s" [0]" compare -> 301 0 }t

Testing %c %%
t{ 350 'A' b" [%c]" sprintf s" [A]" compare -> 350 0 }t
t{ 351 bl  b" [%c]" sprintf s" [ ]" compare -> 351 0 }t
t{ 352 '~' b" [%c]" sprintf s" [~]" compare -> 352 0 }t
t{ 353 'z' b" [%5c]"  sprintf s" [    z]" compare -> 353 0 }t
t{ 354 'z' b" [%-5c]" sprintf s" [z    ]" compare -> 354 0 }t
t{ 355 '-' b" [%.4c]" sprintf s" [----]" compare -> 355 0 }t
t{ 356 '*' b" [%.0c]" sprintf s" []" compare -> 356 0 }t
t{ 357 '@' b" [%05.3c]"  sprintf s" [  @@@]" compare -> 357 0 }t \ No 0's
t{ 358 '&' b" [%-05.3c]" sprintf s" [&&&  ]" compare -> 358 0 }t \ No 0's
t{ 359 '7' b" [%+ lc]" sprintf s" [7]" compare -> 359 0 }t  \ Ignore flags & 'l'
t{ 360 'x' b" [%#c]" sprintf s" ['x']" compare -> 360 0 }t \ Output Forth char
t{ 361 'K' b" [%#.4c]" sprintf s" ['K']" compare -> 361 0 }t \ # flag beats prec
t{ 362 b" [%%]" sprintf s" [%]" compare -> 362 0 }t
t{ 363 b" [%#0-+ l%]" sprintf s" [%]" compare -> 363 0 }t \ Ignore flags & 'l'
t{ 364 b" [%05.3%]" sprintf s" [%]" compare -> 364 0 }t \ Ignore width & prec
t{ 365 7 3 b" [%0*.*%]" sprintf s" [%]" compare -> 365 0 }t \ Ignore width & prec
t{ 366 9 4 '5' b" [%u %*.*%age: %c]" sprintf s" [366 %age: 5]" compare -> 0 }t
t{ 367 6 4 ':' b" [%*.*c]" sprintf s" [  ::::]" compare -> 367 0 }t
 
Testing %s (more tests)
t{ 380 b" abcde" s" [%7s]" sprintf s" [  abcde]" compare -> 380 0 }t
t{ 381 b" fgh" s" [%-6s]" sprintf s" [fgh   ]" compare -> 381 0 }t
t{ 382 b" ijkl" s" [%#0+ 7s]" sprintf s" [   ijkl]" compare -> 382 0 }t
t{ 383 b" mnopq" s" [%7.6s]" sprintf s" [  mnopq]" compare -> 383 0 }t
t{ 384 b" rst" s" [%7.0s]" sprintf s" [       ]" compare -> 384 0 }t
t{ 385 b" uvwx" s" [%4.s]" sprintf s" [    ]" compare -> 385 0 }t
t{ 386 b" yz" s" [%.s]" sprintf s" []" compare -> 386 0 }t
t{ 387 5 2 b" abcdefg" s" [%-*.*s]" sprintf s" [ab   ]" compare -> 387 0 }t

fp-enabled [if]
Testing %e and %E

t{ 400 1.234567e89 b" [%e]" sprintf s" [1.234567e+89]" compare -> 400 0 }t
t{ 401 1.234567e8 b" [%e]" sprintf s" [1.234567e+08]" compare -> 401 0 }t
t{ 402 1.234567e0 b" [%e]" sprintf s" [1.234567e+00]" compare -> 402 0 }t
t{ 403 2.345678e-89 b" [%E]" sprintf s" [2.345678E-89]" compare -> 403 0 }t
t{ 404 3.456789e-89 b" [%+e]" sprintf s" [+3.456789e-89]" compare -> 404 0 }t
t{ 405 4.567890e89 b" [% e]" sprintf s" [ 4.567890e+89]" compare -> 405 0 }t
t{ 406 -5.678901e89 b" [%e]" sprintf s" [-5.678901e+89]" compare -> 406 0 }t
t{ 407 6.789012e89 b" [%+ e]" sprintf s" [+6.789012e+89]" compare -> 407 0 }t
t{ 408 -7.890123e89 b" [%+ e]" sprintf s" [-7.890123e+89]" compare -> 408 0 }t
t{ 409 8.901234e89 b" [%15e]" sprintf s" [   8.901234e+89]" compare -> 409 0 }t
t{ 410 9.012345e89 b" [%016e]" sprintf s" [00009.012345e+89]" compare -> 410 0 }t
t{ 411 0.234567e89 b" [%-14e]" sprintf s" [2.345670e+88  ]" compare -> 411 0 }t
t{ 412 1.234567e89 b" [%0-14e]" sprintf s" [1.234567e+89  ]" compare -> 412 0 }t
t{ 413 1.234567e89 b" [%#e]" sprintf s" [1.234567e+89]" compare -> 413 0 }t
t{ 414 5e0 b" [%e]" sprintf s" [5.000000e+00]" compare -> 414 0 }t
t{ 415 5e0 b" [%.0e]" sprintf s" [5e+00]" compare -> 415 0 }t
t{ 416 5e0 b" [%#.0e]" sprintf s" [5.e+00]" compare -> 416 0 }t
t{ 417 1.234567e0 b" [%.5e]" sprintf s" [1.23457e+00]" compare -> 417 0 }t
t{ 418 1.234567e1 b" [%.3e]" sprintf s" [1.235e+01]" compare -> 418 0 }t
t{ 419 1.234567e2 b" [%.e]" sprintf s" [1e+02]" compare -> 419 0 }t
t{ 420 1.5e5 b" [%.e]" sprintf s" [2e+05]" compare -> 420 0 }t
t{ 421 9.9999994e9 b" [%e]" sprintf s" [9.999999e+09]" compare -> 421 0 }t
t{ 422 9.9999995e9 b" [%e]" sprintf s" [1.000000e+10]" compare -> 422 0 }t
t{ 423 9.9e5 b" [%.e]" sprintf s" [1e+06]" compare -> 423 0 }t
t{ 424 -9.8999e-100 b" [%012.1e]" sprintf s" [-0009.9e-100]" compare -> 424 0 }t
t{ 425 0e1 b" [%e]" sprintf s" [0.000000e+00]" compare -> 425 0 }t

\ Testing %e with a large run of zeroes due to large precision
: check1.25e100%e  ( caddr u -- f )  \ True to pass
   2dup 2>r 157 = swap       ( -- f caddr )  \ Check overall length
   4 s" 1.25" compare 0= and                 \ check initial digits
   -1 2r@ 4 /string 5 - over + swap
   do i c@ '0' = and loop and                \ Check zeroes
   2r> 152 /string s" e+100" compare 0= and
;
\ This should give "1.25...(148 zeroes)...e+100"
t{ 426 1.25e100 b" %.150e" sprintf check1.25e100%e -> 426 -1 }t

Testing %e with width and precision as arguments
t{ 427 1.23e0 12 5 b" [%*.*e]" sprintf s" [ 1.23000e+00]" compare -> 427 0 }t

Testing %f

t{ 500 1.234567e8  b" [%f]" sprintf s" [123456700.000000]" compare -> 500 0 }t
t{ 501 2.345678e6  b" [%f]" sprintf s" [2345678.000000]" compare -> 501 0 }t
t{ 502 3.456789e5  b" [%f]" sprintf s" [345678.900000]" compare -> 502 0 }t
t{ 503 4.567891e0  b" [%f]" sprintf s" [4.567891]" compare -> 503 0 }t
t{ 504 5.678912e-1 b" [%f]" sprintf s" [0.567891]" compare -> 504 0 }t
t{ 505 6.789123e-4 b" [%f]" sprintf s" [0.000679]" compare -> 505 0 }t
t{ 506 7.891234e-6 b" [%f]" sprintf s" [0.000008]" compare -> 506 0 }t
t{ 507 5.912345e-7 b" [%f]" sprintf s" [0.000001]" compare -> 507 0 }t
t{ 508 4.912345e-7 b" [%f]" sprintf s" [0.000000]" compare -> 508 0 }t
t{ 509 5.912345e-8 b" [%f]" sprintf s" [0.000000]" compare -> 509 0 }t
t{ 510 -1.234e0 b" [%+f]" sprintf s" [-1.234000]" compare -> 510 0 }t
t{ 511 1e0 b" [%+f]" sprintf s" [+1.000000]" compare -> 511 0 }t
t{ 512 2e0 b" [% f]" sprintf s" [ 2.000000]" compare -> 512 0 }t
t{ 513 -3e0 b" [%+ f]" sprintf s" [-3.000000]" compare -> 513 0 }t
t{ 514 4e0 b" [%10f]" sprintf s" [  4.000000]" compare -> 514 0 }t
t{ 515 5e0 b" [%010f]" sprintf s" [005.000000]" compare -> 515 0 }t
t{ 516 6e0 b" [%-10f]" sprintf s" [6.000000  ]" compare -> 516 0 }t
t{ 517 7e0 b" [%-+10f]" sprintf s" [+7.000000 ]" compare -> 517 0 }t
t{ 518 0.89123e0 b" [%.3f]" sprintf s" [0.891]" compare -> 518 0 }t
t{ 519 0.89953e0 b" [%.3f]" sprintf s" [0.900]" compare -> 519 0 }t
t{ 520 0.9123e0 b" [%.1f]" sprintf s" [0.9]" compare -> 520 0 }t
t{ 521 0.9623e0 b" [%.1f]" sprintf s" [1.0]" compare -> 521 0 }t
t{ 522 0.9623e0 b" [%.0f]" sprintf s" [1]" compare -> 522 0 }t
t{ 523 0.9623e0 b" [%#.0f]" sprintf s" [1.]" compare -> 523 0 }t
t{ 524 0.9623e10 b" [%.0f]" sprintf s" [9623000000]" compare -> 524 0 }t
t{ 525 0.9623e10 b" [%#.0f]" sprintf s" [9623000000.]" compare -> 525 0 }t
t{ 526 0e1 b" [%f]" sprintf s" [0.000000]" compare -> 526 0 }t
t{ 527 0e1 b" [%.2f]" sprintf s" [0.00]" compare -> 527 0 }t
t{ 528 0e1 b" [%.f]" sprintf s" [0]" compare -> 528 0 }t
t{ 529 0e1 b" [%#.f]" sprintf s" [0.]" compare -> 529 0 }t
t{ 530 0e1 b" [%+.1f]" sprintf s" [+0.0]" compare -> 530 0 }t
t{ 531 -0e1 b" [%.1f]" sprintf s" [-0.0]" compare -> 531 0 }t

Testing runs of zeros due to large exponents or precision
t{ 532 0.123456789012345e40 b" %f" sprintf
   s" 1234567890123450000000000000000000000000.000000" compare -> 532 0 }t
t{ 533 0.123456789012345e15 b" %f" sprintf
                            s" 123456789012345.000000" compare -> 533 0 }t
t{ 534 0.123456789012345e15 b" %.0f" sprintf
                                   s" 123456789012345" compare -> 534 0 }t
t{ 535 0.123456789012345e13 b" %.20f" sprintf
                s" 1234567890123.45000000000000000000" compare -> 535 0 }t
t{ 536 0.123456789012345e9 b" %f" sprintf
                                  s" 123456789.012345" compare -> 536 0 }t
t{ 537 0.123456789012345e2 b" %f" sprintf
                                         s" 12.345679" compare -> 537 0 }t
t{ 538 0.123456789012345e0 b" %.18f" sprintf
                              s" 0.123456789012345000" compare -> 538 0 }t
t{ 539 0.123456789012345e0 b" %.15f" sprintf
                                 s" 0.123456789012345" compare -> 539 0 }t
t{ 540 0.123456789012345e0 b" %.12f" sprintf
                                    s" 0.123456789012" compare -> 540 0 }t
t{ 541 0.123456789012345e-5 b" %.8f" sprintf
                                        s" 0.00000123" compare -> 541 0 }t
t{ 542 0.123456789012345e-5 b" %.20f" sprintf
                            s" 0.00000123456789012345" compare -> 542 0 }t
t{ 543 0.123456789012345e-5 b" %.26f" sprintf
                      s" 0.00000123456789012345000000" compare -> 543 0 }t

Testing %f with width and precision as arguments
t{ 544 9 3 1.2345e0 b" [%*.*f]" sprintf s" [    1.235]" compare -> 544 0 }t 
t{ 545 11 7 1.2345e0 b" [%0*.*f]" sprintf s" [001.2345000]" compare -> 545 0 }t 
t{ 546 15 11 1.2345e-3 b" [%-*.*f]" sprintf s" [0.00123450000  ]" compare
                     -> 546 0 }t

Testing %g and %G

\ Test use of %e or %f conversions by varying exponent, precision=6 (default)
t{ 600 1.2345678e-5 b" [%g]" sprintf s" [1.23457e-05]" compare -> 600 0 }t
t{ 601 1.2345678e-4 b" [%g]" sprintf s" [0.000123457]" compare -> 601 0 }t
t{ 602 1.2345678e-3 b" [%g]" sprintf s" [0.00123457]" compare -> 602 0 }t
t{ 603 1.2345678e-2 b" [%g]" sprintf s" [0.0123457]" compare -> 603 0 }t
t{ 604 1.2345678e-1 b" [%g]" sprintf s" [0.123457]" compare -> 604 0 }t
t{ 605 1.2345678e0  b" [%g]" sprintf s" [1.23457]" compare -> 605 0 }t
t{ 606 1.2345678e1  b" [%g]" sprintf s" [12.3457]" compare -> 606 0 }t
t{ 607 1.2345678e2  b" [%g]" sprintf s" [123.457]" compare -> 607 0 }t
t{ 608 1.2345678e3  b" [%g]" sprintf s" [1234.57]" compare -> 608 0 }t
t{ 609 1.2345678e4  b" [%g]" sprintf s" [12345.7]" compare -> 609 0 }t
t{ 610 1.2345678e5  b" [%g]" sprintf s" [123457]" compare -> 610 0 }t
t{ 611 1.2345678e6  b" [%g]" sprintf s" [1.23457e+06]" compare -> 611 0 }t
t{ 612 1.2345678e7  b" [%g]" sprintf s" [1.23457e+07]" compare -> 612 0 }t
t{ 613 1.2345678e8  b" [%g]" sprintf s" [1.23457e+08]" compare -> 613 0 }t

\ Test use of %e or %f conversions by varying precision
\ Specified precision <= 0 treated as precision = 1
t{ 614 -3 9.87654321e7 b" [%.*g]" sprintf s" [1e+08]" compare -> 614 0 }t
t{ 615 9.87654321e7 b" [%.0g]" sprintf s" [1e+08]" compare -> 615 0 }t
t{ 616 9.87654321e7 b" [%.1g]" sprintf s" [1e+08]" compare -> 616 0 }t
t{ 617 9.87654321e7 b" [%.2g]" sprintf s" [9.9e+07]" compare -> 617 0 }t
t{ 618 9.87654321e7 b" [%.3g]" sprintf s" [9.88e+07]" compare -> 618 0 }t
t{ 619 9.87654321e7 b" [%.4g]" sprintf s" [9.877e+07]" compare -> 619 0 }t
t{ 620 9.87654321e7 b" [%.5g]" sprintf s" [9.8765e+07]" compare -> 620 0 }t
t{ 621 9.87654321e7 b" [%.6g]" sprintf s" [9.87654e+07]" compare -> 621 0 }t
t{ 622 9.87654321e7 b" [%.7g]" sprintf s" [9.876543e+07]" compare -> 622 0 }t
t{ 623 9.87654321e7 b" [%.8g]" sprintf s" [98765432]" compare -> 623 0 }t
t{ 624 9.87654321e7 b" [%.9g]" sprintf s" [98765432.1]" compare -> 624 0 }t

\ Test removal of trailing zeroes
t{ 625 1.2e-5       b" [%g]" sprintf s" [1.2e-05]" compare -> 625 0 }t
t{ 626 1.2399999e-5 b" [%g]" sprintf s" [1.24e-05]" compare -> 626 0 }t

\ Test display of trailing zeroes
t{ 627 1.2e-5 b" [%#g]" sprintf s" [1.20000e-05]" compare -> 627 0 }t

\ Test display of decimal point without following digits
t{ 628 1e5 b" [%#g]" sprintf s" [100000.]" compare -> 628 0 }t

\ Test other flags and width
t{ 629 2.345e-5 b" [%+g]" sprintf s" [+2.345e-05]" compare -> 629 0 }t
t{ 630 2.345e-5 b" [% g]" sprintf s" [ 2.345e-05]" compare -> 630 0 }t
t{ 631 -2.345e-5 b" [%13g]" sprintf s" [   -2.345e-05]" compare -> 631 0 }t
t{ 632 2.345e-5 b" [%0+14g]" sprintf s" [+00002.345e-05]" compare -> 632 0 }t
t{ 633 2.345e-5 b" [%-12g]" sprintf s" [2.345e-05   ]" compare -> 633 0 }t
t{ 634 2.345e-5 b" [% -#14g]" sprintf s" [ 2.34500e-05  ]" compare -> 634 0 }t

Testing infinities, NaNs and BAD
\ Creates an 'out of range' FP number , by generating r^n
: fpinf  ( f -- ) ( r -- r2 ) \ f true if negative required
   begin
      fdup sprf-pad 4 represent
   while
      fabs 2drop fdup f* dup if fnegate then
   repeat
   2drop drop
;

[undefined] SwiftForth [if]
t{ 650 1.5e0 fasin b" [%g]" sprintf s" [nan]" compare -> 650 0 }t
t{ 651 2e5  0 fpinf b" [%e]" sprintf s" [inf]" compare -> 651 0 }t
t{ 652 2e5 -1 fpinf b" [%g]" sprintf s" [-inf]" compare -> 652 0 }t
t{ 653 1.5e0 fasin b" [%E]" sprintf s" [NAN]" compare -> 653 0 }t
t{ 654 2e5  0 fpinf b" [%G]" sprintf s" [INF]" compare -> 654 0 }t
t{ 655 2e5 -1 fpinf b" [%E]" sprintf s" [-INF]" compare -> 655 0 }t
t{ 656 2e5 -1 fpinf b" [%7g]" sprintf s" [   -inf]" compare -> 656 0 }t
t{ 657 2e5 0 fpinf b" [%-7g]" sprintf s" [inf    ]" compare -> 657 0 }t
t{ 658 2e5 0 fpinf b" [%- 7g]" sprintf s" [inf    ]" compare -> 658 0 }t
t{ 659 2e5 0 fpinf b" [%#0+ 9.7g]" sprintf s" [      inf]" compare -> 659 0 }t
t{ 660 1.5e0 7 5 fasin b" [%*.*g]" sprintf s" [    nan]" compare -> 660 0 }t
[then]

\ Test large run of zeroes with %g
t{ 661 0.123456e23 b" %#.33g" sprintf 
                 s" 12345600000000000000000.0000000000" compare -> 661 0 }t
: check1.25e143%g  ( caddr u -- f )  \ True to pass
   2dup 2>r 149 = swap       ( -- f caddr )  \ Check overall length
   4 s" 1.25" compare 0= and                 \ check initial digits
   -1 2r@ 4 /string 5 - over + swap
   do i c@ '0' = and loop and                \ Check zeroes
   2r> 144 /string s" e+143" compare 0= and
;
\ This should give "1.25...(140 zeroes)...e+143"
t{ 662 1.25e143 b" %#.143g" sprintf check1.25e143%g -> 662 -1 }t

\ Test a 'BAD' return from REPRESENT by calling the appropriate internal word
pad max-precision bl fill
t{ 663 pad 1 invalid-number -> 663 #6589 }t

t{ 664 1.23e0 10 8 b" [%#*.*g]" sprintf s" [ 1.2300000]" compare -> 664 0 }t
t{ 665 1.23e-5 11 5 b" [%#*.*g]" sprintf s" [ 1.2300e-05]" compare -> 665 0 }t
t{ 666 1.2345e0 5 4 b" [%#*.*g]" sprintf s" [1.235]" compare -> 666 0 }t
t{ 667 1.2345e4 7 5 b" [%#+*.*g]" sprintf s" [+12345.]" compare -> 667 0 }t
t{ 668 1.234e4 11 4 b" [%#+*.*g]" sprintf s" [ +1.234e+04]" compare -> 668 0 }t

Testing multiple floating point arguments with/out other conversions

clear-fp-stack
t{ 700 4.56e0 5.67e1 6.78e2 b" %e %.1E" sprintf s" 5.670000e+01 6.8E+02" compare
\ cr ." Fdepth: " fdepth . ." , FP tos: " fdup fs. ." , fp-depth: " fp-depth @ . cr
            4.56e0 f- f0= fdepth -> 700 0 true 0 }t
clear-fp-stack
t{ 701 1 1.23e0 '2' 2.34e0 s" (3) " 3.45e0 b" (%d) %.3e, (%c) %.4f, %s%.5g" sprintf
               s" (1) 1.230e+00, (2) 2.3400, (3) 3.45" compare -> 701 0 }t
[then]
Testing errors

cr cr 
fp-enabled [if]
   .( Six error reports should be displayed:)
[else]
   .( Five error reports should be displayed:)
[then] cr
.( ------)
t{ 750 :noname s" This specification %&d is bad" sprintf ; catch -> 750 -1 }t
.( ------)
t{ 751 :noname s" Not enough %d %d arguments" sprintf ; catch -> 751 -1 }t
.( ------)
max-conv-width constant mcw sprbuf-size 10 + to max-conv-width
t{ 752 :noname sprbuf-size 1+ 0 s" %*.0u" sprintf ; catch -> 752 -1 }t
mcw to max-conv-width
.( ------)
variable sprbuf-ad sprbuf$ drop sprbuf-ad ! 0 to sprbuf
t{ 753 b" Buffer not set" sprintf -> 753 }t
sprbuf-ad @ to sprbuf
.( ------)
fp-enabled [if]
   clear-fp-stack
   t{ 754 :noname s" Not enough %e FP arguments" sprintf ; catch -> 754 -1 }t
   .( ------)
   max-fp-args constant mfpa 2 to max-fp-args   \ To induce an error
   t{ 755 :noname s" Too many fp formats %f %e %g" sprintf ; catch -> 755 -1 }t
   mfpa to max-fp-args     \ Restore the default value
   .( ------)
[else]
   t{ 756 :noname s" FP conversion %#+10f - no FP" sprintf ; catch -> 756 -1 }t
   .( ------)
[then]

cr .( End of tests )
cr cr .( -----[ Report ]-------)
#errors @ [if] cr .( *** Errors ***) [then]
cr .stack
cr .fpstack
cr .( Number of tests:)  #tests  @ 6 .r
cr .( Number of errors:) #errors @ 5 .r
cr .( ----------------------)

previous
cr cr

\ ---[ End of file ]------------------------------------------------------------


