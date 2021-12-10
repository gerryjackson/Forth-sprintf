\ Output buffer

\ Copyright (C) Gerry Jackson 2021

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

\ Provides an equivalent to the ANS Forth pictured output buffer (POB) that
\ doesn't have the defects of the POB which are:
\  - transient, can be corrupted by the system e.g. being moved or overwritten
\  - suffering from the same problem as the original ANS Forth locals of being
\    built up from right to left
\  - fixed in size
\  - doesn't use dataspace or the heap
\  - only one buffer being available to the programmer
\  - the inability to nest buffers
\  - system words such as .S may use the FOB hindering debugging
\  - the system need not check POB overflow
\
\ In contrast this Output Buffer:
\  - the memory space is supplied by the user, so can be of any size and
\    come from dataspace or the heap
\  - the contents are available until the user clears the buffer
\  - the output is built up from left to right
\  - any number of buffers can be defined and used whether nested or not
\  - one allocation of memory can be used for multiple buffers
\  - every write to the buffer is checked for buffer overflow
\  - is unused by the system

cr .( Output Buffer 0.2.6 - MIT software license)
cr .( Development version - not yet stable)
cr .( Copyright: G W Jackson) cr cr

\ ---[ The ~buffer structure ]--------------------------------------------------

0 value ~buffer      \ Holds the current ~buffer

: $field:  aligned 2 cells +field  ;   \ To hold (ca u) of a string

begin-structure ~buf-struct
   $field: ~here        \ (ca u) of unused part of ~buffer
   $field: ~buf         \ (ca u) of the ~buffer
   $field: ~concat      \ (ca u) of the current concatenation
    field: zap          \ For future use - holds an xt of a definition
end-structure

\ An optimisation follows as ~HERE has 0 offset, included here and used in
\ definitions in case a field is added before ~HERE
0 ~here 0= [if] : ~here ; immediate [then]

: init~buf  ( ~buf -- )
   dup >r ~buf 2@ over 0 r@ ~concat 2! r> ~here 2!
;

\ ~ZAP is for future use
  : ~zap  ( -- )  ~buffer zap @ execute  ;

: reset ;               \ For future use

\ NEW~BUFFER - (ca u) is the buffer which has been alloted or allocated
\              by the user
\            - ~buf-size is the size of the user buffer structure which is
\              probably an extension of the above structure
\            - xt is the execution token of the caller's RESET word
\            - executing "name" at runtime makes it the current buffer

: new~buffer  ( "name" xt ca u ~buf-size -- )
   create here >r allot r@ ~buf 2! r@ init~buf r> zap !
   does>  to ~buffer
;

: create~buffer  ( "name" ca u -- )  ['] reset -rot ~buf-struct new~buffer  ;

\ ?~BUF-OVERFLOW is deferred so that a user of the format output buffer can
\ re-target it to, for example, throw an exception or recover

defer ?~buf-overflow
:noname  ( f -- )  abort" ~Buffer overflow"  ; is ?~buf-overflow

\ ~BUFFER+ checks there is space for u characters and advances ~HERE by u. It
\ is the callers responsibility to actually save the characters in the buffer.
\ It returns the start address at which characters should be saved.
\ Intended for internal use only but maybe factor out the check on space for
\ external use e.g. for a circular buffer.

: ~buffer+  ( u -- ca1 )   \ ca1 is the pre-increment value
   >r ~buffer dup ~here dup 2@   ( -- ad ad ca1 u1 )  ( R: -- u )
   2dup r@ u< ?~buf-overflow     ( -- ad ad ca1 u1 ca1 )
   swap r@ /string               ( -- ad ad ca1 ca2 u2 )
   2swap >r 2! r>                ( -- ad ca1 )
   r> rot ~concat +!             ( -- ca1 )
;

\ ---[ Interface to buffer ]----------------------------------------------------

: ~hold  ( ch -- )  1 ~buffer+ c!  ;               \ Append character to ~buffer
: ~holds ( ca u -- )  dup ~buffer+ swap cmove  ;   \ Append (ca u) to ~buffer
: ~fill  ( n ch -- )  over ~buffer+ -rot fill  ;   \ Append n copies of ch
: <~  ( -- )  ~buffer init~buf ~zap  ;
\ : ~>     ( -- ca u )  ~buffer dup ~here cell+ @ swap ~concat cell+ @ tuck -  ;
: ~>     ( -- ca u )  ~buffer ~concat 2@  ;
synonym ~@ ~>

\ ~><~ closes the current concatenation and returns the (ca u) of its contents,
\ It does not clear the old concatenation. Opens a new concatenation 
: ~><~  ( -- ca u )  ~> ~buffer dup ~here cell+ @ 0 rot ~concat 2! ~zap  ;

\ ~buf@ is currently only used in the test word ~DUMP
: ~buf@  ( -- ca u )  ~buffer dup ~here @ >r ~buf 2@ r> -  ;

\ ~RESTORE  ( ca1 u1 -- ) where (ca1 u1) is a concatenation just before the
\ current concatenation. Restores the buffer so that (ca1 u1) becomes the
\ current concatenation effectively dropping the current concatenation.
\ It can also be used more generally to return to any location in the
\ current buffer e.g. to shorten the current concatenation, to drop several
\ concatenations or to return to a concatenation later in the buffer

: ~restore  ( ca u -- )
   ~buffer >r 2dup r@ ~concat 2!
   + r@ ~buf 2@ + over -         ( -- ca+u u2 ) \ u2 is ~buf unused
   r> ~here 2!
;

\ [test] [if]
\    : ~dump   ~buffer 24 dump  cr ~buf@ type  ;
\ [then]

\ ---[ End of file ]------------------------------------------------------------
