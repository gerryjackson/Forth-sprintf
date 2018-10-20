# `PRINTF`, `SPRINTF`, `FPRINTF` - formatted output conversion

## User words provided

`PRINTF ( arguments* caddr u -- )`<br>
`SPRINTF ( arguments* caddr u -- caddr2 u2 )`<br>
`FPRINTF ( arguments* caddr u fileid -- )`<br>
`SET-SPRINTF-BUFFER ( caddr size -- )` Sets the output buffer for `SPRINTF` only. It is not needed for `PRINTF` or ``FPRINTF``. `SET-SPRINTF-BUFFER` must be used before the first time `SPRINTF` is called for the first time following loading - a default buffer is not provided by the system.

Where:<br>
`arguments*` is a sequence of zero or more arguments of various types<br>
`(caddr u)` is the format string<br>
`(caddr2 u2)` is the output from `SPRINTF`<br>

## Description

The `PRINTF` family produce output according to a format as described below. `PRINTF` writes output to the display terminal; `FPRINTF` writes output to the open file identified by fileid; `SPRINTF` writes to the string `(caddr u)`.

These three words write the output under the control of a format string that specifies how preceding arguments are converted for output.

`SET-SPRINTF-BUFFER` sets the output buffer for `SPRINTF` only. It is not needed for `PRINTF` or `FPRINTF`. SET-SPRINTF-BUFFER must be used before the first time `SPRINTF` is called for the first time - a default buffer is not provided by the system.

## Return value

`SPRINTF` returns the output string saved in memory. `PRINTF`, `FPRINTF` and `SET-SPRINTF-BUFFER` return nothing.

If an error is encountered, an exception is thrown.

## Format of the format string

The format string is a character string. It is composed of zero or more directives: ordinary characters (not '%'), which are copied unchanged to the output stream; and conversion specifications, each of which results in using zero or more subsequent arguments. Each conversion specification is introduced by the character '%', and ends with a conversion specifier. In between there may be (in this order) zero or more flags, an optional minimum field width, an optional precision and an optional length modifier.

The arguments must correspond properly with the conversion specifier. The arguments are used in the order given, where each `'*'` and each conversion specifier uses the next argument (and it is an error if insufficiently many arguments are provided). The arguments must be in the same left to right order as the conversion specifiers in the format string.

### The flag characters

The character '%' is followed by zero or more of the following flags:

**#**

The value should be converted to an "alternate form". For **d** and **u** conversions a non-zero result has the character '#' prefixed to it. For o conversions, the first character of the output string is made zero (by prefixing a 0 if it was not zero already). For **x** and **X** conversions, a nonzero result has the character '$' prefixed to it. For b conversions, a nonzero result has the character '%' prefixed to it. Any negative integer with a prefix will have the '-' sign after the prefix (e.g. #-); positive integers with a prefix will not include a '+' sign (this matches Forth input specifications). For **r** and **R** conversions with a radix (base) of 2, 8, 10, 16 the number is prefixed as for the **b, 0, d/u** and **x/X** respectively. For the **c** conversion the character is output as a Forth character literal e.g. 'x'. For **e, E, f, g**, and **G** conversions, the result will always contain a decimal point, even if no digits follow it (normally, a decimal point appears in the results of those conversions only if a digit follows). For **g** and **G** conversions, trailing zeros are not removed from the result as they would otherwise be. For other conversions, the # flag is ignored.

**0**

(a zero) The value should be zero padded. For **d, i, o, u, x, X, b, r, R, e, E, f, g**, and **G** conversions, the converted value is padded on the left with zeros rather than blanks. If the 0 and - flags both appear, the 0 flag is ignored. If a precision is given with a numeric conversion (**d, o, u, x, X, b, r** and **R**, the 0 flag is ignored. For other conversions, the 0 flag is ignored.

**-**

The converted value is to be left adjusted on the field boundary. (The default is right justification.) The converted value is padded on the right with blanks, rather than on the left with blanks or zeros. A '-' overrides a '0' if both are given.

**`' '`**

(a space) A blank should be left before a positive number (or empty string) produced by a signed conversion.

**+**

A sign (+ or -) should always be placed before a number produced by a signed conversion. By default a sign is used only for negative numbers. A + overrides a space if both are used. For **r** and **R** conversions + flag makes the conversion signed.

### The field width

An optional decimal digit string (with nonzero first digit) specifying a minimum field width. If the converted value has fewer characters than the field width, it will be padded with spaces on the left (or right, if the left-adjustment flag has been given). Instead of a decimal digit string one may write `\*` to specify that the field width is given in the next argument, which must be an integer. A negative field width in the conversion specifier is taken as a '-' flag followed by a positive field width. A negative field width in an argument is taken to be zero. In no case does a non-existent or small field width cause truncation of a field; if the result of a conversion is wider than the field width, the field is expanded to contain the conversion result. To prevent excessive output the width is limited to a user configurable value `MAX-CONV-WIDTH`.

### The precision

An optional precision, in the form of a period ('.') followed by an optional decimal digit string. Instead of a decimal digit string one may write `\*` to specify that the precision is given in the next argument, which must be an integer. If the precision is given as just '.' the precision is taken to be zero. It is an error for the precision to be negative. The precision gives the minimum number of digits to appear for **d, i, o, u, x, X, b, r** and **R** conversions; the number of digits to appear after the radix character for **e, E**, and **f** conversions; the maximum number of significant digits for **g** and **G** conversions; or the maximum number of characters to be printed from a string for **s** conversions. For a **c** conversion a specified precision p repeats the character argument p times. To prevent excessive output the width is limited to a user configurable value `MAX-CONV-PREC`.

### The length modifier

**l**

(lower case L) For **d, i, o, u, x, X, b, r** or **R** conversions l corresponds to a double integer argument.

### The conversion specifier

A character that specifies the type of conversion to be applied. The conversion specifiers and their meanings are:

**d**

The int argument is converted to signed decimal notation. The precision, if any, gives the minimum number of digits that must appear; if the converted value requires fewer digits, it is padded on the left with zeros. The default precision is 1. When 0 is printed with an explicit precision 0, the output is empty.

**o, u, x, X, b, r, R**

The unsigned int argument is converted to unsigned octal (**o**), unsigned decimal (**u**), unsigned hexadecimal (**x** and **X**), binary (**b**) or radix (**r** and **R**) notation. The letters **abcdef** are used for **x** conversions; the letters **ABCDEF** are used for **X** conversions. The letters **a** to **z** are used for **r** conversions; the letters **A** to **Z** for **R** conversions. The precision, if any, gives the minimum number of digits that must appear; if the converted value requires fewer digits, it is padded on the left with zeros. The default precision is 1. When 0 is printed with an explicit precision 0, the output is empty. 

**e, E**

The floating point argument is rounded and converted in the style \[-\]d.ddd**e**+dd or \[-\]d.ddd**e**-dd where there is one digit ('1' to '9') before the decimal-point character and the number of digits ( '0' to '9') after it is equal to the precision; if the precision is missing, it is taken as 6; if the precision is zero, no decimal-point character appears. An **E** conversion uses the letter **E** (rather than **e**) to introduce the exponent. The exponent always contains at least two digits; if the value is zero, the exponent is 00.

If the floating point argument is not a number, +infinity or -infinity and the Forth system implementation of `REPRESENT` returns a string starting with nan, inf then the output will be nan, +inf or -inf (NAN, +INF, -INF for the **E** conversion) respectively. If the return from `REPRESENT` indicates an invalid floating point number in another way the output will be bad (or BAD).

**f**

The double argument is rounded and converted to decimal notation in the style [-]ddd.ddd, where the number of digits after the decimal-point character is equal to the precision specification. If the precision is missing, it is taken as 6; if the precision is explicitly zero, no decimal-point character appears. If a decimal point appears, at least one digit appears before it.

If the floating point argument is invalid the output is the same as for the e and E conversions above.

**g, G**

The double argument is converted in style **f** or **e** (**f** or **E** for **G** conversions). The precision specifies the number of significant digits. If the precision is missing, 6 digits are given; if the precision is zero, it is treated as 1. Style **e** is used if the exponent from its conversion is less than -4 or greater than or equal to the precision otherwise style **f**. Trailing zeros are removed from the fractional part of the result; a decimal point appears only if it is followed by at least one digit.

If the floating point argument is invalid the output is the same as for the e and E conversions above.

**c**

The character argument is written to the output. If a precision p is specified that character is repeated p times. The #flag overrides precision.

**s**

The string (caddr u ) argument is written to the output. If a precision is specified, no more than the number of characters specified are written.

**%**

A '%' is written. No argument is converted. The complete conversion specification is '%%'. Any flags, width, precision or length are ignored. If any of these are required with the '%' character use the c conversion with a '%' argument.

### Portability

This implementation of the `PRINTF` family conforms to the Forth 2012 standard. Integer prefixes (e.g. #1234 for decimal 1234) and character literals (e.g. 'w').

### Notes

Using the `SPRINTF` buffer as both source and destination can be used to concatenate strings. For example the following will work:
```
s" Some stuff" s" %s" sprintf
s" %s, more stuff" printf
```
will display:   ```Some stuff, more stuff```

Hence `SPRINTF` output need not be copied elsewhere if the next `SPRINTF` to be executed simply needs to concatenate more text.

### Errors

If an error is detected an exception is thrown and reported with a display of the format string and error message. There are 8 types of errors:

1. "Invalid character" in a conversion specifier in the format string
1. "Too few arguments on the stack"
1. "Too few arguments on the FP stack"
1. "`FP-ARGS` array is too small" Too many FP arguments, change the value of `MAX-FP-ARGS` value in the configuration data
1. "Output buffer overflow". Increase the size of the buffer
1. "Run length encoding error". An internal fault, please report the error
1. "Invalid conversion - no floating point" when floating point conversions have not been included and the format string contains an **e, E, f, g** or **G** conversion.
1. "No sprintf buffer set, use `SET-SPRINTF-BUFFER`"

Following the error message, execution is aborted.

### Bugs

This software requires a separate floating point stack. The floating point conversions will not work with a combined data and floating point stack. The other conversions will still work correctly.


