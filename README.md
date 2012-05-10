# Erlang URI Handling Library

## Description

This OTP library provides functions for parsing and formatting RFC3896 URI strings.

## Examples

    1> uri:parse("syslog://somehost:912").
    {ok,{syslog,[],"somehost",912,"/",[]}}
    2> uri_format:to_string(element(2, v(1))).
    "syslog://somehost:912/"

# License

Copyright (c) 2012 Heroku <nem@erlang.geek.nz>  

Permission is hereby granted, free of charge, to any person  
obtaining a copy of this software and associated documentation  
files (the "Software"), to deal in the Software without  
restriction, including without limitation the rights to use,  
copy, modify, merge, publish, distribute, sublicense, and/or sell  
copies of the Software, and to permit persons to whom the  
Software is furnished to do so, subject to the following  
conditions:  

The above copyright notice and this permission notice shall be  
included in all copies or substantial portions of the Software.  

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND  
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT  
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,  
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING  
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR  
OTHER DEALINGS IN THE SOFTWARE.  

# References

* https://tools.ietf.org/html/rfc3986