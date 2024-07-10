# Progress-ABL-4GL-Regex
## Perl's PCRE regular expressions for Progress ABL 4GL

Have you ever dreamed to use regular expressions in your Progress ABL L4G applications without depending on .NET? Now you can do it!

We developed a wrapper for the Windows and HP-UX ia64 11.31 versions of the PCRE library. It is very easy to use. Just instantiate the class, and use its methods.

You can find the code and more explanations in our GitHub repository :

File details :

`inc/regex.def`: TEMP-TABLE definitions for storing matches and sub-matches  
`inc/regex.ds`: DATASET definition holding the temp-tables  
`inc/regex.i`: can be included in any .p program to add support for native PCRE functions; uses the PCRE library DLL or shared library  
`lib/pcre-msvc-x86.dll`: PCRE library for Windows 32 bits
`lib/pcre-msvc-x64.dll`: PCRE library for Windows 64 bits
`lib/libpcre.so`: PCRE library for HP-UX 11.31 ia64  
`lib/libgcc_s.so.0`: needed for PCRE library to work on HP-UX  
`lib/libgcc_s.so.0.README`: just read it!  
`regex.p`: a container for regex.i  
`Regex.cls`: a class that can perform regex operations; uses regex.p  
`Binary.cls`: an helper class to perform bitwise operations  

As you can see in the include file, we use the following functions of the PCRE library:

`pcre_compile2`  
`pcre_study`  
`pcre_exec`  
`pcre_fullinfo`  
`pcre_config`  

The methods in the Regex.cls class are quite straitforward:

`mMatch`: returns true if there's a match  
`mNumMatches`: returns the number of matches, or a number below or equal to zero in case of error  
`mGetMatches`: returns the number of matches and populate a dataset of matches and sub-matches  
`mReplace`: performs a regex replace of a string with another string (sub-matches placeholders are supported in replacement string)  

To use our class, just place all the files in your project and start using it!

You can build a copy of pcre DLL yourself (even x64) by getting it at
https://sourceforge.net/projects/pcre/files/
and opening the folder in Visual Studio.
Configure it to build the shared libraries and support JIT compilation.
Then define PCRE_LIB preprocessor variable to whatever DLL name you have.
I built and included a Release version of PCRE with shared libs, JIT, UTF8 support
both in 32 bits (x86) and 64 bits (x64) and I renamed the resulting pcre.dll
to pcre-msvc-x64.dll and pcre-msvc-x86.dll.
