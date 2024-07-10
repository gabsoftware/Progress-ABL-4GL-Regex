
/*------------------------------------------------------------------------
    File        : regex.i
    Purpose     :

    Syntax      :

    Description : Include file that adds regex support in Progress ABL 4GL

    Author(s)   : Gabriel Hautclocq
    Created     : Tue May 12 16:37:59 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ********************  Preprocessor Definitions  ******************** */
&IF "{&OPSYS}" = "WIN32" &THEN
    /* WINDOWS */
    /* You can build a copy of pcre DLL yourself (even x64) by getting it at
       https://sourceforge.net/projects/pcre/files/
       and opening the folder in Visual Studio.
       Configure it to build the shared libraries and support JIT compilation.
       Then define PCRE_LIB to whatever DLL name you want.
       I built a Release version of PCRE with shared libs, JIT, UTF8 support
       both in 32 bits (x86) and 64 bits (x64)
       and I renamed the resulting pcre.dll
       to pcre-msvc-x64.dll and pcre-msvc-x86.dll.
    */
    &IF "{&PROCESS-ARCHITECTURE}" = "64" &THEN
        &SCOPED-DEFINE PCRE_LIB      pcre-msvc-x64.dll
    &ELSE
        &SCOPED-DEFINE PCRE_LIB      pcre-msvc-x86.dll
    &ENDIF
    &SCOPED-DEFINE PCRE_TYP      CDECL
&ELSE
    /* UNIX */
    &SCOPED-DEFINE PCRE_LIB      libpcre.so
    &SCOPED-DEFINE PCRE_TYP      CDECL
&ENDIF

/* VECTOR_SIZE defines 3x the maximal number of sub-matches that can be returned */
/*             for example, 96 = 32 sub-matches maximum (32 x 3 = 96)              */
&SCOPED-DEFINE     VECTOR_SIZE   96


/* ***************************  Definitions  ************************** */



{ inc/regex.ds }



FUNCTION get-library RETURNS CHARACTER () :

/*    RETURN SEARCH( "lib/{&PCRE_LIB}" ).*/

&IF "{&OPSYS}" = "WIN32" &THEN
    RETURN SEARCH( "lib/{&PCRE_LIB}" ).
&ELSE
    RETURN "{&PCRE_LIB}".
&ENDIF

END FUNCTION.

PROCEDURE pcre_compile :
    DEFINE INPUT               PARAMETER pattern     AS CHARACTER. /* const char *          */
    DEFINE INPUT               PARAMETER options     AS INTEGER.   /* int                   */
    DEFINE       OUTPUT        PARAMETER errptr      AS MEMPTR.    /* const char **         */
    DEFINE       OUTPUT        PARAMETER erroffset   AS INTEGER.   /* int *                 */
    DEFINE INPUT               PARAMETER tableptr    AS INTEGER.   /* const unsigned char * */
    DEFINE              OUTPUT PARAMETER result      AS MEMPTR.    /* pcre *                */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME             = "pcre_compile"
        hCall:LIBRARY               = libName
        hCall:CALL-TYPE             = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS        = 5
        hCall:RETURN-VALUE-DLL-TYPE = "MEMPTR".

    hCall:SET-PARAMETER(1, "CHARACTER"     , "INPUT" , pattern  ).
    hCall:SET-PARAMETER(2, "LONG"          , "INPUT" , options  ).
    hCall:SET-PARAMETER(3, "MEMPTR"        , "OUTPUT", errptr   ).
    hCall:SET-PARAMETER(4, "HANDLE TO LONG", "OUTPUT", erroffset).
    hCall:SET-PARAMETER(5, "LONG"          , "INPUT" , tableptr ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.

PROCEDURE pcre_compile2 :
    DEFINE INPUT               PARAMETER pattern     AS CHARACTER. /* const char *          */
    DEFINE INPUT               PARAMETER options     AS INTEGER.   /* int                   */
    DEFINE       OUTPUT        PARAMETER errcodeptr  AS INTEGER.   /* int *                 */
    DEFINE       OUTPUT        PARAMETER errptr      AS MEMPTR.    /* const char **         */
    DEFINE       OUTPUT        PARAMETER erroffset   AS INTEGER.   /* int *                 */
    DEFINE INPUT               PARAMETER tableptr    AS INTEGER.   /* const unsigned char * */
    DEFINE              OUTPUT PARAMETER result      AS MEMPTR.    /* pcre *                */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_compile2"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 6
        hCall:RETURN-VALUE-DLL-TYPE      = "MEMPTR".

    hCall:SET-PARAMETER(1, "CHARACTER"     , "INPUT" , pattern    ).
    hCall:SET-PARAMETER(2, "LONG"          , "INPUT" , options    ).
    hCall:SET-PARAMETER(3, "HANDLE TO LONG", "OUTPUT", errcodeptr ).
    hCall:SET-PARAMETER(4, "MEMPTR"        , "OUTPUT", errptr     ).
    hCall:SET-PARAMETER(5, "HANDLE TO LONG", "OUTPUT", erroffset  ).
    hCall:SET-PARAMETER(6, "LONG"          , "INPUT" , tableptr   ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.

PROCEDURE pcre_study :
    DEFINE INPUT               PARAMETER code        AS MEMPTR.    /* const pcre *          */
    DEFINE INPUT               PARAMETER options     AS INTEGER.   /* int                   */
    DEFINE       OUTPUT        PARAMETER errptr      AS MEMPTR.    /* const char **         */
    DEFINE              OUTPUT PARAMETER result      AS MEMPTR.    /* pcre_extra *          */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_study"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 3
        hCall:RETURN-VALUE-DLL-TYPE      = "MEMPTR".

    hCall:SET-PARAMETER(1, "MEMPTR", "INPUT" , code    ).
    hCall:SET-PARAMETER(2, "LONG"  , "INPUT" , options ).
    hCall:SET-PARAMETER(3, "MEMPTR", "OUTPUT", errptr  ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.

PROCEDURE pcre_exec :
    DEFINE INPUT               PARAMETER code        AS MEMPTR.         /* const pcre *           */
    DEFINE INPUT               PARAMETER extra       AS MEMPTR.         /* const pcre_extra *     */
    DEFINE INPUT               PARAMETER subject     AS CHARACTER.      /* const char *           */
    DEFINE INPUT               PARAMETER length      AS INTEGER.        /* int                    */
    DEFINE INPUT               PARAMETER startoffset AS INTEGER.        /* int                    */
    DEFINE INPUT               PARAMETER options     AS INTEGER.        /* int                    */
    DEFINE       OUTPUT        PARAMETER ovector     AS INTEGER EXTENT {&VECTOR_SIZE}. /* int *   */
    DEFINE INPUT               PARAMETER ovecsize    AS INTEGER.        /* int                    */
    DEFINE              OUTPUT PARAMETER result      AS INTEGER.        /* int                    */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_exec"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 8
        hCall:RETURN-VALUE-DLL-TYPE      = "LONG".

    hCall:SET-PARAMETER(1, "MEMPTR"        , "INPUT" , code        ).
    hCall:SET-PARAMETER(2, "MEMPTR"        , "INPUT" , extra       ).
    hCall:SET-PARAMETER(3, "CHARACTER"     , "INPUT" , subject     ).
    hCall:SET-PARAMETER(4, "LONG"          , "INPUT" , length      ).
    hCall:SET-PARAMETER(5, "LONG"          , "INPUT" , startoffset ).
    hCall:SET-PARAMETER(6, "LONG"          , "INPUT" , options     ).
    hCall:SET-PARAMETER(7, "HANDLE TO LONG", "OUTPUT", ovector     ).
    hCall:SET-PARAMETER(8, "LONG"          , "INPUT" , ovecsize    ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.

/**
  pcre_free does not exist, it is a pointer to a user-provided function. So we use SET-SIZE() instead.
**/

/*PROCEDURE pcre_free :                                                                               */
/*    DEFINE INPUT               PARAMETER code        AS MEMPTR.         /* const pcre *           */*/
/*                                                                                                    */
/*    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.                                                   */
/*    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.                                                   */
/*                                                                                                    */
/*    libName = get-library().                                                                        */
/*                                                                                                    */
/*    CREATE CALL hCall.                                                                              */
/*    ASSIGN                                                                                          */
/*        hCall:CALL-NAME                  = "pcre_free"                                              */
/*        hCall:LIBRARY                    = libName                                                  */
/*        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"                                                  */
/*        hCall:CALL-TYPE                  = DLL-CALL-TYPE                                            */
/*        hCall:NUM-PARAMETERS             = 1.                                                       */
/*                                                                                                    */
/*    hCall:SET-PARAMETER(1, "MEMPTR"        , "INPUT" , code        ).                               */
/*    hCall:INVOKE().                                                                                 */
/*                                                                                                    */
/*    DELETE OBJECT hCall.                                                                            */
/*                                                                                                    */
/*END PROCEDURE.                                                                                      */

PROCEDURE pcre_free_study :
    DEFINE INPUT               PARAMETER extra        AS MEMPTR.         /* const pcre_extra *           */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_free_study"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 1.

    hCall:SET-PARAMETER(1, "MEMPTR"        , "INPUT" , extra        ).
    hCall:INVOKE().

    DELETE OBJECT hCall.

END PROCEDURE.


PROCEDURE pcre_fullinfo :
    DEFINE INPUT               PARAMETER code        AS MEMPTR . /* const pcre *           */
    DEFINE INPUT               PARAMETER extra       AS MEMPTR . /* const pcre_extra *     */
    DEFINE INPUT               PARAMETER what        AS INTEGER. /* int                    */
    DEFINE       OUTPUT        PARAMETER iwhere      AS INTEGER. /* void *                 */
    DEFINE              OUTPUT PARAMETER result      AS INTEGER. /* int                    */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_fullinfo"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 4
        hCall:RETURN-VALUE-DLL-TYPE      = "LONG".

    hCall:SET-PARAMETER(1, "MEMPTR" , "INPUT" , code   ).
    hCall:SET-PARAMETER(2, "MEMPTR" , "INPUT" , extra  ).
    hCall:SET-PARAMETER(3, "LONG"   , "OUTPUT", what   ).
    hCall:SET-PARAMETER(4, "LONG"   , "OUTPUT", iwhere ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.


PROCEDURE pcre_config :
    DEFINE INPUT               PARAMETER what        AS INTEGER. /* int       */
    DEFINE       OUTPUT        PARAMETER iwhere      AS INTEGER. /* void *    */
    DEFINE              OUTPUT PARAMETER result      AS INTEGER. /* int       */

    DEFINE VARIABLE libName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCall   AS HANDLE    NO-UNDO.

    libName = get-library().

    CREATE CALL hCall.
    ASSIGN
        hCall:CALL-NAME                  = "pcre_config"
        hCall:LIBRARY                    = libName
        hCall:LIBRARY-CALLING-CONVENTION = "CDECL"
        hCall:CALL-TYPE                  = DLL-CALL-TYPE
        hCall:NUM-PARAMETERS             = 2
        hCall:RETURN-VALUE-DLL-TYPE      = "LONG".

    hCall:SET-PARAMETER(1, "LONG"   , "OUTPUT", what   ).
    hCall:SET-PARAMETER(2, "LONG"   , "OUTPUT", iwhere ).
    hCall:INVOKE().
    ASSIGN result = hCall:RETURN-VALUE.

    DELETE OBJECT hCall.

END PROCEDURE.





/* ***************************  Main Block  *************************** */

/*
    Returns the number of matches, or an error code

    Returns :
         1 + n  if there are n sub-matches, with n > 0
         1 if there is one match but no sub-matches
         0 if too many matches or submatches
        -1 if no match
        -2 if parameter error (passing NULL for example)
        -3 if bad option passed
        -4 or less if other error other than regex syntax error
        -999n if regex syntax error, with n equal to:
             1  \ at end of pattern
             2  \c at end of pattern
             3  unrecognized character follows \
             4  numbers out of order in {} quantifier
             5  number too big in {} quantifier
             6  missing terminating ] for character class
             7  invalid escape sequence in character class
             8  range out of order in character class
             9  nothing to repeat
            10  [this code is not in use]
            11  internal error: unexpected repeat
            12  unrecognized character after (? or (?-
            13  POSIX named classes are supported only within a class
            14  missing )
            15  reference to non-existent subpattern
            16  erroffset passed as NULL
            17  unknown option bit(s) set
            18  missing ) after comment
            19  [this code is not in use]
            20  regular expression is too large
            21  failed to get memory
            22  unmatched parentheses
            23  internal error: code overflow
            24  unrecognized character after (?<
            25  lookbehind assertion is not fixed length
            26  malformed number or name after (?(
            27  conditional group contains more than two branches
            28  assertion expected after (?(
            29  (?R or (?[+-]digits must be followed by )
            30  unknown POSIX class name
            31  POSIX collating elements are not supported
            32  this version of PCRE is compiled without UTF support
            33  [this code is not in use]
            34  character value in \x{} or \o{} is too large
            35  invalid condition (?(0)
            36  \C not allowed in lookbehind assertion
            37  PCRE does not support \L, \l, \N{name}, \U, or \u
            38  number after (?C is > 255
            39  closing ) for (?C expected
            40  recursive call could loop indefinitely
            41  unrecognized character after (?P
            42  syntax error in subpattern name (missing terminator)
            43  two named subpatterns have the same name
            44  invalid UTF-8 string (specifically UTF-8)
            45  support for \P, \p, and \X has not been compiled
            46  malformed \P or \p sequence
            47  unknown property name after \P or \p
            48  subpattern name is too long (maximum 32 characters)
            49  too many named subpatterns (maximum 10000)
            50  [this code is not in use]
            51  octal value is greater than \377 in 8-bit non-UTF-8 mode
            52  internal error: overran compiling workspace
            53  internal error: previously-checked referenced subpattern
                  not found
            54  DEFINE group contains more than one branch
            55  repeating a DEFINE group is not allowed
            56  inconsistent NEWLINE options
            57  \g is not followed by a braced, angle-bracketed, or quoted
                  name/number or by a plain number
            58  a numbered reference must not be zero
            59  an argument is not allowed for (*ACCEPT), (*FAIL), or (*COMMIT)
            60  (*VERB) not recognized or malformed
            61  number is too big
            62  subpattern name expected
            63  digit expected after (?+
            64  ] is an invalid data character in JavaScript compatibility mode
            65  different names for subpatterns of the same number are
                  not allowed
            66  (*MARK) must have an argument
            67  this version of PCRE is not compiled with Unicode property
                  support
            68  \c must be followed by an ASCII character
            69  \k is not followed by a braced, angle-bracketed, or quoted name
            70  internal error: unknown opcode in find_fixedlength()
            71  \N is not supported in a class
            72  too many forward references
            73  disallowed Unicode code point (>= 0xd800 && <= 0xdfff)
            74  invalid UTF-16 string (specifically UTF-16)
            75  name is too long in (*MARK), (*PRUNE), (*SKIP), or (*THEN)
            76  character value in \u.... sequence is too large
            77  invalid UTF-32 string (specifically UTF-32)
            78  setting UTF is disabled by the application
            79  non-hex character in \x{} (closing brace missing?)
            80  non-octal character in \o{} (closing brace missing?)
            81  missing opening brace after \o
            82  parentheses are too deeply nested
            83  invalid range in character class
            84  group name must start with a non-digit
            85  parentheses are too deeply nested (stack check)

*/
PROCEDURE match :
    DEFINE INPUT    PARAMETER  pr_ch_str     AS CHARACTER NO-UNDO.
    DEFINE INPUT    PARAMETER  pr_ch_regex   AS CHARACTER NO-UNDO.
    DEFINE INPUT    PARAMETER  pr_in_options AS INT64     NO-UNDO.
    DEFINE OUTPUT   PARAMETER  pr_in_ret     AS INTEGER   NO-UNDO INITIAL -1.

    DEFINE VARIABLE pt_error         AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE ch_error         AS CHARACTER NO-UNDO.

    //DEFINE VARIABLE pt_eroff         AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE in_eroff         AS INTEGER   NO-UNDO.

    DEFINE VARIABLE in_errcode       AS INTEGER   NO-UNDO.

    DEFINE VARIABLE pt_pcre          AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE pt_pcre_extra    AS MEMPTR    NO-UNDO.

    DEFINE VARIABLE in_options32     AS INTEGER   NO-UNDO.

    /*
        - only 2/3 of the vector is exploitable
        - each match and sub-matche uses 2 vector entries
        - in the pair, the first is the start offset, the second the end offset of the match
        - the first and second entries of the vector are for the whole regex match
    */
    DEFINE VARIABLE in_vector       AS INTEGER   NO-UNDO EXTENT {&VECTOR_SIZE}.

    DEFINE VARIABLE in_ret          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_nb           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_cpt          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_cpt2         AS INTEGER   NO-UNDO INITIAL 0.

    DEFINE VARIABLE option_bits     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE utf8            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE d               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE crlf_is_newline AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE options         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE start_offset    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_info         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_conf         AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lg_global       AS LOGICAL   NO-UNDO INITIAL FALSE.

    IF Binary:BIN_AND64( pr_in_options, 0x40000000 ) <> 0 THEN DO :
        ASSIGN
            /* global search */
            lg_global     = TRUE

            /* we remove this special option from the option list */
            pr_in_options = Binary:BIN_AND64(
                pr_in_options,
                Binary:BIN_NOT64( 0x40000000 ) )
        .
    END.
    ASSIGN in_options32 = INTEGER( pr_in_options ).

    SET-SIZE(pt_error) = 128.
    //SET-SIZE(pt_eroff) = 4.

    /* compile the regex */
    RUN pcre_compile2(
        INPUT  pr_ch_regex,
        INPUT  in_options32,
        OUTPUT in_errcode,
        OUTPUT pt_error,
        OUTPUT in_eroff,
        INPUT  0,
        OUTPUT pt_pcre ).

    /* if error, returns an error code and leave */
    IF in_errcode <> 0 THEN DO :
        pr_in_ret = INTEGER( "-999" + STRING( in_errcode ) ).
        IF GET-POINTER-VALUE( pt_error) > 0 THEN DO :
            ASSIGN ch_error = GET-STRING( pt_error, 0 ) NO-ERROR.
            IF ch_error <> ? AND ch_error <> "" THEN MESSAGE ch_error VIEW-AS ALERT-BOX.
            SET-SIZE(pt_error) = 0.
        END.
        IF GET-POINTER-VALUE( pt_pcre) > 0 THEN DO :
            //RUN pcre_free( INPUT pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
        END.
        
        LEAVE.
    END.

    /* if no error */
    IF GET-POINTER-VALUE( pt_pcre ) <> 0 THEN DO :

        /* study the regex */
        RUN pcre_study(
            INPUT  pt_pcre,
            INPUT  0,
            OUTPUT pt_error,
            OUTPUT pt_pcre_extra ).
            
        IF GET-POINTER-VALUE( pt_pcre_extra ) = 0 THEN DO :
            
            IF GET-POINTER-VALUE( pt_error) > 0 THEN DO :
                ASSIGN ch_error = GET-STRING( pt_error, 0 ) NO-ERROR.
                IF ch_error <> ? AND ch_error <> "" THEN MESSAGE ch_error VIEW-AS ALERT-BOX.
                SET-SIZE(pt_error) = 0.
            END.
            
            //RUN pcre_free( INPUT pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
            
            LEAVE.
        END. 
        

        /* test the string with the compiled and optimized regex */
        RUN pcre_exec(
            INPUT  pt_pcre,
            INPUT  pt_pcre_extra,
            INPUT  pr_ch_str,
            INPUT  LENGTH( pr_ch_str ),
            INPUT  0,
            INPUT  0,
            OUTPUT in_vector,
            INPUT  {&VECTOR_SIZE},
            OUTPUT pr_in_ret ).


        /* if we got matches */
        IF pr_in_ret >= 0 THEN DO :

            /* if = 0 then we got more matches than we can hold */
            IF pr_in_ret = 0 THEN DO :
                pr_in_ret = ( {&VECTOR_SIZE} / 3 ).
            END.

            ASSIGN
                in_nb     = pr_in_ret /* number of matches + sub-matches */
                pr_in_ret = 1         /* number of matches */
            .

            /*
                ==============================================
                if global search, we continue searching
                ==============================================
            */
            IF lg_global THEN DO :

                RUN pcre_fullinfo(
                    INPUT  pt_pcre,
                    INPUT  pt_pcre_extra,
                    INPUT  0, /* PCRE_INFO_OPTIONS */
                    OUTPUT option_bits,
                    OUTPUT in_info ).

                utf8        = Binary:BIN_AND(
                    option_bits,
                    0x00000800 ). /* PCRE_UTF8 */
                option_bits = Binary:BIN_AND(
                    option_bits,
                    Binary:BIN_OR(
                        0x00100000, /* PCRE_NEWLINE_CR */
                        Binary:BIN_OR(
                            0x00200000, /* PCRE_NEWLINE_LF */
                            Binary:BIN_OR(
                                0x00300000, /* PCRE_NEWLINE_CRLF */
                                Binary:BIN_OR(
                                    0x00400000, /* PCRE_NEWLINE_ANY */
                                    0x00500000  /* PCRE_NEWLINE_ANYCRLF */
                                )
                            )
                        )
                    )
                ).

                IF option_bits = 0 THEN DO :

                    RUN pcre_config(
                        INPUT  1, /* PCRE_CONFIG_NEWLINE */
                        OUTPUT d,
                        OUTPUT in_conf ).

                    CASE d :
                        WHEN 13 THEN ASSIGN option_bits = 0x00100000. /* PCRE_NEWLINE_CR */
                        WHEN 10 THEN ASSIGN option_bits = 0x00200000. /* PCRE_NEWLINE_LF */
                        WHEN Binary:BIN_AND( Binary:BIN_LSHIFT( 13, 8 ), 10 )
                                THEN ASSIGN option_bits = 0x00300000. /* PCRE_NEWLINE_CRLF */
                        WHEN -2 THEN ASSIGN option_bits = 0x00500000. /* PCRE_NEWLINE_ANYCRLF */
                        WHEN -1 THEN ASSIGN option_bits = 0x00400000. /* PCRE_NEWLINE_ANY */
                        OTHERWISE    ASSIGN option_bits = 0.
                    END CASE.

                END.

                ASSIGN
                crlf_is_newline = option_bits = 0x00400000 /* PCRE_NEWLINE_ANY */
                               OR option_bits = 0x00300000 /* PCRE_NEWLINE_CRLF */
                               OR option_bits = 0x00500000 /* PCRE_NEWLINE_ANYCRLF */
                .

                /* loop for next matches */
                loop_next_match:
                DO WHILE TRUE :

                    ASSIGN
                        options      = 0
                        start_offset = in_vector[ 2 ]

                        NO-ERROR
                    .

                    IF in_vector[ 1 ] = in_vector[ 2 ] THEN DO :
                        IF in_vector[ 1 ] = LENGTH( pr_ch_str ) THEN DO :
                            LEAVE loop_next_match.
                        END.

                        options = Binary:BIN_OR(
                            0x10000000, /* PCRE_NOTEMPTY_ATSTART */
                            0x00000010  /* PCRE_ANCHORED         */
                        ).
                    END.

                    RUN pcre_exec(
                        INPUT  pt_pcre,
                        INPUT  pt_pcre_extra,
                        INPUT  pr_ch_str,
                        INPUT  LENGTH( pr_ch_str ),
                        INPUT  start_offset,
                        INPUT  options,
                        OUTPUT in_vector,
                        INPUT  {&VECTOR_SIZE},
                        OUTPUT in_ret ).

                    IF in_ret = -1 /* PCRE_ERROR_NOMATCH */
                    THEN DO :
                        IF options = 0 THEN LEAVE loop_next_match.
                        in_vector[ 2 ] = start_offset + 1.
                        IF  crlf_is_newline
                        AND start_offset < LENGTH( pr_ch_str ) - 1
                        AND SUBSTRING( pr_ch_str, start_offset + 1, 1 ) = "~r"
                        AND SUBSTRING( pr_ch_str, start_offset + 2, 1 ) = "~n"
                        THEN DO :
                            ASSIGN in_vector[ 2 ] = in_vector[ 2 ] + 1.
                        END.
                        ELSE IF utf8 <> 0 THEN DO :
                            loop_internal:
                            DO WHILE in_vector[ 2 ] < LENGTH( pr_ch_str ) :
                                IF Binary:BIN_AND(
                                    ASC( SUBSTRING( pr_ch_str, in_vector[ 2 ], 1 ) ),
                                    0xC0
                                ) <> 0x80 THEN LEAVE loop_internal.
                                ASSIGN in_vector[ 2 ] = in_vector[ 2 ] + 1 NO-ERROR.
                            END. /* loop_internal */
                        END.
                        NEXT loop_next_match.
                    END.

                    IF in_ret < 0 THEN DO :
                        LEAVE loop_next_match.
                    END.

                    IF in_ret = 0 THEN DO :
                        in_ret = ( {&VECTOR_SIZE} / 3 ).
                    END.

                    ASSIGN
                        in_nb  = in_ret                /* number of matches + sub-matches */
                        in_ret = 1                     /* number of matches               */
                        pr_in_ret = pr_in_ret + in_ret /* total nbre matches              */
                    .

                END. /* loop_next_match */

            END. /* if lg_global */
            
        END. /* IF pr_in_ret >= 0 */

        IF GET-POINTER-VALUE( pt_pcre_extra) > 0 THEN DO :
            RUN pcre_free_study( pt_pcre_extra ).
            //IF GET-POINTER-VALUE( pt_pcre_extra) > 0 THEN SET-SIZE(pt_pcre_extra) = 0.
        END.

        IF GET-POINTER-VALUE( pt_pcre) > 0 THEN DO :
            //RUN pcre_free( INPUT pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
        END.

    END. /* IF pt_pcre <> ? AND in_errcode = 0 */

END PROCEDURE.



/*
    Returns the number of matches, or an error code.
    Also returns a DATASET containing matches

    Returns :
         1 + n  if there are n sub-matches, with n > 0
         1 if there is one match but no sub-matches
         0 if too many matches or submatches
        -1 if no match
        -2 if parameter error (passing NULL for example)
        -3 if bad option passed
        -4 or less if other error other than regex syntax error
        -999n if regex syntax error, with n equal to:
             1  \ at end of pattern
             2  \c at end of pattern
             3  unrecognized character follows \
             4  numbers out of order in {} quantifier
             5  number too big in {} quantifier
             6  missing terminating ] for character class
             7  invalid escape sequence in character class
             8  range out of order in character class
             9  nothing to repeat
            10  [this code is not in use]
            11  internal error: unexpected repeat
            12  unrecognized character after (? or (?-
            13  POSIX named classes are supported only within a class
            14  missing )
            15  reference to non-existent subpattern
            16  erroffset passed as NULL
            17  unknown option bit(s) set
            18  missing ) after comment
            19  [this code is not in use]
            20  regular expression is too large
            21  failed to get memory
            22  unmatched parentheses
            23  internal error: code overflow
            24  unrecognized character after (?<
            25  lookbehind assertion is not fixed length
            26  malformed number or name after (?(
            27  conditional group contains more than two branches
            28  assertion expected after (?(
            29  (?R or (?[+-]digits must be followed by )
            30  unknown POSIX class name
            31  POSIX collating elements are not supported
            32  this version of PCRE is compiled without UTF support
            33  [this code is not in use]
            34  character value in \x{} or \o{} is too large
            35  invalid condition (?(0)
            36  \C not allowed in lookbehind assertion
            37  PCRE does not support \L, \l, \N{name}, \U, or \u
            38  number after (?C is > 255
            39  closing ) for (?C expected
            40  recursive call could loop indefinitely
            41  unrecognized character after (?P
            42  syntax error in subpattern name (missing terminator)
            43  two named subpatterns have the same name
            44  invalid UTF-8 string (specifically UTF-8)
            45  support for \P, \p, and \X has not been compiled
            46  malformed \P or \p sequence
            47  unknown property name after \P or \p
            48  subpattern name is too long (maximum 32 characters)
            49  too many named subpatterns (maximum 10000)
            50  [this code is not in use]
            51  octal value is greater than \377 in 8-bit non-UTF-8 mode
            52  internal error: overran compiling workspace
            53  internal error: previously-checked referenced subpattern
                  not found
            54  DEFINE group contains more than one branch
            55  repeating a DEFINE group is not allowed
            56  inconsistent NEWLINE options
            57  \g is not followed by a braced, angle-bracketed, or quoted
                  name/number or by a plain number
            58  a numbered reference must not be zero
            59  an argument is not allowed for (*ACCEPT), (*FAIL), or (*COMMIT)
            60  (*VERB) not recognized or malformed
            61  number is too big
            62  subpattern name expected
            63  digit expected after (?+
            64  ] is an invalid data character in JavaScript compatibility mode
            65  different names for subpatterns of the same number are
                  not allowed
            66  (*MARK) must have an argument
            67  this version of PCRE is not compiled with Unicode property
                  support
            68  \c must be followed by an ASCII character
            69  \k is not followed by a braced, angle-bracketed, or quoted name
            70  internal error: unknown opcode in find_fixedlength()
            71  \N is not supported in a class
            72  too many forward references
            73  disallowed Unicode code point (>= 0xd800 && <= 0xdfff)
            74  invalid UTF-16 string (specifically UTF-16)
            75  name is too long in (*MARK), (*PRUNE), (*SKIP), or (*THEN)
            76  character value in \u.... sequence is too large
            77  invalid UTF-32 string (specifically UTF-32)
            78  setting UTF is disabled by the application
            79  non-hex character in \x{} (closing brace missing?)
            80  non-octal character in \o{} (closing brace missing?)
            81  missing opening brace after \o
            82  parentheses are too deeply nested
            83  invalid range in character class
            84  group name must start with a non-digit
            85  parentheses are too deeply nested (stack check)

*/
PROCEDURE get_matches :
    DEFINE INPUT    PARAMETER  pr_ch_str     AS CHARACTER NO-UNDO.
    DEFINE INPUT    PARAMETER  pr_ch_regex   AS CHARACTER NO-UNDO.
    DEFINE INPUT    PARAMETER  pr_in_options AS INT64     NO-UNDO.
    DEFINE OUTPUT   PARAMETER  pr_in_ret     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT   PARAMETER  DATASET       FOR ds_regex.



    DEFINE VARIABLE pt_error        AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE ch_error        AS CHARACTER NO-UNDO.

    //DEFINE VARIABLE pt_eroff        AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE in_eroff        AS INTEGER   NO-UNDO.

    DEFINE VARIABLE in_errcode      AS INTEGER   NO-UNDO.

    DEFINE VARIABLE pt_pcre         AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE pt_pcre_extra   AS MEMPTR    NO-UNDO.

    DEFINE VARIABLE in_options32    AS INTEGER   NO-UNDO.

    /*
        - only 2/3 of the vector is exploitable
        - each match and sub-matche uses 2 vector entries
        - in the pair, the first is the start offset, the second the end offset of the match
        - the first and second entries of the vector are for the whole regex match
    */
    DEFINE VARIABLE in_vector       AS INTEGER   NO-UNDO EXTENT {&VECTOR_SIZE}.

    DEFINE VARIABLE in_ret          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_nb           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_cpt          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_cpt2         AS INTEGER   NO-UNDO INITIAL 0.

    DEFINE VARIABLE option_bits     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE utf8            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE d               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE crlf_is_newline AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE options         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE start_offset    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_info         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE in_conf         AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lg_global       AS LOGICAL   NO-UNDO INITIAL FALSE.

    IF Binary:BIN_AND64( pr_in_options, 0x40000000 ) <> 0 THEN DO :
        ASSIGN
            /* global search */
            lg_global     = TRUE

            /* we remove this special option from the option list */
            pr_in_options = Binary:BIN_AND64(
                pr_in_options,
                Binary:BIN_NOT64( 0x40000000 ) )
        .
    END.
    ASSIGN in_options32 = INTEGER( pr_in_options ).

    SET-SIZE(pt_error) = 128.
    //SET-SIZE(pt_eroff) = 4.

    /* compile the regex */
    RUN pcre_compile2(
        INPUT  pr_ch_regex,
        INPUT  in_options32,
        OUTPUT in_errcode,
        OUTPUT pt_error,
        OUTPUT in_eroff,
        INPUT  0,
        OUTPUT pt_pcre ).

    /* if error, returns an error code and leave */
    IF in_errcode <> 0 THEN DO :
        pr_in_ret = INTEGER( "-999" + STRING( in_errcode ) ).
        IF GET-POINTER-VALUE( pt_error) > 0 THEN DO :
            ASSIGN ch_error = GET-STRING( pt_error, 0 ) NO-ERROR.
            IF ch_error <> ? AND ch_error <> "" THEN MESSAGE ch_error VIEW-AS ALERT-BOX.
            SET-SIZE(pt_error) = 0.
        END.
        IF GET-POINTER-VALUE( pt_pcre) > 0 THEN DO :
            //RUN pcre_free( pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
        END.
        
        LEAVE.
    END.

    /* if no errors */
    IF GET-POINTER-VALUE( pt_pcre ) <> 0 THEN DO :

        /* study the regex */
        RUN pcre_study(
            INPUT  pt_pcre,
            INPUT  0,
            OUTPUT pt_error,
            OUTPUT pt_pcre_extra ).
            
        IF GET-POINTER-VALUE( pt_pcre_extra ) = 0 THEN DO :
            
            IF GET-POINTER-VALUE( pt_error) > 0 THEN DO :
                ASSIGN ch_error = GET-STRING( pt_error, 0 ) NO-ERROR.
                IF ch_error <> ? AND ch_error <> "" THEN MESSAGE ch_error VIEW-AS ALERT-BOX.
                SET-SIZE(pt_error) = 0.
            END.
            
            //RUN pcre_free( pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
            
            LEAVE.
        END.

        /* test the string with the compiled and optimized regex */
        RUN pcre_exec(
            INPUT  pt_pcre,
            INPUT  pt_pcre_extra,
            INPUT  pr_ch_str,
            INPUT  LENGTH( pr_ch_str ),
            INPUT  0,
            INPUT  0,
            OUTPUT in_vector,
            INPUT  {&VECTOR_SIZE},
            OUTPUT pr_in_ret ).

        /* if we got matches */
        IF pr_in_ret >= 0 THEN DO :

            /* if = 0 then more matches than */
            IF pr_in_ret = 0 THEN DO :
                pr_in_ret = ( {&VECTOR_SIZE} / 3 ).
            END.

            ASSIGN
                in_nb     = pr_in_ret /* number of matches + sub-matches */
                pr_in_ret = 1         /* number of matches */
            .

            /* initialize the dataset */
            DATASET ds_regex:EMPTY-DATASET().

            /* Fill the DATASET with the VECTOR */
            DO in_cpt = 1 TO in_nb :

                IF in_cpt = 1 THEN DO :
                    CREATE tt_matches.
                    ASSIGN
                        in_cpt2           = in_cpt2 + 1
                        tt_matches.NUM    = in_cpt
                        tt_matches.NBSM   = 0
                        tt_matches.OSTART = in_vector[ in_cpt * 2 - 1 ] + 1
                        tt_matches.OEND   = in_vector[ in_cpt * 2     ] + 1
                        tt_matches.LEN    = tt_matches.OEND - tt_matches.OSTART
                        tt_matches.VAL    = SUBSTRING(
                            pr_ch_str,         /* source string      */
                            tt_matches.OSTART, /* offset start match */
                            tt_matches.LEN     /* length of match    */
                        ) NO-ERROR.
                END.
                ELSE DO :
                    CREATE tt_submatches.
                    ASSIGN
                        tt_matches.NBSM      = tt_matches.NBSM + 1
                        tt_submatches.NUM    = tt_matches.NUM
                        tt_submatches.SNUM   = in_cpt - 1
                        tt_submatches.OSTART = in_vector[ in_cpt * 2 - 1 ] + 1
                        tt_submatches.OEND   = in_vector[ in_cpt * 2     ] + 1
                        tt_submatches.LEN    = tt_submatches.OEND - tt_submatches.OSTART
                        tt_submatches.VAL    = SUBSTRING(
                            pr_ch_str,            /* source string      */
                            tt_submatches.OSTART, /* offset start match */
                            tt_submatches.LEN     /* length of match    */
                        ) NO-ERROR. 
                END.
            END.

            /*
                ==============================================
                if global search, we continue searching
                ==============================================
            */
            IF lg_global THEN DO :

                RUN pcre_fullinfo(
                    INPUT  pt_pcre,
                    INPUT  pt_pcre_extra,
                    INPUT  0, /* PCRE_INFO_OPTIONS */
                    OUTPUT option_bits,
                    OUTPUT in_info ).

                utf8        = Binary:BIN_AND(
                    option_bits,
                    0x00000800 ). /* PCRE_UTF8 */
                option_bits = Binary:BIN_AND(
                    option_bits,
                    Binary:BIN_OR(
                        0x00100000, /* PCRE_NEWLINE_CR */
                        Binary:BIN_OR(
                            0x00200000, /* PCRE_NEWLINE_LF */
                            Binary:BIN_OR(
                                0x00300000, /* PCRE_NEWLINE_CRLF */
                                Binary:BIN_OR(
                                    0x00400000, /* PCRE_NEWLINE_ANY */
                                    0x00500000  /* PCRE_NEWLINE_ANYCRLF */
                                )
                            )
                        )
                    )
                ).

                IF option_bits = 0 THEN DO :

                    RUN pcre_config(
                        INPUT  1, /* PCRE_CONFIG_NEWLINE */
                        OUTPUT d,
                        OUTPUT in_conf ).

                    CASE d :
                        WHEN 13 THEN ASSIGN option_bits = 0x00100000. /* PCRE_NEWLINE_CR */
                        WHEN 10 THEN ASSIGN option_bits = 0x00200000. /* PCRE_NEWLINE_LF */
                        WHEN Binary:BIN_AND( Binary:BIN_LSHIFT( 13, 8 ), 10 )
                                THEN ASSIGN option_bits = 0x00300000. /* PCRE_NEWLINE_CRLF */
                        WHEN -2 THEN ASSIGN option_bits = 0x00500000. /* PCRE_NEWLINE_ANYCRLF */
                        WHEN -1 THEN ASSIGN option_bits = 0x00400000. /* PCRE_NEWLINE_ANY */
                        OTHERWISE    ASSIGN option_bits = 0.
                    END CASE.

                END.

                ASSIGN
                crlf_is_newline = option_bits = 0x00400000 /* PCRE_NEWLINE_ANY */
                               OR option_bits = 0x00300000 /* PCRE_NEWLINE_CRLF */
                               OR option_bits = 0x00500000 /* PCRE_NEWLINE_ANYCRLF */
                .

                /* loop for next matches */
                loop_next_match:
                DO WHILE TRUE :

                    ASSIGN
                        options      = 0
                        start_offset = in_vector[ 2 ]

                        NO-ERROR
                    .

                    IF in_vector[ 1 ] = in_vector[ 2 ] THEN DO :
                        IF in_vector[ 1 ] = LENGTH( pr_ch_str ) THEN DO :
                            LEAVE loop_next_match.
                        END.

                        options = Binary:BIN_OR(
                            0x10000000, /* PCRE_NOTEMPTY_ATSTART */
                            0x00000010  /* PCRE_ANCHORED         */
                        ).
                    END.

                    RUN pcre_exec(
                        INPUT  pt_pcre,
                        INPUT  pt_pcre_extra,
                        INPUT  pr_ch_str,
                        INPUT  LENGTH( pr_ch_str ),
                        INPUT  start_offset,
                        INPUT  options,
                        OUTPUT in_vector,
                        INPUT  {&VECTOR_SIZE},
                        OUTPUT in_ret ).

                    IF in_ret = -1 /* PCRE_ERROR_NOMATCH */
                    THEN DO :
                        IF options = 0 THEN LEAVE loop_next_match.
                        in_vector[ 2 ] = start_offset + 1.
                        IF  crlf_is_newline
                        AND start_offset < LENGTH( pr_ch_str ) - 1
                        AND SUBSTRING( pr_ch_str, start_offset + 1, 1 ) = "~r"
                        AND SUBSTRING( pr_ch_str, start_offset + 2, 1 ) = "~n"
                        THEN DO :
                            ASSIGN in_vector[ 2 ] = in_vector[ 2 ] + 1.
                        END.
                        ELSE IF utf8 <> 0 THEN DO :
                            loop_internal:
                            DO WHILE in_vector[ 2 ] < LENGTH( pr_ch_str ) :
                                IF Binary:BIN_AND(
                                    ASC( SUBSTRING( pr_ch_str, in_vector[ 2 ], 1 ) ),
                                    0xC0
                                ) <> 0x80 THEN LEAVE loop_internal.
                                ASSIGN in_vector[ 2 ] = in_vector[ 2 ] + 1 NO-ERROR.
                            END. /* loop_internal */
                        END.
                        NEXT loop_next_match.
                    END.

                    IF in_ret < 0 THEN DO :
                        LEAVE loop_next_match.
                    END.

                    IF in_ret = 0 THEN DO :
                        in_ret = ( {&VECTOR_SIZE} / 3 ).
                    END.

                    ASSIGN
                        in_nb  = in_ret                /* number of matches + sub-matches */
                        in_ret = 1                     /* number of matches               */
                        pr_in_ret = pr_in_ret + in_ret /* total number of matches         */
                    .

                    /* fill the DATASET with the VECTOR */
                    DO in_cpt = 1 TO in_nb :

                        IF in_cpt = 1 THEN DO :
                            CREATE tt_matches.
                            ASSIGN
                                tt_matches.NUM    = in_cpt2 + in_cpt
                                tt_matches.NBSM   = 0
                                tt_matches.OSTART = in_vector[ in_cpt * 2 - 1 ] + 1
                                tt_matches.OEND   = in_vector[ in_cpt * 2     ] + 1
                                tt_matches.LEN    = tt_matches.OEND - tt_matches.OSTART
                                tt_matches.VAL    = SUBSTRING(
                                    pr_ch_str,         /* source string      */
                                    tt_matches.OSTART, /* offset start match */
                                    tt_matches.LEN     /* length of match    */
                                ).
                                in_cpt2 = in_cpt2 + 1
                            .
                        END.
                        ELSE DO :
                            CREATE tt_submatches.
                            ASSIGN
                                tt_matches.NBSM      = tt_matches.NBSM + 1
                                tt_submatches.NUM    = tt_matches.NUM
                                tt_submatches.SNUM   = in_cpt - 1
                                tt_submatches.OSTART = in_vector[ in_cpt * 2 - 1 ] + 1
                                tt_submatches.OEND   = in_vector[ in_cpt * 2     ] + 1
                                tt_submatches.LEN    = tt_submatches.OEND - tt_submatches.OSTART
                                tt_submatches.VAL    = SUBSTRING(
                                    pr_ch_str,            /* source string      */
                                    tt_submatches.OSTART, /* offset start match */
                                    tt_submatches.LEN     /* length of match    */
                                ).
                            .
                        END.

                    END. /* fill DATASET */

                END. /* loop_next_match */

            END. /* if lg_global */

        END. /* IF pr_in_ret >= 0 */

        IF GET-POINTER-VALUE( pt_pcre_extra) > 0 THEN DO :
            RUN pcre_free_study( pt_pcre_extra ).
            IF GET-POINTER-VALUE( pt_pcre_extra) > 0 THEN SET-SIZE(pt_pcre_extra) = 0.
        END.

        IF GET-POINTER-VALUE( pt_pcre) > 0 THEN DO :
            //RUN pcre_free( pt_pcre ).
            IF GET-POINTER-VALUE( pt_pcre) > 0 THEN SET-SIZE(pt_pcre) = 0.
        END.

    END. /* IF pt_pcre <> ? AND in_errcode = 0 */

END PROCEDURE.




