
 /*------------------------------------------------------------------------
    File        : Regex.cls
    Purpose     :
    Syntax      :
    Description : Adds regular expression support in Progress ABL 4GL
    Author(s)   : Gabriel Hautclocq
    Created     : Mon May 18 16:42:29 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Regex:


    { inc/regex.ds }

    DEFINE VARIABLE hd_regex AS HANDLE NO-UNDO.



    /* Constructor */
    CONSTRUCTOR PUBLIC Regex() :

        IF NOT VALID-HANDLE( hd_regex ) THEN DO :
            RUN VALUE( "regex.p" ) PERSISTENT SET hd_regex NO-ERROR.
        END.

    END CONSTRUCTOR.

    /* Constructor 2 */
    CONSTRUCTOR PUBLIC Regex(
        INPUT pr_ch_regex   AS CHARACTER,
        INPUT pr_ch_options AS CHARACTER ) :

        THIS-OBJECT().

        ASSIGN
            THIS-OBJECT:pch_Regex   = pr_ch_regex
            THIS-OBJECT:pch_Options = pr_ch_options
        .

    END CONSTRUCTOR.

    /* Constructor 3 */
    CONSTRUCTOR PUBLIC Regex(
        INPUT pr_ch_str     AS CHARACTER,
        INPUT pr_ch_regex   AS CHARACTER,
        INPUT pr_ch_options AS CHARACTER ) :

        THIS-OBJECT( INPUT pr_ch_regex, INPUT pr_ch_options ).

        ASSIGN THIS-OBJECT:pch_Str = pr_ch_str.

    END CONSTRUCTOR.



    /* Destructor */
    DESTRUCTOR PUBLIC Regex() :

        IF VALID-HANDLE( hd_regex ) THEN DO :
            DELETE PROCEDURE hd_regex NO-ERROR.
        END.

    END DESTRUCTOR.



    /*
        contains the string to test
        (only useful with constructor 3)
    */
    DEFINE PUBLIC PROPERTY pch_Str     AS CHARACTER NO-UNDO
        PUBLIC    GET.
        PROTECTED SET.

    /*
        contains the pattern of the regex
        (only useful with constructors 2 and 3)
    */
    DEFINE PUBLIC PROPERTY pch_Regex   AS CHARACTER NO-UNDO
        PUBLIC    GET.
        PROTECTED SET.

    /*
        contains the options
        (only useful with constructors 2 and 3)
    */
    DEFINE PUBLIC PROPERTY pch_Options AS CHARACTER NO-UNDO
        PUBLIC    GET.
        PROTECTED SET.




    /*
        == Method mMatch ==

        Description:
            Test if a string matches agains a regular expression.

        Parameters :
            pr_ch_str     : string to test
            pr_ch_regex   : regex pattern
            pr_ch_options : regex options (see doc of mOptions)

        Returns :
            TRUE if pr_ch_str got at least one matche against pr_ch_regex.

        Examples :
            mMatch( "234",   "^[0-9]~{3}$", "" ) returns TRUE  (exactly 3 digits               => TRUE )
            mMatch( "23456", "^[0-9]~{3}$", "" ) returns FALSE (exactly 3 digits               => FALSE)
            mMatch( "A",     "a", ""  )          returns FALSE (exactly character A lower case => FALSE)
            mMatch( "A",     "a", "i" )          returns TRUE  (exactly character A            => TRUE )
    */
    METHOD PUBLIC LOGICAL mMatch(
        INPUT pr_ch_str     AS CHARACTER,
        INPUT pr_ch_regex   AS CHARACTER,
        INPUT pr_ch_options AS CHARACTER ) :

        DEFINE VARIABLE in_options AS INT64 NO-UNDO.

        in_options = THIS-OBJECT:mOptions( pr_ch_options ).

        DEFINE VARIABLE in_ret AS INTEGER NO-UNDO.
        RUN match IN hd_regex( INPUT pr_ch_str, INPUT pr_ch_regex, INPUT in_options, OUTPUT in_ret ).

        RETURN in_ret > -1.

    END METHOD.


    /* same but using 2nd constructor */
    METHOD PUBLIC LOGICAL mMatch(
        INPUT pr_ch_str     AS CHARACTER ) :

        RETURN THIS-OBJECT:mMatch( INPUT pr_ch_str, INPUT THIS-OBJECT:pch_Regex, INPUT THIS-OBJECT:pch_Options ).

    END METHOD.


    /* same but using 3rd constructor */
    METHOD PUBLIC LOGICAL mMatch() :

        RETURN THIS-OBJECT:mMatch( INPUT THIS-OBJECT:pch_Str ).

    END METHOD.






    /*
        == Method mNumMatches ==

        Description:
            Finds the number of matches of a string against a regular expression.

        Parameters :
            pr_ch_str     : string to test
            pr_ch_regex   : regex pattern
            pr_ch_options : regex options (see doc of mOptions)

        Returns :
            The number of matches of pr_ch_str against pr_ch_regex.

            Returned values :
				 1 + n  if there are n sub-matches, with n > 0
				 1 if there is one match but no sub-matches
				 0 if too many matches or submatches
				-1 if no match
				-2 if parameter error (passing NULL for example)
				-3 if bad option passed
				-4 or less if other error other than regex syntax error
				-999n if regex syntax error, with n equal to:
                -999n si erreur de syntaxe du regex, avec n égal à :
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
                    53  internal error: previously-checked referenced subpattern not found
                    54  DEFINE group contains more than one branch
                    55  repeating a DEFINE group is not allowed
                    56  inconsistent NEWLINE options
                    57  \g is not followed by a braced, angle-bracketed, or quoted name/number or by a plain number
                    58  a numbered reference must not be zero
                    59  an argument is not allowed for (*ACCEPT), (*FAIL), or (*COMMIT)
                    60  (*VERB) not recognized or malformed
                    61  number is too big
                    62  subpattern name expected
                    63  digit expected after (?+
                    64  ] is an invalid data character in JavaScript compatibility mode
                    65  different names for subpatterns of the same number are not allowed
                    66  (*MARK) must have an argument
                    67  this version of PCRE is not compiled with Unicode property support
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

        Examples :
            mNumMatches( "234",   "^[0-9]~{3}$", ""  ) returns  1 (only one match)
            mNumMatches( "23456", "^[0-9]~{3}$", ""  ) returns -1 (no match)
            mNumMatches( "abcabc", "(bc)"      , "g" ) returns  3 (match + 2 sub-matches)
    */
    METHOD PUBLIC INTEGER mNumMatches(
        INPUT pr_ch_str     AS CHARACTER,
        INPUT pr_ch_regex   AS CHARACTER,
        INPUT pr_ch_options AS CHARACTER ) :

        DEFINE VARIABLE in_options AS INT64   NO-UNDO.
        DEFINE VARIABLE in_ret     AS INTEGER NO-UNDO.

        /* generate options */
        in_options = THIS-OBJECT:mOptions( pr_ch_options ).

        RUN match IN hd_regex( INPUT pr_ch_str, INPUT pr_ch_regex, INPUT in_options, OUTPUT in_ret ).

        RETURN in_ret.

    END METHOD.


    /* same but using the 2nd constructor */
    METHOD PUBLIC INTEGER mNumMatches(
        INPUT pr_ch_str     AS CHARACTER ) :

        RETURN THIS-OBJECT:mNumMatches( INPUT pr_ch_str, INPUT THIS-OBJECT:pch_Regex, INPUT THIS-OBJECT:pch_Options ).

    END METHOD.


    /* same but using the 3rd constructor */
    METHOD PUBLIC INTEGER mNumMatches() :

        RETURN THIS-OBJECT:mNumMatches( INPUT pch_Str ).

    END METHOD.


    /*
        == Method mGetMatches ==

        Description:
            Finds the number of matches of a string against a regular expression
            and get the matches and sub-matches in a DATASET.

        Parameters :
            pr_ch_str     : string to test
            pr_ch_regex   : regex pattern
            pr_ch_options : regex options (see doc of mOptions)
            ds_regex      : DATASET containing the matches and sub-matches

        Returns :
            The number of matches of pr_ch_str against pr_ch_regex.
            In OUTPUT parameter, a DATASET containing matches and sub-matches.


            Returned values :
         1 + n  if there are n sub-matches, with n > 0
         1 if there is one match but no sub-matches
         0 if too many matches or submatches
        -1 if no match
        -2 if parameter error (passing NULL for example)
        -3 if bad option passed
        -4 or less if other error other than regex syntax error
        -999n if regex syntax error, with n equal to:
                -999n si erreur de syntaxe du regex, avec n égal à :
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
                    53  internal error: previously-checked referenced subpattern not found
                    54  DEFINE group contains more than one branch
                    55  repeating a DEFINE group is not allowed
                    56  inconsistent NEWLINE options
                    57  \g is not followed by a braced, angle-bracketed, or quoted name/number or by a plain number
                    58  a numbered reference must not be zero
                    59  an argument is not allowed for (*ACCEPT), (*FAIL), or (*COMMIT)
                    60  (*VERB) not recognized or malformed
                    61  number is too big
                    62  subpattern name expected
                    63  digit expected after (?+
                    64  ] is an invalid data character in JavaScript compatibility mode
                    65  different names for subpatterns of the same number are not allowed
                    66  (*MARK) must have an argument
                    67  this version of PCRE is not compiled with Unicode property support
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

        Examples :
            mGetMatches( "234",   "^[0-9]~{3}$", "", OUTPUT DATASET ds_regex ) returns  1 (exactly 3 digits => TRUE)
            mGetMatches( "23456", "^[0-9]~{3}$", "", OUTPUT DATASET ds_regex ) returns -1 (exactly 3 digits => FALSE)
    */
    METHOD PUBLIC INTEGER mGetMatches(
        INPUT  pr_ch_str     AS  CHARACTER,
        INPUT  pr_ch_regex   AS  CHARACTER,
        INPUT  pr_ch_options AS  CHARACTER,
        OUTPUT DATASET       FOR ds_regex ) :

        DEFINE VARIABLE in_options AS INT64 NO-UNDO.
        DEFINE VARIABLE in_ret AS INTEGER NO-UNDO.

        /* generate options */
        in_options = THIS-OBJECT:mOptions( pr_ch_options ).

        /* get the matches */
        RUN get_matches IN hd_regex( INPUT pr_ch_str, INPUT pr_ch_regex, INPUT in_options, OUTPUT in_ret, OUTPUT DATASET ds_regex ).

        RETURN in_ret.

    END METHOD.


    /* same but using the 2nd constructor */
    METHOD PUBLIC INTEGER mGetMatches(
        INPUT  pr_ch_str     AS  CHARACTER,
        OUTPUT DATASET       FOR ds_regex ) :

        RETURN THIS-OBJECT:mGetMatches( INPUT pr_ch_str, INPUT pch_Regex, INPUT pch_Options, OUTPUT DATASET ds_regex ).

    END METHOD.


    /* same but using the 3rd constructor */
    METHOD PUBLIC INTEGER mGetMatches(
        OUTPUT DATASET FOR ds_regex ) :

        RETURN THIS-OBJECT:mGetMatches( INPUT pch_Str, OUTPUT DATASET ds_regex ).

    END METHOD.






    /*
        == Method mReplace ==

        Description:
            In a string, replace the matches of a regex by another string, with sub-matches support

        Parameters :
            pr_ch_str     : string to test
            pr_ch_regex   : regex pattern
            pr_ch_replace : the replacement string
                            Sub-matches can be referenced in the replacement string
                                with $~{n} or more simply with $n (if n < 10)
                                    where n is the number of the sub-match
            pr_ch_options : regex options (see doc of mOptions)

        Returns :
            The string after replacement has been done

        Example : mReplace(
                    "<h1>Test</h1><p>Test2</p>",
                    "<([a-z0-9]+)>(.+)<~\/([a-z0-9]+)>",
                    "<span>$1,$2,$3<span>",
                    "gi" ).
            Returns: "<span>h1,Test,h1</span><span>p,Test2,p</span>"

        Example 2 : suppress useless spaces in a string
            mReplace(
                    "a    string  with    too   many      spaces",
                    "~\s~\s+",
                    " ",
                    "g" ).
            Returns: "a string with too many spaces"

        */
    METHOD PUBLIC CHARACTER mReplace(
        INPUT  pr_ch_str     AS CHARACTER,
        INPUT  pr_ch_regex   AS CHARACTER,
        INPUT  pr_ch_replace AS CHARACTER,
        INPUT  pr_ch_options AS CHARACTER ) :

        DEFINE VARIABLE ch_res     AS CHARACTER NO-UNDO INITIAL "".
        DEFINE VARIABLE in_options AS INT64     NO-UNDO.
        DEFINE VARIABLE in_ret     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE ch_tmp     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ch_tmp2    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE in_pos     AS INTEGER   NO-UNDO INITIAL 1.

        /* generate options */
        in_options = THIS-OBJECT:mOptions( pr_ch_options ).

        /* get the matches */
        RUN get_matches IN hd_regex( INPUT pr_ch_str, INPUT pr_ch_regex, INPUT in_options, OUTPUT in_ret, OUTPUT DATASET ds_regex ).


        /* for each matches */
        FOR EACH tt_matches :

            ASSIGN

                /* adds everything before the match */
                ch_res = ch_res + SUBSTRING( pr_ch_str, in_pos, tt_matches.OSTART - in_pos )

                /* copy the replacement string */
                ch_tmp = pr_ch_replace
            .

            /* replacing of sub-matches */
            FOR EACH tt_submatches OF tt_matches :

                /* replaces strings of the form ${1} ${2} ... */
                ASSIGN
                    ch_tmp2 = ch_tmp.
                    ch_tmp  = REPLACE( ch_tmp, "$~{" + STRING( tt_submatches.SNUM ) + "}", tt_submatches.VAL )
                .

                /* if no previous replacement, replace strings of the form $1 $2 etc... */
                IF ch_tmp2 = ch_tmp THEN DO :
                    ch_tmp  = REPLACE( ch_tmp, "$"   + STRING( tt_submatches.SNUM )      , tt_submatches.VAL ).
                END.

            END.

            ASSIGN
                ch_res = ch_res + ch_tmp /* adds the replaced match in the output string */
                in_pos = tt_matches.OEND  /* we move the cursor at the end of the match   */
            .

        END.

        /* end of string */
        IF in_pos < LENGTH( pr_ch_str ) THEN DO :
            ASSIGN ch_res = ch_res + SUBSTRING( pr_ch_str, in_pos ).
        END.

        RETURN ch_res.

    END METHOD.


    /* same but using the 2nd constructor */
    METHOD PUBLIC CHARACTER mReplace(
        INPUT pr_ch_str     AS CHARACTER,
        INPUT pr_ch_replace AS CHARACTER ) :

        RETURN THIS-OBJECT:mReplace( INPUT pr_ch_str, INPUT pch_Regex, INPUT pr_ch_replace, INPUT pch_Options ).

    END METHOD.


    /* same but using the 3rd constructor */
    METHOD PUBLIC CHARACTER mReplace( INPUT pr_ch_replace AS CHARACTER ) :

        RETURN THIS-OBJECT:mReplace( INPUT pch_Str, INPUT pch_Regex, INPUT pr_ch_replace, INPUT pch_Options ).

    END METHOD.











    /*
        == Method mOptions ==

        Description :
            Builds the options following the content of pr_ch_options. Several options can be concatenated.

            List of recognized options:

            g (global matches)

            i (PCRE_CASELESS)

            m (PCRE_MULTILINE)

            s (PCRE_DOTALL)

            x (PCRE_EXTENDED)

            A (PCRE_ANCHORED)

            D (PCRE_DOLLAR_ENDONLY)

            X (PCRE_EXTRA)

            U (PCRE_UNGREEDY)

            u (PCRE_UTF8)

        Parameters :
            pr_ch_options : a string containing options to pass. Empty string ("") means no options.

        Returns :
            An integer representing the specified options

    */
    METHOD PRIVATE INT64 mOptions( INPUT pr_ch_options AS CHARACTER ) :

        DEFINE VARIABLE in_options AS INT64     NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_cpt     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE ch_opt     AS CHARACTER NO-UNDO CASE-SENSITIVE.

        DO in_cpt = 1 TO LENGTH( pr_ch_options ) :

            ch_opt = SUBSTRING( pr_ch_options, in_cpt, 1 ).

            CASE TRUE :

                WHEN COMPARE( "i", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x001 ).
                WHEN COMPARE( "m", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x002 ).
                WHEN COMPARE( "s", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x004 ).
                WHEN COMPARE( "x", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x008 ).
                WHEN COMPARE( "A", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x010 ).
                WHEN COMPARE( "D", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x020 ).
                WHEN COMPARE( "X", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x040 ).
                WHEN COMPARE( "U", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x200 ).
                WHEN COMPARE( "u", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x800 ).

                /* special option for g */
                WHEN COMPARE( "g", "=", ch_opt, "CASE-SENSITIVE" ) THEN in_options = Binary:BIN_OR64( in_options, 0x40000000 ).

            END CASE.

        END.

        RETURN in_options.

    END METHOD.

END CLASS.