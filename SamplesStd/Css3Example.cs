using System;
using System.Globalization;
using System.Linq;
using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming
// ReSharper disable StringLiteralTypo

namespace Samples;

/// <summary>
/// CSS 3 Parser, based on https://github.com/antlr/grammars-v4/tree/master/css3
/// </summary>
public static class Css3Example
{
    /// <summary> Handle characters for case-insensitive AND escapable CSS keywords </summary>
    private static BNF CssCharEsc(char c)
    {
        BNF
            u       = char.ToUpper(c), // upper case char
            l       = char.ToLower(c), // lower case char
            zs      = Repeat('0', 0, 4), // up to 4 '0's
            esc     = ((int)c).ToString("X2").CaseInsensitive(),
            no_esc  = CharacterNotInRanges(('0', '9'), ('a', 'f'), ('A', 'F'), ' ', '\t', '\r', '\n', '\f') | "\r\n",
            pattern = u | l | ('\\' > zs > (esc | no_esc));

        return pattern;
    }

    /// <summary> Handle a string for case-insensitive AND escapable CSS keywords</summary>
    private static BNF CssStrEsc(string s) => Composite(s.Select(CssCharEsc));

    /// <summary>
    /// This is translated from https://github.com/antlr/grammars-v4/tree/master/css3
    /// which is a set of samples for Antlr.
    /// </summary>
    public static Package Css3_Antlr()
    {
        #region Lexer side

        BNF // Fragments
            OpenBracket  = '[',
            CloseBracket = ']',
            OpenParen    = '(',
            CloseParen   = ')',
            OpenBrace    = '{',
            CloseBrace   = '}',
            SemiColon    = ';',
            Equal        = '=',
            Colon        = ':',
            Dot          = '.',
            Multiply     = '*',
            Divide       = '/',
            Pipe         = '|',
            Underscore   = '_',
            DblQuote     = '"',
            SingleQuote  = '\'',
            Cdo          = "<!--",
            Cdc          = "-->",
            Plus         = '+',
            Minus        = '-',
            Greater      = '>',
            Comma        = ',',
            Tilde        = '~',
            EscChar      = '\\',
            Bang         = '!',
            At           = '@';

        BNF // Composite fragments
            DashChar       = CssCharEsc('-'),
            Number         = FractionalDecimal(allowLoneDecimal: true, allowLeadingZero: true),
            NonAscii       = CharacterInRanges((FirstNonAscii, MaxUtf)),
            Hex            = CharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')),
            Space          = +OneOf(' ', '\t', '\r', '\n', '\f'),
            Whitespace     = !Space,
            Newline        = OneOf('\r', '\n') | "\r\n",
            NewlineOrSpace = !(Newline | OneOf(' ', '\t', '\f')),
            Unicode        = '\\' > Hex.Repeat(1, 6) > NewlineOrSpace,
            Escape         = Unicode | ('\\' > CharacterNotInRanges('\r', '\n', '\f', ('0', '9'), ('a', 'f'), ('A', 'F'))),
            Nmstart        = CharacterInRanges('_', ('a', 'z'), ('A', 'Z')) | NonAscii | Escape,
            Nmchar         = CharacterInRanges('_', '-', ('a', 'z'), ('A', 'Z'), ('0', '9')) | NonAscii | Escape,
            Comment        = "/*" > -(AnyChar / "*/") > "*/",
            Name           = +Nmchar,
            Variable       = "--" > Nmstart > -Nmchar,
            Ident          = !DashChar > Nmstart > -Nmchar,
            EscBreak       = (Newline | Hex),
            EscSeq         = EscChar > ((AnyChar / EscBreak) | (Hex.Repeat(1, 6) > Whitespace)),
            DblString      = '"' > -((AnyChar / ('"' | EscChar)) | EscSeq | (EscChar > Newline)) > '"',
            SglString      = "'" > -((AnyChar / ("'" | EscChar)) | EscSeq | (EscChar > Newline)) > "'",
            String_        = DblString | SglString,
            UrlSymbol      = OneOf('!', '#', '$', '%', '&', '*', '-', '~'),
            UrlStringDbl   = Whitespace > !DblQuote > -(Escape | NonAscii | UrlSymbol) > !DblQuote > Whitespace,
            UrlStringSgl   = Whitespace > !SingleQuote > -(Escape | NonAscii | UrlSymbol) > !SingleQuote > Whitespace,
            Url            = CssStrEsc("url") > '(' > (UrlStringDbl | UrlStringSgl) > ')',
            Url_           = "url(",
            Calc           = "calc(",
            Function       = Ident > '(',
            Hash           = '#' > Name;

        BNF // Directives
            VendorPrefix      = '-' > (CssStrEsc("moz") | CssStrEsc("webkit") | CssStrEsc("o")) > '-',
            Import            = '@' > CssStrEsc("import"),
            Page              = '@' > CssStrEsc("page"),
            Media             = '@' > CssStrEsc("media"),
            Namespace         = '@' > CssStrEsc("namespace"),
            Viewport          = '@' > CssStrEsc("viewport"),
            Charset           = "@charset ",
            Important         = '!' > -(Space | Comment) > CssStrEsc("important"),
            FontFace          = '@' > CssStrEsc("font-face"),
            CounterStyle      = '@' > CssStrEsc("counter-style"),
            FontFeatureValues = '@' > CssStrEsc("font-feature-values"),
            Supports          = '@' > CssStrEsc("supports"),
            PseudoNot         = ':' > CssStrEsc("not") > '(',
            DxImageTransform  = "progid:DXImageTransform.Microsoft." > Function,
            MediaOnly         = CssStrEsc("only"),
            Not               = CssStrEsc("not"),
            And               = CssStrEsc("and"),
            Or                = CssStrEsc("or"),
            From              = CssStrEsc("from"),
            To                = CssStrEsc("to"),
            Var               = "var(",
            AtKeyword         = '@' > Ident;

        BNF // Match types
            Includes       = "~=",
            DashMatch      = "|=",
            PrefixMatch    = "^=",
            SuffixMatch    = "$=",
            SubstringMatch = "*=";

        BNF // Units
            FontRelative     = Number > (CssStrEsc("em") | CssStrEsc("ex") | CssStrEsc("ch") | CssStrEsc("rem")),
            ViewportRelative = Number > (CssStrEsc("vw") | CssStrEsc("vh") | CssStrEsc("vmin") | CssStrEsc("vmax")),
            AbsLength        = Number > (CssStrEsc("px") | CssStrEsc("cm") | CssStrEsc("mm") | CssStrEsc("in") | CssStrEsc("pt") | CssStrEsc("pc") | CssStrEsc("q")),
            Angle            = Number > (CssStrEsc("deg") | CssStrEsc("rad") | CssStrEsc("grad") | CssStrEsc("turn")),
            Time             = Number > (CssStrEsc("ms") | CssStrEsc("s")),
            Freq             = Number > (CssStrEsc("hz") | CssStrEsc("khz")),
            Resolution       = Number > (CssStrEsc("dpi") | CssStrEsc("dpcm") | CssStrEsc("dppx")),
            Length           = AbsLength | FontRelative | ViewportRelative,
            Dimension        = Length | Time | Freq | Resolution | Angle,
            Percentage       = Number > '%',
            UnknownDimension = (Number > Ident) / (Dimension | Percentage);

        BNF
            UnicodeRange = (BNF)('u' | 'U') > '+' > (
                (((BNF)"?").Repeat(1, 6)) |
                (Hex.Repeat(1) > "?".Repeat(0, 5)) |
                (Hex.Repeat(2) > "?".Repeat(0, 4)) |
                (Hex.Repeat(3) > "?".Repeat(0, 3)) |
                (Hex.Repeat(4) > "?".Repeat(0, 2)) |
                (Hex.Repeat(1) > "?".Repeat(0, 1))
            ),
            ws = -(Comment | Space);

        #endregion Lexer side

        #region Parser side

        BNF // Number forms
            number           = !(Plus | Minus) > Number,
            percentage       = !(Plus | Minus) > Percentage,
            unknownDimension = !(Plus | Minus) > UnknownDimension,
            dimension        = !(Plus | Minus) > Dimension,
            hexcolor         = Hash > ws
            ;

        BNF // Strings and Markers
            prio = Important > ws,
            url  = (Url_ > ws > String_ > ws > ')') | Url
            ;

        var _expr    = Forward();
        var _calcSum = Forward();
        BNF // Calc and Func Expressions
            ident            = Ident | MediaOnly | Not | And | Or | From | To,
            operator_        = ('/' > ws) | (Comma > ws) | (Space > ws) | ('=' > ws),
            var_             = Var > ws > Variable > ws > ')' > ws,
            calcValue        = (number > ws) | (dimension > ws) | (unknownDimension > ws) | (percentage > ws) | ('(' > ws > _calcSum > ')' > ws),
            calcProduct      = calcValue > -(('*' > ws > calcValue) | ('/' > ws > number > ws)),
            calcSum          = calcProduct > -(Space > ws > (Plus | Minus) > ws > Space > ws > calcProduct),
            calc             = Calc > ws > calcSum > ')' > ws,
            function_        = Function > ws > _expr > ')' > ws,
            dxImageTransform = DxImageTransform > ws > _expr > ')' > ws,
            term = (number > ws)
                 | (percentage > ws)
                 | (dimension > ws)
                 | (String_ > ws)
                 | (UnicodeRange > ws)
                 | (ident > ws)
                 | var_
                 | (url > ws)
                 | hexcolor
                 | calc
                 | function_
                 | (unknownDimension > ws)
                 | dxImageTransform,
            expr = term > -(!operator_ > term);

        _calcSum.Is(calcSum);
        _expr.Is(expr);

        BNF // Properties
            featureValueDefinition = ident > ws > ':' > ws > number > -(ws > number),
            featureType            = AtKeyword,
            featureValueBlock      = featureType > ws > '{' > ws > !featureValueDefinition > -(ws > ';' > ws > featureValueDefinition) > '}' > ws,
            fontFamilyName         = String_ | (ident % ws), //(ident > -(ws > ident))
            fontFamilyNameList     = fontFamilyName > -(ws > Comma > ws > fontFamilyName),
            fontFeatureValuesRule  = FontFeatureValues > ws > fontFamilyNameList > ws > '{' > ws > -featureValueBlock > '}' > ws,
            property_              = (ident > ws) | (Variable > ws) | ('*' > ident) | ('_' > ident),
            declaration            = property_ > ':' > ws > expr > !prio,
            declarationList        = -(';' > ws) > declaration > ws > -(';' > ws > !declaration),
            counterStyle           = CounterStyle > ws > ident > ws > '{' > ws > !declarationList > '}' > ws;

        BNF // Pages
            pseudoPage = ':' > ident > ws,
            page       = Page > ws > !pseudoPage > '{' > ws > !declaration > -(';' > ws > !declaration) > '}' > ws;

        BNF // Media Expressions
            mediaType       = ident,
            mediaFeature    = ident > ws,
            mediaExpression = '(' > ws > mediaFeature > !(':' > ws > expr) > ')' > ws,
            mediaQuery = (!(MediaOnly | Not) > ws > mediaType > ws > -(And > ws > mediaExpression))
                       | (mediaExpression > -(And > ws > mediaExpression)),
            mediaQueryList = !(mediaQuery > -(Comma > ws > mediaQuery)) > ws;

        BNF // Major parts
            charset = (Charset > ws > String_ > ws > ';' > ws) // Good charset
                    | (Charset > ws > String_ > ws), // 'Bad' charset
            imports = (Import > ws > (String_ | url) > ws > mediaQueryList > ';' > ws)
                    | (Import > ws > (String_ | url) > ws > ';' > ws)
                    | (Import > ws > (String_ | url) > ws > mediaQueryList) // badImport
                    | (Import > ws > (String_ | url) > ws), // badImport
            namespacePrefix = ident,
            namespace_ = (Namespace > ws > !(namespacePrefix > ws) > (String_ | url) > ws > ';' > ws)
                       | (Namespace > ws > !(namespacePrefix > ws) > (String_ | url) > ws);


        BNF stylesheet = ws >
                         -(charset > -(Comment | Space | Cdo | Cdc)) >
                         -(imports > (Comment | Space | Cdo | Cdc)) >
                         -(namespace_ > -(Comment | Space | Cdo | Cdc)) >
                         -(nestedStatement > -(Comment | Space | Cdo | Cdc));

        #endregion Parser side

        throw new Exception("not ready");
    }

    /// <summary>
    /// This is from the w3.org 'railroad' diagrams, which do not seem to be complete or correct
    /// </summary>
    public static Package Css3_W3C()
    {
        // Normative = https://www.w3.org/TR/css-syntax-3/

        BNF // Fragments
            dash           = CssCharEsc('-'),
            any_quote      = OneOf('"', '\''),
            esc_char       = '\\',
            bang           = '!',
            name_start     = CharacterInRanges(('a', 'z'), ('A', 'Z'), '_', (FirstNonAscii, MaxUtf)),
            name_char      = CharacterInRanges(('a', 'z'), ('A', 'Z'), ('0', '9'), '-', '_', (FirstNonAscii, MaxUtf)),
            newline        = (BNF)"\r\n" | '\r' | '\n' | '\f',
            whitespace     = OneOf(' ', '\t') | newline,
            ws             = -whitespace,
            non_printable  = UtfCategory(UnicodeCategory.Control),
            url            = CssStrEsc("url"),
            hex_digit      = CharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')),
            escape_brk     = (newline | hex_digit),
            escape_seq     = esc_char > ((AnyChar / escape_brk) | (hex_digit.Repeat(1, 6) > !whitespace)),
            name_start_esc = name_start | escape_seq,
            name_char_esc  = name_char | escape_seq;

        BNF // Comments and String types
            comment    = "/*" > -(AnyChar / "*/") > "*/",
            dbl_string = '"' > -((AnyChar / ('"' | esc_char)) | escape_seq | (esc_char > newline)) > '"',
            sgl_string = "'" > -((AnyChar / ("'" | esc_char)) | escape_seq | (esc_char > newline)) > "'",
            url_brk    = esc_char | "'" | '"' | '(' | ')' | ws | non_printable,
            url_string = !any_quote > -((AnyChar / url_brk) | escape_seq) > !any_quote;

        BNF // Tokens
            whitespace_tok = +whitespace,
            ident_tok      = ("--" | (!dash > name_start_esc)) > -name_char_esc,
            function_tok   = ident_tok > '(',
            at_keyword_tok = '@' > ident_tok,
            hash_tok       = '#' > -name_char_esc,
            string_tok     = dbl_string | sgl_string,
            url_tok        = url > '(' > ws > (-url_string) > ws > ')',
            number_tok     = FractionalDecimal(allowLoneDecimal: true, allowLeadingZero: true),
            dimension_tok  = number_tok > ident_tok,
            percent_tok    = number_tok > '%',
            cdo_tok        = "<!--",
            cdc_tok        = "-->";

        // https://www.w3.org/TR/css-syntax-3/#preserved-tokens "Any token produced by the tokenizer except for <function-token>s, <{-token>s, <(-token>s, and <[-token>s."
        BNF
            non_func_token = whitespace_tok | ident_tok | at_keyword_tok | hash_tok | string_tok
                           | url_tok | number_tok | dimension_tok | percent_tok | cdo_tok | cdc_tok,
            preserved_tok = non_func_token / OneOf('{', '(', '[');


        var _decl_list   = Forward();
        var _any_block   = Forward();
        var _brace_block = Forward();

        BNF // Stylesheet parts
            component_value = ws > (preserved_tok | _any_block) > ws,
            important       = bang > ws > CssStrEsc("important") > ws,
            declaration     = ident_tok > ws > ':' > (-component_value) > !important,
            at_rule         = at_keyword_tok > -(component_value) > (_brace_block | ';'),
            decl_list       = ws > ((!declaration > !(';' > _decl_list)) | (at_rule > _decl_list)),
            qualified_rule  = -component_value > _brace_block,
            rule_list       = -(whitespace_tok | qualified_rule | at_rule),
            stylesheet      = -(cdo_tok | cdc_tok | comment | rule_list);


        BNF // Block definitions
            brace_block    = '{' > -component_value > '}',
            paren_block    = '(' > -component_value > ')',
            bracket_block  = '[' > -component_value > ']',
            function_block = function_tok > -component_value > ')',
            any_block      = brace_block | paren_block | bracket_block | function_block;

        _brace_block.Is(brace_block);
        _any_block.Is(any_block);
        _decl_list.Is(decl_list);

        comment.TagWith("comment");
        qualified_rule.TagWith("qualified rule");
        at_rule.TagWith("at rule");
        whitespace_tok.TagWith("whitespace");

        // Antlr:
        // Lexer side https://github.com/antlr/grammars-v4/blob/master/css3/css3Lexer.g4
        //   see also https://github.com/antlr/antlr4/blob/4.13.2/doc/lexer-rules.md
        // Parser side (https://github.com/antlr/grammars-v4/blob/master/css3/css3Parser.g4)


        return stylesheet.WithOptions(Options.None);
    }
}