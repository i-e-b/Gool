using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Linq;
using Gool;
using Gool.Parsers.Composite;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

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
            zs      = Repeat('0', 0, 4), // up to 4 '0'
            esc     = ((int)c).ToString("X2"), // and two hex chars for the character
            pattern = u | l | (zs > esc); // any of the above

        return pattern;
    }

    /// <summary> Handle a string for case-insensitive AND escapable CSS keywords</summary>
    private static BNF CssStrEsc(string s) => new Union(s.Select(CssCharEsc));

    /// <summary>
    /// This is translated from https://github.com/antlr/grammars-v4/tree/master/css3
    /// which is a set of samples for Antlr.
    /// </summary>
    public static Package Css3_Antlr()
    {
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
            number_tok     = FractionalDecimal(allowLeadingZero: true, allowLoneDecimal: true),
            dimension_tok  = number_tok > ident_tok,
            percent_tok    = number_tok > '%',
            cdo_tok        = "<!--",
            cdc_tok        = "-->";

        // https://www.w3.org/TR/css-syntax-3/#preserved-tokens "Any token produced by the tokenizer except for <function-token>s, <{-token>s, <(-token>s, and <[-token>s."
        BNF
            non_func_token = whitespace_tok | ident_tok | at_keyword_tok | hash_tok | string_tok
                           | url_tok | number_tok | dimension_tok | percent_tok | cdo_tok | cdc_tok,
            preserved_tok = non_func_token / OneOf('{', '(', '[');


        var _decl_list = Forward();
        var _any_block = Forward();
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