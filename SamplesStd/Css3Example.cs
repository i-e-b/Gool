using System.Diagnostics.CodeAnalysis;
using Gool;
// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// CSS 3 Parser, based on https://github.com/antlr/grammars-v4/tree/master/css3
/// </summary>
public static class Css3Example
{
    [SuppressMessage("ReSharper", "SuggestVarOrType_SimpleTypes")]
    public static BNF.Package Css3()
    {
        // Lexer side https://github.com/antlr/grammars-v4/blob/master/css3/css3Lexer.g4
        //   see also https://github.com/antlr/antlr4/blob/4.13.2/doc/lexer-rules.md
        BNF
            openBracket  = '[',
            closeBracket = ']',
            openParen    = '(',
            closeParen   = ')',
            openBrace    = '{',
            closeBrace   = '}',
            semiColon    = ':',
            equal        = '=',
            colon        = ':',
            dot          = '.',
            multiply     = '*',
            divide       = '/',
            pipe         = '|',
            underscore   = '_',
            at           = '@';

        BNF ws             = -BNF.WhiteSpace;
        BNF nonAscii       = BNF.Regex(@"[^\u0000-\u007f]");
        BNF hexChar        = BNF.Regex("[0-9a-fA-F]");
        BNF newlineOrSpace = "\r\n" | BNF.OneOf('\n', '\r', ' ', '\t', '\f');
        BNF unicode        = BNF.Regex(@"\\[0-9a-fA-F]{1,6}") > newlineOrSpace;
        BNF escapeChar     = BNF.Regex(@"[^\r\n\f0-9a-fA-F]");
        BNF escape         = unicode | ('\\' > escapeChar);
        BNF nameStart      = BNF.Regex(@"[_a-zA-Z]") | nonAscii | escape;
        BNF nameChar       = BNF.Regex(@"[_a-zA-Z0-9\-]") | nonAscii | escape;

        BNF
            comment = "/*" > +( BNF.AnyChar / "*/") > "*/"; // Comment: '/*' ~'*'* '*'+ ( ~[/*] ~'*'* '*'+)* '/';

        BNF

            urlFunc = "url(".CaseInsensitive(),
            urlPrefix = BNF.On
            url = urlFunc > ws >



        // Parser side (https://github.com/antlr/grammars-v4/blob/master/css3/css3Parser.g4)



        return BNF.Empty.WithOptions(BNF.Options.SkipWhitespace); // TODO: replace placeholder
    }
}