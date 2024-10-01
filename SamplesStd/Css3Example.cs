using System.Linq;
using Gool;
using static Gool.BNF;

// ReSharper disable SuggestVarOrType_SimpleTypes

// ReSharper disable InconsistentNaming
// ReSharper disable StringLiteralTypo

namespace Samples;

/// <summary>
/// CSS 3 Parser, based on https://github.com/antlr/grammars-v4/tree/master/css3
/// </summary>
public static class Css3Example
{
    /// <summary>
    /// This is translated from https://github.com/antlr/grammars-v4/tree/master/css3
    /// which is a set of samples for Antlr.
    /// </summary>
    public static Package Css3_Antlr()
    {
        #region Lexer side

        BNF // Fragments
            DblQuote    = '"',
            SingleQuote = '\'',
            Cdo         = "<!--",
            Cdc         = "-->",
            Plus        = '+',
            Minus       = '-',
            Greater     = '>',
            Comma       = ',',
            Tilde       = '~',
            EscChar     = '\\',
            At          = '@';

        BNF // Composite fragments
            DashChar       = CssCharEsc('-'),
            Number         = FractionalDecimal(allowLoneDecimal: true, allowLeadingZero: true),
            NonAscii       = CharacterInRanges((FirstNonAscii, MaxUtf)),
            Hex            = CharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')),
            Space          = OneOf(' ', '\t', '\r', '\n', '\f'),
            ReqSpace       = PreviousEndsWith(Space) | Space,
            Whitespace     = !Space,
            Newline        = OneOf('\r', '\n') | "\r\n",
            NewlineOrSpace = !(Newline | OneOf(' ', '\t', '\f')),
            Unicode        = '\\' > Hex.Repeat(1, 6) > NewlineOrSpace,
            Escape         = Unicode | ('\\' > CharacterNotInRanges('\r', '\n', '\f', ('0', '9'), ('a', 'f'), ('A', 'F'))),
            NameStart      = CharacterInRanges('_', ('a', 'z'), ('A', 'Z')) | NonAscii | Escape,
            NameChar       = CharacterInRanges('_', '-', ('a', 'z'), ('A', 'Z'), ('0', '9')) | NonAscii | Escape,
            Comment        = "/*" > -(AnyChar / "*/") > "*/",
            ws             = -(Comment | Space),
            Name           = +NameChar,
            Variable       = "--" > NameStart > -NameChar,
            Ident          = !DashChar > NameStart > -NameChar,
            EscBreak       = (Newline | Hex),
            EscSeq         = EscChar > ((AnyChar / EscBreak) | (Hex.Repeat(1, 6) > Whitespace)),
            DblString      = '"' > -((AnyChar / ('"' | EscChar)) | EscSeq | (EscChar > Newline)) > '"',
            SglString      = "'" > -((AnyChar / ("'" | EscChar)) | EscSeq | (EscChar > Newline)) > "'",
            String_        = DblString | SglString,
            UrlEsc         = '%' > Hex > Hex,
            UrlChar        = UrlEsc | CharacterInRanges('_', ':', '/', '-', ('a', 'z'), ('A', 'Z'), ('0', '9')),
            UrlSymbol      = OneOf('!', '#', '$', '&', '*', '-', '~', ':', ';', '/', '@', '=', '.', '+', ','), // https://www.ietf.org/rfc/rfc3986.txt
            UrlExtSym      = OneOf('(', ')'),
            UrlStringDbl   = Whitespace > DblQuote > -(Escape | NonAscii | UrlSymbol | UrlChar | UrlExtSym | "'") > DblQuote > Whitespace,
            UrlStringSgl   = Whitespace > SingleQuote > -(Escape | NonAscii | UrlSymbol | UrlChar | UrlExtSym | '"') > SingleQuote > Whitespace,
            UrlStringUnq   = Whitespace > -(Escape | NonAscii | UrlSymbol | UrlChar) > Whitespace,
            Url            = CssStrEsc("url") > '(' > (UrlStringDbl | UrlStringSgl | UrlStringUnq) > ')',
            Url_           = "url(",
            Calc           = "calc(",
            Function_      = Ident > '(',
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
            DxImageTransform  = "progid:DXImageTransform.Microsoft." > Function_,
            MediaOnly         = CssStrEsc("only"),
            Not               = CssStrEsc("not"),
            And               = CssStrEsc("and"),
            Or                = CssStrEsc("or"),
            From              = CssStrEsc("from"),
            To                = CssStrEsc("to"),
            Keyframes         = At > !VendorPrefix > CssStrEsc("keyframes"),
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
            );

        #endregion Lexer side

        #region Parser side

        BNF // Number forms
            number           = !(Plus | Minus) > Number,
            percentage       = !(Plus | Minus) > Percentage,
            unknownDimension = !(Plus | Minus) > UnknownDimension,
            dimension        = !(Plus | Minus) > Dimension,
            hexColor         = Hash > ws
            ;

        BNF // Strings and Markers
            important = Important > ws,
            url       = (Url_ > ws > String_ > ws > ')') | Url;

        var _expr    = Forward();
        var _calcSum = Forward();

        BNF // Calc and Func Expressions
            ct               = !Comment,
            ident            = Ident | MediaOnly | Not | And | Or | From | To,
            operator_        = ('/' > ws) | (Comma > ws) | (ReqSpace > ws) | ('=' > ws),
            var_             = Var > ws > Variable > ws > ')' > ws,
            calcValue        = (number > ct) | (dimension > ct) | (unknownDimension > ct) | (percentage > ct) | ('(' > ws > _calcSum > ')' > ct),
            calcProduct      = calcValue > -(('*' > ws > calcValue) | ('/' > ws > number > ws)),
            calcSum          = calcProduct > -(ReqSpace > ws > (Plus | Minus) > ct > ReqSpace > ws > calcProduct),
            calc             = Calc > ws > calcSum > ')' > ws,
            function_        = Function_ > ws > _expr > ')' > ws,
            dxImageTransform = DxImageTransform > ws > _expr > ')' > ws,
            term = (number > ws)
                 | (percentage > ws)
                 | (dimension > ws)
                 | (String_ > ws)
                 | (UnicodeRange > ws)
                 | (ident > ws)
                 | var_
                 | (url > ws)
                 | hexColor
                 | calc
                 | function_
                 | (unknownDimension > ws)
                 | dxImageTransform,
            expr = term > -(!operator_ > term)
            ;

        _calcSum.Is(calcSum);
        _expr.Is(expr);

        BNF // Properties
            featureValueDefinition = ident > ws > ':' > ws > number > -(ws > number),
            featureType            = AtKeyword,
            featureValueBlock      = featureType > ws > '{' > ws > !featureValueDefinition > -(ws > ';' > ws > featureValueDefinition) > '}' > ws,
            fontFamilyName         = String_ | (ident > -(ws > ident)),
            fontFamilyNameList     = fontFamilyName > -(ws > Comma > ws > fontFamilyName),
            fontFeatureValuesRule  = FontFeatureValues > ws > fontFamilyNameList > ws > '{' > ws > -featureValueBlock > '}' > ws,
            property_              = (ident > ws) | (Variable > ws) | ('*' > ident) | ('_' > ident),
            declaration            = property_ > ':' > ws > expr > !important,
            declarationList        = -(';' > ws) > declaration > ws > -(';' > ws > !declaration),
            counterStyle           = CounterStyle > ws > ident > ws > '{' > ws > !declarationList > '}' > ws;

        BNF // Pages
            pseudoPage = ':' > ident > ws,
            page       = Page > ws > !pseudoPage > '{' > ws > !declaration > -(';' > ws > !declaration) > '}' > ws;


        var _any_            = Forward();
        var _nestedStatement = Forward();
        var _block           = Forward();

        BNF // Media Expressions
            mediaType       = ident,
            mediaFeature    = ident > ws,
            mediaExpression = '(' > ws > mediaFeature > !(':' > ws > expr) > ')' > ws,
            mediaQuery = (!(MediaOnly | Not) > ws > mediaType > ws > -(And > ws > mediaExpression))
                       | (mediaExpression > -(And > ws > mediaExpression)),
            mediaQueryList = !(mediaQuery > -(Comma > ws > mediaQuery)) > ws,
            groupRuleBody  = '{' > ws > -_nestedStatement > '}' > ws,
            media          = Media > ws > mediaQueryList > groupRuleBody > ws
            ;

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

        BNF
            block = '{' > ws > -(declarationList | _nestedStatement | _any_ | _block | (AtKeyword > ws) | (';' > ws)) > '}' > ws,
            unused = block
                   | (AtKeyword > ws)
                   | (';' > ws)
                   | (Cdo > ws)
                   | (Cdc > ws);
        _block.Is(block);

        BNF
            any_ = (ident > ws)
                 | (number > ws)
                 | (percentage > ws)
                 | (dimension > ws)
                 | (unknownDimension > ws)
                 | (String_ > ws)
                 | (url > ws)
                 | (Hash > ws)
                 | (UnicodeRange > ws)
                 | (Includes > ws)
                 | (DashMatch > ws)
                 | (':' > ws)
                 | (Function_ > ws > -(_any_ | unused) > ')' > ws)
                 | ('(' > ws > -(_any_ | unused) > ')' > ws)
                 | ('[' > ws > -(_any_ | unused) > ']' > ws);
        _any_.Is(any_);

        BNF
            combinator          = (Plus > ws) | (Greater > ws) | (Tilde > ws) | (ReqSpace > ws),
            elementName         = ident,
            typeNamespacePrefix = !(ident | '*') > '|',
            typeSelector        = (!typeNamespacePrefix) > elementName,
            universal           = '*' | (typeNamespacePrefix > '*'),
            className           = '.' > ident,
            attrib = '[' > ws > !typeNamespacePrefix > ident > ws >
                     !((PrefixMatch | SuffixMatch | SubstringMatch | '=' | Includes | DashMatch) > ws > (ident | String_) > ws)
                   > ']',
            expression       = +((Plus | Minus | Dimension | UnknownDimension | Number | String_ | ident) > ws),
            functionalPseudo = Function_ > ws > expression > ')',
            pseudo           = ':'.Repeat(1, 2) > (ident | functionalPseudo),
            negationArg      = typeSelector | universal | Hash | className | attrib | pseudo,
            negation         = PseudoNot > ws > negationArg > ws > ')',
            simpleSelectorSequence = (-(typeSelector | universal) > -(Hash | className | attrib | pseudo | negation))
                                   | +(Hash | className | attrib | pseudo | negation),
            selector      = simpleSelectorSequence > ws > -(combinator > simpleSelectorSequence > ws),
            selectorGroup = selector > -((Comma | ReqSpace) > ws > selector),
            ruleset = (selectorGroup > '{' > ws > !declarationList > '}' > ws) // knownRuleset
                    | (-any_ > ws > '{' > ws > !declarationList > '}' > ws); // unknownRuleset

        BNF
            viewport = Viewport > ws > '{' > ws > !declarationList > '}' > ws,
            value    = +(any_ | block | (AtKeyword > ws)),
            atRule   = AtKeyword > ws > -any_ > (block | (';' > ws)), // unknownAtRule
            fontFaceDeclaration = (property_ > ':' > ws > expr) // knownFontFaceDeclaration
                                | (property_ > ':' > ws > value), // unknownFontFaceDeclaration
            fontFaceRule     = FontFace > ws > '{' > ws > !fontFaceDeclaration > -(';' > ws > !fontFaceDeclaration) > '}' > ws,
            keyframeSelector = (From | To | Percentage) > ws > -(Comma > ws > (From | To | Percentage) > ws),
            keyframeBlock    = keyframeSelector > '{' > ws > !declarationList > '}' > ws,
            keyframesRule    = Keyframes > ws > ReqSpace > ws > ident > ws > '{' > ws > -keyframeBlock > '}' > ws;

        var _supportsCondition = Forward();
        BNF
            generalEnclosed              = (Function_ | '(') > -(any_ | unused) > ')',
            supportsDeclarationCondition = '(' > ws > declaration > ')',
            supportsConditionInParens    = ('(' > ws > _supportsCondition > ws > ')') | supportsDeclarationCondition | generalEnclosed,
            supportsDisjunction          = supportsConditionInParens > +(ws > ReqSpace > ws > Or > ws > ReqSpace > ws > supportsConditionInParens),
            supportsConjunction          = supportsConditionInParens > +(ws > ReqSpace > ws > And > ws > ReqSpace > ws > supportsConditionInParens),
            supportsNegation             = Not > ws > ReqSpace > ws > supportsConditionInParens,
            supportsCondition            = supportsNegation | supportsConjunction | supportsDisjunction | supportsConditionInParens,
            supportsRule                 = Supports > ws > supportsCondition > ws > groupRuleBody;
        _supportsCondition.Is(supportsCondition);


        BNF // Statements
            nestedStatement = ruleset
                            | media
                            | page
                            | fontFaceRule
                            | keyframesRule
                            | supportsRule
                            | viewport
                            | counterStyle
                            | fontFeatureValuesRule
                            | atRule
            ;
        _nestedStatement.Is(nestedStatement);

        BNF stylesheet = ws >
                         -(charset > -(Comment | Space | Cdo | Cdc)) >
                         -(imports > -(Comment | Space | Cdo | Cdc)) >
                         -(namespace_ > -(Comment | Space | Cdo | Cdc)) >
                         -(nestedStatement > -(Comment | Space | Cdo | Cdc))
                       > EndOfInput
            ;

        #endregion Parser side

        #region Tagging

        ruleset.TagWith("ruleset");
        media.TagWith("media");
        page.TagWith("page");
        fontFaceRule.TagWith("fontFaceRule");
        keyframesRule.TagWith("keyframesRule");
        supportsRule.TagWith("supportsRule");
        viewport.TagWith("viewport");
        counterStyle.TagWith("counterStyle");
        fontFeatureValuesRule.TagWith("fontFeatureValuesRule");
        atRule.TagWith("atRule");

        #endregion Tagging

        return stylesheet.WithOptions(Options.None);
    }

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
}