using System.Collections.Generic;
using Gool;
using static Gool.BNF;
// ReSharper disable InconsistentNaming

namespace Samples;

public static class HtmlEntityExample
{
    #region Named Entities
    /// <summary>
    /// Entity name => display character(s).
    /// This is not exhaustive.
    /// </summary>
    private static readonly Dictionary<string, string> _entityMap = new() {
        { "dollar", "$" }, { "cent", "\u00a2" }, { "pound", "£" }, { "curren", "\u00a4" }, { "yen", "\u00a5" },

        { "copy", "\u00A9" }, { "reg", "\u00AE" }, { "trade", "\u2122" }, { "commat", "\u0040" },

        { "excl", "!" }, { "num", "#" }, { "percnt", "%" }, { "amp", "&" }, { "lpar", "(" }, { "rpar", ")" },
        { "comma", "," }, { "period", "." }, { "sol", "\u002F" }, { "colon", ":" }, { "semi", ";" }, { "quest", "?" },
        { "lbrack", "[" }, { "bsol", "\u005C" }, { "rbrack", "]" }, { "Hat", "^" }, { "lowbar", "_" },
        { "grave", "\u0060" }, { "lbrace", "{" }, { "vert", "|" }, { "rbrace", "}" }, { "tilde", "~" },
        { "circ", "\u02C6" }, { "nbsp", " " }, { "ensp", " " }, { "emsp", " " }, { "thinsp", " " }, { "zwnj", "\u200C" },
        { "zwj", "\u200D" }, { "lrm", "\u200E" }, { "rlm", "\u200F" }, { "iexcl", "\u00A1" }, { "brvbar", "\u00A6" },
        { "sect", "\u00A7" }, { "uml", "\u00A8" }, { "sup2", "\u00B2" }, { "sup3", "\u00B3" }, { "acute", "\u00B4" },
        { "micro", "\u00B5" }, { "para", "\u00B6" }, { "middot", "\u00B7" }, { "cedil", "\u00B8" }, { "sup1", "\u00B9" },
        { "iquest", "\u00BF" }, { "ndash", "-" }, { "mdash", "-" }, { "bull", "\u2022" }, { "hellip", "\u2026" },
        { "quot", "\"" }, { "apos", "'" }, { "plus", "+" }, { "minus", "-" }, { "times", "\u00D7" },
        { "divide", "\u00F7" }, { "equals", "\u003D" }, { "plusmn", "\u00B1" }, { "ne", "\u2260" }, { "lt", "<" },
        { "gt", ">" }, { "deg", "\u00B0" }
    };
    #endregion

    public static readonly ParserPackage Parser = MakeParser();

    private static ParserPackage MakeParser()
    {
        BNF // Character and Entity References
            entity_name = OneOf(_entityMap.Keys),
            dec_value   = IntegerRange(1, 65535),
            hex_value   = IntegerRange(1, 65535, useHex: true),
            char_ref    = ("&#" > dec_value > ';') | ("&#x" > hex_value > ';'),
            entity_ref  = '&' > entity_name > ';',
            reference   = entity_ref | char_ref;

        BNF
            text = AnyChar,
            document = -(reference | text);

        dec_value.TagWith(DecimalEntity);
        hex_value.TagWith(HexEntity);
        entity_name.TagWith(NamedEntity);
        text.TagWith(Text);

        return document.Build();
    }

    public const string HexEntity = "decimal";
    public const string DecimalEntity = "hex";
    public const string NamedEntity = "name";
    public const string Text = "text";

    public static string ConvertByName(string? entityName)
    {
        if (string.IsNullOrWhiteSpace(entityName)) return "";
        return _entityMap.GetValueOrDefault(entityName, "");
    }
}