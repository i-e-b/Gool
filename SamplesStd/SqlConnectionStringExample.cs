using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Connection string parser, derived from descriptions at
/// https://www.connectionstrings.com/formating-rules-for-connection-strings/
/// and
/// https://learn.microsoft.com/en-us/sql/relational-databases/native-client/applications/using-connection-string-keywords-with-sql-server-native-client?view=sql-server-ver15
/// </summary>
public static class SqlConnectionStringExample
{
    public static ParserPackage Parser()
    {
        BNF
            key = +(AnyChar / '='),
            value = QuotedValue('{', AnyChar, '}') // note use of function to make BNF more readable
                  | QuotedValue('"', AnyChar, '"')
                  | QuotedValue("'", AnyChar, "'")
                  | QuotedValue("", AnyChar, ""),
            
            setting  = key > '=' > value,
            settings = Empty | ";" | ((setting % ';') > Optional(';'));

        key.TagWith("Key").PivotScope();

        return settings.BuildWithOptions(Options.IgnoreCase | Options.SkipWhitespace);
    }

    private static BNF QuotedValue(char open, BNF content, char close)
    {
        var valueContent = +(content / close);
        valueContent.TagWith("Value");

        return open > valueContent > close;
    }

    private static BNF QuotedValue(string open, BNF content, string close)
    {
        var valueContent
            = open.Length < 1
                ? +(content / ';')
                : +(content / close);

        valueContent.TagWith("Value");

        if (open.Length < 1) return valueContent;
        return (open > valueContent > close);
    }
}