using Gool;

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
    public static BNF.Package Connection()
    {
        BNF
            key = +(BNF.AnyChar / '='),
            value = QuotedValue('{', BNF.AnyChar, '}') // note use of function to make BNF more readable
                  | QuotedValue('"', BNF.AnyChar, '"')
                  | QuotedValue("'", BNF.AnyChar, "'")
                  | QuotedValue("",  BNF.AnyChar, ""),
            setting = key > '=' > value > ';',
            settings = BNF.Empty | ";" | +(setting);

        key.TagWith("Key").PivotScope();

        return settings.WithOptions(BNF.Options.IgnoreCase | BNF.Options.SkipWhitespace);
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