using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class LispExample
{
    public static readonly ParserPackage Parser = MakeParser();

    private static ParserPackage MakeParser()
    {
        // This isn't any particular lisp dialect

        BNF
            identifier    = Regex("[_a-zA-Z][_a-zA-Z0-9]*"),
            number        = Regex(@"\-?[0-9][_0-9]*(\.[_0-9]+)?"),
            quoted_string = Regex("\"([^'\"]|\\\")*");

        BNF
            dot         = '.',
            name        = ':' > identifier,
            normal_list = "(",                                                                                          // https://xkcd.com/297/
            quoted_list = "'(",
            end_list    = ")",
            comment     = ';' > (-NotLineEnd);

        BNF
            list_item  = identifier.Tagged(Atom) | name | quoted_string | number | dot,
            start_list = normal_list | quoted_list,
            document   = Recursive(tree => +(list_item | start_list | end_list | comment | tree));                      // https://xkcd.com/224/

        dot.TagWith(Atom);
        name.TagWith(Name);
        number.TagWith(Number);
        quoted_string.TagWith(String);

        normal_list.OpenScope().TagWith(List);
        quoted_list.OpenScope().TagWith(Quote);
        end_list.CloseScope().TagWith(End);

        return document.BuildWithOptions(Options.SkipWhitespace);
    }

    public const string Quote  = "Quote";
    public const string Name   = "Name";
    public const string Atom   = "Atom";
    public const string Number = "Number";
    public const string String = "String";
    public const string List   = "List";
    public const string End    = "End";
}