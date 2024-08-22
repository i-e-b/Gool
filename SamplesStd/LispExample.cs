using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class LispExample
{
    public static readonly BNF.Package Parser = MakeParser();
    private static BNF.Package MakeParser()
    {
        // This isn't any particular lisp dialect
        
        BNF identifier    = BNF.Regex("[_a-zA-Z][_a-zA-Z0-9]*");
        BNF number        = BNF.Regex(@"\-?[0-9][_0-9]*(\.[_0-9]+)?");
        BNF quoted_string = BNF.Regex("\"([^'\"]|\\\")*");
        
        BNF dot         = '.';
        BNF name        = ':' > identifier;
        BNF normal_list =  "("; // https://xkcd.com/297/
        BNF quoted_list = "'(";
        BNF end_list    =  ")";
        BNF comment     = ';' > (-BNF.NotLineEnd);
        
        BNF list_item  = identifier.Tagged(Atom) | name | quoted_string | number | dot;
        BNF start_list = normal_list | quoted_list;

        dot.TagWith(Atom);
        name.TagWith(Name);
        number.TagWith(Number);
        quoted_string.TagWith(String);

        normal_list.OpenScope().TagWith(List);
        quoted_list.OpenScope().TagWith(Quote);
        end_list.CloseScope().TagWith(End);

        return BNF
            .Recursive(tree => +(list_item | start_list | end_list | comment | tree)) // https://xkcd.com/224/
            .WithOptions(BNF.Options.SkipWhitespace);
    }
    
    public const string Quote = "Quote";
    public const string Name = "Name";
    public const string Atom = "Atom";
    public const string Number = "Number";
    public const string String = "String";
    public const string List = "List";
    public const string End = "End"; 
}