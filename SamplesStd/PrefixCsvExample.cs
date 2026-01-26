using Gool;
using Gool.Results;
using static Gool.BNF;
// ReSharper disable InconsistentNaming

namespace Samples;

public static class PrefixCsvExample
{
    public static readonly ParserPackage Parser = MakeParser();

    private static ParserPackage MakeParser()
    {
        // This format is something I saw being used for LLM inputs.
        // It has the odd structure where all values are unenclosed
        // strings, with comma separation; but if a number 'n' in
        // square brackets is found, the next 'n' values are a child
        // list, and this can continue recursively.
        //
        // This is so simple that it would probably be better to
        // custom implement a parser, but supporting this kind of
        // contextual parameter seems interesting to add to Gool.

        var _list_item = Forward();

        BNF
            item_count = IntegerRange(1, int.MaxValue).TagWith(ItemCount),
            data       = StringToEndOrTerminatedBy(",", "\r", "\n", "[") / "[",
            sub_list = "[" >
                       Context(
                           item_count > ']',
                           m => m.FindIntByTag(ItemCount, out var itemCount) ? Repeat(_list_item, itemCount, itemCount) : null),
            list_item = (data > !("," | LineEnd)) | sub_list,
            list      = -list_item;

        _list_item.Is(list_item);

        sub_list.TagWith(List);
        data.TagWith(Item);

        return list.BuildWithOptions(Options.SkipWhitespace);
    }

    public const string ItemCount = "ItemCount";
    public const string List      = "List";
    public const string Item      = "Item";
}
