using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class JsonParser
{
    public static readonly ParserPackage Json = BuildBnf();

    /// <summary>
    /// JSON parser directly from the spec at https://www.json.org/json-en.html
    /// </summary>
    private static ParserPackage BuildBnf()
    {
        var value = Forward();

        BNF // Basic components
            ws     = AnyWhiteSpace,
            number = FractionalDecimal(groupMark: "", decimalMark: ".", allowLeadingZero: false, allowLeadingPlus: false);

        BNF // Strings
            unicodeEsc    = 'u' > CharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')).Repeat(4),
            escape        = OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't') | unicodeEsc,
            character     = NoneOf('"', '\\') | ('\\' > escape),
            characters    = -character,
            quoted_string = '"' > characters > '"';

        BNF // Elements (lone or in arrays)
            element  = ws > value > ws,
            elements = element % ',';

        BNF // Members of objects
            member_key = quoted_string.Copy(),
            member     = ws > member_key > ws > ':' > element,
            members    = member % ',';

        BNF // Objects
            object_enter = '{',
            object_leave = '}',
            object_block = object_enter > (ws | members) > object_leave;

        BNF // Arrays
            array_enter = '[',
            array_leave = ']',
            array_block = array_enter > elements > array_leave;

        BNF // Single values
            primitive = quoted_string >= number >= "true" >= "false" >= "null";

        value.Is(object_block >= array_block >= primitive);

        array_enter.OpenScope().TagWith("array");
        array_leave.CloseScope();

        object_enter.OpenScope().TagWith("object");
        object_leave.CloseScope();

        member_key.TagWith("key");
        primitive.TagWith("value");

        return element.Build();
    }
}