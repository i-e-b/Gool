using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class JsonParser
{
    public static readonly Package Json = BuildBnf();

    /// <summary>
    /// JSON parser directly from the spec at https://www.json.org/json-en.html
    /// </summary>
    private static Package BuildBnf()
    {
        var value = Forward();

        BNF // Basic components
            ws = AnyWhiteSpace;

        BNF // Strings
            unicodeEsc    = 'u' > AnyCharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')).Repeat(4),
            escape        = OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't') | unicodeEsc,
            character     = AnyCharacterNotInRanges('"', '\\') | ('\\' > escape),
            characters    = -character,
            quoted_string = '"' > characters > '"';

        BNF // Elements of arrays
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

        BNF number = FractionalDecimal(groupMark: "", decimalMark: "."); // this is slightly out of spec, as it allows "01234" or "+1234"

        BNF primitive = quoted_string | number | "true" | "false" | "null";

        value.Is(object_block | array_block | primitive);


        array_enter.OpenScope().TagWith("array");
        array_leave.CloseScope();

        object_enter.OpenScope().TagWith("object");
        object_leave.CloseScope();

        member_key.TagWith("key");
        primitive.TagWith("value");

        return element.WithOptions(Options.None);
    }
}