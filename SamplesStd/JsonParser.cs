using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class JsonParser
{
    public static readonly BNF.Package Json = BuildBnf();

    /// <summary>
    /// JSON parser directly from the spec at https://www.json.org/json-en.html
    /// </summary>
    private static BNF.Package BuildBnf()
    {
        var value = BNF.Forward();

        BNF // Basic components
            ws = BNF.AnyWhiteSpace;

        BNF // Strings
            unicodeEsc    = 'u' > BNF.AnyCharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')).Repeat(4),
            escape        = BNF.OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't') | unicodeEsc,
            character     = BNF.AnyCharacterNotInRanges('"', '\\') | ('\\' > escape),
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

        /*
        BNF // Number literals
            neg          = '-',
            digit        = BNF.AnyCharacterInRanges(('0', '9')),
            nonZeroDigit = BNF.AnyCharacterInRanges(('1', '9')),
            exp          = BNF.OneOf('e', 'E'),
            sign         = BNF.OneOf('+', '-'),
            digits       = +digit,
            exponent     = !(exp > sign > digits),
            fraction     = !('.' > digits),
            integer      = '0' | ((!neg) > (nonZeroDigit) > (-digit)),
            number       = integer > fraction > exponent;
        */
        BNF number = BNF.FractionalDecimal(groupMark: "", decimalMark: "."); // this is slightly out of spec, as it allows "01234" or "+1234"

        BNF primitive = quoted_string | number | "true" | "false" | "null";

        value.Is(object_block | array_block | primitive);


        array_enter.OpenScope().TagWith("array");
        array_leave.CloseScope();

        object_enter.OpenScope().TagWith("object");
        object_leave.CloseScope();

        member_key.TagWith("key");
        primitive.TagWith("value");

        return element.WithOptions(BNF.Options.None);
    }
}