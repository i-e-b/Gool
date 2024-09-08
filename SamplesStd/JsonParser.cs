using System.Text.RegularExpressions;
using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class JsonParser
{
    public static readonly BNF.Package Json = BuildBnf();

    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture;
    }

    /// <summary>
    /// JSON parser directly from the spec at https://www.json.org/json-en.html
    /// </summary>
    private static BNF.Package BuildBnf()
    {
        BNF.RegexOptions = Options();

        var value = BNF.Forward();

        BNF ws = BNF.Regex(@"\s*");
        BNF neg = '-';
        BNF digit = BNF.Regex("[0-9]");
        BNF exp = BNF.OneOf('e', 'E');
        BNF sign = BNF.OneOf('+', '-');

        BNF escape = BNF.OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't') | BNF.Regex("u[0-9a-fA-F]{4}");
        BNF character = BNF.Regex("""[^"\\]""") | ('\\' > escape);
        BNF characters = -character;
        BNF quoted_string = '"' > characters > '"';

        BNF element = ws > value > ws;
        BNF elements = element % ',';

        BNF member_key = quoted_string.Copy();
        BNF member = ws > member_key > ws > ':' > element;
        BNF members = member % ',';

        BNF object_enter = '{';
        BNF object_leave = '}';
        BNF object_block = object_enter > (ws | members) > object_leave;

        BNF array_enter = '[';
        BNF array_leave = ']';
        BNF array_block = array_enter > elements > array_leave;

        BNF digits = +digit;
        BNF exponent = !(exp > sign > digits);
        BNF fraction = !('.' > digits);
        BNF integer = (!neg) > (+digit); // this is slightly out of spec, as it allows "01234"
        BNF number = integer > fraction > exponent;

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