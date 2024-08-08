using System.Text.RegularExpressions;
using Phantom;
using Phantom.Parsers;
// ReSharper disable InconsistentNaming

namespace Samples;

public class JsonParser
{
    public IParser TheParser { get; }

    public JsonParser()
    {
        TheParser = Json();
    }

    private RegexOptions ops()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }

    private IParser Json()
    {
        BNF.RegexOptions = ops();

        // From https://www.json.org/json-en.html
        var _value = new Recursion();
        
        BNF ws = @"#\s*";
        BNF neg = '-';
        BNF digit = "#[0-9]";
        BNF exp = "#[eE]";
        BNF sign = BNF.OneOf('+', '-');

        BNF escape = BNF.OneOf('"','\\','/','b','f','n','r','t') | "#u[0-9a-fA-F]{4}";
        BNF character = "#[^\"\\\\]" | ( '\\' > escape );
        BNF characters = -character;
        BNF quoted_string = '"' > characters > '"';

        BNF element = ws > _value > ws;
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
        BNF value = object_block | array_block | primitive;


        array_enter.OpenScope().Tag("array");
        array_leave.CloseScope();
        
        object_enter.OpenScope().Tag("object");
        object_leave.CloseScope();

        member_key.Tag("key");
        primitive.Tag("value");
        
        _value.Source = value.Result();
        return element.Result();
    }
}