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
        var _element = new Recursion();
        var _value = new Recursion();
        
        BNF ws = @"#\s*";
        BNF neg = '-';
        BNF digit = "#[0-9]";
        BNF zero = '0';
        BNF exp = "#[eE]";
        BNF sign = BNF.OneOf('+', '-');

        BNF escape = BNF.OneOf('"','\\','/','b','f','n','r','t') | "#u[0-9a-fA-F]{4}";
        BNF character = "#[^\"\\\\]" | ( '\\' > escape );
        BNF characters = -character;
        BNF j_string = '"' > characters > '"';
        
        BNF member = ws > j_string > ws > ':' > _element;
        BNF members = member % ',';
        BNF j_object = '{' > ( ws | members) > '}';

        BNF element = ws > _value > ws;
        BNF elements = element % ',';

        BNF j_array = '[' > elements > ']';


        BNF digits = +digit;
        BNF exponent = !(exp > sign > digits);
        BNF fraction = !('.' > digits);
        BNF integer = (!neg) > (+digit); // this is slightly out of spec, as it allows "01234"
        BNF number = integer > fraction > exponent;
        
        BNF value = j_object | j_array | j_string | number | "true" | "false" | "null";
        
        _element.Source = element.Result();
        _value.Source = value.Result();

        j_string.Tag("string");
        number.Tag("number");
        _value.Tag("value");
        
        return _element;
    }
}