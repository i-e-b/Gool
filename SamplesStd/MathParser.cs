using System.Text.RegularExpressions;
using Phantom;
// ReSharper disable InconsistentNaming

namespace Samples;

public class MathParser
{
    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }

    public static IParser TheParser()
    {
        BNF.RegexOptions = Options();

        var _expression = BNF.Forward();

        BNF add_sub = BNF.OneOf('+', '-');
        BNF mul_div = BNF.OneOf('*', '/');

        BNF open_par = '(';
        BNF close_par = ')';

        BNF number = @"#[0-9]+(\.[0-9]+)?";
        BNF factor = number | (open_par > _expression > close_par);
        BNF power = factor > !('^' > factor);
        BNF term = power % mul_div;
        BNF expression = term % add_sub;

        _expression.Is(expression);

        add_sub.Tag("operation").PivotScope();
        mul_div.Tag("operation").PivotScope();
        open_par.OpenScope();
        close_par.CloseScope();
        number.Tag("value");

        return expression.Parser();
    }
}