using System.Text.RegularExpressions;
using Phantom;
// ReSharper disable InconsistentNaming

namespace Samples;

public static class ArithmeticExample
{
    public static readonly BNF Parser = Arithmetic();
    
    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }

    private static BNF Arithmetic()
    {
        BNF.RegexOptions = Options();

        var _expression = BNF.Forward();

        BNF add_sub = BNF.OneOf('+', '-');
        BNF mul_div = BNF.OneOf('*', '/');
        BNF exp = '^';

        BNF number = @"#\-?[0-9]+(\.[0-9]+)?"; // signed numbers
        BNF factor = number | ('(' > _expression > ')');
        BNF power = factor > !(exp > factor);
        BNF term = power % mul_div;
        BNF expression = term % add_sub;

        _expression.Is(expression);

        add_sub.Tag(Operation).PivotScope();
        mul_div.Tag(Operation).PivotScope();
        exp.Tag(Operation).PivotScope();
        number.Tag(Value);

        return expression;
    }

    public const string Operation = "operation";
    public const string Value = "value";
}