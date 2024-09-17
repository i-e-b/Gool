using System.Text.RegularExpressions;
using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class ArithmeticExample
{
    public static readonly BNF.Package Parser = Arithmetic();
    
    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }

    private static BNF.Package Arithmetic()
    {
        BNF.RegexOptions = Options();

        var _expression = BNF.Forward();

        BNF
            add_sub = BNF.OneOf('+', '-'), // same as: (BNF)'+' | '-';
            mul_div = BNF.OneOf('*', '/'),
            exp     = '^';

        BNF
            number     = BNF.FractionalDecimal(),
            factor     = number | ('(' > _expression > ')'),
            power      = factor > !(exp > factor),
            term       = power % mul_div,
            expression = term % add_sub;

        _expression.Is(expression);

        add_sub.TagWith(Operation).PivotScope();
        mul_div.TagWith(Operation).PivotScope();
        exp.TagWith(Operation).PivotScope();
        number.TagWith(Value);

        return expression.WithOptions(BNF.Options.SkipWhitespace);
    }

    public const string Operation = "operation";
    public const string Value = "value";
}