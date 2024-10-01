using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class ArithmeticExample
{
    public static readonly Package Parser = Arithmetic();

    private static Package Arithmetic()
    {
        var _expression = Forward();

        BNF
            add_sub  = OneOf('+', '-'),
            mul_div  = OneOf('*', '/'),
            exponent = '^';

        BNF
            number     = FractionalDecimal(),
            factor     = number | ('(' > _expression > ')'),
            power      = factor > !(exponent > factor),
            term       = power % mul_div,
            expression = term % add_sub;

        _expression.Is(expression);

        add_sub.TagWith(Operation).PivotScope();
        mul_div.TagWith(Operation).PivotScope();
        exponent.TagWith(Operation).PivotScope();
        number.TagWith(Value);

        return expression.WithOptions(Options.SkipWhitespace);
    }

    public const string Operation = "operation";
    public const string Value     = "value";
}