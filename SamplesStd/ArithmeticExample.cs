using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class ArithmeticExample
{
    public static ParserPackage Arithmetic()
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

        return expression.BuildWithOptions(Options.SkipWhitespace);
    }


    public static ParserPackage ExpressionWithVariablesAndFunctions()
    {
        var _expression = Forward();

        BNF
            variable = IdentifierString(),
            function = IdentifierString(),
            add_sub  = OneOf('+', '-'),
            mul_div  = OneOf('*', '/'),
            exponent = '^';

        BNF
            number     = FractionalDecimal(),
            factor     = number | variable | (function > '(' > !_expression > ')') | ('(' > _expression > ')'),
            power      = factor > !(exponent > factor),
            term       = power % mul_div,
            expression = term % add_sub;

        _expression.Is(expression);

        add_sub.TagWith(Operation).PivotScope();
        mul_div.TagWith(Operation).PivotScope();
        exponent.TagWith(Operation).PivotScope();
        number.TagWith(Value);
        variable.TagWith(Variable);
        function.TagWith(Function);

        return expression.BuildWithOptions(Options.SkipWhitespace);
    }

    public const string Operation = "operation";
    public const string Value     = "value";
    public const string Variable  = "variable";
    public const string Function  = "function";
}