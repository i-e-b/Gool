using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace TestLanguageImplementation;

public static class LanguageDefinition
{
    public static readonly Package Instance = Def();

    private static Package Def()
    {
        BNF // Comments and Headers
            shebang     = "#!" > -(AnyChar / LineEnd),
            declKey     = IdentifierString(),
            declValue   = +(AnyChar / ';'),
            declSetting = declKey > '=' > declValue,
            headerDecl  = "#set" > (declSetting < ';'),
            comment     = '#' > -(AnyChar / LineEnd);

        shebang.NoAutoAdvance();
        comment.NoAutoAdvance();

        BNF // Strings
            unicodeEsc   = "\\u" > FixedSizeInteger(0, 0xffff, 4, useHex: true),
            escape       = '\\' > OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't'),
            literalChar  = NoneOf('"', '\\'),
            character    = literalChar | escape | unicodeEsc,
            characters   = -character,
            quotedString = '"' > characters > '"';

        quotedString.NoAutoAdvance();

        BNF // Identifier types
            variable  = IdentifierString(),
            parameter = IdentifierString(),
            number    = FractionalDecimal(),
            function  = IdentifierString();

        BNF // block delimiters
            start_block = '{',
            end_block   = '}';

        var _innerExpr = Forward();
        BNF // General Expression
            add_sub    = OneOf('+', '-'),
            mul_div    = OneOf('*', '/'),
            exponent   = '^',
            factor     = quotedString | number | variable | (function > '(' > !(_innerExpr % ',') > ')') | ('(' > _innerExpr > ')'),
            power      = factor > !(exponent > factor),
            term       = power % mul_div,
            expression = term % add_sub,
            rootExpr   = expression.Copy();
        _innerExpr.Is(expression);

        var _statement = Forward();
        BNF // Statements and Blocks
            call          = function > '(' > !(rootExpr % ',') > ')' > ';',
            assign        = variable > '=' > rootExpr > ';',
            equality      = OneOf("=", "<", ">", "<=", ">="),
            comparison    = rootExpr > equality > rootExpr,
            else_block    = "else" > start_block > (-_statement) > end_block,
            if_block      = "if" > comparison > start_block > (-_statement) > end_block > !else_block,
            break_call    = "break" > variable > ';',
            continue_call = "continue" > variable > ';',
            loop          = "loop" > variable > start_block > (+_statement) > end_block,
            return_call   = "return" > !variable > ';',
            statement     = call | assign | if_block | loop | break_call | continue_call | return_call | comment;
        _statement.Is(statement);

        BNF // Func definition and full program file.
            definition = "fn" > function > !(parameter % ',') > start_block > -(statement) > end_block,
            language   = !shebang > headerDecl > -(comment | definition);

        headerDecl.TagWith(FileHeader);
        declKey.TagWith(FileHeaderKey);
        declValue.TagWith(FileHeaderValue);
        declSetting.EncloseScope().TagWith(FileHeaderSetting);

        comment.Atomic().TagWith(Comment);

        definition.EncloseScope().TagWith(FunctionDefinition);
        call.EncloseScope().TagWith(FunctionCall);
        if_block.EncloseScope().TagWith(IfBlock);
        else_block.EncloseScope().TagWith(ElseBlock);

        comparison.EncloseScope().TagWith(Comparison);
        equality.PivotScope().TagWith(EqualityOp);

        assign.EncloseScope().TagWith(Assignment);
        rootExpr.EncloseScope().TagWith(Expression);
        loop.EncloseScope().TagWith(Loop);

        add_sub.PivotScope().TagWith(MathOp);
        mul_div.PivotScope().TagWith(MathOp);
        exponent.PivotScope().TagWith(MathOp);

        start_block.OpenScope().TagWith(StartBlock);
        end_block.CloseScope().TagWith(EndBlock);

        break_call.EncloseScope().TagWith(BreakCall);
        continue_call.EncloseScope().TagWith(ContinueCall);
        return_call.EncloseScope().TagWith(ReturnCall);

        function.TagWith(FunctionName);
        number.TagWith(Number);
        variable.TagWith(Variable);
        quotedString.Atomic().TagWith(QuotedString);
        parameter.TagWith(Parameter);

        return language.WithOptions(Options.SkipWhitespace);
    }

    // ReSharper disable MemberCanBePrivate.Global
    public const string Comment = "Comment";

    public const string EqualityOp = "EqualityOp";
    public const string MathOp     = "MathOp";

    public const string Comparison = "Comparison";
    public const string Assignment = "Assignment";
    public const string Expression = "Expression";
    public const string Loop       = "Loop";

    public const string Number       = "Number";
    public const string Variable     = "Variable";
    public const string QuotedString = "QuotedString";
    public const string Parameter    = "Parameter";

    public const string StartBlock = "StartBlock";
    public const string EndBlock   = "EndBlock";
    public const string IfBlock    = "IfBlock";
    public const string ElseBlock  = "ElseBlock";

    public const string BreakCall    = "BreakCall";
    public const string ContinueCall = "ContinueCall";
    public const string ReturnCall   = "ReturnCall";

    public const string FunctionDefinition = "FunctionDefinition";
    public const string FunctionName       = "FunctionName";
    public const string FunctionCall       = "FunctionCall";

    public const string FileHeader      = "FileHeader";
    public const string FileHeaderKey   = "FileHeaderKey";
    public const string FileHeaderValue = "FileHeaderValue";

    public const string FileHeaderSetting = "FileHeaderSetting";
}