using System.Globalization;
using Gool;
using Gool.Results;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace TestLanguageImplementation;

public static class LanguageDefinition
{
    public static readonly Package Instance = Def();

    private static Package Def()
    {
        BNF // Headers
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

        BNF // identifier types
            variable  = IdentifierString(),
            parameter = IdentifierString(),
            number    = FractionalDecimal(),
            function  = IdentifierString();

        BNF // block delimiters
            start_block = '{',
            end_block   = '}';

        BNF // infix operators
            add_sub  = OneOf('+', '-'),
            mul_div  = OneOf('*', '/'),
            exponent = '^';

        var _innerExpr = Forward();
        BNF // general expression
            factor     = quotedString | number | variable | (function > '(' > !(_innerExpr % ',') > ')') | ('(' > _innerExpr > ')'),
            power      = factor > !(exponent > factor),
            term       = power % mul_div,
            expression = term % add_sub,
            rootExpr   = expression.Copy();
        _innerExpr.Is(expression);

        var _statement = Forward();
        BNF // Parts
            call          = function > '(' > !(rootExpr % ',') > ')' > ';',
            assign        = variable > '=' > rootExpr > ';',
            equality      = OneOf("=", "<", ">", "<=", ">="),
            else_block    = "else" > start_block > (-_statement) > end_block,
            if_block      = "if" > rootExpr > equality > rootExpr > start_block > (-_statement) > end_block > !else_block,
            break_call    = "break" > variable > ';',
            continue_call = "continue" > variable > ';',
            loop          = "loop" > variable > '{' > (-_statement) > '}',
            return_call   = "return" > variable > ';',
            statement     = call | assign | if_block | loop | break_call | continue_call | return_call | comment;

        _statement.Is(statement);

        BNF // Func definition and full program file.
            definition = "fn" > function > !(parameter % ',') > start_block > -(statement) > end_block,
            language   = !shebang > headerDecl > -(comment | definition);

        comment.Atomic().TagWith(Comment);
        headerDecl.TagWith(FileHeader);
        declKey.TagWith(FileHeaderKey);
        declValue.TagWith(FileHeaderValue);
        declSetting.EncloseScope().TagWith(FileHeaderSetting);
        equality.TagWith(EqualityOp);

        definition.EncloseScope().TagWith(FunctionDefinition);
        call.TagWith(FunctionCall).EncloseScope();
        if_block.EncloseScope().TagWith(IfBlock);
        else_block.EncloseScope().TagWith(ElseBlock);

        assign.EncloseScope().TagWith(Assignment);
        rootExpr.EncloseScope().TagWith(Expression);
        loop.TagWith(Loop);

        add_sub.TagWith(MathOp).PivotScope();
        mul_div.TagWith(MathOp).PivotScope();
        exponent.TagWith(MathOp).PivotScope();

        start_block.TagWith(StartBlock).OpenScope();
        end_block.TagWith(EndBlock).CloseScope();
        break_call.TagWith(BreakCall).EncloseScope();
        continue_call.TagWith(ContinueCall).EncloseScope();
        return_call.TagWith(ReturnCall).EncloseScope();

        function.TagWith(FunctionName);
        number.TagWith(Number);
        variable.TagWith(Variable);
        quotedString.TagWith(QuotedString);
        parameter.TagWith(Parameter);

        return language.WithOptions(Options.SkipWhitespace);
    }

    // ReSharper disable MemberCanBePrivate.Global
    public const string Comment = "Comment";

    public const string EqualityOp = "EqualityOp";
    public const string MathOp     = "MathOp";

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
    // ReSharper restore MemberCanBePrivate.Global
}