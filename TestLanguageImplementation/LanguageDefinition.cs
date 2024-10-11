﻿using Gool;
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
            headerDecl  = "#tli" > (declSetting < ';'),
            comment     = '#' > -(AnyChar / LineEnd);

        shebang.NoAutoAdvance();
        comment.NoAutoAdvance();

        BNF // Strings
            unicodeEsc   = "\\u" > CharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')).Repeat(4),
            escape       = '\\' > OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't'),
            character    = NoneOf('"', '\\') | escape | unicodeEsc,
            characters   = -character,
            quotedString = '"' > characters > '"';

        var _expression = Forward();

        BNF // identifier types
            variable = IdentifierString(),
            number   = FractionalDecimal(),
            function = IdentifierString();

        BNF // block delimiters
            start_block = '{',
            end_block   = '}';

        BNF // infix operators
            add_sub  = OneOf('+', '-'),
            mul_div  = OneOf('*', '/'),
            exponent = '^';

        BNF // general expression
            factor     = quotedString | number | variable | (function > '(' > !_expression > ')') | ('(' > _expression > ')'),
            power      = factor > !(exponent > factor),
            term       = power % mul_div,
            expression = term % add_sub;

        _expression.Is(expression);

        var _statement = Forward();
        BNF // Parts
            call          = function > '(' > expression > ')' > ';',
            assign        = variable > '=' > expression > ';',
            equality      = OneOf("=", "<", ">", "<=", ">="),
            else_block    = "else" > start_block > (-_statement) > end_block,
            if_block      = "if" > expression > equality > expression > start_block > (-_statement) > end_block > !else_block,
            break_call    = "break" > variable > ';',
            continue_call = "continue" > variable > ';',
            loop          = "loop" > variable > '{' > (-_statement) > '}',
            return_call   = "return" > variable > ';',
            statement     = call | assign | if_block | loop | break_call | continue_call | return_call | comment;

        _statement.Is(statement);

        BNF // Full definition
            definition = "fn" > function > start_block > -(statement) > end_block,
            language   = !shebang > headerDecl > -(comment | definition)
            ;

        comment.Atomic().TagWith(Comment);
        headerDecl.TagWith(FileHeader);
        declKey.TagWith(FileHeaderKey);
        declValue.TagWith(FileHeaderValue);
        declSetting.EncloseScope().TagWith(FileHeaderSetting);
        equality.TagWith(EqualityOp);

        definition.EncloseScope().TagWith(FunctionDefinition);
        function.TagWith(FunctionName);
        call.TagWith(FunctionCall);
        if_block.EncloseScope().TagWith(IfBlock);
        else_block.EncloseScope().TagWith(ElseBlock);

        assign.EncloseScope().TagWith(Assignment);
        expression.TagWith(Expression);

        add_sub.TagWith(MathOp).PivotScope();
        mul_div.TagWith(MathOp).PivotScope();
        exponent.TagWith(MathOp).PivotScope();
        start_block.TagWith(StartBlock).OpenScope();
        end_block.TagWith(EndBlock).CloseScope();

        number.TagWith(Number);
        variable.TagWith(Variable);

        return language.WithOptions(Options.SkipWhitespace);
    }


    // ReSharper disable MemberCanBePrivate.Global
    public const string Comment = "Comment";

    public const string EqualityOp = "EqualityOp";
    public const string MathOp     = "MathOp";
    public const string Assignment = "Assignment";
    public const string Expression = "Expression";

    public const string Number   = "Number";
    public const string Variable = "Variable";

    public const string StartBlock = "StartBlock";
    public const string EndBlock   = "EndBlock";
    public const string IfBlock    = "IfBlock";
    public const string ElseBlock  = "ElseBlock";

    public const string FunctionDefinition = "FunctionDefinition";
    public const string FunctionName       = "FunctionName";
    public const string FunctionCall       = "FunctionCall";

    public const string FileHeader      = "FileHeader";
    public const string FileHeaderKey   = "FileHeaderKey";
    public const string FileHeaderValue = "FileHeaderValue";

    public const string FileHeaderSetting = "FileHeaderSetting";
    // ReSharper restore MemberCanBePrivate.Global
}