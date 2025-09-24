using System;
using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Builds a parser for old-fashioned Pascal files, using the Gool parser system.
/// Built from the 1979 Apple Pascal poster.
/// </summary>
public static class PascalExample
{
    public static readonly ParserPackage Parser = Pascal();

    // https://archive.org/details/pascal-poster-v-3-a-1
    private static ParserPackage Pascal()
    {
        // Forward references
        var _type       = BNF.Forward();
        var _fieldList  = BNF.Forward();
        var _block      = BNF.Forward();
        var _statement  = BNF.Forward();
        var _variable   = BNF.Forward();
        var _expression = BNF.Forward();
        var _factor     = BNF.Forward();

        BNF // Basic literals
            unsignedInteger     = BNF.IntegerRange(0, long.MaxValue),
            identifier          = BNF.IdentifierString(),
            pascalString        = "'" > -((BNF.AnyChar / "'") | ("''")) > "'",
            comment             = '{' > -(BNF.AnyChar / '}') > '}',
            whiteSpaceOrComment = +(BNF.WhiteSpaceString | comment),
            compilerDirective   = '#' > identifier > -(BNF.AnyChar / ';'),
            plusOrMinus         = BNF.OneOf('-', '+');

        pascalString.NoAutoAdvance(); // Don't skip whitespace or comments in here
        pascalString.Atomic(); // compact all sub-matches into a single result
        comment.Atomic();

        BNF // Keywords
            k_program   = "program".Keyword(),
            k_if        = "if".Keyword(),
            k_set       = "set".Keyword(),
            k_of        = "of".Keyword(),
            k_array     = "array".Keyword(),
            k_record    = "record".Keyword(),
            k_end       = "end".Keyword(),
            k_to        = "to".Keyword(),
            k_down_to   = "downto".Keyword(),
            k_begin     = "begin".Keyword(),
            k_file      = "file".Keyword(),
            k_exit      = "exit".Keyword(),
            k_var       = "var".Keyword(),
            k_else      = "else".Keyword(),
            k_label     = "label".Keyword(),
            k_const     = "const".Keyword(),
            k_type      = "type".Keyword(),
            k_packed    = "packed".Keyword(),
            k_nil       = "nil".Keyword(),
            k_function  = "function".Keyword(),
            k_do        = "do".Keyword(),
            k_then      = "then".Keyword(),
            k_for       = "for".Keyword(),
            k_case      = "case".Keyword(),
            k_until     = "until".Keyword(),
            k_procedure = "procedure".Keyword(),
            k_repeat    = "repeat".Keyword(),
            k_while     = "while".Keyword(),
            k_with      = "with".Keyword(),
            k_goto      = "goto".Keyword();

        BNF // Operators
            o_comma = ','.Keyword(),
            o_dot   = '.'.Keyword(),
            o_colon = ':'.Keyword(),
            o_equal = '='.Keyword(),
            o_slash = '/'.Keyword(),
            o_mul   = '*'.Keyword(),
            o_ref   = '^'.Keyword(),
            o_range = "..".Keyword(),
            o_or    = "or".Keyword(),
            o_not   = "not".Keyword(),
            o_and   = "and".Keyword(),
            o_div   = "div".Keyword(),
            o_mod   = "mod".Keyword(),
            o_set   = ":=".Keyword();

        BNF // Scope markers
            s_open_paren    = '('.Keyword(),
            s_close_paren   = ')'.Keyword(),
            s_open_bracket  = '['.Keyword(),
            s_close_bracket = ']'.Keyword();

        BNF // Terminators / Separators
            t_semi = ';';

        BNF // Structures
            identifierList = identifier % o_comma,
            parameters     = s_open_paren > identifierList > s_close_paren,
            label          = k_label > (unsignedInteger % o_comma) > t_semi;

        BNF // Constants
            unsignedNumber   = unsignedInteger > !('.' > unsignedInteger) > !('e' > (!plusOrMinus) > unsignedInteger), // note: we don't use '.' operator here
            unsignedConstant = pascalString | k_nil | unsignedNumber | identifier,
            constant         = (unsignedConstant) | (plusOrMinus > (identifier | unsignedNumber)),
            constantBlock    = k_const > +(identifier > o_equal > constant > t_semi);

        BNF // Expressions
            arrayRange = s_open_bracket > !((_expression > !(o_range > _expression)) % o_comma) > s_close_bracket,
            factor =
                unsignedConstant
              | _variable
              | (identifier > !(s_open_paren > (_expression % o_comma) > s_close_paren))
              | (s_open_paren > _expression > s_close_paren)
              | (o_not > _factor)
              | arrayRange,
            term             = factor % (o_and | o_div | o_mod | o_slash | o_mul),
            inequality       = ((BNF)"<" | "<=" | "=" | "<>" | ">=" | ">" | "in"),
            simpleExpression = !(plusOrMinus) > ((term % o_or) % plusOrMinus),
            expression       = simpleExpression > !(inequality > simpleExpression);

        BNF // Variables
            innerVariable = (s_open_bracket > (expression % o_comma) > s_close_bracket) | (o_dot > identifier),
            variable      = identifier > (innerVariable | (o_ref > innerVariable));

        BNF // Statements and blocks
            ifBlock   = k_if > expression > k_then > _statement > !(k_else > _statement),
            forBlock  = k_for > identifier > o_set > expression > (k_to | k_down_to) > expression > k_do > _statement,
            caseBlock = k_case > expression > k_of > (((constant % o_comma) > o_colon > _statement) % t_semi) > k_end,
            statement =
                !(unsignedInteger > o_colon)
              | (k_begin > (_statement % t_semi) > k_end)
              | (identifier > !(s_open_paren > (expression % o_comma) > s_close_paren))
              | (variable > o_set > expression)
              | ifBlock
              | (k_repeat > (_statement % t_semi) > k_until > expression)
              | (k_while > expression > k_do > _statement)
              | forBlock
              | caseBlock
              | (k_with > (variable % o_comma) > k_do > _statement)
              | (k_goto > unsignedInteger)
              | (k_exit > s_open_paren > (identifier | k_program) > s_close_paren)
              | compilerDirective,
            statementBlock = k_begin > (statement % t_semi) > k_end;

        BNF // Case and field list
            constantFieldList = (constant % o_comma) > o_colon > s_open_paren > _fieldList > s_close_paren,
            caseStatement     = k_case > !(identifier > o_colon) > identifier > k_of > (constantFieldList < t_semi),
            fieldList         = caseStatement | (-(identifierList > o_colon > _type) % t_semi);

        BNF // Types
            simpleType = identifier | parameters | (constant > o_range > constant),
            complexType =
                (k_set > k_of > simpleType)
              | (k_array > s_open_bracket > (simpleType % o_comma) > s_close_bracket > k_of > _type)
              | (k_record > fieldList > k_end)
              | (k_file > !(k_of > _type)),
            type = simpleType | (o_ref > identifier) | ((!k_packed) > complexType);

        BNF // Subroutine definition
            singleParameter = (!k_var) > identifierList > o_colon > identifier,
            parameterList   = !(s_open_paren > (singleParameter % t_semi) > s_close_paren),
            procedure       = k_procedure > identifier > parameterList > t_semi > _block > t_semi,
            function        = k_function > identifier > parameterList > o_colon > identifier > t_semi > _block > t_semi;

        BNF // Main blocks
            varBlock  = k_var > +(identifierList > o_colon > type > t_semi),
            typeBlock = k_type > +(identifier > o_equal > type > t_semi),
            block     = !(label | constantBlock | typeBlock | varBlock | procedure | function) > statementBlock,
            program   = k_program > identifier > (!parameters) > t_semi > block > o_dot;


        // Link forward references
        _type.Is(type);
        _fieldList.Is(fieldList);
        _block.Is(block);
        _statement.Is(statement);
        _variable.Is(variable);
        _expression.Is(expression);
        _factor.Is(expression);

        // Apply tags
        t_semi.TagWith(StatementEnd);
        identifier.TagWith(Identifier);
        pascalString.TagWith(PascalString);
        expression.TagWith(Expression);
        constant.TagWith(Constant);
        inequality.TagWith(Inequality);

        // Scopes
        k_begin.OpenScope();
        k_end.CloseScope();
        s_open_paren.TagWith(OpenParen).OpenScope();
        s_close_paren.TagWith(CloseParen).CloseScope();
        s_open_bracket.TagWith(OpenBracket).OpenScope();
        s_close_bracket.TagWith(CloseBracket).CloseScope();

        // Actions
        compilerDirective.MatchAction(match => throw new Exception("Hit the compiler directive: "+match));

        return program.BuildWithOptions(BNF.Options.IgnoreCase | BNF.Options.IncludeSkippedElements, autoAdvance: whiteSpaceOrComment);
    }

    // ReSharper disable MemberCanBePrivate.Global
    public const string Identifier   = "identifier";
    public const string PascalString = "string";
    public const string Expression   = "expression";
    public const string Constant     = "constant";
    public const string Inequality   = "inequality";
    public const string OpenParen    = "openParen";
    public const string CloseParen   = "closeParen";
    public const string OpenBracket  = "openBracket";
    public const string CloseBracket = "closeBracket";

    public const string StatementEnd = "statement";
    // ReSharper restore MemberCanBePrivate.Global
}