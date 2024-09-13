using System.Text.RegularExpressions;
using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Builds a parser for old-fashioned Pascal files, using the Gool parser system.
/// Built from the 1979 Apple Pascal poster.
/// </summary>
public static class PascalExample
{
    public static readonly BNF.Package Parser = Pascal();

    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }


    // https://archive.org/details/pascal-poster-v-3-a-1
    private static BNF.Package Pascal()
    {
        BNF.RegexOptions = Options();

        // Forward references
        var _type       = BNF.Forward();
        var _fieldList  = BNF.Forward();
        var _block      = BNF.Forward();
        var _statement  = BNF.Forward();
        var _variable   = BNF.Forward();
        var _expression = BNF.Forward();
        var _factor     = BNF.Forward();

        BNF // Basic literals
            unsignedInteger = BNF.Regex(@"\d+"),
            identifier      = BNF.Regex("[_a-zA-Z][_a-zA-Z0-9]*"),
            pascalString    = BNF.Regex("'([^']|'')*'"), // Pascal uses two single-quotes to mark a single quote.
            plusOrMinus     = BNF.OneOf('-', '+');

        BNF // Keywords
            k_program   = "program",
            k_if        = "if",
            k_set       = "set",
            k_of        = "of",
            k_array     = "array",
            k_record    = "record",
            k_end       = "end",
            k_to        = "to",
            k_down_to   = "downto",
            k_begin     = "begin",
            k_file      = "file",
            k_exit      = "exit",
            k_var       = "var",
            k_else      = "else",
            k_label     = "label",
            k_const     = "const",
            k_type      = "type",
            k_packed    = "packed",
            k_nil       = "nil",
            k_function  = "function",
            k_do        = "do",
            k_then      = "then",
            k_for       = "for",
            k_case      = "case",
            k_until     = "until",
            k_procedure = "procedure",
            k_repeat    = "repeat",
            k_while     = "while",
            k_with      = "with",
            k_goto      = "goto";

        BNF // Operators
            o_comma = ',',
            o_dot   = '.',
            o_colon = ':',
            o_equal = '=',
            o_slash = '/',
            o_mul   = '*',
            o_ref   = '^',
            o_range = "..",
            o_or    = "or",
            o_not   = "not",
            o_and   = "and",
            o_div   = "div",
            o_mod   = "mod",
            o_set   = ":=";

        BNF // Scope markers
            s_open_paren    = '(',
            s_close_paren   = ')',
            s_open_bracket  = '[',
            s_close_bracket = ']';
        
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
              | (k_exit > s_open_paren > (identifier | k_program) > s_close_paren),
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
        BNF.TagAll(Keyword, 
            k_program, k_set, k_of, k_array, k_record, k_end, k_to, k_down_to, k_begin, k_file,
            k_exit, k_var, k_else, k_label, k_const, k_type, k_packed, k_nil, k_function, k_do,
            k_then, k_for, k_case, k_until, k_procedure, k_repeat, k_while, k_with, k_goto);

        BNF.TagAll(Operator, o_comma, o_dot, o_colon, o_equal, o_slash, o_mul, o_ref, o_range,
            o_or, o_not, o_and, o_div, o_mod, o_set);

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

        return program.WithOptions(BNF.Options.IgnoreCase | BNF.Options.SkipWhitespace | BNF.Options.IncludeSkippedElements);
    }

    // ReSharper disable MemberCanBePrivate.Global
    public const string Identifier = "identifier";
    public const string PascalString = "string";
    public const string Expression = "expression";
    public const string Constant = "constant";
    public const string Keyword = "keyword";
    public const string Operator = "operator";
    public const string Inequality = "inequality";
    public const string OpenParen = "openParen";
    public const string CloseParen = "closeParen";
    public const string OpenBracket = "openBracket";
    public const string CloseBracket = "closeBracket";
    public const string StatementEnd = "statement";
    // ReSharper restore MemberCanBePrivate.Global
}