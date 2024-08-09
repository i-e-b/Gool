using System.Text.RegularExpressions;
using Phantom;
// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Builds a parser for old-fashioned Pascal files, using the Phantom parser system.
/// Built from the 1979 Apple Pascal poster.
/// </summary>
public class PascalParser
{
    public IParser TheParser { get; }

    public PascalParser()
    {
        TheParser = Pascal();
    }

    private static RegexOptions Options()
    {
        return RegexOptions.ExplicitCapture
               | RegexOptions.IgnoreCase
               | RegexOptions.Multiline;
    }


    // https://archive.org/details/pascal-poster-v-3-a-1
    private IParser Pascal()
    {
        BNF.RegexOptions = Options();

        var _empty_ = BNF.Empty();

        // Forward references
        var _type = BNF.Forward();
        var _fieldList = BNF.Forward();
        var _block = BNF.Forward();
        var _statement = BNF.Forward();
        var _variable = BNF.Forward();
        var _expression = BNF.Forward();
        var _factor = BNF.Forward();

        // Basic literals
        BNF unsignedInteger = @"#\d+";
        BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";
        BNF pascalString = "#'([^']|'')*'"; // Pascal uses two single-quotes to mark a single quote.
        BNF plusOrMinus = BNF.OneOf('-', '+');//"#[\\-+]");

        // Keywords
        BNF k_program = "program";
        BNF k_if = "if";
        BNF k_set = "set";
        BNF k_of = "of";
        BNF k_array = "array";
        BNF k_record = "record";
        BNF k_end = "end";
        BNF k_to = "to";
        BNF k_down_to = "downto";
        BNF k_begin = "begin";
        BNF k_file = "file";
        BNF k_exit = "exit";
        BNF k_var = "var";
        BNF k_else = "else";
        BNF k_label = "label";
        BNF k_const = "const";
        BNF k_type = "type";
        BNF k_packed = "packed";
        BNF k_nil = "nil";
        BNF k_function = "function";
        BNF k_do = "do";
        BNF k_then = "then";
        BNF k_for = "for";
        BNF k_case = "case";
        BNF k_until = "until";
        BNF k_procedure = "procedure";
        BNF k_repeat = "repeat";
        BNF k_while = "while";
        BNF k_with = "with";
        BNF k_goto = "goto";
        
        // Operators
        BNF o_comma = ',';
        BNF o_dot = '.';
        BNF o_colon = ':';
        BNF o_equal = '=';
        BNF o_slash = '/';
        BNF o_mul = '*';
        BNF o_ref = '^';
        BNF o_range = "..";
        BNF o_or = "or";
        BNF o_not = "not";
        BNF o_and = "and";
        BNF o_div = "div";
        BNF o_mod = "mod";
        BNF o_set = ":=";
        
        // Scope markers
        BNF s_open_paren = '(';
        BNF s_close_paren = ')';
        BNF s_open_bracket = '[';
        BNF s_close_bracket = ']';
        
        // Terminators / Separators
        BNF t_semi = ';';

        // Structures
        BNF identifierList = identifier % o_comma;
        BNF parameters = s_open_paren > identifierList > s_close_paren;
        BNF label = k_label > (unsignedInteger % o_comma) > t_semi;

        BNF unsignedNumber = unsignedInteger > !('.' > unsignedInteger) > !('e' > (!plusOrMinus) > unsignedInteger); // note: we don't use '.' operator here
        BNF unsignedConstant = pascalString | k_nil | unsignedNumber | identifier;
        BNF constant = (unsignedConstant) | (plusOrMinus > (identifier | unsignedNumber));
        BNF constantBlock = k_const > +(identifier > o_equal > constant > t_semi);

        BNF arrayRange = s_open_bracket > !((_expression > !(o_range > _expression)) % o_comma) > s_close_bracket;
        BNF factor =
              unsignedConstant
            | _variable
            | (identifier > !(s_open_paren > (_expression % o_comma) > s_close_paren))
            | (s_open_paren > _expression > s_close_paren)
            | (o_not > _factor)
            | arrayRange;
        BNF term = factor % (o_and | o_div | o_mod | o_slash | o_mul);

        BNF inequality = ((BNF)"<" | "<=" | "=" | "<>" | ">=" | ">" | "in");
        BNF simpleExpression = !(plusOrMinus) > ((term % o_or) % plusOrMinus);
        BNF expression = simpleExpression > !(inequality > simpleExpression);


        BNF innerVariable = (s_open_bracket > (expression % o_comma) > s_close_bracket) | (o_dot > identifier);
        BNF variable = identifier > (innerVariable | (o_ref > innerVariable));

        BNF ifBlock = k_if > expression > k_then > _statement > !(k_else > _statement);
        BNF forBlock = k_for > identifier > o_set > expression > (k_to | k_down_to) > expression > k_do > _statement;
        BNF caseBlock = k_case > expression > k_of > (((constant % o_comma) > o_colon > _statement) % t_semi) > k_end;
        BNF statement =
            !(unsignedInteger > o_colon)
            | _empty_
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
            | (k_exit > s_open_paren > (identifier | k_program) > s_close_paren);

        BNF statementBlock = k_begin > (statement % t_semi) > k_end;

        BNF constantFieldList = (constant % o_comma) > o_colon > s_open_paren > _fieldList > s_close_paren;
        BNF caseStatement = k_case > !(identifier > o_colon) > identifier > k_of > (constantFieldList < t_semi);
        BNF fieldList = caseStatement | (-(identifierList > o_colon > _type) % t_semi);

        BNF simpleType = identifier | parameters | (constant > o_range > constant);
        BNF complexType =
            (k_set > k_of > simpleType)
            | (k_array > s_open_bracket > (simpleType % o_comma) > s_close_bracket > k_of > _type)
            | (k_record > fieldList > k_end)
            | (k_file > !(k_of > _type));
        BNF type = simpleType | (o_ref > identifier) | ((!k_packed) > complexType);


        BNF singleParameter = (!k_var) > identifierList > o_colon > identifier;
        BNF parameterList = !(s_open_paren > (singleParameter % t_semi) > s_close_paren);
        BNF procedure = k_procedure > identifier > parameterList > t_semi > _block > t_semi;
        BNF function = k_function > identifier > parameterList > o_colon > identifier > t_semi > _block > t_semi;

        BNF varBlock = k_var > +(identifierList > o_colon > type > t_semi);

        BNF typeBlock = k_type > +(identifier > o_equal > type > t_semi);
        BNF block = !(label | constantBlock | typeBlock | varBlock | procedure | function) > statementBlock;
        BNF program = k_program > identifier > (!parameters) > t_semi > block > o_dot;


        // Link up forward references
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

        t_semi.Tag(StatementEnd);
        identifier.Tag(Identifier);
        pascalString.Tag(PascalString);
        expression.Tag(Expression);
        constant.Tag(Constant);
        inequality.Tag(Inequality);

        // Scopes
        k_begin.OpenScope();
        k_end.CloseScope();
        s_open_paren.Tag(OpenParen).OpenScope();
        s_close_paren.Tag(CloseParen).CloseScope();
        s_open_bracket.Tag(OpenBracket).OpenScope();
        s_close_bracket.Tag(CloseBracket).CloseScope();

        return program.Result();
    }

    // ReSharper disable MemberCanBePrivate.Global
    public const string Identifier = "identifier";
    public const string PascalString = "string";
    public const string Expression = "expression";
    public const string Constant = "constant";
    public const string Keyword = "keyword";
    public const string Operator = "operator";
    public const string Inequality = "inequality";
    public const string OpenParen = "(";
    public const string CloseParen = ")";
    public const string OpenBracket = "[";
    public const string CloseBracket = "]";
    public const string StatementEnd = ";";
    // ReSharper restore MemberCanBePrivate.Global
}