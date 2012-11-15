using System;
using System.Text.RegularExpressions;
using Phantom;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Parsers.Terminals;

namespace SampleGrammars
{
	/// <summary>
	/// Builds a parser for old-fashioned Pascal files, using the Phantom parser system.
	/// Built from the 1979 Apple Pascal poster.
	/// </summary>
	public class PascalParser {
		readonly IParser root;

		public IParser TheParser { get { return root; } }

		public PascalParser() {
			root = Pascal();
		}

		protected RegexOptions ops() {
			return RegexOptions.ExplicitCapture
				| RegexOptions.IgnoreCase
				| RegexOptions.Compiled
				| RegexOptions.Multiline;
		}
		
		//  http://pascal-central.com/images/pascalposter.jpg
		IParser Pascal()
		{
			// The BNF syntax isn't really up to this -- the precedence rules are all wrong!
			// makes for a really fragile parser.

			var _empty_ = new BNF(new EmptyMatch());

			var _type = new Recursion();
			var _fieldList = new Recursion();
			var _block = new Recursion();
			var _statement = new Recursion();
			var _variable = new Recursion();
			var _expression = new Recursion();
			var _factor = new Recursion();

			BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";
			BNF pascalString = "#'([^']|'')*'"; // Pascal uses two single-quotes to mark a single quote.
			BNF plusOrMinus = "#[\\-+]";
			
			BNF set = "set";
			BNF of = "of";
			BNF array = "array";
			BNF record = "record";
			BNF not = "not";
			BNF and = "and";
			BNF div = "div";
			BNF mod = "mod";
			BNF end = "end";
			BNF to = "to";
			BNF downto = "downto";
			BNF begin = "begin";
			BNF file = "file";
			BNF exit = "exit";
			BNF VAR = "var";
			BNF Else = "else";

			BNF identifierList = identifier % ',';
			BNF parameters = '(' > identifierList > ')';
			BNF unsignedInteger = "#\\d+";
			BNF label = "label" > ( unsignedInteger % ',' ) > ';';

			BNF unsignedNumber = unsignedInteger > !("." > unsignedInteger) > !('e' > (!plusOrMinus) > unsignedInteger);
			BNF unsignedConstant = pascalString | "nil" | unsignedNumber | identifier;
			BNF constant = (unsignedConstant) | ( plusOrMinus > (identifier | unsignedNumber));
			BNF constantBlock = "const" > +(identifier > '=' > constant > ';');

			BNF arrayRange = '[' > !(( _expression > !((BNF)".." > _expression)) % ',' ) > ']';
			BNF factor = 
				  (unsignedConstant)
				| (_variable)
				| (identifier > !('(' > ((BNF)_expression % ',') > ')'))
				| ((BNF)'(' > _expression > ')')
				| (not > _factor)
				| arrayRange;
			BNF term = factor % (and | div | mod | "/" | "*");

			BNF inequality = ((BNF)"<" | "<=" | "=" | "<>" | ">=" | ">" | "in");
			BNF simpleExpression = !(plusOrMinus) > ((term % "or") % plusOrMinus);
			BNF expression = simpleExpression > !(inequality > simpleExpression);


			BNF innerVariable = ('[' > (expression % ',') > ']') | ('.' > identifier);
			BNF variable = identifier > (innerVariable | ('^' > innerVariable));

			BNF ifBlock = "if" > expression > "then" > _statement > !(Else > _statement);
			BNF forBlock = "for" > identifier > ":=" > expression > (to | downto) > expression > "do" > _statement;
			BNF caseBlock = "case" > expression > "of" > (( (constant % ',') > ':' > _statement ) % ';') > end;
			BNF statement = 
				 !(unsignedInteger > ':')
				| _empty_
				| (identifier > !( '(' > (expression % ',') > ')'))
				| (variable > ":=" > expression)
				| ifBlock
				| ("repeat" > ((BNF)_statement % ';') > "until" > expression)
				| ("while" > expression > "do" > _statement)
				| forBlock
				| caseBlock
				| ("with" > (variable % ',') > "do" > _statement)
				| ("goto" > unsignedInteger)
				| (exit > '(' > (identifier | "program") > ')')
				| ("begin" > ((BNF)_statement % ';') > "end") ;

			BNF statementBlock = begin > (statement % ';') > end;

			BNF constantFieldList = (constant % ',') > ':' > '(' > _fieldList > ')';
			BNF caseStatement = "case" > !(identifier > ':') > identifier > of > (constantFieldList < ';');
			BNF fieldList = caseStatement | ( -(identifierList > ':' > _type) % ';');

			BNF simpleType = identifier | parameters | (constant > ".." > constant);
			BNF complexType = 
				  (set > of > simpleType)
				| (array > '[' > (simpleType % ',') > ']' > of > _type)
				| (record > fieldList > end)
				| (file > !(of > _type));
			BNF type = simpleType | ('^'>identifier) | (!((BNF)"packed") > complexType);


			BNF singleParameter = (!VAR) > identifierList > ':' > identifier;
			BNF parameterList = !('(' > (singleParameter % ';') > ')');
			BNF procedure = "procedure" > identifier > parameterList > ';' > _block > ';';
			BNF function = "function" > identifier > parameterList > ':' > identifier > ';'> _block > ';';

			BNF varBlock = "var" > +(identifierList > ':' > _type > ';');

			BNF typeBlock = "type" > +(identifier > '=' > type > ';');
			BNF block = !(label | constantBlock | typeBlock | varBlock | procedure | function) > statementBlock;
			BNF program = "program" > identifier > (!parameters) > ';' > block > '.';



			_type.Source = type.Result();
			_fieldList.Source = fieldList.Result();
			_block.Source = block.Result();
			_statement.Source = statement.Result();
			_variable.Source = variable.Result();
			_expression.Source = expression.Result();
			_factor.Source = expression.Result();

			return program.Result();
		}
	}

	class Consoler : Parser, IMatchingParser
	{
		readonly IMatchingParser src;

		public Consoler(IMatchingParser src)
		{
			this.src = src;
		}

		public ParserMatch TryMatch(IScanner scan)
		{
			Console.WriteLine(scan.RemainingData());
			return src.TryMatch(scan);
		}
	}
}
