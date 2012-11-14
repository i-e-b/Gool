using System.Text.RegularExpressions;
using Phantom;
using Phantom.Parsers;
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

		IParser Pascal()
		{
			var _type = new Recursion();
			var _fieldList = new Recursion();
			var _block = new Recursion();


			BNF identifier = "#[_a-zA-Z]\\w*";
			BNF pascalString = "#'([^']|'')*'"; // Pascal uses two single-quotes to mark a single quote.
			BNF plusOrMinus = "#[\\-+]";
			
			BNF set = @"set";
			BNF of = @"of";
			BNF array = @"array";
			BNF record = @"record";
			BNF not = @"not";
			BNF end = @"end";
			BNF begin = @"end";
			BNF file = @"file";
			BNF VAR = @"var";
			BNF _empty_ = new BNF(new EmptyMatch());

			BNF identifierList = identifier < ',';
			BNF parameters = '(' > identifierList > ')';
			BNF unsignedInteger = "#\\d+";
			BNF label = "label" > ( unsignedInteger < ',' ) > ';';


			BNF statement = 
				  _empty_
				| ...

			BNF statementBlock = begin > (statement < ';') > end;
			BNF unsignedNumber = unsignedInteger > !("." > unsignedInteger) > !('e' > (!plusOrMinus) > unsignedInteger);
			BNF unsignedConstant = pascalString | "nil" | unsignedNumber | identifier;
			BNF constant = (unsignedConstant) | ( plusOrMinus > (identifier | unsignedNumber));
			BNF constantBlock = "const" > +(identifier > '=' > constant > ';');


			BNF constantFieldList = (constant < ',') > ':' > '(' > _fieldList > ')';
			BNF caseStatement = "case" > !(identifier > ':') > identifier > of > (constantFieldList < ';');
			BNF fieldList = caseStatement | ( -(identifierList > ':' > _type) < ';');

			BNF simpleType = identifier | parameters | (constant > ".." > constant);
			BNF complexType = 
				  (set > of > simpleType)
				| (array > '[' > (simpleType < ',') > ']' > of > _type)
				| (record > fieldList > end)
				| (file > !(of > _type));


			BNF singleParameter = !VAR > identifierList > ':' > identifier;
			BNF parameterList = !('(' > (singleParameter < ';') > ')');
			BNF procedure = "procedure" > identifier > parameterList > ';' > _block > ';';
			BNF function = "function" > identifier > parameterList > ':' > identifier > ';'> _block > ';';

			BNF varBlock = "var" > +(identifierList > ':' > _type > ';');

			BNF typeBlock = "type" > +(identifier > '=' > _type > ';');
			BNF block = !(label | constantBlock | typeBlock | varBlock | procedure | function) > statementBlock;
			BNF program = "program" > identifier > (!parameters) > ';' > block > '.';


			BNF type = simpleType | ('^'>identifier) | (!((BNF)"packed") > complexType);

			_type.Source = type.Result();
			_fieldList.Source = fieldList.Result();
			_block.Source = block.Result();

			return program.Result();
		}


		protected Parser OldPascal() {
			BNF.RegexOptions = ops();


			BNF plus_minus = "#[\\-+]";
			BNF ident = "#[_a-zA-Z]\\w*";
			BNF u_int = "#\\d+";
			BNF u_num = u_int > !("." > u_int) > !('e' > (!plus_minus) > u_int);
			BNF pstr = "#'([^']|'')*'"; // Pascal uses two single-quotes to mark a single quote.
			BNF u_const = u_num | ident | @"#nil\s" | pstr;
			BNF p_const = u_const | (plus_minus > (ident | u_num));
			BNF simple_type = (ident | (p_const > ".." > p_const) | ('(' > (ident % ',') > ')'));

			BNF set = @"#set\s";
			BNF of = @"#of\s";
			BNF array = @"#array\s";
			BNF record = @"#record\s";
			BNF not = @"#not\s";

			#region types
			// Mutual dependence requires a holding parser (so the object reference stays the same);
			var field_list_h = new Recursion();
			var p_type_h = new Recursion();

			BNF pt_setof = (set > of > simple_type);
			BNF pt_array = (array > '[' > (simple_type % ',') > ']' > of > p_type_h);

			BNF pt_recrd = (record > field_list_h > @"#end");
			BNF pt_files = (@"#file\s" > (!(of > p_type_h)));
			BNF p_type = (simple_type | ('^' > ident) | (pt_setof | pt_array | pt_recrd | pt_files) );


			p_type_h.Source = p_type.Result(); // complete self-reference


			var field_list = (!(((ident%',')>':'>p_type)<';'))
				> (!(@"#case\s" > !(ident % ':') > ident > @"#of\s" 
				> (((p_const % ',') > ':' > '(' > field_list_h > ')') % ';')));


			field_list_h.Source = field_list.Result(); // complete mutual dependance
			#endregion

			#region expressions
			var factor_h = new Recursion();

			var compares = ((BNF)"<" | "<=" | "=" | "<>" | ">=" | ">" | @"#in\s");
			var operators = ((BNF)'*' | '/' | @"#div\s" | @"#mod\s" | @"#in\s");
			var p_term = (factor_h % operators);
			var simp_expr = -(!(plus_minus) > (p_term % @"#or\s"));
			var p_expression = simp_expr > !(compares > simp_expr);
			var p_variable = ident > !(-(('[' > (p_expression % '.') > ']')
										 | ('.' > ident)
										 | '^'));

			var fact_array = '[' > -( ((p_expression%',') > !("..">p_expression))%',' ) > ']';
			var factor = (u_const | p_variable | (ident > !('(' > (p_expression % ',') > ')'))
			| ('('>p_expression>')') | (not > factor_h) | fact_array );

			factor_h.Source = factor.Result();
			#endregion

			return factor_h;
		}
	}
}
