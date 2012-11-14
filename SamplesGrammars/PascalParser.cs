using System.Text.RegularExpressions;
using Phantom;
using Phantom.Parsers;

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

		protected Parser Pascal() {
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
			var field_list_h = new HoldingParser();
			var p_type_h = new HoldingParser();

			BNF pt_setof = (set > of > simple_type);
			BNF pt_array = (array > '[' > (simple_type % ',') > ']' > of > p_type_h);

			BNF pt_recrd = (record > field_list_h > @"#end");
			BNF pt_files = (@"#file\s" > (!(of > p_type_h)));
			BNF p_type = (simple_type | ('^' > ident) | (pt_setof | pt_array | pt_recrd | pt_files) );
			p_type_h.HeldParser = p_type.Result(); // complete self-reference


			var field_list = (!(((ident%',')>':'>p_type)<';'))
				> (!(@"#case\s" > !(ident % ':') > ident > @"#of\s" 
				> (((p_const % ',') > ':' > '(' > field_list_h > ')') % ';')));
			field_list_h.HeldParser = field_list.Result(); // complete mutual dependance
			#endregion

			#region expressions
			var factor_h = new HoldingParser();

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

			factor_h.HeldParser = factor.Result();
			#endregion

			return factor_h;
		}
	}
}
