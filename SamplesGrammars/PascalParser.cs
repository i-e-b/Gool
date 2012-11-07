using System.Text.RegularExpressions;
using Phantom.Parsers;

namespace SampleGrammars
{
	/// <summary>
	/// Builds a parser for old-fashioned Pascal files, using the Phantom parser system.
	/// Built from the 1979 Apple Pascal poster.
	/// </summary>
	public class PascalParser {
		protected Parser root;

		public Parser TheParser { get { return root; } }

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
			var plus_minus = ((Parser)"[\\-+]")[ops()];
			var ident = ((Parser)"[_a-zA-Z]\\w*")[ops()];
			var u_int = ((Parser)"\\d+")[ops()];
			var u_num = u_int > !("." > u_int) > !('e' > (!plus_minus) > u_int);
			var pstr = ((Parser)"'([^']|'')*'")[ops()]; // Pascal uses two single-quotes to mark a single quote.
			var u_const = u_num | ident | @"#nil\s" | pstr;
			var p_const = u_const | (plus_minus > (ident | u_num));
			var simple_type = (ident | (p_const > ".." > p_const) | ('(' > (ident % ',') > ')'));

			#region types
			// Mutual dependence requires a holding parser (so the object reference stays the same);
			var field_list_h = new HoldingParser();
			var p_type_h = new HoldingParser();
			var pt_setof = (@"#set\s" > ((Parser)@"#of\s") > simple_type);
			var pt_array = (@"#array\s" > ((Parser)'[') > (simple_type % ',') > ']' > @"#of\s" > p_type_h);
			var pt_recrd = (@"#record\s" > field_list_h > @"#end");
			var pt_files = (@"#file\s" > (!(@"#of\s" > p_type_h)));
			var p_type = (simple_type | ('^' > ident) | (pt_setof | pt_array | pt_recrd | pt_files) );
			p_type_h.HeldParser = p_type; // complete self-reference
			var field_list = (!(((ident%',')>':'>p_type)<';'))
				> (!(@"#case\s" > !(ident % ':') > ident > @"#of\s" 
				> (((p_const % ',') > ':' > '(' > field_list_h > ')') % ';')));
			field_list_h.HeldParser = field_list; // complete mutual dependance
			#endregion

			#region expressions
			var factor_h = new HoldingParser();
			var compares = ((Parser)"<" | "<=" | "=" | "<>" | ">=" | ">" | @"#in\s");
			var operators = ((Parser)'*' | '/' | @"#div\s" | @"#mod\s" | @"#in\s");
			var p_term = (factor_h % operators);
			var simp_expr = -(!(plus_minus) > (p_term % @"#or\s"));
			var p_expression = simp_expr > !(compares > simp_expr);
			var p_variable = ident > !(-(('[' > (p_expression % '.') > ']')
										 | ('.' > ident)
										 | '^'));

			var fact_array = Atom.Wrap('[' > -( ((p_expression%',') > !("..">p_expression))%',' ) > ']');
			var factor = (u_const | p_variable | (ident > !('(' > (p_expression % ',') > ')'))
			| ('('>p_expression>')') | (@"#not\s">factor_h) | fact_array );
			factor_h.HeldParser = factor;
			#endregion

			return (factor);

		}
	}
}
