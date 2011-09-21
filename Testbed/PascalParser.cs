using System;
using System.Collections.Generic;
using System.Text;
using Phantom;
using Phantom.Scanners;
using Phantom.Parsers;
using System.Text.RegularExpressions;

namespace Testbed {

	/// <summary>
	/// Builds a parser for old-fashioned Pascal files, using the Phantom parser system.
	/// Built from the 1979 Apple Pascal poster.
	/// </summary>
	class PascalParser {
		protected Parser root;
		protected List<string> langStack;

		public Parser TheParser { get { return root; } }
		public List<string> ResultList { get { return langStack; } }

		public PascalParser() {
			langStack = new List<string>();
			root = Pascal();
		}

		public void act (Object sender, SemanticActionArgs args) {
			langStack.Add(args.Value);
		}

		protected RegexOptions ops() {
			return RegexOptions.ExplicitCapture
				| RegexOptions.IgnoreCase
				| RegexOptions.Compiled
				| RegexOptions.Multiline;
		}

		protected Parser Pascal() {
			Parser plus_minus = ((Parser)"[\\-+]")[ops()];
			Parser ident = ((Parser)"[_a-zA-Z]\\w*")[ops()];
			Parser u_int = ((Parser)"\\d+")[ops()];
			Parser u_num = u_int > !("." > u_int) > !('e' > (!plus_minus) > u_int);
			Parser pstr = ((Parser)"'([^']|'')*'")[ops()]; // Pascal uses two single-quotes to mark a single quote.
			Parser u_const = u_num | ident | @"#nil\s" | pstr;
			Parser p_const = u_const | (plus_minus > (ident | u_num));
			Parser para_item = !((Parser)@"#var\s") > (ident % ",") > ':' > ident;
			Parser para_list = !('(' > (para_item % ';') > ')');
			Parser simple_type = (ident | (p_const > ".." > p_const) | ('(' > (ident % ',') > ')'));

			#region types
			// Mutual dependence requires a holding parser (so the object reference stays the same);
			HoldingParser field_list_h = new HoldingParser();
			HoldingParser p_type_h = new HoldingParser();
			Parser pt_setof = (@"#set\s" > ((Parser)@"#of\s") > simple_type);
			Parser pt_array = (@"#array\s" > ((Parser)'[') > (simple_type % ',') > ']' > ((Parser)@"#of\s") > p_type_h);
			Parser pt_recrd = (@"#record\s" > field_list_h > @"#end");
			Parser pt_files = (@"#file\s" > (!(@"#of\s" > p_type_h)));
			Parser p_type = (simple_type | ('^' > ident) | (pt_setof | pt_array | pt_recrd | pt_files) );
			//Parser p_type = (pt_setof | simple_type | ('^' > ident) );
			p_type_h.HeldParser = p_type; // complete self-reference
			Parser field_list = (!(((ident%',')>':'>p_type)<';'))
				> (!(@"#case\s" > !(ident % ':') > ident > @"#of\s" 
				> (((p_const % ',') > ':' > '(' > field_list_h > ')') % ';')));
			field_list_h.HeldParser = field_list; // complete mutual dependance
			#endregion

			#region expressions
			HoldingParser factor_h = new HoldingParser();
			Parser compares = ((Parser)"<" | "<=" | "=" | "<>" | ">=" | ">" | (Parser)@"#in\s");
			Parser operators = ((Parser)'*' | '/' | @"#div\s" | @"#mod\s" | (Parser)@"#in\s");
			Parser p_term = (factor_h % operators)[act];
			Parser simp_expr = -(!(plus_minus) > (p_term % @"#or\s"));
			Parser p_expression = simp_expr > !(compares > simp_expr);
			Parser p_variable = ident > !(-(('[' > (p_expression % '.') > ']')
										 | ('.' > ident)
										 | '^'));

			Parser fact_array = Atom.Wrap((Parser)'[' > -( ((p_expression%',') > !("..">p_expression))%',' ) > ']');
			Parser factor = (u_const | p_variable | (ident > !('(' > (p_expression % ',') > ')'))
			| ('('>p_expression>')') | (@"#not\s">factor_h) | fact_array );
			factor_h.HeldParser = factor;
			#endregion

			return (factor);

		}
	}
}
