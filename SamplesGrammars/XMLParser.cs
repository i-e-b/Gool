using Phantom.Parsers;
using System.Text.RegularExpressions;

namespace SampleGrammars {
	public class XMLParser {
		protected Parser root;

		public Parser TheParser { get { return root; } }

		public XMLParser () {
			root = Xml();
		}

		protected RegexOptions ops() {
			return RegexOptions.ExplicitCapture
				| RegexOptions.IgnoreCase
				| RegexOptions.Multiline;
		}

		protected Parser Xml () {
			/*Parser text = ((Parser)"[^<>]*")[ops()];
			Parser identifier = ((Parser)"[_a-zA-Z][_a-zA-Z0-9]*")[ops()];
			Parser quoted_string = "\"" > identifier > "\"";
			Parser attribute = identifier > "=" > quoted_string;
			Parser open_tag = Atom.Wrap("<" > identifier > (!attribute) > ">");
			Parser close_tag = Atom.Wrap("</" > identifier > ">");

			var xml_tree = new HoldingParser();
			Parser content = (open_tag > -(xml_tree | text) > close_tag);
			xml_tree.HeldParser = !content;

			return xml_tree;*/
			return null;
		}
	}
}
