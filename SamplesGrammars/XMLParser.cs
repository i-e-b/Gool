using Phantom;
using System.Text.RegularExpressions;

namespace SampleGrammars {
	public class XMLParser {
		protected IParser root;

		public IParser TheParser { get { return root; } }

		public XMLParser () {
			root = Xml();
		}

		protected RegexOptions ops() {
			return RegexOptions.ExplicitCapture
				| RegexOptions.IgnoreCase
				| RegexOptions.Multiline;
		}

		protected IParser Xml () {
			BNF.RegexOptions = ops();

			BNF text = "#[^<>]*";
			BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";
			BNF quoted_string = "\"" > identifier > "\"";
			BNF attribute = identifier > "=" > quoted_string;
			BNF open_tag = "<" > identifier > (!attribute) > ">";
			BNF close_tag = "</" > identifier > ">";

			BNF xml_tree = BNF.SelfRecursive(self => !(open_tag > -(self | text) > close_tag));

			return xml_tree.Result();
		}
	}
}
