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

			/*
			 * This isn't a serious parser -- it can't handle
			 * real world XML. But it does show off simple 
			 * parsing of a recursive data structure
			 */

			BNF text          = "#[^<>]*";
			BNF identifier    = "#[_a-zA-Z][_a-zA-Z0-9]*";
			BNF quoted_string = "\"" > identifier > "\"";
			BNF attribute     = identifier > "=" > quoted_string;
			BNF open_tag      = "<" > identifier > (!attribute) > ">";
			BNF close_tag     = "</" > identifier > ">"; // TODO: need a way match the `identifier` of `open_tag` and `close_tag`

			return BNF.Recursive(tree =>
				
				!(open_tag > -(tree | text) > close_tag)
				
				).Result();
		}
	}
}
