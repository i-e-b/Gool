using Phantom;
using System.Text.RegularExpressions;
// ReSharper disable InconsistentNaming

namespace SampleGrammars {
	public class XMLParser {
		public IParser TheParser { get; }

		public XMLParser () {
			TheParser = Xml();
		}

		private RegexOptions ops() {
			return RegexOptions.ExplicitCapture
				| RegexOptions.IgnoreCase
				| RegexOptions.Multiline;
		}

		private IParser Xml()
		{
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
			BNF close_tag     = "</" > identifier > ">";

			text.Tag("text");
			open_tag.Tag("open");
			close_tag.Tag("close");
			
			return BNF.Recursive(tree => !(open_tag > -(tree | text) > close_tag)).Result();
		}
	}
}
