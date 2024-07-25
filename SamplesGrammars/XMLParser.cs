using Phantom;
using System.Text.RegularExpressions;
using Phantom.Parsers.Composite;

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

		public const string Text = "text";
		public const string OpenTag = "open";
		public const string CloseTag = "close";
		public const string TagId = "tagId";
		public const string Attribute = "attribute";
		
		private IParser Xml()
		{
			BNF.RegexOptions = ops();

			/*
			 * This isn't a serious parser -- it can't handle
			 * all real world XML. But it does show off simple 
			 * parsing of a recursive data structure
			 */

			BNF text          = "#[^<>]+";
			BNF identifier    = "#[_a-zA-Z][_a-zA-Z0-9]*";
			BNF whitespace    = @"#\W+";
			
			BNF quoted_string = '"' > identifier > '"';
			BNF attribute     = whitespace > identifier > '=' > quoted_string;
			//BNF attr_part     = identifier > '=' > quoted_string;
			//BNF attr_list     = !whitespace > (attr_part % whitespace);
			
			BNF tag_id        = identifier.Copy().Tag(TagId);
			//BNF open_tag      = '<' > tag_id > -attribute > '>'; // TODO: this is correct, but is faulting
			//BNF open_tag      = '<' > tag_id > ((BNF)new Repetition(attribute.Result(), 0, 0) > '>'); // passes when given no attrs at root
			BNF open_tag      = '<' > tag_id > ((BNF)new Repetition(attribute.Result(), 0, 2) > '>'); // passes when given 2 attrs or none
			//BNF open_tag      = '<' > tag_id > ((BNF)new Repetition(attribute.Result(), 0, 3) > '>'); // fails when given 2 attrs or none
			//BNF open_tag      = '<' > tag_id > new Repetition(attribute.Result(), 0, 2) > '>'; // fails!
			//BNF open_tag      = '<' > tag_id > !attribute > '>'; // TODO: this is wrong but works
			//BNF open_tag      = '<' > tag_id > /*!attribute >*/ '>'; // TODO: this is wrong but works
			//BNF open_tag      = '<' > tag_id > attr_list > '>'; // ???
			
			BNF close_tag     = "</" > tag_id > '>';

			attribute.Tag(Attribute);
			text.Tag(Text);
			open_tag.Tag(OpenTag);
			close_tag.Tag(CloseTag);
			
			return (open_tag > BNF.Recursive(tree => +(open_tag > -(tree | text) > close_tag)) > close_tag).Result();
		}
	}
}
