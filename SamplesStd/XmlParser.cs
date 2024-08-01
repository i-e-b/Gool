using System.Text.RegularExpressions;
using Phantom;
// ReSharper disable InconsistentNaming

namespace Samples;

public class XmlParser
{
    public IParser TheParser { get; }

    public XmlParser()
    {
        TheParser = Xml();
    }

    private RegexOptions ops()
    {
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

        BNF text = "#[^<>]+";
        BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";
        BNF whitespace = @"#\W+";

        BNF quoted_string = '"' > identifier > '"';
        BNF attribute = whitespace > identifier > '=' > quoted_string;
        //BNF attr_part     = identifier > '=' > quoted_string;
        //BNF attr_list     = !whitespace > (attr_part % whitespace);

        BNF tag_id = identifier.Copy().Tag(TagId);
        BNF open_tag      = '<' > tag_id /*> -attribute*/ > '>';

        BNF close_tag = "</" > tag_id > '>';

        attribute.Tag(Attribute);
        text.Tag(Text);
        open_tag.Tag(OpenTag);
        close_tag.Tag(CloseTag);

        return BNF.Recursive(tree => -(open_tag > -(tree | text) > close_tag)).Result();
    }
}