using System.Text.RegularExpressions;
using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

public static class XmlExample
{
    public static readonly BNF.Package Parser = Xml();

    private static RegexOptions Options() => RegexOptions.ExplicitCapture | RegexOptions.IgnoreCase | RegexOptions.Multiline;

    private static BNF.Package Xml()
    {
        BNF.RegexOptions = Options();

        /*
         * This isn't a serious parser -- it can't handle
         * all real world XML. But it does show off simple
         * parsing of a recursive data structure
         */

        BNF text       = BNF.Regex("[^<>]+");
        BNF identifier = BNF.Regex("[_a-zA-Z][_a-zA-Z0-9]*");
        BNF whitespace = BNF.Regex(@"\s+");

        BNF quoted_string = '"' > identifier > '"';
        BNF attribute = whitespace > identifier > '=' > quoted_string;

        BNF tag_id = identifier.Tagged(TagId);
        BNF open_tag = '<' > tag_id > -attribute > '>';

        BNF close_tag = "</" > tag_id > '>';

        attribute.TagWith(Attribute);
        text.TagWith(Text);
        open_tag.TagWith(OpenTag).OpenScope();
        close_tag.TagWith(CloseTag).CloseScope();

        return BNF
            .Recursive(tree => -(open_tag > -(tree | text) > close_tag))
            .WithOptions(BNF.Options.None);
    }

    public const string Text = "text";
    public const string OpenTag = "open";
    public const string CloseTag = "close";
    public const string TagId = "tagId";
    public const string Attribute = "attribute";

}