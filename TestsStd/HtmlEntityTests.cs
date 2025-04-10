using System.Globalization;
using System.Text;
using Gool.Parsers.Composite;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class HtmlEntityTests
{
    private const string Sample =
        """
        &#60;h1&#62;Big&nbsp;Thinks&#60;&#47;h1&#62;&#60;p&#62;&#60;br&#62;&#60;&#47;p&#62;&#60;p&#62;&middot;A place to put grand&ndash; &amp;/or general&ndash;ideas&#44; that don&#39;t yet deserve a place on the backlog.
        This is a broken entity, and should be treated as a plain ampersand: '&'
        This is a broken entity, and should be output verbatim: '&what;'
        """;

    private const string Expected =
        """
        <h1>Big Thinks</h1><p><br></p><p>·A place to put grand- &/or general-ideas, that don't yet deserve a place on the backlog.
        This is a broken entity, and should be treated as a plain ampersand: '&'
        This is a broken entity, and should be output verbatim: '&what;'
        """;


    [Test]
    public void convert_html_to_plain()
    {
        var result = HtmlEntityExample.Parser.ParsePartialString(Sample);

        var sb = new StringBuilder();
        foreach (var match in result.TaggedTokensDepthFirst())
        {
            switch (match.Tag)
            {
                case HtmlEntityExample.Text:
                    sb.Append(match.Value);
                    break;

                case HtmlEntityExample.NamedEntity:
                {
                    var entityName = match.Value;
                    sb.Append(HtmlEntityExample.ConvertByName(entityName));
                    break;
                }

                case HtmlEntityExample.DecimalEntity:
                {
                    if (int.TryParse(match.Value, out var entityCode))
                        sb.Append(char.ConvertFromUtf32(entityCode));
                    break;
                }

                case HtmlEntityExample.HexEntity:
                {
                    if (int.TryParse(match.Value, NumberStyles.HexNumber, null, out var entityCode))
                        sb.Append(char.ConvertFromUtf32(entityCode));
                    break;
                }
            }
        }

        var actual = sb.ToString();
        Console.WriteLine(actual);

        Assert.That(actual, Is.EqualTo(Expected));
    }
}