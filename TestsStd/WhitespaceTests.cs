using Gool;
using Gool.Scanners;
using NUnit.Framework;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class WhitespaceTests
{
    private const string Sample =
        """
        There now is your insular city of the Manhattoes, belted round by
        wharves as Indian isles by    coral reefs-commerce surrounds it with her
        surf. Right and left, the    streets take you waterward. Its extreme
        downtown is the battery, where that noble mole is washed by waves, and
        cooled by breezes, which a few hours previous were out of sight of
        land. Look at the crowds of water-gazers there.
        """;

    private IParser MakeParser()
    {
        BNF word = @"#\w+";
        BNF punctuation = @"#[.,\-]";

        BNF word_list = -(word | punctuation);

        word.TagWith("word");
        punctuation.TagWith("punctuation");

        return word_list;
    }

    [Test]
    public void decompose_with_scanner_whitespace_skip()
    {
        var parser = MakeParser();
        var scanner = new ScanStrings(Sample) { SkipWhitespace = true };

        var result = parser.Parse(scanner);

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var taggedTokens = result.TaggedTokensDepthFirst().ToList();

        foreach (var token in taggedTokens)
        {
            if (token.Tag == "word") Console.Write(' ');
            Console.Write(token.Value);
        }
        
        Assert.That(taggedTokens.Count, Is.GreaterThan(42));
    }
}