using Gool;
using NUnit.Framework;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class LineCountingTest
{
    
    private const string Sample =
        """
        There now is your insular city of the Manhattoes,
        belted round by wharves as Indian isles by coral
        reefs; commerce surrounds it with her surf.

        Right and left, the streets take you waterward.
        Its extreme downtown is the battery, where that
        noble mole is washed by waves, and cooled by
        breezes, which a few hours previous were out of
        sight of land.

        Look at the crowds of water-gazers there.
        """;

    private static BNF.Package MakeParser()
    {
        BNF line_text = +BNF.NoneOf('\r', '\n');
        BNF line_end = BNF.LineEnd;

        BNF word_list = -(line_text | line_end);

        line_text.TagWith("text");
        line_end.TagWith("line");

        return word_list.WithOptions(BNF.Options.None);
    }

    [Test]
    public void decompose_with_scanner_whitespace_skip()
    {
        var result = MakeParser().ParseString(Sample);

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var taggedTokens = result.TaggedTokensDepthFirst().ToList();

        var lines = 1; // count the implicit first 'line'
        var notEmpty = 0;
        foreach (var token in taggedTokens)
        {
            if (token.Tag == "line")
            {
                lines++;
            }
            else
            {
                Console.WriteLine(token.Value);
                if (!string.IsNullOrWhiteSpace(token.Value)) notEmpty++;
            }
        }

        Console.WriteLine($"\r\nCounted {lines} lines, of which {notEmpty} are not empty");
    }
}