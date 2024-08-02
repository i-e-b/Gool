using NUnit.Framework;
using Phantom;
using Phantom.Scanners;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class LispTests
{
    private const string Sample =
        """

        (loop for x in '(1 2 3)
          do (print x))

        """;

    private IParser MakeParser()
    {
        BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";

        BNF atom = ':' > identifier;
        BNF quoted_string = '"' > identifier > '"';
        BNF normal_list = '(';
        BNF quoted_list = "'(";
        BNF end_list = ')';
        
        BNF list_item = identifier.Tagged("Name") | atom | quoted_string;
        BNF start_list = normal_list | quoted_list;

        quoted_list.Tag("Quote");
        atom.Tag("Atom");
        quoted_string.Tag("String");
        normal_list.Tag("(");
        end_list.Tag(")");

        return BNF.Recursive(tree => +(list_item | start_list | end_list | tree)).Result();
    }

    [Test]
    public void decompose_s_expression()
    {
        Console.WriteLine(Sample);

        var parser = MakeParser();
        var scanner = new ScanStrings(Sample) { SkipWhitespace = true };

        var result = parser.Parse(scanner);

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(Sample));

        var taggedTokens = result.TaggedTokens();

        var indent = 0;
        foreach (var token in taggedTokens)
        {
            switch (token.Tag)
            {
                case "Atom":
                    Console.WriteLine(I(indent) + "Atom " + token.Value);
                    break;
                
                case "String":
                    Console.WriteLine(I(indent) + "String " + token.Value);
                    break;

                case "Name":
                    Console.WriteLine(I(indent) + token.Value);
                    break;

                case "Quote":
                    Console.WriteLine(I(indent) + "'(");
                    indent++;
                    break;

                case "(":
                    Console.WriteLine(I(indent) + "(");
                    indent++;
                    break;

                case ")":
                    indent--;
                    Console.WriteLine(I(indent) + ")");
                    break;
            }
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 2);
    }
}