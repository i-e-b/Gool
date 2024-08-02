using NUnit.Framework;
using Phantom.Scanners;
using Samples;

namespace TestsStd;

[TestFixture]
public class PascalLanguageTests
{
    [Test]
    public void BasicPascalProgramParsesOK()
    {
        var parser = new PascalParser().TheParser;
        var scanner = new ScanStrings(sample_program)
        {
            SkipWhitespace = true,
            Transform = new TransformToLower()
        };

        var result = parser.Parse(scanner);

        Assert.That(result.Success, Is.True, String.Join("\n\n", scanner.ListFailures()));
        Assert.That(result.Value.ToLower(), Is.EqualTo(sample_program.ToLower()));
    }

    [Test]
    [TestCase(missing_quote)]
    [TestCase(missing_begin)]
    public void InvalidProgramFails(string program)
    {
        var parser = new PascalParser().TheParser;
        var scanner = new ScanStrings(missing_quote)
        {
            SkipWhitespace = true,
            Transform = new TransformToLower()
        };

        var result = parser.Parse(scanner);

        Assert.That(result.Success, Is.False);
    }


    private const string sample_program =
        @"program WriteName;
var
  i:Integer;
  j:String;
begin
  Write('Please tell me your name: ');
  ReadLn(Name);
  for i := 1 to 100 do
  begin
    WriteLn('Hello ', Name)
  end
end.";

    private const string missing_begin =
        @"program WriteName;
var
  i:Integer;
  j:String;
begin
  Write('Please tell me your name: ');
  ReadLn(Name);
  for i := 1 to 100 do
    WriteLn('Hello ', Name)
  end
end.";

    private const string missing_quote =
        @"program WriteName;
var
  i:Integer;
  j:String;
begin
  Write('Please tell me your name: );
  ReadLn(Name);
  for i := 1 to 100 do
  begin
    WriteLn('Hello ', Name)
  end
end.";
}