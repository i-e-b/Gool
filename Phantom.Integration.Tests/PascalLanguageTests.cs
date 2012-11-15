using System;
using NUnit.Framework;
using Phantom.Scanners;
using SampleGrammars;

namespace Phantom.Integration.Tests
{
	[TestFixture]
	public class PascalLanguageTests
	{
		const string sample_program = 
@"program WriteName;
var
  i:Integer;
begin
  Write('Please tell me your name: ');
  ReadLn(Name);
  for i := 1 to 100 do
  begin
    WriteLn('Hello ', Name)
  end
end.";
		[Test]
		public void BasicPascalProgramParsesOK()
		{
			var parser = new PascalParser().TheParser;
			var scanner = new ScanStrings(sample_program){SkipWhitespace = true};
			scanner.Transform = new TransformToLower();

			var result = parser.Parse(scanner);

			Assert.That(result.Success, Is.True, String.Join("\n\n", scanner.ListFailures()));
			Assert.That(result.Value.ToLower(), Is.EqualTo(sample_program.ToLower()));
		}
	}
}
