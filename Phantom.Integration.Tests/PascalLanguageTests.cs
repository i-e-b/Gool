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
  i    : Integer;        {variable to be used for looping}
  Name : String;         {declares the variable Name as a string}
begin
  Write('Please tell me your name: ');
  ReadLn(Name);          {ReadLn returns the string entered by the user}
  for i := 1 to 100 do
  begin
    WriteLn('Hello ', Name)
  end
end.";
		[Test, Ignore("Not working -- probably a bad parser definition")]
		public void BasicPascalProgramParsesOK()
		{
			var parser = new PascalParser().TheParser;
			var scanner = new ScanStrings(sample_program);

			var result = parser.Parse(scanner);

			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Assert.That(result.Value, Is.EqualTo(sample_program));
		}
	}
}
