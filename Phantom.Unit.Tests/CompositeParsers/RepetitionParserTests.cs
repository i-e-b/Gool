using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class RepetitionParserTests
	{
		[Test]
		public void input_that_repeats_more_than_parsers_upper_bound_passes_and_captures_only_expected_max_range ()
		{
			var term = new LiteralString("op!");
			var scanner = new ScanStrings("op!op!op!op!op!op!op!op!");
			IParser subject = new Repetition(term, 3, 3);


			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("op!op!op!"));
		}

		[Test]
		public void input_that_repeats_less_than_parsers_lower_bound_fails ()
		{
			var term = new LiteralString("op!");
			var scanner = new ScanStrings("op!op!");
			IParser subject = new Repetition(term, 4, 5);


			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void input_that_repeats_exactly_as_many_as_the_lower_bound_passes_and_captures_all_input ()
		{
			var term = new LiteralString("op!");
			var scanner = new ScanStrings("op!op!op!");
			IParser subject = new Repetition(term, 3, 5);


			var result = subject.Parse(scanner);
			
			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("op!op!op!"));
		}

		[Test]
		public void input_that_repeats_between_the_lower_and_upper_bounds_passes_and_captures_all_input ()
		{
			var term = new LiteralString("op!");
			var scanner = new ScanStrings("op!op!op!");
			IParser subject = new Repetition(term, 1, 5);


			var result = subject.Parse(scanner);
			
			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("op!op!op!"));
		}

		[Test]
		public void input_that_repeats_exactly_as_many_as_the_upper_bound_passes_and_captures_all_input ()
		{
			var term = new LiteralString("op!");
			var scanner = new ScanStrings("op!op!op!");
			IParser subject = new Repetition(term, 1, 3);


			var result = subject.Parse(scanner);
			
			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("op!op!op!"));
		}
	}
}
