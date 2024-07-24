using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanning_Diagnostics
	{
		private IScanner subject;
		private Parser dummy_parser;
		private const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			subject = new ScanStrings(Input);
			dummy_parser = new EmptyMatch();
		}

		[Test]
		public void	BadPatch_gives_a_substring_of_the_requested_length_from_the_rightmost_ParserMatch_created ()
		{
			subject.CreateMatch(dummy_parser, 5, 0);
			Assert.That(subject.BadPatch(5), Is.EqualTo("is my"));
		}

		[Test]
		public void	BadPatch_gives_a_substring_of_the_requested_length_strating_from_zero_if_no_matches_created ()
		{
			Assert.That(subject.BadPatch(5), Is.EqualTo("This "));
		}

		[Test]
		public void stack_stats_traces_the_maximum_recursion_depth_reached ()
		{
			Assert.That(subject.StackStats( 0),  Is.EqualTo( 0));
			Assert.That(subject.StackStats( 2),  Is.EqualTo( 2));
			Assert.That(subject.StackStats( 1),  Is.EqualTo( 2));
			Assert.That(subject.StackStats(10),  Is.EqualTo(10));
			Assert.That(subject.StackStats( 5),  Is.EqualTo(10));
			Assert.That(subject.StackStats( 8),  Is.EqualTo(10));
			Assert.That(subject.StackStats(20),  Is.EqualTo(20));
		}

		[Test]
		public void listing_failures_gives_substring_from_failure_point_to_end_of_line_and_parser_string ()
		{
			subject.AddFailure(dummy_parser, 8);
			subject.AddFailure(dummy_parser, 11);

			Assert.That(subject.ListFailures(), Contains.Item("my input --> "+dummy_parser));
			Assert.That(subject.ListFailures(), Contains.Item("input --> "+dummy_parser));
		}

		[Test]
		public void listing_failures_after_clearing_failures_returns_empty ()
		{
			subject.AddFailure(dummy_parser, 8);
			subject.AddFailure(dummy_parser, 11);
			subject.ClearFailures();
			
			Assert.That(subject.ListFailures(), Is.Empty);
		}

		[Test]
		public void listing_failures_returns_empty_if_no_failures_marked ()
		{
			Assert.That(subject.ListFailures(), Is.Empty);
		}

		[Test]
		public void FurthestMatch_gives_a_string_for_the_scanner_match_with_the_greatest_extent ()
		{
			subject.CreateMatch(dummy_parser, 5, 1);                // extent = 6
			var expected = subject.CreateMatch(dummy_parser, 6, 3); // extent = 9
			subject.CreateMatch(dummy_parser, 1, 7);                // extent = 8

			Assert.That(subject.FurthestMatch(), Is.EqualTo(expected.ToString()));
		}

		[Test]
		public void FurthestMatch_returns_null_if_no_matches_created ()
		{
			Assert.That(subject.FurthestMatch(), Is.Null);
		}

	}
}
