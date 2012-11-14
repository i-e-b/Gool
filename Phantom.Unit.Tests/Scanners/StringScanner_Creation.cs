using System;
using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_Creation
	{
		[Test]
		public void creating_with_a_string_and_no_offset_starts_at_beginning_and_scans_to_end()
		{
			IScanner subject = new ScanStrings("input");
			Assert.That(subject.EndOfInput, Is.False);
			Assert.That(subject.Peek(), Is.EqualTo('i')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('n')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('p')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('u')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('t')); Assert.That(subject.Read(), Is.False);
			Assert.That(subject.EndOfInput, Is.True);
		}
		
		[Test]
		public void creating_with_a_string_and_offset_starts_at_offset_and_scans_to_end()
		{
			IScanner subject = new ScanStrings("input", 2);
			Assert.That(subject.EndOfInput, Is.False);
			Assert.That(subject.Peek(), Is.EqualTo('p')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('u')); Assert.That(subject.Read(), Is.True);
			Assert.That(subject.Peek(), Is.EqualTo('t')); Assert.That(subject.Read(), Is.False);
			Assert.That(subject.EndOfInput, Is.True);
		}
		
		[Test]
		public void empty_initial_input_is_not_accepted ()
		{
			var ex = Assert.Throws<ArgumentException>(()=>new ScanStrings(""));
			Assert.That(ex.Message, Contains.Substring("Initial input is empty"));
		}
		
		[Test]
		public void null_initial_input_is_not_accepted ()
		{
			var ex = Assert.Throws<ArgumentException>(()=>new ScanStrings(null));
			Assert.That(ex.Message, Contains.Substring("Initial input is empty"));
		}

		[Test]
		public void Default_whitespace_skipping_is_OFF ()
		{
			var subject = new ScanStrings("...");
			Assert.That(subject.SkipWhitespace, Is.False);
		}
	}
}
