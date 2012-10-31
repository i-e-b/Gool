using System;
using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_ReadingAndSeeking
	{
		IScanner subject;
		const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			subject = new ScanStrings(Input);
		}

		[Test]
		public void offset_starts_at_zero ()
		{
			Assert.That(subject.Offset, Is.EqualTo(0));
		}

		[Test]
		public void reading_increments_offset ()
		{
			for (int i = 1; i < Input.Length; i++)
			{
				subject.Read();
				Assert.That(subject.Offset, Is.EqualTo(i));
			}
		}

		[Test]
		public void offset_stops_at_end_of_input ()
		{
			for (int i = 0; i < Input.Length * 2; i++)
			{
				subject.Read();
			}
			Assert.That(subject.Offset, Is.EqualTo(Input.Length));
		}

		[Test]
		public void seeking_changes_offset ()
		{
			subject.Seek(10);
			Assert.That(subject.Offset, Is.EqualTo(10));
		}

		[Test]
		public void setting_offset_property_is_equivalent_to_seeking ()
		{
			subject.Offset = 10;
			Assert.That(subject.Offset, Is.EqualTo(10));
			Assert.That(subject.Peek(), Is.EqualTo(Input[10]));

			subject.Read();
			Assert.That(subject.Offset, Is.EqualTo(11));
		}

		[Test]
		public void seeking_past_end_of_input_throws_an_exception ()
		{
			var ex = Assert.Throws<Exception>(() => subject.Seek(1000));
			Assert.That(ex.Message, Contains.Substring("Scanner seek offset out of bounds"));
		}

		[Test]
		public void seeking_to_before_start_of_input_throws_an_exception ()
		{
			var ex = Assert.Throws<Exception>(() => subject.Seek(-1000));
			Assert.That(ex.Message, Contains.Substring("Scanner seek offset out of bounds"));
		}

		[Test]
		public void reading_forwards_through_input_returns_true ()
		{
			Assert.IsTrue(subject.Read());
		}

		[Test]
		public void reading_forwards_past_end_of_input_returns_false ()
		{
			subject.Seek(Input.Length);
			Assert.IsFalse(subject.Read());
		}

		[Test]
		public void peeking_does_not_affect_offset ()
		{
			subject.Seek(4);
			for (int i = 0; i < 10; i++)
			{
				subject.Peek();
			}
			Assert.That(subject.Offset, Is.EqualTo(4));
		}
	}
}
