using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_RecursionCheck
	{
		IScanner subject;
		const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			subject = new ScanStrings(Input);
		}

		[Test]
		public void recursion_check_returns_false_if_a_given_object_changes_offset()
		{
			var key = new object();

			for (int i = 0; i < 10; i++)
			{
				var result = subject.RecursionCheck(key, i);
				Assert.IsFalse(result);
			}
		}

		[Test]
		public void recursion_check_returns_true_if_a_given_object_repeats_an_offset()
		{
			var key = new object();

			subject.RecursionCheck(key, 3);
			var result = subject.RecursionCheck(key, 3);
			Assert.IsTrue(result);
		}

		[Test]
		public void recursion_check_treats_each_key_separately ()
		{
			var k1 = new object();
			var k2 = new object();

			Assert.IsFalse(subject.RecursionCheck(k1, 0));
			Assert.IsFalse(subject.RecursionCheck(k2, 0));
			Assert.IsFalse(subject.RecursionCheck(k1, 1));
			Assert.IsTrue(subject.RecursionCheck(k2, 0));
		}

		[Test]
		public void recursion_can_continue_after_returning_true()
		{
			var key = new object();

			for (int i = 0; i < 10; i++)
			{
				Assert.IsFalse(subject.RecursionCheck(key, i));
				Assert.IsTrue(subject.RecursionCheck(key, i));
			}
		}
	}
}
