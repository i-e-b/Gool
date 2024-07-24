using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_ReadingAndPeekingWithATransformer
	{
		private IScanner subject;
		private ITransform transformer;
		private const string Input = "this is an untransformed input";

		[SetUp]
		public void A_string_scanner_with_a_transformer()
		{
			/*transformer = Substitute.For<ITransform>();
			transformer.Transform('t').Returns('1');
			transformer.Transform('h').Returns('2');
			transformer.Transform('i').Returns('3');*/

			transformer = new TestTransformer();
			subject = new ScanStrings(Input) {Transform = transformer};
		}

		public class TestTransformer : ITransform
		{
			public string Transform(string s) => s;

			public char Transform(char c)
			{
				switch (c)
				{
					case 't':
						return '1';
					case 'h':
						return '2';
					case 'i':
						return '3';
					default:
						return c;
				}
			}
		}

		[Test]
		public void Reading_one_character_should_call_the_transformer_and_return_the_resulting_character ()
		{
			var result = subject.Peek();

			//transformer.Received().Transform(Input[0]);
			Assert.That(result, Is.EqualTo('1'));
		}

		[Test]
		public void Reading_one_character_and_peeking_transforms_the_character_and_returns_the_second_result()
		{
			subject.Read();
			var result = subject.Peek();

			//transformer.Received().Transform(Input[1]);
			Assert.That(result, Is.EqualTo('2'));
		}
	}
}
