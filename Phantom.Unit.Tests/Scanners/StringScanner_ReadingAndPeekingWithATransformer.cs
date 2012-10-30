using NUnit.Framework;
using NSubstitute;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_ReadingAndPeekingWithATransformer
	{
		IScanner subject;
		ITransform transformer;
		const string Input = "this is an untransformed input";

		[SetUp]
		public void A_string_scanner_with_a_transformer()
		{
			transformer = Substitute.For<ITransform>();
			transformer.Transform('t').Returns('1');
			transformer.Transform('h').Returns('2');
			transformer.Transform('i').Returns('3');

			subject = new ScanStrings(Input) {Transform = transformer};
		}

		[Test]
		public void Reading_one_character_should_call_the_transformer_and_return_the_resulting_character ()
		{
			var result = subject.Peek();

			transformer.Received().Transform(Input[0]);
			Assert.That(result, Is.EqualTo('1'));
		}

		[Test]
		public void Reading_one_character_and_peeking_transforms_the_character_and_returns_the_second_result()
		{
			subject.Read();
			var result = subject.Peek();

			transformer.Received().Transform(Input[1]);
			Assert.That(result, Is.EqualTo('2'));
		}
	}
}
