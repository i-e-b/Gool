using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class TerminatedListParserTests
	{

		[Test]
		public void single_item_without_terminator_fails ()
		{
			Assert.Inconclusive();
		}

		[Test]
		public void single_item_with_terminator_passes ()
		{
			Assert.Inconclusive();
		}

		[Test]
		public void multiple_items_with_end_terminator_missing_fails ()
		{
			Assert.Inconclusive();
		}

		[Test]
		public void multiple_items_with_end_terminator_passes ()
		{
			Assert.Inconclusive();
		}

		[Test]
		public void terminator_only_fails ()
		{
			Assert.Inconclusive();
		}

		[Test]
		public void double_terminator_matches_single_item_only()
		{
			Assert.Inconclusive();
		}
	}
}
